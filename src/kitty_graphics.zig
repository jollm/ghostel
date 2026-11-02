/// Kitty Graphics Protocol support via libghostty-vt.
///
/// Queries libghostty's authoritative placement and image state during
/// each redraw cycle, converts pixel data to PPM for Emacs display,
/// and calls into Elisp to apply image overlays.
const std = @import("std");
const Allocator = std.mem.Allocator;
const emacs = @import("emacs.zig");
const GhostelTerm = @import("GhostelTerm.zig");
const gt = @import("ghostty-vt");
const ppm = @import("ppm.zig");

/// Query all visible kitty graphics placements from libghostty and
/// emit them to Elisp during redraw.
///
/// Programs that animate redraw a cell every frame.  Each frame is a
/// fresh image with an implicit placement id, so libghostty accumulates
/// one placement per frame at the same cell.  Emitting every one of them
/// costs time linear in playback length, and zero-width rows pile up a
/// stack of overlays.  Keep only the newest internal placement per screen
/// footprint for this redraw; explicit placement ids are updated in place
/// and therefore never duplicate, and virtual placements are drawn from
/// their placeholder runs.
pub fn emitPlacements(env: emacs.Env, term: *GhostelTerm) !void {
    const storage = &term.terminal.screens.active.kitty_images;
    const pages = &term.terminal.screens.active.pages;
    var newest: std.AutoHashMapUnmanaged(CellKey, DedupedPlacement) = .empty;
    defer newest.deinit(term.alloc);

    var iterator = storage.placements.iterator();
    while (iterator.next()) |entry| {
        const key = entry.key_ptr.*;
        const placement = entry.value_ptr;
        const image = storage.images.getPtr(key.image_id) orelse continue;
        const anchor = resolveAnchor(storage, placement) orelse continue;
        if (key.placement_id.tag != .internal) {
            emitPinned(env, term, image, placement, anchor.pin, anchor.h_off, anchor.v_off) catch continue;
            continue;
        }
        // A pruned anchor page leaves the pin parked at (0,0).
        if (anchor.pin.garbage) continue;
        const pin_screen = pages.pointFromPin(.screen, anchor.pin.*) orelse continue;
        const grid = placement.gridSize(image.*, &term.terminal);
        const cell = CellKey{
            .row = @as(i64, @intCast(pin_screen.screen.y)) + anchor.v_off,
            .col = @as(i64, @intCast(pin_screen.screen.x)) + anchor.h_off,
            .rows = grid.rows,
            .cols = grid.cols,
        };
        const gop = try newest.getOrPut(term.alloc, cell);
        if (!gop.found_existing or key.placement_id.id > gop.value_ptr.placement_id) {
            gop.value_ptr.* = .{
                .image_id = key.image_id,
                .placement = placement,
                .pin = anchor.pin,
                .h_off = anchor.h_off,
                .v_off = anchor.v_off,
                .placement_id = key.placement_id.id,
            };
        }
    }

    var remaining = newest.valueIterator();
    while (remaining.next()) |entry| {
        const image = storage.images.getPtr(entry.image_id) orelse continue;
        emitPinned(env, term, image, entry.placement, entry.pin, entry.h_off, entry.v_off) catch continue;
    }

    emitVirtualRuns(env, term, storage);
}

const PlacementIterator = struct {
    row_it: gt.PageList.RowIterator,
    row: ?gt.Pin,

    const Run = struct {
        placement: gt.kitty.graphics.unicode.IncompletePlacement,
        pin: gt.Pin,

        fn complete(self: Run) gt.kitty.graphics.unicode.Placement {
            return .{
                .pin = self.pin,
                .image_id = self.placement.imageId(),
                .placement_id = self.placement.placement_id orelse 0,
                .col = self.placement.col orelse 0,
                .row = self.placement.row orelse 0,
                .width = self.placement.width,
                .height = 1,
            };
        }
    };

    fn init(pin: gt.Pin, limit: ?gt.Pin) PlacementIterator {
        var row_it = pin.rowIterator(.right_down, limit);
        const row = row_it.next();
        return .{ .row_it = row_it, .row = row };
    }

    fn next(self: *PlacementIterator) ?gt.kitty.graphics.unicode.Placement {
        while (self.row) |*row| {
            if (!row.rowAndCell().row.kitty_virtual_placeholder) {
                self.row = self.row_it.next();
                continue;
            }

            var run: ?Run = null;
            const cells = row.cells(.right);
            for (cells, row.x..) |*cell, x| {
                row.x = @intCast(x);

                if (cell.codepoint() != gt.kitty.graphics.unicode.placeholder) {
                    if (run) |prev| return prev.complete();
                    continue;
                }

                const style = row.style(cell);
                const placement = gt.kitty.graphics.unicode.IncompletePlacement.init(
                    &style,
                    row.grapheme(cell) orelse &.{},
                );
                if (run) |*prev| {
                    if (!prev.placement.append(&placement)) return prev.complete();
                } else {
                    run = .{ .placement = placement.start(), .pin = row.* };
                }
            }

            self.row = self.row_it.next();
            if (run) |prev| return prev.complete();
        }

        return null;
    }
};

fn placementIterator(pin: gt.Pin, limit: ?gt.Pin) PlacementIterator {
    return .init(pin, limit);
}

/// Emit one Elisp call per unicode placeholder run from the row this
/// redraw's render started at down to the last active row, covering rows
/// that left the active area since the previous redraw.
fn emitVirtualRuns(env: emacs.Env, term: *GhostelTerm, storage: *const gt.kitty.graphics.ImageStorage) void {
    if (storage.placements.count() == 0) return;
    const pages = &term.terminal.screens.active.pages;
    const top = term.renderer.rendered_from orelse return;
    const bottom = pages.getBottomRight(.active) orelse return;
    const bottom_y = (pages.pointFromPin(.screen, bottom) orelse return).screen.y;
    var it = placementIterator(top, bottom);
    // Image data handed to Emacs, per image id, for this redraw only.
    var images: std.AutoHashMapUnmanaged(u32, emacs.Value) = .empty;
    defer images.deinit(term.alloc);
    // Per-run errors skip that run only.
    while (it.next()) |run| {
        emitVirtualRun(env, term, storage, &run, bottom_y, &images) catch continue;
    }
}

fn emitVirtualRun(
    env: emacs.Env,
    term: *GhostelTerm,
    storage: *const gt.kitty.graphics.ImageStorage,
    run: *const gt.kitty.graphics.unicode.Placement,
    bottom_y: u32,
    images: *std.AutoHashMapUnmanaged(u32, emacs.Value),
) !void {
    const image = storage.images.getPtr(run.image_id) orelse return error.ImageNotFound;
    const target = storage.placeholderTarget(run.image_id, run.placement_id) orelse return error.PlacementNotFound;
    if (target.placement.location != .virtual) return error.PlacementNotFound;

    const t = &term.terminal;
    const grid_cols = if (target.placement.columns > 0) target.placement.columns else try cellsFor(image.width, t.width_px, t.cols);
    const grid_rows = if (target.placement.rows > 0) target.placement.rows else try cellsFor(image.height, t.height_px, t.rows);

    // Elisp locates the run by counting placeholders on its row.
    var ordinal: u32 = 0;
    for (run.pin.cells(.left)[0..run.pin.x]) |*cell| {
        if (cell.codepoint() == gt.kitty.graphics.unicode.placeholder) ordinal += 1;
    }
    const pin_screen = t.screens.active.pages.pointFromPin(.screen, run.pin) orelse return error.NotVisible;

    const val = images.get(run.image_id) orelse val: {
        const data = try getImageData(term.alloc, image, ppm.Rect.full(image.width, image.height));
        defer term.alloc.free(data);
        const v = env.makeUnibyteString(data) orelse return error.MakeString;
        try images.put(term.alloc, run.image_id, v);
        break :val v;
    };
    _ = env.f("ghostel--kitty-display-virtual", .{
        val,
        bottom_y - pin_screen.screen.y,
        ordinal,
        run.row,
        run.col,
        run.width,
        grid_cols,
        grid_rows,
    });
}

/// Cells covering `px` pixels at the terminal's cell pitch.
fn cellsFor(px: u32, total_px: u32, count: usize) !u32 {
    const cell: u32 = @intCast(total_px / count);
    if (cell == 0) return error.NoCellSize;
    return (px + cell - 1) / cell;
}

/// Anchor of a pinned placement: the screen pin plus any relative offsets.
const Anchor = struct {
    pin: *const gt.PageList.Pin,
    h_off: i32,
    v_off: i32,
};

/// Screen cell and footprint identifying where a placement lands.
const CellKey = struct {
    row: i64,
    col: i64,
    rows: u32,
    cols: u32,
};

/// Newest placement kept for one screen footprint.
const DedupedPlacement = struct {
    image_id: u32,
    placement: *const gt.kitty.graphics.ImageStorage.Placement,
    pin: *const gt.PageList.Pin,
    h_off: i32,
    v_off: i32,
    placement_id: u32,
};

/// Resolve PLACEMENT to the pin and offsets it draws from.  Virtual
/// placements are drawn from their placeholder runs and have no single
/// anchor; virtual roots under relative placements are not implemented.
fn resolveAnchor(
    storage: *const gt.kitty.graphics.ImageStorage,
    placement: *const gt.kitty.graphics.ImageStorage.Placement,
) ?Anchor {
    return switch (placement.location) {
        .virtual => null,
        .pin => |pin| .{ .pin = pin, .h_off = 0, .v_off = 0 },
        .relative => |rel| blk: {
            const chain = storage.resolveChain(rel) orelse break :blk null;
            const pin = switch (chain.root.location) {
                .pin => |root_pin| root_pin,
                .virtual, .relative => break :blk null,
            };
            break :blk .{
                .pin = pin,
                .h_off = chain.horizontal_offset,
                .v_off = chain.vertical_offset,
            };
        },
    };
}

/// Emit a placement anchored at PIN, offset by H_OFF/V_OFF cells.
fn emitPinned(
    env: emacs.Env,
    term: *GhostelTerm,
    image: *const gt.kitty.graphics.Image,
    placement: *const gt.kitty.graphics.ImageStorage.Placement,
    pin: *const gt.PageList.Pin,
    h_off: i32,
    v_off: i32,
) !void {
    const pixel_size = placement.pixelSize(image.*, &term.terminal);
    const grid_size = placement.gridSize(image.*, &term.terminal);
    const pages = &term.terminal.screens.active.pages;
    // A pruned anchor page leaves the pin parked at (0,0) until reaped.
    if (pin.garbage) return error.NotVisible;
    const pin_screen = pages.pointFromPin(.screen, pin.*) orelse return error.NotVisible;
    const active_tl = pages.getTopLeft(.active);
    const active_screen = pages.pointFromPin(.screen, active_tl) orelse return error.NotVisible;
    // i64: protocol offsets and grid sizes are unbounded.  Elisp clips negatives.
    const screen_row: i64 = @as(i64, @intCast(pin_screen.screen.y)) + v_off;
    const active_row: i64 = screen_row - @as(i64, @intCast(active_screen.screen.y));
    const active_col: i64 = @as(i64, @intCast(pin_screen.screen.x)) + h_off;
    const visible = active_row + grid_size.rows > 0 and active_row < term.terminal.rows and
        active_col + grid_size.cols > 0 and active_col < term.terminal.cols;

    if (!visible) return error.NotVisible;

    // Emacs crops only post-scale, so crop before it sees the pixels.
    const src = placement.sourceRect(image.*);
    const data = try getImageData(term.alloc, image, .{ .x = src.x, .y = src.y, .width = src.width, .height = src.height });
    defer term.alloc.free(data);

    const img_val = env.makeUnibyteString(data) orelse return error.MakeString;
    _ = env.f("ghostel--kitty-display-image", .{
        img_val,
        screen_row,
        active_col,
        grid_size.cols,
        grid_size.rows,
        pixel_size.width,
        pixel_size.height,
    });
}

/// PPM bytes of the RECT sub-rectangle of IMAGE, owned by `alloc`.
fn getImageData(alloc: Allocator, image: *const gt.kitty.graphics.Image, rect: ppm.Rect) ![]const u8 {
    // Decompression happens at transmit time; anything else is a libghostty change.
    if (image.compression != .none) return error.UnsupportedCompression;
    // Chunked transmissions still in flight have no complete bytes yet.
    const data = image.renderData().bytes() orelse return error.EmptyImage;
    if (data.len == 0 or image.width == 0 or image.height == 0) return error.EmptyImage;
    // Alpha is dropped, not composited (see ppm.createPpm doc comment).
    // PNG is decoded to RGBA at transmit time by the hook in module.zig.
    const channels: u32 = switch (image.format) {
        .png => unreachable,
        .rgba => 4,
        .rgb => 3,
        .gray_alpha => 2,
        .gray => 1,
    };
    return ppm.createPpm(alloc, data, image.width, image.height, channels, rect) orelse error.PpmConvert;
}
