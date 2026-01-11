const std = @import("std");
const errors = @import("errors.zig");
const params = @import("params.zig");
const route = @import("route.zig");

const Allocator = std.mem.Allocator;
const MatchError = errors.MatchError;
const InsertError = errors.InsertError;
const InsertResult = errors.InsertResult;
const MergeError = errors.MergeError;
const MergeResult = errors.MergeResult;
const ParamRemapping = params.ParamRemapping;
const Params = params.Params;
const remappingDeinit = params.remappingDeinit;
const remappingEqual = params.remappingEqual;
const normalizeParams = params.normalizeParams;
const denormalizeParams = params.denormalizeParams;
const UnescapedRoute = route.UnescapedRoute;
const UnescapedRef = route.UnescapedRef;
const findWildcard = route.findWildcard;
const segmentTerminator = route.segmentTerminator;
const isEmptyOrSlash = route.isEmptyOrSlash;
const verify_enabled = std.debug.runtime_safety;

/// Result of a successful match. Owns its Params; call deinit when done.
/// The value pointer is borrowed from the router and is invalidated by router
/// mutations (insert/remove/merge) or deinit.
pub fn Match(comptime T: type) type {
    return struct {
        const Self = @This();
        value: *const T,
        params: Params,

        pub fn deinit(self: *Self) void {
            self.params.deinit();
            self.* = undefined;
        }
    };
}

/// Mutable result of a successful match. Owns its Params; call deinit when done.
/// The value pointer is borrowed from the router and is invalidated by router
/// mutations (insert/remove/merge) or deinit.
pub fn MatchMut(comptime T: type) type {
    return struct {
        const Self = @This();
        value: *T,
        params: Params,

        pub fn deinit(self: *Self) void {
            self.params.deinit();
            self.* = undefined;
        }
    };
}

const NodeType = union(enum) {
    Root,
    Param: struct { suffix: bool },
    CatchAll,
    Static,
};

pub fn Router(comptime T: type) type {
    return struct {
        const Self = @This();
        pub const Error = Allocator.Error || MatchError;
        const Node = struct {
            const SelfNode = @This();
            const ChildEntry = struct {
                index: u8,
                node: *SelfNode,
            };

            prefix: UnescapedRoute = .{},
            priority: u32 = 0,
            wild_child: bool = false,
            node_type: NodeType = .Static,
            children: std.MultiArrayList(ChildEntry) = .{},
            value: ?T = null,
            remapping: ParamRemapping = .{},

            const InsertState = struct {
                parent: *SelfNode,
                child: ?usize,

                fn parentNode(self: *const InsertState) ?*SelfNode {
                    return if (self.child == null) null else self.parent;
                }

                fn node(self: *const InsertState) *const SelfNode {
                    if (self.child) |i| {
                        return self.parent.childNode(i);
                    }
                    return self.parent;
                }

                fn nodeMut(self: *InsertState) *SelfNode {
                    if (self.child) |i| {
                        return self.parent.childNode(i);
                    }
                    return self.parent;
                }

                fn setChild(self: *const InsertState, i: usize) InsertState {
                    if (self.child) |prev| {
                        return .{
                            .parent = self.parent.childNode(prev),
                            .child = i,
                        };
                    }

                    return .{
                        .parent = self.parent,
                        .child = i,
                    };
                }
            };

            const Skipped = struct {
                node: *const SelfNode,
                path: []const u8,
                params_len: usize,
            };

            fn initEmpty() SelfNode {
                return .{};
            }

            fn create(allocator: Allocator) Allocator.Error!*SelfNode {
                const node = try allocator.create(SelfNode);
                node.* = SelfNode.initEmpty();
                return node;
            }

            fn deinit(self: *SelfNode, allocator: Allocator) void {
                for (self.childNodes()) |child| {
                    child.destroy(allocator);
                }
                self.children.deinit(allocator);
                self.prefix.deinit(allocator);
                remappingDeinit(&self.remapping, allocator);
            }

            fn destroy(self: *SelfNode, allocator: Allocator) void {
                self.deinit(allocator);
                allocator.destroy(self);
            }

            fn childNodes(self: *const SelfNode) []*SelfNode {
                return self.children.slice().items(.node);
            }

            fn childIndices(self: *const SelfNode) []u8 {
                return self.children.slice().items(.index);
            }

            fn childCount(self: *const SelfNode) usize {
                return self.children.len;
            }

            fn staticCount(self: *const SelfNode) usize {
                if (self.wild_child and self.children.len > 0) {
                    return self.children.len - 1;
                }
                return self.children.len;
            }

            fn childNode(self: *const SelfNode, index: usize) *SelfNode {
                return self.childNodes()[index];
            }

            fn hasLiteralSuffix(self: *const SelfNode) bool {
                switch (self.node_type) {
                    .Param => {},
                    else => return false,
                }

                for (self.childNodes()) |child| {
                    if (child.node_type != .Static) continue;
                    if (!isEmptyOrSlash(child.prefix.unescaped())) return true;
                }

                return false;
            }

            fn verifyNodeType(self: *const SelfNode) void {
                const prefix_bytes = self.prefix.unescaped();
                switch (self.node_type) {
                    .Param => {
                        std.debug.assert(prefix_bytes.len >= 3);
                        std.debug.assert(prefix_bytes[0] == '{');
                        std.debug.assert(prefix_bytes[prefix_bytes.len - 1] == '}');
                        std.debug.assert(std.mem.indexOfScalar(u8, prefix_bytes, '/') == null);
                    },
                    .CatchAll => {
                        std.debug.assert(prefix_bytes.len >= 4);
                        std.debug.assert(prefix_bytes[0] == '{');
                        std.debug.assert(prefix_bytes[1] == '*');
                        std.debug.assert(prefix_bytes[prefix_bytes.len - 1] == '}');
                        std.debug.assert(std.mem.indexOfScalar(u8, prefix_bytes, '/') == null);
                        std.debug.assert(self.childCount() == 0);
                    },
                    else => {},
                }
            }

            fn verify(self: *const SelfNode) void {
                self.verifyNodeType();

                const children_len = self.childCount();
                const static_count = self.staticCount();
                std.debug.assert(static_count <= children_len);

                if (self.wild_child) {
                    std.debug.assert(children_len > 0);
                    std.debug.assert(static_count + 1 == children_len);
                    const wild_child = self.childNode(children_len - 1);
                    switch (wild_child.node_type) {
                        .Param, .CatchAll => {},
                        else => std.debug.assert(false),
                    }
                    std.debug.assert(self.childIndices()[children_len - 1] == 0);
                } else {
                    std.debug.assert(static_count == children_len);
                }

                const indices = self.childIndices();
                var i: usize = 0;
                while (i < static_count) : (i += 1) {
                    const child = self.childNode(i);
                    switch (child.node_type) {
                        .Param, .CatchAll => std.debug.assert(false),
                        else => {},
                    }

                    const index_byte = indices[i];
                    const child_prefix = child.prefix.unescaped();
                    std.debug.assert(child_prefix.len > 0);
                    if (index_byte == 0) {
                        switch (self.node_type) {
                            .Param => |param| std.debug.assert(param.suffix),
                            else => std.debug.assert(false),
                        }
                    } else {
                        std.debug.assert(child_prefix[0] == index_byte);
                    }
                }

                for (self.childNodes()) |child| {
                    child.verify();
                }
            }

            fn refEql(value: UnescapedRef, other: *const UnescapedRoute) bool {
                if (value.inner.len != other.len()) return false;
                if (!std.mem.eql(u8, value.inner, other.unescaped())) return false;
                var i: usize = 0;
                while (i < value.inner.len) : (i += 1) {
                    if (value.isEscaped(i) != other.isEscaped(i)) return false;
                }
                return true;
            }

            fn validateRoute(route_value: UnescapedRef) ?InsertError {
                var remaining = route_value;
                while (true) {
                    const wildcard_opt = findWildcard(remaining) catch return .{ .InvalidParam = {} };
                    if (wildcard_opt == null) return null;
                    const wildcard = wildcard_opt.?;

                    if (remaining.inner[wildcard.start + 1] == '*') {
                        if (wildcard.end != remaining.inner.len) {
                            return .{ .InvalidCatchAll = {} };
                        }
                        return null;
                    }

                    const from_wildcard = remaining.sliceOff(wildcard.start);
                    const terminator = segmentTerminator(from_wildcard.inner);
                    const wildcard_len = wildcard.len();
                    const suffix = from_wildcard.sliceUntil(terminator).sliceOff(wildcard_len);

                    const suffix_wildcard = findWildcard(suffix) catch return .{ .InvalidParam = {} };
                    if (suffix_wildcard != null) return .{ .InvalidParamSegment = {} };

                    remaining = from_wildcard.sliceOff(terminator);
                    if (remaining.inner.len == 0) return null;
                }
            }

            fn conflict(
                allocator: Allocator,
                route_value: *const UnescapedRoute,
                prefix: UnescapedRef,
                current: *const SelfNode,
            ) Allocator.Error!InsertError {
                var conflict_route = try route_value.clone(allocator);
                defer conflict_route.deinit(allocator);

                if (std.mem.eql(u8, prefix.unescaped(), current.prefix.unescaped())) {
                    try denormalizeParams(allocator, &conflict_route, &current.remapping);
                    const with = try conflict_route.toOwnedUnescaped(allocator);
                    return .{ .Conflict = .{ .bytes = with } };
                }

                const prefix_len = prefix.inner.len;
                conflict_route.truncate(conflict_route.len() - prefix_len);

                if (!std.mem.endsWith(u8, conflict_route.unescaped(), current.prefix.unescaped())) {
                    try conflict_route.append(allocator, &current.prefix);
                }

                var child_opt: ?*const SelfNode = if (current.childCount() > 0)
                    current.childNode(0)
                else
                    null;

                while (child_opt) |child| {
                    try conflict_route.append(allocator, &child.prefix);
                    child_opt = if (child.childCount() > 0)
                        child.childNode(0)
                    else
                        null;
                }

                var last = current;
                while (last.childCount() > 0) {
                    last = last.childNode(0);
                }

                try denormalizeParams(allocator, &conflict_route, &last.remapping);
                const with = try conflict_route.toOwnedUnescaped(allocator);
                return .{ .Conflict = .{ .bytes = with } };
            }

            fn prefixWildChildInSegment(self: *const SelfNode) bool {
                if (self.node_type == .Root and self.prefix.len() == 0) return false;

                if (self.prefix.unescaped().len != 0 and
                    self.prefix.unescaped()[self.prefix.unescaped().len - 1] == '/')
                {
                    for (self.childNodes()) |child| {
                        if (child.prefixWildChildInSegment()) return true;
                    }
                    return false;
                }

                for (self.childNodes()) |child| {
                    if (child.wildChildInSegment()) return true;
                }
                return false;
            }

            fn wildChildInSegment(self: *const SelfNode) bool {
                if (std.mem.indexOfScalar(u8, self.prefix.unescaped(), '/') != null) return false;

                if (self.node_type == .Param) return true;

                for (self.childNodes()) |child| {
                    if (child.wildChildInSegment()) return true;
                }
                return false;
            }

            fn suffixWildChildInSegment(self: *const SelfNode) bool {
                if (self.hasLiteralSuffix()) return true;

                for (self.childNodes()) |child| {
                    if (std.mem.indexOfScalar(u8, child.prefix.unescaped(), '/') != null) {
                        continue;
                    }
                    if (child.suffixWildChildInSegment()) return true;
                }

                return false;
            }

            fn addChild(
                self: *SelfNode,
                allocator: Allocator,
                child: *SelfNode,
            ) Allocator.Error!usize {
                const entry = ChildEntry{ .index = 0, .node = child };
                const len = self.children.len;
                if (self.wild_child and len > 0) {
                    try self.children.insert(allocator, len - 1, entry);
                    return len - 1;
                }

                try self.children.append(allocator, entry);
                return len;
            }

            fn addSuffixChild(
                self: *SelfNode,
                allocator: Allocator,
                child: *SelfNode,
            ) Allocator.Error!usize {
                var i: usize = 0;
                const nodes = self.childNodes();
                while (i < nodes.len) : (i += 1) {
                    if (nodes[i].prefix.len() < child.prefix.len()) {
                        break;
                    }
                }

                try self.children.insert(allocator, i, .{ .index = 0, .node = child });
                return i;
            }

            fn addStaticChild(
                self: *SelfNode,
                allocator: Allocator,
                index_byte: u8,
                child: *SelfNode,
            ) Allocator.Error!usize {
                try self.children.ensureUnusedCapacity(allocator, 1);

                const len = self.children.len;
                const insert_at = if (self.wild_child and len > 0) len - 1 else len;

                if (insert_at < len) {
                    self.children.insertAssumeCapacity(insert_at, .{ .index = index_byte, .node = child });
                } else {
                    self.children.appendAssumeCapacity(.{ .index = index_byte, .node = child });
                }

                return insert_at;
            }

            fn updateChildPriority(self: *SelfNode, i: usize) usize {
                const slice = self.children.slice();
                const nodes = slice.items(.node);
                const indices = slice.items(.index);
                nodes[i].priority += 1;
                const priority = nodes[i].priority;

                var updated = i;
                while (updated > 0 and nodes[updated - 1].priority < priority) {
                    std.mem.swap(*SelfNode, &nodes[updated - 1], &nodes[updated]);
                    std.mem.swap(u8, &indices[updated - 1], &indices[updated]);
                    updated -= 1;
                }

                return updated;
            }

            const InsertAction = union(enum) {
                continue_walk,
                done: ?InsertError,
            };

            fn commonPrefixLen(remaining: UnescapedRef, node: *const SelfNode) usize {
                const len = if (remaining.inner.len < node.prefix.len())
                    remaining.inner.len
                else
                    node.prefix.len();

                var common_prefix: usize = len;
                var i: usize = 0;
                while (i < len) : (i += 1) {
                    if (remaining.inner[i] != node.prefix.inner.items[i] or
                        remaining.isEscaped(i) != node.prefix.isEscaped(i))
                    {
                        common_prefix = i;
                        break;
                    }
                }

                return common_prefix;
            }

            fn splitNodeAtCommonPrefix(
                state: *InsertState,
                allocator: Allocator,
                common_prefix: usize,
            ) Allocator.Error!bool {
                const node = state.node();
                if (node.prefix.len() <= common_prefix) return false;

                var node_mut = state.nodeMut();
                const prefix_ref = node_mut.prefix.asRef();
                const index_byte = prefix_ref.inner[common_prefix];

                const child = try SelfNode.create(allocator);
                var child_owned = true;
                errdefer if (child_owned) child.destroy(allocator);
                child.prefix = try prefix_ref.sliceOff(common_prefix).toOwned(allocator);
                const new_prefix = try prefix_ref.sliceUntil(common_prefix).toOwned(allocator);
                child.value = node_mut.value;
                node_mut.value = null;
                child.wild_child = node_mut.wild_child;
                node_mut.wild_child = false;
                child.children = node_mut.children;
                node_mut.children = .{};
                child.remapping = node_mut.remapping;
                node_mut.remapping = .{};
                child.priority = node_mut.priority - 1;
                child.node_type = .Static;

                node_mut.prefix.deinit(allocator);
                node_mut.prefix = new_prefix;
                node_mut.children = .{};
                _ = try node_mut.addStaticChild(allocator, index_byte, child);
                child_owned = false;
                return true;
            }

            fn finishInsertAtPrefix(
                state: *InsertState,
                allocator: Allocator,
                route_value: *const UnescapedRoute,
                remaining: UnescapedRef,
                value: T,
                remapping: *ParamRemapping,
            ) Allocator.Error!?InsertError {
                var node_mut = state.nodeMut();
                if (node_mut.value != null) {
                    const err = try conflict(allocator, route_value, remaining, node_mut);
                    return err;
                }

                node_mut.value = value;
                remappingDeinit(&node_mut.remapping, allocator);
                node_mut.remapping = remapping.*;
                remapping.* = .{};
                return null;
            }

            fn handleParamInsert(
                state: *InsertState,
                allocator: Allocator,
                route_value: *const UnescapedRoute,
                common_remaining: UnescapedRef,
                remaining: *UnescapedRef,
                value: T,
                remapping: *ParamRemapping,
                param_suffix: bool,
            ) Allocator.Error!InsertAction {
                var rem = remaining.*;
                const node = state.node();

                const terminator = segmentTerminator(rem.inner);
                const suffix = rem.sliceUntil(terminator);

                var extra_trailing_slash = false;
                const child_nodes = node.childNodes();
                var child_idx: usize = 0;
                while (child_idx < child_nodes.len) : (child_idx += 1) {
                    const child = child_nodes[child_idx];
                    if (std.mem.eql(u8, child.prefix.unescaped(), suffix.unescaped())) {
                        state.* = state.setChild(child_idx);
                        state.nodeMut().priority += 1;
                        remaining.* = rem;
                        return .continue_walk;
                    }

                    if (child.prefix.len() <= suffix.inner.len) {
                        const child_prefix = child.prefix.unescaped();
                        const common = suffix.unescaped()[0..child_prefix.len];
                        const remaining_suffix = suffix.unescaped()[child_prefix.len..];
                        if (std.mem.eql(u8, common, child_prefix) and
                            std.mem.eql(u8, remaining_suffix, "/"))
                        {
                            extra_trailing_slash = true;
                        }
                    }
                }

                if (!extra_trailing_slash and !isEmptyOrSlash(suffix.unescaped())) {
                    if (state.parentNode()) |parent| {
                        // Avoid ambiguous matches where a param suffix would overlap
                        // a wildcard already present in the same path segment.
                        if (parent.prefixWildChildInSegment()) {
                            const err = try conflict(allocator, route_value, common_remaining, parent);
                            return .{ .done = err };
                        }
                    }
                }

                const suffix_wildcard = findWildcard(suffix) catch null;
                if (suffix_wildcard != null) {
                    return .{ .done = .{ .InvalidParamSegment = {} } };
                }

                const child = try SelfNode.create(allocator);
                var child_owned = true;
                errdefer if (child_owned) child.destroy(allocator);
                child.prefix = try suffix.toOwned(allocator);
                child.node_type = .Static;
                child.priority = 1;

                const suffix_index = try state.nodeMut().addSuffixChild(allocator, child);
                child_owned = false;

                const has_suffix = param_suffix or suffix.inner.len != 0;
                state.nodeMut().node_type = .{ .Param = .{ .suffix = has_suffix } };
                state.* = state.setChild(suffix_index);

                if (terminator == rem.inner.len) {
                    var node_mut = state.nodeMut();
                    node_mut.value = value;
                    remappingDeinit(&node_mut.remapping, allocator);
                    node_mut.remapping = remapping.*;
                    remapping.* = .{};
                    remaining.* = rem;
                    return .{ .done = null };
                }

                rem = rem.sliceOff(terminator);

                if (rem.inner[0] != '{' or rem.isEscaped(0)) {
                    const static_child = try SelfNode.create(allocator);
                    var static_owned = true;
                    errdefer if (static_owned) static_child.destroy(allocator);
                    static_child.node_type = .Static;
                    static_child.priority = 1;
                    const static_index = try state.nodeMut().addStaticChild(
                        allocator,
                        rem.inner[0],
                        static_child,
                    );
                    static_owned = false;
                    state.* = state.setChild(static_index);
                }

                if (try state.nodeMut().insertRoute(allocator, rem, value, remapping)) |err| {
                    remaining.* = rem;
                    return .{ .done = err };
                }

                remaining.* = rem;
                return .{ .done = null };
            }

            fn tryAdvanceStaticChild(state: *InsertState, remaining: UnescapedRef) bool {
                const next = remaining.inner[0];
                const node = state.node();
                var idx: usize = 0;
                const indices = node.childIndices();
                const static_count = node.staticCount();
                while (idx < static_count) : (idx += 1) {
                    if (next == indices[idx]) {
                        if ((next == '{' or next == '}') and !remaining.isEscaped(0)) {
                            continue;
                        }

                        const updated = state.nodeMut().updateChildPriority(idx);
                        state.* = state.setChild(updated);
                        return true;
                    }
                }

                return false;
            }

            fn insertNewStaticChild(
                state: *InsertState,
                allocator: Allocator,
                route_value: *const UnescapedRoute,
                common_remaining: UnescapedRef,
                remaining: UnescapedRef,
                value: T,
                remapping: *ParamRemapping,
            ) Allocator.Error!?InsertError {
                var node_mut = state.nodeMut();
                const terminator = segmentTerminator(remaining.inner);
                const segment = remaining.sliceUntil(terminator);
                const wildcard_opt = findWildcard(segment) catch null;
                if (wildcard_opt) |wildcard| {
                    // A wildcard in the middle of a static segment conflicts with any
                    // existing param suffix in that segment.
                    if (wildcard.start > 0 and node_mut.suffixWildChildInSegment()) {
                        const err = try conflict(allocator, route_value, remaining, node_mut);
                        return err;
                    }

                    const suffix = segment.sliceOff(wildcard.end);
                    // If the wildcard is followed by static text, it can't coexist with
                    // another wildcard earlier in the same segment.
                    if (!isEmptyOrSlash(suffix.unescaped()) and node_mut.prefixWildChildInSegment()) {
                        const err = try conflict(allocator, route_value, common_remaining, node_mut);
                        return err;
                    }
                }

                const child = try SelfNode.create(allocator);
                var child_owned = true;
                errdefer if (child_owned) child.destroy(allocator);
                const child_index = try node_mut.addStaticChild(allocator, remaining.inner[0], child);
                child_owned = false;
                const updated = node_mut.updateChildPriority(child_index);

                if (try node_mut.childNode(updated).insertRoute(allocator, remaining, value, remapping)) |err| {
                    return err;
                }

                return null;
            }

            fn advanceWildChild(
                state: *InsertState,
                allocator: Allocator,
                route_value: *const UnescapedRoute,
                remaining: UnescapedRef,
            ) Allocator.Error!?InsertError {
                const wild_child = state.node().childCount() - 1;
                state.* = state.setChild(wild_child);
                state.nodeMut().priority += 1;

                const prefix_len = state.node().prefix.len();
                if (remaining.inner.len >= prefix_len) {
                    const wildcard_slice = remaining.inner[0..prefix_len];
                    if (!std.mem.eql(u8, wildcard_slice, state.node().prefix.unescaped())) {
                        const err = try conflict(allocator, route_value, remaining, state.node());
                        return err;
                    }
                }

                if (state.node().node_type == .CatchAll) {
                    const err = try conflict(allocator, route_value, remaining, state.node());
                    return err;
                }

                if (state.parentNode()) |parent| {
                    if (!(parent.prefix.unescaped().len != 0 and
                        parent.prefix.unescaped()[parent.prefix.unescaped().len - 1] == '/'))
                    {
                        switch (state.node().node_type) {
                            .Param => {
                                if (state.node().hasLiteralSuffix()) {
                                    // Block empty suffix matches when a literal suffix exists,
                                    // otherwise "/{id}suffix" would overlap "/{id}".
                                    const terminator = segmentTerminator(remaining.inner);
                                    const segment = remaining.sliceUntil(terminator);
                                    const wildcard_opt = findWildcard(segment) catch null;
                                    if (wildcard_opt) |wildcard| {
                                        const suffix = remaining.sliceOff(wildcard.end);
                                        if (isEmptyOrSlash(suffix.unescaped())) {
                                            const err = try conflict(allocator, route_value, remaining, parent);
                                            return err;
                                        }
                                    }
                                }
                            },
                            else => {},
                        }
                    }
                }

                return null;
            }

            fn insertRoute(
                self: *SelfNode,
                allocator: Allocator,
                prefix: UnescapedRef,
                value: T,
                remapping: *ParamRemapping,
            ) Allocator.Error!?InsertError {
                var node = self;
                var remaining = prefix;

                while (true) {
                    const wildcard_opt = findWildcard(remaining) catch return .{ .InvalidParam = {} };
                    if (wildcard_opt == null) {
                        node.value = value;
                        node.prefix.deinit(allocator);
                        node.prefix = try remaining.toOwned(allocator);
                        remappingDeinit(&node.remapping, allocator);
                        node.remapping = remapping.*;
                        remapping.* = .{};
                        return null;
                    }

                    const wildcard = wildcard_opt.?;

                    if (remaining.inner[wildcard.start + 1] == '*') {
                        if (wildcard.end != remaining.inner.len) {
                            return .{ .InvalidCatchAll = {} };
                        }

                        if (wildcard.start > 0) {
                            node.prefix.deinit(allocator);
                            node.prefix = try remaining.sliceUntil(wildcard.start).toOwned(allocator);
                            remaining = remaining.sliceOff(wildcard.start);
                        }

                        const child = try SelfNode.create(allocator);
                        var child_owned = true;
                        errdefer if (child_owned) child.destroy(allocator);
                        child.prefix = try remaining.toOwned(allocator);
                        child.node_type = .CatchAll;
                        child.value = value;
                        child.priority = 1;
                        child.remapping = remapping.*;
                        remapping.* = .{};

                        const child_index = try node.addChild(allocator, child);
                        child_owned = false;
                        node.wild_child = true;
                        _ = child_index;
                        return null;
                    }

                    if (wildcard.start > 0) {
                        node.prefix.deinit(allocator);
                        node.prefix = try remaining.sliceUntil(wildcard.start).toOwned(allocator);
                        remaining = remaining.sliceOff(wildcard.start);
                    }

                    const terminator = segmentTerminator(remaining.inner);
                    const wildcard_len = wildcard.len();
                    const wildcard_ref = remaining.sliceUntil(wildcard_len);
                    const suffix = remaining.sliceUntil(terminator).sliceOff(wildcard_len);
                    remaining = remaining.sliceOff(terminator);

                    const suffix_wildcard = findWildcard(suffix) catch return .{ .InvalidParam = {} };
                    if (suffix_wildcard != null) {
                        return .{ .InvalidParamSegment = {} };
                    }

                    const has_suffix = suffix.inner.len != 0;
                    const child = try SelfNode.create(allocator);
                    var child_owned = true;
                    errdefer if (child_owned) child.destroy(allocator);
                    child.priority = 1;
                    child.node_type = .{ .Param = .{ .suffix = has_suffix } };
                    child.prefix = try wildcard_ref.toOwned(allocator);

                    const child_index = try node.addChild(allocator, child);
                    child_owned = false;
                    node.wild_child = true;
                    node = node.childNode(child_index);

                    if (suffix.inner.len != 0) {
                        const suffix_child = try SelfNode.create(allocator);
                        var suffix_owned = true;
                        errdefer if (suffix_owned) suffix_child.destroy(allocator);
                        suffix_child.priority = 1;
                        suffix_child.node_type = .Static;
                        suffix_child.prefix = try suffix.toOwned(allocator);

                        const suffix_index = try node.addSuffixChild(allocator, suffix_child);
                        suffix_owned = false;
                        node = node.childNode(suffix_index);
                    }

                    if (remaining.inner.len == 0) {
                        node.value = value;
                        remappingDeinit(&node.remapping, allocator);
                        node.remapping = remapping.*;
                        remapping.* = .{};
                        return null;
                    }

                    if (remaining.inner[0] != '{' or remaining.isEscaped(0)) {
                        const static_child = try SelfNode.create(allocator);
                        var static_owned = true;
                        errdefer if (static_owned) static_child.destroy(allocator);
                        static_child.priority = 1;
                        const static_index = try node.addStaticChild(allocator, remaining.inner[0], static_child);
                        static_owned = false;
                        node = node.childNode(static_index);
                    }
                }
            }

            fn insert(
                self: *SelfNode,
                allocator: Allocator,
                route_str: []const u8,
                value: T,
            ) Allocator.Error!?InsertError {
                var route_value = try UnescapedRoute.init(allocator, route_str);
                defer route_value.deinit(allocator);

                var remapping = normalizeParams(allocator, &route_value) catch |err| switch (err) {
                    error.InvalidParam => return .{ .InvalidParam = {} },
                    error.TooManyParams => return .{ .TooManyParams = {} },
                    error.OutOfMemory => return error.OutOfMemory,
                };
                defer remappingDeinit(&remapping, allocator);

                var remaining = route_value.asRef();

                if (validateRoute(remaining)) |err| {
                    return err;
                }

                self.priority += 1;

                if (self.value == null and self.childCount() == 0) {
                    if (try self.insertRoute(allocator, remaining, value, &remapping)) |err| {
                        return err;
                    }
                    self.node_type = .Root;
                    return null;
                }

                var state = InsertState{ .parent = self, .child = null };

                walk: while (true) {
                    const node = state.node();
                    const common_prefix = commonPrefixLen(remaining, node);

                    if (try splitNodeAtCommonPrefix(&state, allocator, common_prefix)) {
                        continue :walk;
                    }

                    if (remaining.inner.len == common_prefix) {
                        return try finishInsertAtPrefix(
                            &state,
                            allocator,
                            &route_value,
                            remaining,
                            value,
                            &remapping,
                        );
                    }

                    const common_remaining = remaining;
                    remaining = remaining.sliceOff(common_prefix);
                    const next = remaining.inner[0];

                    switch (node.node_type) {
                        .Param => |param| {
                            switch (try handleParamInsert(
                                &state,
                                allocator,
                                &route_value,
                                common_remaining,
                                &remaining,
                                value,
                                &remapping,
                                param.suffix,
                            )) {
                                .continue_walk => continue :walk,
                                .done => |err| return err,
                            }
                        },
                        else => {},
                    }

                    if (tryAdvanceStaticChild(&state, remaining)) {
                        continue :walk;
                    }

                    if ((next != '{' or remaining.isEscaped(0)) and node.node_type != .CatchAll) {
                        return try insertNewStaticChild(
                            &state,
                            allocator,
                            &route_value,
                            common_remaining,
                            remaining,
                            value,
                            &remapping,
                        );
                    }

                    if (node.wild_child) {
                        if (try advanceWildChild(&state, allocator, &route_value, remaining)) |err| {
                            return err;
                        }
                        continue :walk;
                    }

                    const wildcard_opt = findWildcard(remaining) catch null;
                    if (wildcard_opt) |wildcard| {
                        const suffix = remaining.sliceOff(wildcard.end);

                        // A trailing suffix after a wildcard is incompatible with a prior
                        // wildcard within the same segment.
                        if (!isEmptyOrSlash(suffix.unescaped()) and node.prefixWildChildInSegment()) {
                            const err = try conflict(allocator, &route_value, remaining, node);
                            return err;
                        }

                        if (common_prefix > 0) {
                            const i_prefix = common_prefix - 1;
                            // If we split within a segment, reject any param suffix that would
                            // compete with this wildcard.
                            if (common_remaining.inner[i_prefix] != '/' and node.suffixWildChildInSegment()) {
                                const err = try conflict(allocator, &route_value, remaining, node);
                                return err;
                            }
                        }
                    }

                    if (try state.nodeMut().insertRoute(allocator, remaining, value, &remapping)) |err| {
                        return err;
                    }

                    return null;
                }
            }

            fn removeChild(
                self: *SelfNode,
                allocator: Allocator,
                i: usize,
                remapping: *const ParamRemapping,
            ) ?T {
                const child = self.childNode(i);
                if (!remappingEqual(&child.remapping, remapping)) {
                    return null;
                }

                if (child.childCount() == 0) {
                    const value = child.value;
                    const child_type = child.node_type;
                    self.children.orderedRemove(i);
                    if (child_type != .Static) {
                        self.wild_child = false;
                    }
                    child.destroy(allocator);
                    return value;
                }

                const value = child.value;
                child.value = null;
                return value;
            }

            fn remove(
                self: *SelfNode,
                allocator: Allocator,
                route_str: []const u8,
            ) Allocator.Error!?T {
                var route_value = try UnescapedRoute.init(allocator, route_str);
                defer route_value.deinit(allocator);

                var remapping = normalizeParams(allocator, &route_value) catch |err| switch (err) {
                    error.InvalidParam => return null,
                    error.TooManyParams => return null,
                    error.OutOfMemory => return error.OutOfMemory,
                };
                defer remappingDeinit(&remapping, allocator);

                var remaining = route_value.asRef();

                if (refEql(remaining, &self.prefix)) {
                    const value = self.value;
                    self.value = null;

                    if (self.childCount() == 0) {
                        self.deinit(allocator);
                        self.* = SelfNode.initEmpty();
                    }

                    return value;
                }

                var node: *SelfNode = self;

                walk: while (true) {
                    if (remaining.inner.len <= node.prefix.len()) return null;

                    const prefix = remaining.sliceUntil(node.prefix.len());
                    if (!refEql(prefix, &node.prefix)) return null;

                    const rest = remaining.sliceOff(node.prefix.len());
                    const next = rest.inner[0];
                    remaining = rest;

                    if (node.node_type == .Param) {
                        const terminator = segmentTerminator(remaining.inner);
                        const suffix = remaining.sliceUntil(terminator);

                        const child_nodes = node.childNodes();
                        var child_idx: usize = 0;
                        while (child_idx < child_nodes.len) : (child_idx += 1) {
                            const child = child_nodes[child_idx];
                            if (refEql(suffix, &child.prefix)) {
                                if (terminator == remaining.inner.len) {
                                    return node.removeChild(allocator, child_idx, &remapping);
                                }

                                remaining = remaining.sliceOff(terminator - child.prefix.len());
                                node = node.childNode(child_idx);
                                continue :walk;
                            }
                        }
                    }

                    var idx: usize = 0;
                    const indices = node.childIndices();
                    const child_nodes = node.childNodes();
                    const static_count = node.staticCount();
                    while (idx < static_count) : (idx += 1) {
                        if (indices[idx] == next) {
                            if ((next == '{' or next == '}') and !remaining.isEscaped(0)) {
                                continue;
                            }
                            if (refEql(remaining, &child_nodes[idx].prefix)) {
                                return node.removeChild(allocator, idx, &remapping);
                            }

                            node = child_nodes[idx];
                            continue :walk;
                        }
                    }

                    if (!node.wild_child) return null;

                    const last = node.childCount() - 1;
                    if (refEql(remaining, &node.childNode(last).prefix)) {
                        return node.removeChild(allocator, last, &remapping);
                    }

                    node = node.childNode(last);
                }
            }

            const MatchStep = union(enum) {
                continue_walk,
                matched: Match(T),
                break_walk,
            };

            fn tryMatchNodeValue(
                node: *const SelfNode,
                path: []const u8,
                params_value: *Params,
            ) ?Match(T) {
                if (!std.mem.eql(u8, path, node.prefix.unescaped())) return null;
                if (node.value) |*value| {
                    params_value.applyRemapping(&node.remapping);
                    const params_out = params_value.*;
                    params_value.* = undefined;
                    return .{ .value = value, .params = params_out };
                }
                return null;
            }

            fn tryAdvanceStaticMatch(
                node: *const SelfNode,
                path: []const u8,
                backtracking: bool,
                previous: []const u8,
                skipped: *std.ArrayListUnmanaged(Skipped),
                params_value: *Params,
                allocator: Allocator,
            ) Allocator.Error!?*const SelfNode {
                if (backtracking) return null;
                const next = path[0];
                var idx: usize = 0;
                const indices = node.childIndices();
                const child_nodes = node.childNodes();
                const static_count = node.staticCount();
                while (idx < static_count) : (idx += 1) {
                    if (indices[idx] == next) {
                        if (node.wild_child) {
                            // Keep the wildcard path as a fallback if this static branch fails.
                            try skipped.append(allocator, .{
                                .node = node,
                                .path = previous,
                                .params_len = params_value.len(),
                            });
                        }

                        return child_nodes[idx];
                    }
                }

                return null;
            }

            fn handleParamMatch(
                node: **const SelfNode,
                path: *[]const u8,
                params_value: *Params,
                backtracking: *bool,
                param_suffix: bool,
            ) (MatchError || Allocator.Error)!MatchStep {
                var path_value = path.*;
                const current = node.*;

                if (!param_suffix) {
                    const slash_opt = std.mem.indexOfScalar(u8, path_value, '/');
                    if (slash_opt) |idx| {
                        if (idx == 0) return .break_walk;
                        const param_value = path_value[0..idx];
                        const rest = path_value[idx..];

                        if (current.childCount() != 1) return .break_walk;

                        try params_value.push("", param_value);
                        path_value = rest;
                        node.* = current.childNode(0);
                        backtracking.* = false;
                        path.* = path_value;
                        return .continue_walk;
                    }

                    if (current.value) |*value| {
                        try params_value.push("", path_value);
                        params_value.applyRemapping(&current.remapping);
                        const params_out = params_value.*;
                        params_value.* = undefined;
                        return .{ .matched = .{ .value = value, .params = params_out } };
                    }

                    return .break_walk;
                }

                const slash_opt = std.mem.indexOfScalar(u8, path_value, '/');
                if (slash_opt) |idx| {
                    if (idx == 0) return .break_walk;
                }
                const terminator = if (slash_opt) |idx| idx + 1 else path_value.len;

                for (current.childNodes()) |child| {
                    if (child.prefix.len() >= terminator) {
                        continue;
                    }

                    const suffix_start = terminator - child.prefix.len();
                    const param_value = path_value[0..suffix_start];
                    const suffix = path_value[suffix_start..terminator];

                    if (std.mem.eql(u8, suffix, child.prefix.unescaped())) {
                        node.* = child;
                        path_value = path_value[suffix_start..];
                        backtracking.* = false;
                        try params_value.push("", param_value);
                        path.* = path_value;
                        return .continue_walk;
                    }
                }

                if (slash_opt == null) {
                    if (current.value) |*value| {
                        try params_value.push("", path_value);
                        params_value.applyRemapping(&current.remapping);
                        const params_out = params_value.*;
                        params_value.* = undefined;
                        return .{ .matched = .{ .value = value, .params = params_out } };
                    }
                }

                return .break_walk;
            }

            fn matchCatchAllNode(
                node: *const SelfNode,
                path: []const u8,
                params_value: *Params,
            ) (MatchError || Allocator.Error)!Match(T) {
                if (node.value) |*value| {
                    params_value.applyRemapping(&node.remapping);

                    const prefix_bytes = node.prefix.unescaped();
                    const key = prefix_bytes[2 .. prefix_bytes.len - 1];
                    try params_value.push(key, path);

                    const params_out = params_value.*;
                    params_value.* = undefined;
                    return .{ .value = value, .params = params_out };
                }

                return error.NotFound;
            }

            fn handleWildChildMatch(
                node: **const SelfNode,
                path: *[]const u8,
                params_value: *Params,
                backtracking: *bool,
            ) (MatchError || Allocator.Error)!MatchStep {
                const current = node.*;
                switch (current.node_type) {
                    .Param => |param| return try handleParamMatch(node, path, params_value, backtracking, param.suffix),
                    .CatchAll => {
                        const match_value = try matchCatchAllNode(current, path.*, params_value);
                        return .{ .matched = match_value };
                    },
                    else => unreachable,
                }
            }

            fn at(
                self: *const SelfNode,
                allocator: Allocator,
                path_input: []const u8,
            ) (MatchError || Allocator.Error)!Match(T) {
                var node: *const SelfNode = self;
                var path = path_input;
                var backtracking = false;
                var params_value = Params.init(allocator);
                errdefer params_value.deinit();

                var skipped = std.ArrayListUnmanaged(Skipped){};
                defer skipped.deinit(allocator);

                backtrack: while (true) {
                    walk: while (true) {
                        if (path.len <= node.prefix.len()) {
                            if (tryMatchNodeValue(node, path, &params_value)) |matched| {
                                return matched;
                            }
                            break :walk;
                        }

                        const prefix_len = node.prefix.len();
                        const prefix = path[0..prefix_len];
                        if (!std.mem.eql(u8, prefix, node.prefix.unescaped())) {
                            break :walk;
                        }

                        const previous = path;
                        path = path[prefix_len..];

                        if (try tryAdvanceStaticMatch(
                            node,
                            path,
                            backtracking,
                            previous,
                            &skipped,
                            &params_value,
                            allocator,
                        )) |child| {
                            node = child;
                            continue :walk;
                        }

                        if (!node.wild_child) break :walk;

                        node = node.childNode(node.childCount() - 1);
                        switch (try handleWildChildMatch(&node, &path, &params_value, &backtracking)) {
                            .continue_walk => continue :walk,
                            .matched => |matched| return matched,
                            .break_walk => break :walk,
                        }
                    }

                    while (skipped.items.len > 0) {
                        const skipped_entry = skipped.pop().?;
                        // Only backtrack when the saved path still aligns with the
                        // remaining suffix of the current path.
                        if (std.mem.endsWith(u8, skipped_entry.path, path)) {
                            path = skipped_entry.path;
                            node = skipped_entry.node;
                            backtracking = true;
                            params_value.truncate(skipped_entry.params_len);
                            continue :backtrack;
                        }
                    }

                    return error.NotFound;
                }
            }
        };

        allocator: Allocator,
        root: Node,

        pub fn init(allocator: Allocator) Self {
            return .{
                .allocator = allocator,
                .root = Node.initEmpty(),
            };
        }

        pub fn deinit(self: *Self) void {
            self.root.deinit(self.allocator);
            self.* = undefined;
        }

        pub fn verify(self: *const Self) void {
            self.root.verify();
        }

        pub fn insert(self: *Self, route_str: []const u8, value: T) Allocator.Error!InsertResult {
            var result: InsertResult = .ok;
            if (try self.root.insert(self.allocator, route_str, value)) |err| {
                result = .{ .err = err };
            }
            if (verify_enabled) self.verify();
            return result;
        }

        pub fn match(self: *const Self, path: []const u8) Error!Match(T) {
            return self.root.at(self.allocator, path);
        }

        pub fn matchMut(self: *Self, path: []const u8) Error!MatchMut(T) {
            const found = try self.root.at(self.allocator, path);
            return .{ .value = @constCast(found.value), .params = found.params };
        }

        pub fn remove(self: *Self, route_str: []const u8) Allocator.Error!?T {
            const result = try self.root.remove(self.allocator, route_str);
            if (verify_enabled) self.verify();
            return result;
        }

        pub fn mergeFrom(self: *Self, other: *Self) Allocator.Error!MergeResult {
            const Entry = struct {
                prefix: UnescapedRoute,
                node: *Node,
            };

            var errors_value = MergeError{};
            errdefer errors_value.deinit(self.allocator);

            var stack = std.ArrayListUnmanaged(Entry){};
            errdefer {
                for (stack.items) |*entry| {
                    entry.prefix.deinit(self.allocator);
                }
                stack.deinit(self.allocator);
            }

            const root_prefix = try other.root.prefix.clone(self.allocator);
            try stack.append(self.allocator, .{ .prefix = root_prefix, .node = &other.root });

            while (stack.items.len > 0) {
                var entry = stack.pop().?;
                errdefer entry.prefix.deinit(self.allocator);

                try denormalizeParams(self.allocator, &entry.prefix, &entry.node.remapping);

                if (entry.node.value) |value| {
                    entry.node.value = null;
                    switch (try self.insert(entry.prefix.unescaped(), value)) {
                        .ok => {},
                        .err => |err| {
                            var owned_err = err;
                            errdefer owned_err.deinit(self.allocator);
                            try errors_value.errors.append(self.allocator, owned_err);
                        },
                    }
                }

                for (entry.node.childNodes()) |child| {
                    var child_prefix = try entry.prefix.clone(self.allocator);
                    errdefer child_prefix.deinit(self.allocator);
                    try child_prefix.append(self.allocator, &child.prefix);
                    try stack.append(self.allocator, .{ .prefix = child_prefix, .node = child });
                }

                entry.prefix.deinit(self.allocator);
            }

            stack.deinit(self.allocator);

            other.root.deinit(other.allocator);
            other.root = Node.initEmpty();

            var result: MergeResult = undefined;
            if (errors_value.errors.items.len == 0) {
                errors_value.deinit(self.allocator);
                result = .ok;
            } else {
                result = .{ .err = errors_value };
            }

            if (verify_enabled) self.verify();
            return result;
        }
    };
}
