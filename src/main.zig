const std = @import("std");
const root = @import("root.zig");
const fs = std.fs;
const eq = @import("equation.zig");
const Equation = eq.Equation;
const INT = eq.Equation.InnerNodeType;

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var e = Equation.init(allocator);
    defer e.deinit();

    const sampleNode = INT{ .Data = .{ .symbol = .{ .representation = "a" }, .value = .{ .Computed = .{ .Integer = 1 } } } };

    var rn = try e.changeRoot(sampleNode);
    var n1 = try rn.addChildByData(sampleNode, allocator);
    var n2 = try rn.addChildByData(sampleNode, allocator);
    var n3 = try rn.addChildByData(sampleNode, allocator);
    _ = try n1.addChildByData(sampleNode, allocator);
    _ = try n1.addChildByData(sampleNode, allocator);
    _ = try n2.addChildByData(sampleNode, allocator);
    _ = try n2.addChildByData(sampleNode, allocator);
    _ = try n2.addChildByData(sampleNode, allocator);
    _ = try n2.addChildByData(sampleNode, allocator);
    _ = try n3.addChildByData(sampleNode, allocator);
    _ = try n3.addChildByData(sampleNode, allocator);
    try e.root_node.?.tryPrint(stdout, 0, true);

    try bw.flush();
}
