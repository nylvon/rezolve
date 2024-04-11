const std = @import("std");
const root = @import("root.zig");
const fs = std.fs;
const eq = @import("equation.zig");
const Equation = eq.EquationUnmanaged;
const INT = Equation.InnerNodeType;
const SBDT = Equation.SymbolicBaseDataType;
const Symbol = Equation.SymbolicType;
const Timer = std.time.Timer; // Will be used for benchmarking later.

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var e = Equation.init(allocator);
    defer e.deinit(allocator);

    const sampleNode = INT{ .Data = SBDT.init("k", false, .{ .Computed = .{ .Integer = 1 } }) };
    var sampleData = Equation.DataType{ .Constant = SBDT.init("z", false, .{ .NeedsRecompute = .{ .Integer = 2 } }) };

    const rn = try e.changeRoot(sampleNode, allocator);
    const rn2 = try e.tryAddNode(@constCast(rn), sampleNode, allocator);
    _ = try e.tryAddNode(@constCast(rn2), sampleNode, allocator);
    _ = try e.tryAddReference(@constCast(rn2), Symbol.init("a", false), true, allocator);
    _ = try e.tryAddReference(@constCast(rn2), Symbol.init("b", false), true, allocator);
    _ = try e.tryAddReference(@constCast(rn2), Symbol.init("ref(b)", false), true, allocator);
    // var n1 = try rn.addChildByData(sampleNode, allocator);
    // var n2 = try rn.addChildByData(sampleNode, allocator);
    // var n3 = try rn.addChildByData(sampleNode, allocator);
    // _ = try n1.addChildByData(sampleNode, allocator);
    // _ = try n1.addChildByData(sampleNode, allocator);
    // _ = try n2.addChildByData(sampleNode, allocator);
    // _ = try n2.addChildByData(sampleNode, allocator);
    // _ = try n2.addChildByData(sampleNode, allocator);
    // _ = try n2.addChildByData(sampleNode, allocator);
    // _ = try n3.addChildByData(sampleNode, allocator);
    // _ = try n3.addChildByData(sampleNode, allocator);

    try e.tryDefineData(&sampleData, false);
    try e.tryPrint(stdout, true);

    try bw.flush();
}
