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

    // const sampleNode = INT{ .Data = SBDT.init("k", false, .{ .Computed = .{ .Integer = 1 } }) };
    var sampleData = Equation.DataType{ .Constant = SBDT.init("z", false, .{ .Computed = .{ .Integer = 2 } }) };

    const rn = try e.start(allocator);
    // const rn1 = try e.tryAddNode(@constCast(rn), sampleNode, allocator);

    try e.tryDefineData(&sampleData, false);

    _ = try e.tryAddReference(@constCast(rn), Symbol.init("z", false), false, allocator);
    _ = try e.tryAddReference(@constCast(rn), Symbol.init("z", false), false, allocator);
    _ = try e.tryAddReference(@constCast(rn), Symbol.init("z", false), false, allocator);
    _ = try e.tryAddReference(@constCast(rn), Symbol.init("z", false), true, allocator);
    _ = try e.tryAddReference(@constCast(rn), Symbol.init("z", false), true, allocator);
    _ = try e.tryAddReference(@constCast(rn), Symbol.init("z", false), true, allocator);

    try e.tryPrint(stdout, true);

    try bw.flush();
}
