const std = @import("std");
const root = @import("root.zig");
const fs = std.fs;
const eq = @import("equation.zig");
const Equation = eq.EquationUnmanaged;
const INT = Equation.InnerNodeType;
const SBDT = Equation.SymbolicDataType;
const Symbol = Equation.SymbolicType;
const Timer = std.time.Timer; // Will be used for benchmarking later.

/// this is bloated as hell
/// and it is just a joke for now
/// this will ofc all be cleared up later
pub fn binaryAddition(inputs: []Equation.ValueType) []Equation.ValueType {
    switch (inputs[0].getValue()) {
        .Integer => |x| switch (inputs[1].getValue()) {
            .Integer => |y| {
                const z = x + y;
                const result = [1]Equation.ValueType{Equation.ValueType{ .Computed = .{ .Integer = z } }};
                return @constCast(&result);
            },
            .Real => |y| {
                const z = @as(Equation.RealValueType, @floatFromInt(x)) + y;
                const result = [1]Equation.ValueType{Equation.ValueType{ .Computed = .{ .Real = z } }};
                return @constCast(&result);
            },
        },
        .Real => |x| switch (inputs[1].getValue()) {
            .Integer => |y| {
                const z = x + @as(Equation.RealValueType, @floatFromInt(y));
                const result = [1]Equation.ValueType{Equation.ValueType{ .Computed = .{ .Real = z } }};
                return @constCast(&result);
            },
            .Real => |y| {
                const z = x + y;
                const result = [1]Equation.ValueType{Equation.ValueType{ .Computed = .{ .Real = z } }};
                return @constCast(&result);
            },
        },
    }
}

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
    var sampleData = Equation.DataType{ .Constant = SBDT.init(Symbol.initNoAlloc("z"), .{ .Computed = .{ .Integer = 2 } }) };
    var simpleOperator = Equation.DataType{ .Operator = Equation.SymbolicOperatorType.init(Symbol.initNoAlloc("+"), Equation.OperatorDefinitionType.binary(&binaryAddition)) };

    const rn = try e.start(allocator);
    // const rn1 = try e.tryAddNode(@constCast(rn), sampleNode, allocator);

    try e.tryDefineDataUnsafe(&sampleData, false);
    try e.tryDefineDataUnsafe(&simpleOperator, false);

    _ = try e.tryAddReferenceUnsafe(@constCast(rn), Symbol.init("z", false), false, allocator);
    _ = try e.tryAddReferenceUnsafe(@constCast(rn), Symbol.init("z", false), false, allocator);
    _ = try e.tryAddReferenceUnsafe(@constCast(rn), Symbol.init("z", false), false, allocator);
    _ = try e.tryAddReferenceUnsafe(@constCast(rn), Symbol.init("z", false), true, allocator);
    _ = try e.tryAddReferenceUnsafe(@constCast(rn), Symbol.init("z", false), true, allocator);
    _ = try e.tryAddReferenceUnsafe(@constCast(rn), Symbol.init("z", false), true, allocator);

    try e.tryPrint(stdout, true);

    try bw.flush();
}
