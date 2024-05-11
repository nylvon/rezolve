const std = @import("std");
const NodeLib = @import("node.zig");
const NodeTypeFunction = NodeLib.NodeTypeFunction;
const NodeType = NodeLib.NodeType;
const AULib = @import("autounion.zig");
const AutoUnion = AULib.OptimalAutoUnionWrapper;

/// This does integrity checks on whether the arrays that hold
/// the definitions for the node types.
pub fn CheckIODefinitions(
    // The call-site of this error, for debugging
    comptime source: [:0]const u8,
    // An array of all the input and output types for each node type
    comptime InputTypesAll: []?[]const type,
    comptime OutputTypesAll: []?[]const type,
) void {
    comptime {
        // Check for length mismatches
        if (InputTypesAll.len != OutputTypesAll.len) {
            @compileError( //
                std.fmt.comptimePrint( //
                "[{s} fatal error]" ++
                "\nInputTypesAll and OutputTypesAll do not have the same length!" ++
                "\n\tInputTypesAll.len = '{d}'" ++
                "\n\tOutputTypesAll.len = '{d}'", .{ source, InputTypesAll.len, OutputTypesAll.len }));
        }
    }
}

/// Creates an array of types that represent the function types
/// for each node definition in the arrays given
pub fn ControllableNodeTypeFunctions(
    // An array of all the input and output types for each node type
    comptime InputTypesAll: []?[]const type,
    comptime OutputTypesAll: []?[]const type,
) []const type {
    comptime {
        CheckIODefinitions("ControllableNodeTypeFunctions", InputTypesAll, OutputTypesAll);

        // If we're here, both arrays are fine, and of the same size
        const length = InputTypesAll.len;
        var FunctionsTypes: [length]type = undefined;

        for (0..length) |i| {
            FunctionsTypes[i] = NodeTypeFunction(InputTypesAll[i], OutputTypesAll[i]);
        }

        const ConstFunctionTypes = FunctionsTypes;
        return ConstFunctionTypes;
    }
}

/// Creates an auto-union of function types from the node definitions.
pub fn UnifiedControllableNodeTypeFunctions(
    // An array of all the input and output types for each node type
    comptime InputTypesAll: []?[]const type,
    comptime OutputTypesAll: []?[]const type,
) type {
    const FunctionTypes = ControllableNodeTypeFunctions(InputTypesAll, OutputTypesAll);
    const FunctionTypesUnion = AutoUnion(FunctionTypes);
    return FunctionTypesUnion;
}

/// Creates a node factory that can link nodes and interact with them much easier.
/// TODO: Write a better description
pub fn ControllableNodeFactory(
    // An array of all the input and output types for each node type
    comptime InputTypesAll: []?[]const type,
    comptime OutputTypesAll: []?[]const type,
    // An array of all the functions that are used, combined in an auto-union form.
    comptime FunctionsAll: []const UnifiedControllableNodeTypeFunctions(InputTypesAll, OutputTypesAll),
) type {}
