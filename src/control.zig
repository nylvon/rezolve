const std = @import("std");
const NodeLib = @import("node.zig");
const NodeTypeFunction = NodeLib.NodeTypeFunction;
const NodeType = NodeLib.NodeType;
const AULib = @import("autounion.zig");
const AutoUnion = AULib.OptimalAutoUnionWrapper;
const utils = @import("utils.zig");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const expect = testing.expect;
const expectError = testing.expectError;

/// This does integrity checks on whether the arrays that hold
/// the definitions for the node types.
pub fn CheckIODefinitions(
    // The call-site of this error, for debugging
    comptime source: [:0]const u8,
    // An array of all the input and output types for each node type
    comptime InputTypesAll: []const ?[]const type,
    comptime OutputTypesAll: []const ?[]const type,
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
pub fn GenerateNodeTypeFunctionsFrom(
    // An array of all the input and output types for each node type
    comptime InputTypesAll: []const ?[]const type,
    comptime OutputTypesAll: []const ?[]const type,
) []const type {
    comptime {
        CheckIODefinitions("GenerateNodeTypeFunctionsFrom", InputTypesAll, OutputTypesAll);

        // If we're here, both arrays are fine, and of the same size
        const length = InputTypesAll.len;
        var FunctionsTypes: [length]type = undefined;

        for (0..length) |i| {
            FunctionsTypes[i] = NodeTypeFunction(InputTypesAll[i], OutputTypesAll[i]);
        }

        const ConstFunctionTypes = FunctionsTypes;
        return &ConstFunctionTypes;
    }
}

test "GenerateNodeTypeFunctionsFrom" {
    // Each node definition has an input type array and an output type array
    // There's 3 arbitray node definitions here.
    const input_types = [_]?[]const type{ //
        &[_]type{ i32, i32 },
        null,
        &[_]type{ f32, f32, f32 },
    };
    const output_types = [_]?[]const type{ //
        &[_]type{i32},
        &[_]type{i32},
        &[_]type{f32},
    };

    // This is guaranteed to be okay because it's verified
    // in its own file with its own tests.
    const expected_function_types = [_]type{
        NodeTypeFunction(input_types[0], output_types[0]),
        NodeTypeFunction(input_types[1], output_types[1]),
        // We'll make this last one invalid, changing its input types
        NodeTypeFunction(input_types[0], output_types[2]),
    };

    // This is the testing target
    const generated_function_types = GenerateNodeTypeFunctionsFrom(&input_types, &output_types);

    // Should match the length at least
    try expect(expected_function_types.len == generated_function_types.len);

    // Then check that they're all the same
    inline for (expected_function_types, 0..) |EFT, i| {
        const GFT = generated_function_types[i];

        // For our edge case, try and check that the types indeed are not right
        if (i == 2) {
            try expect(EFT != GFT);
        } else try expect(EFT == GFT);
    }
}

/// Creates an auto-union of function types from the node definitions.
/// This is used to carry-over function data with type safety.
pub fn UnifiedNodeTypeFunction(
    // An array of all the input and output types for each node type
    comptime InputTypesAll: []const ?[]const type,
    comptime OutputTypesAll: []const ?[]const type,
) type {
    const FunctionTypes = GenerateNodeTypeFunctionsFrom(InputTypesAll, OutputTypesAll);
    const FunctionTypesUnion = AutoUnion(FunctionTypes);
    return FunctionTypesUnion;
}

/// Wraps a function from one node definition's type in a unified form,
/// in the context of a set of multiple node definitions.
pub fn WrapNodeFunctionInUnifiedForm(
    // An array of all the input and output types for this function to be transformed.
    comptime InputTypesThis: ?[]const type,
    comptime OutputTypesThis: ?[]const type,
    // The function to be wrapped
    comptime ThisFunction: NodeTypeFunction(InputTypesThis, OutputTypesThis),
    // An array of all the input and output types for each node type
    comptime InputTypesAll: []const ?[]const type,
    comptime OutputTypesAll: []const ?[]const type,
) UnifiedNodeTypeFunction(InputTypesAll, OutputTypesAll) {
    comptime {
        const UnifiedFunctionType = UnifiedNodeTypeFunction(InputTypesAll, OutputTypesAll);
        const WrappedFunction = try UnifiedFunctionType.Create(@TypeOf(ThisFunction), ThisFunction);
        return WrappedFunction;
    }
}

test "WrapNodeFunctionInUnifiedForm" {
    // Each node definition has an input type array and an output type array
    // There's 3 arbitray node definitions here.
    const input_types = [_]?[]const type{ //
        &[_]type{ i32, i32 },
        null,
        &[_]type{ f32, f32, f32 },
    };
    const output_types = [_]?[]const type{ //
        &[_]type{i32},
        &[_]type{i32},
        &[_]type{f32},
    };

    // The expected function types to be the active fields of the unified unions below
    const expected_function_types = GenerateNodeTypeFunctionsFrom(&input_types, &output_types);

    // Some random, arbitrary functions that match the signatures
    const expected_function_1 = comptime utils.EmptyFunctionBinary(NodeLib.DataBusReferenceType(utils.TypeArrayToOptionalArrayOptional(utils.TypeArrayToPointerArrayOptional(input_types[0]))), NodeLib.DataBusReferenceType(output_types[0]), anyerror!void, null);
    const expected_function_2 = comptime utils.EmptyFunctionUnary(NodeLib.DataBusReferenceType(output_types[1]), anyerror!void, null);
    const expected_function_3 = comptime utils.EmptyFunctionBinary(NodeLib.DataBusReferenceType(utils.TypeArrayToOptionalArrayOptional(utils.TypeArrayToPointerArrayOptional(input_types[2]))), NodeLib.DataBusReferenceType(output_types[2]), anyerror!void, null);

    // The unified type wrapper
    const unified_function_type = UnifiedNodeTypeFunction(&input_types, &output_types);

    // How they should be generated
    const expected_unified_layout = [_]unified_function_type{
        try unified_function_type.Create(expected_function_types[0], expected_function_1),
        try unified_function_type.Create(expected_function_types[1], expected_function_2),
        try unified_function_type.Create(expected_function_types[2], expected_function_3),
    };

    // How they're actually generated
    const generated_unified_layout = [_]unified_function_type{
        comptime WrapNodeFunctionInUnifiedForm(input_types[0], output_types[0], expected_function_1, &input_types, &output_types),
        comptime WrapNodeFunctionInUnifiedForm(input_types[1], output_types[1], expected_function_2, &input_types, &output_types),
        comptime WrapNodeFunctionInUnifiedForm(input_types[2], output_types[2], expected_function_3, &input_types, &output_types),
    };

    // Check
    inline for (expected_unified_layout, 0..) |EUL_Entry, i| {
        const GUL_Entry = generated_unified_layout[i];

        try expect(try EUL_Entry.GetValue(expected_function_types[i]) == try GUL_Entry.GetValue(expected_function_types[i]));
    }
}

/// Creates a unified reference type from node definitions and unified function wrappers
pub fn UnifiedReferenceType(
    // An array of all the input and output types for each node type
    comptime InputTypesAll: []const ?[]const type,
    comptime OutputTypesAll: []const ?[]const type,
    // An array of all the functions that are used, combined in an auto-union form.
    comptime FunctionsAll: []const UnifiedNodeTypeFunction(InputTypesAll, OutputTypesAll),
) type {
    var AllNodeOptionalReferenceTypes: [InputTypesAll.len]type = undefined;

    inline for (0..InputTypesAll.len) |index| {
        // Get the input and output types associated with this node
        const AssociatedInputTypes = InputTypesAll[index];
        const AssociatedOutputTypes = OutputTypesAll[index];
        // Get the wrapped function associated
        const AssociatedFunctionWrapper = FunctionsAll[index];
        // Derive the function type from the inputs
        const AssociatedFunctionType = NodeTypeFunction(AssociatedInputTypes, AssociatedOutputTypes);
        // Unwrap the function
        const AssociatedFunction = try AssociatedFunctionWrapper.GetValue(AssociatedFunctionType);
        // Create the node type
        const AssociatedNodeType = NodeType(AssociatedInputTypes, AssociatedOutputTypes, AssociatedFunction);
        AllNodeOptionalReferenceTypes[index] = ?*AssociatedNodeType;
    }

    const ConstAllNodeOptionalReferenceTypes = AllNodeOptionalReferenceTypes;
    const UnifiedReference = AutoUnion(&ConstAllNodeOptionalReferenceTypes);
    return UnifiedReference;
}

pub const ControlPointErrors = error{
    NullNodeReference,
    InputToInputConnection,
    OutputToOutputConnection,
};

/// A control point is an interface that can be used in order to connect
/// two nodes' ports and ensure that the connection is possible.
pub fn ControlPointType(
    // An array of all the input and output types for each node type
    comptime InputTypesAll: []const ?[]const type,
    comptime OutputTypesAll: []const ?[]const type,
    // An array of all the functions that are used, combined in an auto-union form.
    comptime FunctionsAll: []const UnifiedNodeTypeFunction(InputTypesAll, OutputTypesAll),
) type {
    comptime {
        return struct {
            node_reference: UnifiedReferenceBase,
            bus: NodeLib.Bus,
            port: usize,

            pub const UnifiedReferenceBase = UnifiedReferenceType(InputTypesAll, OutputTypesAll, FunctionsAll);
            pub const NodeTypesBase = UnifiedReferenceBase.BaseTypes;

            /// Verifies that the node reference is not null
            pub fn CheckIfValidNodeReference(target: @This()) !void {
                switch (target.node_reference.inner) {
                    inline else => |NodeReference| {
                        if (NodeReference == null) {
                            return ControlPointErrors.NullNodeReference;
                        }
                    },
                }
            }

            /// Verifies that the given bus on the node reference exists.
            pub fn CheckIfValidBus(target: @This()) !void {
                try target.CheckIfValidNodeReference();

                switch (target.node_reference.inner) {
                    inline else => |NodeReference| {
                        if (NodeReference) |NR| {
                            // If it has a bus, it has at least one entry, which should be at index 0.
                            try NR.SelfIndexCheck(0, target.bus);
                        }
                    },
                }
            }

            /// Verifies that the given port on the given bus on the node reference exists.
            pub fn CheckIfValidPort(target: @This()) !void {
                try target.CheckIfValidNodeReference();

                switch (target.node_reference.inner) {
                    inline else => |NodeReference| {
                        if (NodeReference) |NR| {
                            // If it has a bus, it has at least one entry, which should be at index 0.
                            try NR.SelfIndexCheck(target.port, target.bus);
                        }
                    },
                }
            }

            /// Aliasing valid connection to mean this makes it easier to read.
            pub const CheckIfValidConnection = CheckIfValidPort;

            /// Connects two control points.
            /// Only input-output connections are allowed.
            pub fn Connect(this: @This(), that: @This(), T: type) !void {
                try this.CheckIfValidPort();
                try that.CheckIfValidPort();

                switch (this.node_reference.inner) {
                    inline else => |ThisNode| {
                        switch (that.node_reference.inner) {
                            inline else => |ThatNode| {
                                var node_a = ThisNode.?;
                                var node_b = ThatNode.?;

                                switch (this.bus) {
                                    .Input => {
                                        switch (that.bus) {
                                            .Input => return ControlPointErrors.InputToInputConnection,
                                            .Output => {
                                                // node_a = input
                                                // node_b = output
                                                try node_a.SetInput(this.port, T, try node_b.GetReference(that.port, .Output, T));
                                            },
                                        }
                                    },
                                    .Output => {
                                        switch (that.bus) {
                                            .Input => {
                                                // node_a = output
                                                // node_b = input
                                                try node_b.SetInput(that.port, T, try node_a.GetReference(this.port, .Output, T));
                                            },
                                            .Output => return ControlPointErrors.OutputToOutputConnection,
                                        }
                                    },
                                }
                            },
                        }
                    },
                }
            }
        };
    }
}

// Simulate how control points work, without them being embedded in a type
test "ControlPointType, external use" {
    // Each node definition has an input type array and an output type array
    // There's 3 arbitray node definitions here.
    const input_types = [_]?[]const type{ //
        &[_]type{ i32, i32 },
        null,
        &[_]type{ f32, f32, f32 },
    };
    const output_types = [_]?[]const type{ //
        &[_]type{i32},
        &[_]type{i32},
        &[_]type{f32},
    };

    // Some random, arbitrary functions that match the signatures
    const expected_function_1 = comptime utils.EmptyFunctionBinary(NodeLib.DataBusReferenceType(utils.TypeArrayToOptionalArrayOptional(utils.TypeArrayToPointerArrayOptional(input_types[0]))), NodeLib.DataBusReferenceType(output_types[0]), anyerror!void, null);
    const expected_function_2 = comptime utils.EmptyFunctionUnary(NodeLib.DataBusReferenceType(output_types[1]), anyerror!void, null);
    const expected_function_3 = comptime utils.EmptyFunctionBinary(NodeLib.DataBusReferenceType(utils.TypeArrayToOptionalArrayOptional(utils.TypeArrayToPointerArrayOptional(input_types[2]))), NodeLib.DataBusReferenceType(output_types[2]), anyerror!void, null);

    // The unified type wrapper
    const unified_function_type = UnifiedNodeTypeFunction(&input_types, &output_types);

    // How they're actually generated
    const expected_functions = [_]unified_function_type{
        comptime WrapNodeFunctionInUnifiedForm(input_types[0], output_types[0], expected_function_1, &input_types, &output_types),
        comptime WrapNodeFunctionInUnifiedForm(input_types[1], output_types[1], expected_function_2, &input_types, &output_types),
        comptime WrapNodeFunctionInUnifiedForm(input_types[2], output_types[2], expected_function_3, &input_types, &output_types),
    };

    // Generate some example node type and some nodes to try to simulate the control points on their own
    const node_type_1 = NodeType(input_types[0], output_types[0], expected_function_1);
    var node_1 = node_type_1.Create();
    var node_2 = node_type_1.Create();

    // Generating the unified reference type and some node references for the control points
    const unified_reference_type = UnifiedReferenceType(&input_types, &output_types, &expected_functions);
    const node_1_ref = try unified_reference_type.Create(?*node_type_1, @constCast(&node_1));
    const node_2_ref = try unified_reference_type.Create(?*node_type_1, @constCast(&node_2));

    // Generating some control points to simulate them
    const ctrl_type = comptime ControlPointType(&input_types, &output_types, &expected_functions);
    const ctrl_1 = ctrl_type{ .node_reference = node_1_ref, .bus = .Input, .port = 1 };
    const ctrl_2 = ctrl_type{ .node_reference = node_2_ref, .bus = .Output, .port = 0 };
    const ctrl_3 = ctrl_type{ .node_reference = node_1_ref, .bus = .Input, .port = 0 };
    const ctrl_4 = ctrl_type{ .node_reference = node_1_ref, .bus = .Output, .port = 0 };
    const ctrl_5 = ctrl_type{ .node_reference = node_1_ref, .bus = .Output, .port = 1 };

    // This should work, it's an input<->output connection, on ports that exist on the buses given
    try ctrl_2.Connect(ctrl_1, i32);
    // These two should fail because these are input<->input connections and output<->output connections, which are invalid
    try expectError(ControlPointErrors.InputToInputConnection, ctrl_3.Connect(ctrl_1, i32));
    try expectError(ControlPointErrors.OutputToOutputConnection, ctrl_4.Connect(ctrl_2, i32));
    // This should fail because there is only one entry on the output bus of the node, so "port 1" doesn't exist on the output bus.
    try expectError(NodeLib.NodeErrors.InvalidIndex, ctrl_5.Connect(ctrl_2, i32));
}
