const std = @import("std");
const AULib = @import("autounion.zig");
const AutoUnion = AULib.OptimalAutoUnionWrapper;
const utils = @import("utils.zig");
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;
const expectError = testing.expectError;

/// Returns the function signature of a node's function given the input and output types.
/// The types are generated automatically by the NodeType function, and this function should not be
/// used on its own, unless you really know what you are doing.
/// Explanation:
///     Each node has an input bus, which is usually an array of Optimal Auto Union Wrappers, derived from
///     'InputTypes' but with each type converted to its own (non-const) pointer form.
///     Each node also has an output data bus, which is an array of Optimal Auto Union Wrappers, derived from
///     'OutputTypes', in the form as it was given.
///     If either array is null, it will not be transformed into an array, but into 'void'.
///     A node function should be a function that takes in a reference to the input bus and a reference
///     to the output bus. This function's task is to generate that function signature.
/// NOTE: You will see these being the just (non-const) pointer forms of the internal NodeType's
///       input and output data bus types.
pub fn NodeTypeFunction(comptime InputTypes: ?[]const type, comptime OutputTypes: ?[]const type) type {
    const InputBusReferenceType = DataBusReferenceType(utils.TypeArrayToPointerArrayOptional(InputTypes));
    const OutputBusReferenceType = DataBusReferenceType(OutputTypes);

    switch (InputBusReferenceType) {
        void => switch (OutputBusReferenceType) {
            void => return comptime *const fn () anyerror!void,
            else => return comptime *const fn (OutputBusReferenceType) anyerror!void,
        },
        else => switch (OutputBusReferenceType) {
            void => return comptime *const fn (InputBusReferenceType) anyerror!void,
            else => return comptime *const fn (InputBusReferenceType, OutputBusReferenceType) anyerror!void,
        },
    }
}

/// Returns the type of a data bus by creating an array of Optimal Auto Union Wrappers
/// given by the length and by the types of 'Types'.
/// If 'Types' is null, this will return void, indicating a lack of a data bus.
pub fn DataBusType(comptime Types: ?[]const type) type {
    return if (Types) |Ts| [Ts.len]AutoUnion(Ts) else void;
}

/// Returns the type of a referenced data bus by creating an array of Optimal Auto Union Wrappers
/// given by the length and by the types of 'Types'.
/// If 'Types' is null, this will return void, indicating a lack of a data bus.
pub fn DataBusReferenceType(comptime Types: ?[]const type) type {
    return if (Types) |Ts| *[Ts.len]AutoUnion(Ts) else void;
}

/// PLACEHOLDER: This returns a node type with pointers to the input types and values of the output types.
///              Nodes like these can be safely intertwined and chained, to form a graph of nodes.
/// TODO: Write a better, more throughout description here.
pub fn NodeType(comptime InputTypes: ?[]const type, comptime OutputTypes: ?[]const type, comptime FunctionReference: NodeTypeFunction(InputTypes, OutputTypes)) type {
    return struct {
        input_data_ports: InputDataBusType,
        output_data_ports: OutputDataBusType,

        /// The base types that were used in constructing this type, if not null;
        pub const InputTypesBase = if (InputTypes) |IT| IT else void;
        pub const OutputTypesBase = if (OutputTypes) |OT| OT else void;

        /// The derived optimal auto union wrapper types for the entries to the data buses.
        pub const InputDataBusEntryType = AutoUnion(utils.TypeArrayToPointerArray(InputTypesBase));
        pub const OutputDataBusEntryType = AutoUnion(OutputTypesBase);

        /// The derived data bus types.
        pub const InputDataBusType = DataBusType(utils.TypeArrayToPointerArrayOptional(InputTypes));
        pub const OutputDataBusType = DataBusType(OutputTypes);

        /// The derived function type, inferred from where it was created, in the parameter type.
        pub const FunctionType = @TypeOf(Function);

        /// The actual function pointer to be used. Since it's const, it should be optimized away nicely.
        /// Ideally the function should be marked as inline, too.
        pub const Function = FunctionReference;

        /// The error set used for nodes.
        /// TODO: Write a more coherent description.
        pub const Errors = error{
            InvalidIndex,
            LacksBus,
        };

        /// Ease of use enum to use in functions that can refer to either bus.
        pub const Bus = enum { Input, Output };

        /// If the index is within bounds, this will return void.
        /// If it's not, or if the bus doesn't exist, this will return an error.
        pub fn IndexCheck(self: *@This(), index: usize, bus: Bus) !void {
            switch (bus) {
                .Input => {
                    comptime if (InputDataBusType == void) return Errors.LacksBus;
                    if (index >= self.input_data_ports.len) return Errors.InvalidIndex;
                },
                .Output => {
                    comptime if (OutputDataBusType == void) return Errors.LacksBus;
                    if (index >= self.output_data_ports.len) return Errors.InvalidIndex;
                },
            }
        }

        /// Sets an entry of the input data bus to point to a target.
        pub fn SetInput(self: *@This(), index: usize, T: type, target: *T) !void {
            try self.IndexCheck(index, .Input);

            self.input_data_ports[index] = try InputDataBusEntryType.Create(*T, target);
        }

        /// TODO: Implement this. Maybe?
        // pub fn SetOutput()

        /// Maps the current inputs to the outputs, using the function of the node.
        pub fn Execute(self: *@This()) !void {
            switch (InputDataBusType) {
                void => switch (OutputDataBusType) {
                    void => return Function(),
                    else => return Function(&self.output_data_ports),
                },
                else => switch (OutputDataBusType) {
                    void => return Function(&self.input_data_ports),
                    else => return Function(&self.input_data_ports, &self.output_data_ports),
                },
            }
        }

        /// Creates an empty node of this type.
        pub fn Create() @This() {
            switch (InputDataBusType) {
                void => switch (OutputDataBusType) {
                    void => return .{},
                    else => return .{ .output_data_ports = undefined },
                },
                else => switch (OutputDataBusType) {
                    void => return .{ .input_data_ports = undefined },
                    else => return .{ .input_data_ports = undefined, .output_data_ports = undefined },
                },
            }
        }
    };
}

/// An example binary addition function.
pub fn BinaryAddFn(inputs: DataBusReferenceType(utils.TypeArrayToPointerArray(&[_]type{ i32, i32 })), outputs: DataBusReferenceType(&[_]type{i32})) !void {
    const in_0 = try inputs[0].GetValue(*i32);
    const in_1 = try inputs[1].GetValue(*i32);
    const out_0 = try outputs[0].GetReference(i32);
    out_0.* = in_0.* + in_1.*;
}

// Tests whether the function part of the nodes works.
// Detached from an actual node type.
// This is simulating what NodeType should do in the case
// of a node that does binary addition of two inputs of
// type i32, and holds the result in an output of type i32.
test "Function Tests" {
    // some example types for the input and output buses.
    const out_types = &[_]type{i32};
    const in_types = &[_]type{ i32, i32 };

    // the entries in the data buses
    const in_oau_type = AutoUnion(utils.TypeArrayToPointerArray(in_types));
    const out_oau_type = AutoUnion(out_types);

    // some example forced input values.
    const in_1_forced: i32 = 10;
    const in_2_forced: i32 = 20;
    const out_1_before: i32 = 0;

    // some example values for two inputs of some node, and a temporary output value.
    const in_1 = try in_oau_type.Create(*i32, @constCast(&in_1_forced));
    const in_2 = try in_oau_type.Create(*i32, @constCast(&in_2_forced));
    const out_1 = try out_oau_type.Create(i32, out_1_before);

    // the simulated data buses
    var in_bus: DataBusType(utils.TypeArrayToPointerArray(in_types)) = .{ in_1, in_2 };
    var out_bus: DataBusType(out_types) = .{out_1};

    // before the operation, the input values should be the ones we forced in
    // and the output should be zero, since we didn't do anything with it yet.
    const in_1_value_before = try in_bus[0].GetValue(*i32);
    const in_2_value_before = try in_bus[1].GetValue(*i32);
    const out_1_value_before = try out_bus[0].GetValue(i32);

    // check
    try expect(in_1_value_before.* == in_1_forced);
    try expect(in_2_value_before.* == in_2_forced);
    try expect(out_1_value_before == 0);

    // do the operation
    try BinaryAddFn(&in_bus, &out_bus);

    // now the output should have changed, but the input should not have
    const in_1_value_after = try in_bus[0].GetValue(*i32);
    const in_2_value_after = try in_bus[1].GetValue(*i32);
    const out_1_value_after = try out_bus[0].GetValue(i32);

    // the expected state of the output.
    const out_1_value_after_expected = in_1_forced + in_2_forced;

    // check
    try expect(in_1_value_after.* == in_1_forced);
    try expect(in_2_value_after.* == in_2_forced);
    try expect(out_1_value_after == out_1_value_after_expected);
}

// Same test, but now with the NodeType
test "NodeType Binary Test" {
    // the input and output types
    const input_types = &[_]type{ i32, i32 };
    const output_types = &[_]type{i32};

    // the node type generated, with the sample binary addition function
    const binary_add_node_type = NodeType(input_types, output_types, BinaryAddFn);

    // forced inputs to the node
    const in_1_forced: i32 = 10;
    const in_2_forced: i32 = 20;

    // create the node
    var example_node = binary_add_node_type.Create();

    // set its inputs up
    try example_node.SetInput(0, i32, @constCast(&in_1_forced));
    try example_node.SetInput(1, i32, @constCast(&in_2_forced));

    // check the values on the data bus before executing the function
    const in_1_value_before = try example_node.input_data_ports[0].GetValue(*i32);
    const in_2_value_before = try example_node.input_data_ports[1].GetValue(*i32);
    const out_1_value_before = try example_node.output_data_ports[0].GetValue(i32);

    // check
    try expect(in_1_value_before.* == in_1_forced);
    try expect(in_2_value_before.* == in_2_forced);
    try expect(out_1_value_before == 0);

    // try executing, should not fail
    try example_node.Execute();

    // get the values again, and check that the inputs have not changed
    // but the output should have changed how we expect it to have.
    const in_1_value_after = try example_node.input_data_ports[0].GetValue(*i32);
    const in_2_value_after = try example_node.input_data_ports[1].GetValue(*i32);
    const out_1_value_after = try example_node.output_data_ports[0].GetValue(i32);
    const out_1_value_after_expected = in_1_forced + in_2_forced;

    // check
    try expect(in_1_value_after.* == in_1_forced);
    try expect(in_2_value_after.* == in_2_forced);
    try expect(out_1_value_after == out_1_value_after_expected);
}
