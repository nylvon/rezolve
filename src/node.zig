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
    comptime {
        const InputPointers = utils.TypeArrayToPointerArrayOptional(InputTypes);
        const InputBusReferenceType = DataBusReferenceType(InputPointers);
        const OutputBusReferenceType = DataBusReferenceType(OutputTypes);

        switch (InputBusReferenceType) {
            void => switch (OutputBusReferenceType) {
                void => return *const fn () anyerror!void,
                else => return *const fn (OutputBusReferenceType) anyerror!void,
            },
            else => switch (OutputBusReferenceType) {
                void => return *const fn (InputBusReferenceType) anyerror!void,
                else => return *const fn (InputBusReferenceType, OutputBusReferenceType) anyerror!void,
            },
        }
    }
}

/// Returns the type of a data bus by creating an array of Optimal Auto Union Wrappers
/// given by the length and by the types of 'Types'.
/// If 'Types' is null, this will return void, indicating a lack of a data bus.
pub fn DataBusType(comptime Types: ?[]const type) type {
    comptime {
        if (Types) |Ts| {
            const DataBusEntry = AutoUnion(Ts);
            const DataBus = [Ts.len]DataBusEntry;
            return DataBus;
        } else return void;
    }
}

/// Returns the type of a referenced data bus by creating an array of Optimal Auto Union Wrappers
/// given by the length and by the types of 'Types'.
/// If 'Types' is null, this will return void, indicating a lack of a data bus.
pub fn DataBusReferenceType(comptime Types: ?[]const type) type {
    comptime {
        if (Types) |Ts| {
            const DataBusEntry = AutoUnion(Ts);
            const DataBusReference = *[Ts.len]DataBusEntry;
            return DataBusReference;
        } else return void;
    }
}
/// Returns a node type that has an input data bus, an output data bus, and a function that maps
/// the inputs to the outputs.
/// The input types get converted to an optimal auto union wrapper, and then the input data bus is formed
/// as an array of these new union types. The same goes for the output data bus.
/// The optimal auto union wrapper is used because of its flexibility as a type, not having the same
/// issues that a generated auto struct would have, such as not being able to access its fields because
/// the @field builtin can only take in field names that are known at compile time.
/// This adds its own complexities, but the important take aways are:
///     This function returns a node type.
///     You can connect multiple nodes between eachother (and other types) with some type agnostic functions.
///     They are the backbone of the ReZolve system.
pub fn NodeType(comptime InputTypes: ?[]const type, comptime OutputTypes: ?[]const type, comptime FunctionReference: NodeTypeFunction(InputTypes, OutputTypes)) type {
    return struct {
        input_data_ports: InputDataBusType,
        output_data_ports: OutputDataBusType,

        /// The base types that were used in constructing this type, if not null;
        pub const InputTypesBase = if (InputTypes) |IT| IT else void;
        pub const OutputTypesBase = if (OutputTypes) |OT| OT else void;
        pub const InputTypesBaseLength = if (InputTypes) |IT| IT.len else @as(usize, 0);
        pub const OutputTypesBaseLength = if (OutputTypes) |OT| OT.len else @as(usize, 0);

        /// The derived optimal auto union wrapper types for the entries to the data buses.
        pub const InputDataBusEntryType = if (InputTypes) |IT| AutoUnion(utils.TypeArrayToPointerArray(IT)) else void;
        pub const OutputDataBusEntryType = if (OutputTypes) |OT| AutoUnion(OT) else void;

        /// The derived data bus types.
        pub const InputDataBusType = DataBusType(utils.TypeArrayToPointerArrayOptional(InputTypes));
        pub const OutputDataBusType = DataBusType(OutputTypes);

        /// The default values of the data buses.
        pub const DefaultInputDataBusState = GetDefaultInputDataBusState();
        pub const DefaultOutputDataBusState = GetDefaultOutputDataBusState();

        /// The derived function type, inferred from where it was created, in the parameter type.
        pub const FunctionType = @TypeOf(Function);

        /// The actual function pointer to be used. Since it's const, it should be optimized away nicely.
        /// Ideally the function should be marked as inline, too.
        pub const Function = FunctionReference;

        /// The complete error set used for nodes.
        pub const Errors = error{
            InvalidIndex,
            LacksBus,
        };

        /// Ease of use enum to use in functions that can refer to either bus.
        pub const Bus = enum { Input, Output };

        /// If the index is within bounds, this will return void.
        /// If it's not, or if the bus doesn't exist, this will return an error.
        pub fn IndexCheck(index: usize, bus: Bus) !void {
            switch (bus) {
                .Input => {
                    if (InputTypesBaseLength == 0) return Errors.LacksBus;
                    if (index >= InputTypesBaseLength) return Errors.InvalidIndex;
                },
                .Output => {
                    if (OutputTypesBaseLength == 0) return Errors.LacksBus;
                    if (index >= OutputTypesBaseLength) return Errors.InvalidIndex;
                },
            }
        }

        /// Sets an entry of the input data bus to point to a target.
        pub fn SetInput(self: *@This(), index: usize, T: type, target: *T) !void {
            try IndexCheck(index, .Input);

            self.input_data_ports[index] = try InputDataBusEntryType.Create(*T, target);
        }

        /// Sets the target's underlying pointer to point to an entry of the output data bus.
        pub fn SetOutput(self: *@This(), index: usize, T: type, target: **T) !void {
            try IndexCheck(index, .Output);

            target.* = try self.output_data_ports[index].GetReference(T);
        }

        /// Gets the value of a data entry on the specified bus at the given index.
        /// If the input bus is mentioned, the pointer will be dereferenced.
        /// NOTE: The if checks are because the compiler sometimes can't figure out that it's not a void type
        ///       that we're indexing, even though it should be known at this stage.
        pub fn GetValue(self: *@This(), index: usize, bus: Bus, T: type) !T {
            try IndexCheck(index, bus);
            switch (bus) {
                .Input => {
                    if (InputDataBusType != void) {
                        const input_pointer = try self.input_data_ports[index].GetValue(*T);
                        return input_pointer.*;
                    } else unreachable;
                },
                .Output => {
                    if (OutputDataBusType != void) {
                        return try self.output_data_ports[index].GetValue(T);
                    } else unreachable;
                },
            }
        }

        /// Gets the reference to a data entry on the specified bus at the given index.
        /// NOTE: The if checks are because the compiler sometimes can't figure out that it's not a void type
        ///       that we're indexing, even though it should be known at this stage.
        pub fn GetReference(self: *@This(), index: usize, bus: Bus, T: type) !*T {
            try IndexCheck(index, bus);
            switch (bus) {
                .Input => {
                    if (InputDataBusType != void) {
                        const input_pointer = try self.input_data_ports[index].GetReference(*T);
                        return input_pointer.*;
                    } else unreachable;
                },
                .Output => {
                    if (OutputDataBusType != void) {
                        return try self.output_data_ports[index].GetReference(T);
                    } else unreachable;
                },
            }
        }

        /// Gets a double pointer to an entry on the input data bus.
        /// This is used in order to modify an input pointer from the outside.
        /// Used for the SetOutput function in the context of other nodes.
        pub fn PointToInput(self: *@This(), index: usize, T: type) !**T {
            try IndexCheck(index, .Input);

            if (InputDataBusType != void) {
                const input_double_pointer = try self.input_data_ports[index].GetReference(*T);
                return input_double_pointer;
            } else unreachable;
        }

        /// This will return the original type of the data entry in the data bus definitions.
        /// If the input bus is selected, the type will be in (non-const) pointer form.
        pub fn GetType(bus: Bus, index: usize) !type {
            try IndexCheck(index, bus);
            switch (bus) {
                .Input => return *InputTypesBase[index],
                .Output => return OutputTypesBase[index],
            }
        }

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

        /// Gets the default state of the input data bus type,
        /// with all input entries having their active field
        /// specified by the input type at their mapped index.
        pub fn GetDefaultInputDataBusState() InputDataBusType {
            comptime {
                var inputs: InputDataBusType = undefined;
                if (InputDataBusType != void) {
                    for (utils.TypeArrayToPointerArray(InputTypes.?), 0..) |IT, i| {
                        inputs[i] = try InputDataBusEntryType.CreateUndefined(IT);
                    }
                }
                return inputs;
            }
        }

        /// Gets the default state of the output data bus type,
        /// with all output entries having their active field
        /// specified by the output type at their mapped index.
        pub fn GetDefaultOutputDataBusState() OutputDataBusType {
            comptime {
                var outputs: OutputDataBusType = undefined;
                if (OutputDataBusType != void) {
                    for (OutputTypes.?, 0..) |OT, i| {
                        outputs[i] = try OutputDataBusEntryType.CreateUndefined(OT);
                    }
                }
                return outputs;
            }
        }

        /// Creates an empty node of this type.
        /// Sets the input and output buses to their default state.
        pub fn Create() @This() {
            return .{ .input_data_ports = DefaultInputDataBusState, .output_data_ports = DefaultOutputDataBusState };
        }

        /// Returns a text representation of its layout, across multiple lines.
        pub fn LayoutToString() [][:0]const u8 {
            // Borders
            comptime {
                var lines: [7][:0]const u8 = [1][:0]const u8{""} ** 7;

                var input_types_line: [:0]const u8 = "|";
                var output_types_line: [:0]const u8 = "|";
                if (InputTypes) |ITs| {
                    for (ITs) |IT| {
                        input_types_line = std.fmt.comptimePrint("{s}{s}|", .{ input_types_line, @typeName(*IT) });
                    }
                } else input_types_line = std.fmt.comptimePrint("{s} x |", .{input_types_line});
                if (OutputTypes) |OTs| {
                    for (OTs) |OT| {
                        output_types_line = std.fmt.comptimePrint("{s}{s}|", .{ output_types_line, @typeName(OT) });
                    }
                } else input_types_line = std.fmt.comptimePrint("{s} x |", .{.output_types_line});

                if (input_types_line.len > output_types_line.len) {
                    const delta_output = input_types_line.len - output_types_line.len;
                    const shift = delta_output / 2;
                    const extra = delta_output - 2 * shift;

                    var pad_left: [:0]const u8 = "";
                    var pad_right: [:0]const u8 = "";
                    for (0..shift) |i| {
                        _ = i;
                        pad_left = std.fmt.comptimePrint("{s} ", .{pad_left});
                        pad_right = std.fmt.comptimePrint("{s} ", .{pad_right});
                    }
                    for (0..extra) |i| {
                        _ = i;
                        pad_right = std.fmt.comptimePrint("{s} ", .{pad_right});
                    }

                    const new_output_line: [:0]const u8 = std.fmt.comptimePrint("|{s}{s}{s}|", .{
                        //
                        pad_left,
                        output_types_line[1 .. output_types_line.len - 1],
                        pad_right,
                    });

                    output_types_line = new_output_line;
                } else {
                    const delta_output = output_types_line.len - input_types_line.len;
                    const shift = delta_output / 2;
                    const extra = delta_output - 2 * shift;

                    var pad_left: [:0]const u8 = "";
                    var pad_right: [:0]const u8 = "";
                    for (0..shift) |i| {
                        _ = i;
                        pad_left = std.fmt.comptimePrint("{s} ", .{pad_left});
                        pad_right = std.fmt.comptimePrint("{s} ", .{pad_right});
                    }
                    for (0..extra) |i| {
                        _ = i;
                        pad_right = std.fmt.comptimePrint("{s} ", .{pad_right});
                    }

                    const new_input_line: [:0]const u8 = std.fmt.comptimePrint("|{s}{s}{s}|", .{
                        //
                        pad_left,
                        input_types_line[1 .. output_types_line.len - 1],
                        pad_right,
                    });

                    input_types_line = new_input_line;
                }

                const width = input_types_line.len;
                var function_line: [:0]const u8 = "|";
                var wall_line: [:0]const u8 = "--";

                const f_point_1 = if (@mod(width, 2) == 0) width / 2 - 2 else width / 2 - 1;
                const f_point_2 = width / 2 - 1;

                for (0..width - 2) |i| {
                    if (i == f_point_1) {
                        function_line = std.fmt.comptimePrint("{s}f", .{function_line});
                    } else if (i == f_point_2) {
                        function_line = std.fmt.comptimePrint("{s}n", .{function_line});
                    } else {
                        function_line = std.fmt.comptimePrint("{s} ", .{function_line});
                    }
                    wall_line = std.fmt.comptimePrint("{s}-", .{wall_line});
                }
                function_line = std.fmt.comptimePrint("{s}|", .{function_line});

                lines[0] = wall_line;
                lines[1] = input_types_line;
                lines[2] = wall_line;
                lines[3] = function_line;
                lines[4] = wall_line;
                lines[5] = output_types_line;
                lines[6] = wall_line;

                return &lines;
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
    const out_types = comptime [_]type{i32};
    const in_types = comptime [_]type{ i32, i32 };

    const in_oau_type = comptime AutoUnion(utils.TypeArrayToPointerArray(&in_types));
    const out_oau_type = comptime AutoUnion(&out_types);

    // some example forced input values.
    const in_1_forced: i32 = 10;
    const in_2_forced: i32 = 20;
    const out_1_before: i32 = 0;

    // some example values for two inputs of some node, and a temporary output value.
    const in_1 = try in_oau_type.Create(*i32, @constCast(&in_1_forced));
    const in_2 = try in_oau_type.Create(*i32, @constCast(&in_2_forced));
    const out_1 = try out_oau_type.Create(i32, out_1_before);

    // the simulated data buses
    var in_bus: DataBusType(utils.TypeArrayToPointerArray(&in_types)) = .{ in_1, in_2 };
    var out_bus: DataBusType(&out_types) = .{out_1};

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
test "NodeType Binary Test, simulated inputs" {
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

    // check
    // the function has not been executed, so the result should be zero.
    try expect(try example_node.GetValue(0, .Input, i32) == in_1_forced);
    try expect(try example_node.GetValue(1, .Input, i32) == in_2_forced);
    try expect(try example_node.GetValue(0, .Output, i32) == 0);

    // try executing, should not fail
    try example_node.Execute();

    // check
    // the function has been executed, so the result should be the one we're anticipating.
    try expect(try example_node.GetValue(0, .Input, i32) == in_1_forced);
    try expect(try example_node.GetValue(1, .Input, i32) == in_2_forced);
    try expect(try example_node.GetValue(0, .Output, i32) == in_1_forced + in_2_forced);
}

/// Generates a function that generates a value of type 'T'
/// Example usage:
///     const Generate10asI32 = GenerateAs(i32).WithValue(10);
///     const my_value = Generate10asI32();
pub fn GenerateAs(comptime T: type) *const fn (T) (*const fn () T) {
    return struct {
        pub fn WithValue(comptime value: T) *const fn () T {
            return struct {
                pub fn Generate() T {
                    return value;
                }
            }.Generate;
        }
    }.WithValue;
}

/// Generates a function for an unary generator node.
pub fn GeneratorFunction(comptime OutputTypes: []const type, comptime T: type, comptime Value: T) NodeTypeFunction(null, OutputTypes) {
    return struct {
        pub fn Generate(outputs: DataBusReferenceType(OutputTypes)) !void {
            const out_0 = try outputs[0].GetReference(T);
            out_0.* = Value;
        }
    }.Generate;
}

// Same test, but now with real nodes that are generating the input values through cascading.
test "NodeType Binary Test, real inputs" {
    // Defining the output types of the generators
    const generator_out_types = &[_]type{i32};
    // The values of the generators
    const generator_1_value: i32 = 10;
    const generator_2_value: i32 = 20;
    // The functions of the generator nodes
    const generator_1_function = comptime GeneratorFunction(generator_out_types, i32, generator_1_value);
    const generator_2_function = comptime GeneratorFunction(generator_out_types, i32, generator_2_value);
    // The actual generator node types
    const generator_1_node_type = NodeType(null, generator_out_types, generator_1_function);
    const generator_2_node_type = NodeType(null, generator_out_types, generator_2_function);

    // Defining the input and output types of the adder
    const adder_in_types = &[_]type{ i32, i32 };
    const adder_out_types = &[_]type{i32};
    // Defining the actual adder node type
    const adder_node_type = NodeType(adder_in_types, adder_out_types, BinaryAddFn);

    // instantiating the nodes
    var generator_1 = generator_1_node_type.Create();
    var generator_2 = generator_2_node_type.Create();
    var adder_1 = adder_node_type.Create();

    // set the inputs to the generator outputs
    try adder_1.SetInput(0, i32, try generator_1.GetReference(0, .Output, i32));
    try adder_1.SetInput(1, i32, try generator_2.GetReference(0, .Output, i32));
    // OR:
    // set the outputs of the generators as the inputs to the adder
    try generator_1.SetOutput(0, i32, try adder_1.PointToInput(0, i32));
    try generator_2.SetOutput(0, i32, try adder_1.PointToInput(1, i32));

    //
    //  STAGE 0
    //      Generators are not outputting anything, outputs should be zero.
    //      Addition shoud yield 0.
    //

    // make sure that the outputs of the generators are outputting the right value
    try expect(try generator_1.GetValue(0, .Output, i32) == 0);
    try expect(try generator_2.GetValue(0, .Output, i32) == 0);

    // the inputs should be the same as the generators' outputs.
    try expect(try adder_1.GetValue(0, .Input, i32) == 0);
    try expect(try adder_1.GetValue(1, .Input, i32) == 0);
    try expect(try adder_1.GetValue(0, .Output, i32) == 0);

    // try to add 0 and 0 to get 0
    try adder_1.Execute();

    // check
    try expect(try adder_1.GetValue(0, .Input, i32) == 0);
    try expect(try adder_1.GetValue(1, .Input, i32) == 0);
    try expect(try adder_1.GetValue(0, .Output, i32) == 0);

    //
    //  STAGE 1
    //      Generator 1 should be outputting 10, whilst generator 2 should be outputting 0.
    //      Addition shoud yield 10.
    //

    // enable generator 1
    try generator_1.Execute();

    // make sure that the outputs of the generators are outputting the right value
    try expect(try generator_1.GetValue(0, .Output, i32) == generator_1_value);
    try expect(try generator_2.GetValue(0, .Output, i32) == 0);

    // the inputs should be the same as the generators' outputs.
    try expect(try adder_1.GetValue(0, .Input, i32) == generator_1_value);
    try expect(try adder_1.GetValue(1, .Input, i32) == 0);
    try expect(try adder_1.GetValue(0, .Output, i32) == 0);

    // try to add 10 and 0 to get 0
    try adder_1.Execute();

    // check
    try expect(try adder_1.GetValue(0, .Input, i32) == generator_1_value);
    try expect(try adder_1.GetValue(1, .Input, i32) == 0);
    try expect(try adder_1.GetValue(0, .Output, i32) == generator_1_value);

    //
    //  STAGE 2
    //      Generator 1 should be outputting 10, whilst generator 2 should be outputting 20.
    //      Addition shoud yield 30.
    //

    // enable generator 2 as well
    try generator_2.Execute();

    // make sure that the outputs of the generators are outputting the right value
    try expect(try generator_1.GetValue(0, .Output, i32) == generator_1_value);
    try expect(try generator_2.GetValue(0, .Output, i32) == generator_2_value);

    // the inputs should be the same as the generators' outputs.
    try expect(try adder_1.GetValue(0, .Input, i32) == generator_1_value);
    try expect(try adder_1.GetValue(1, .Input, i32) == generator_2_value);
    // we should expect the last result to still remain in the buffer.
    try expect(try adder_1.GetValue(0, .Output, i32) == generator_1_value);

    // try to add 10 and 20 to get 30
    try adder_1.Execute();

    // check
    try expect(try adder_1.GetValue(0, .Input, i32) == generator_1_value);
    try expect(try adder_1.GetValue(1, .Input, i32) == generator_2_value);
    try expect(try adder_1.GetValue(0, .Output, i32) == generator_1_value + generator_2_value);
}

// NOTE: Uncomment the code below if you want to see some visual demo for printing the layout of some complex node.
// /// This function does absolutely nothing., but a node needs one in order to be defined.
// fn PrintingLayoutsSample(inputs: DataBusReferenceType(utils.TypeArrayToPointerArray(&[_]type{ i64, f64, []const u8, bool })), outputs: DataBusReferenceType(&[_]type{ []f64, []bool })) !void {
//     _ = inputs;
//     _ = outputs;
// }

// // Uncomment this test if you want to see how the layout of a node can be inspected by visually.
// test "Printing Layouts" {
//     const input_types = &[_]type{ i64, f64, []const u8, bool };
//     const output_types = &[_]type{ []f64, []bool };
//     const node_type = NodeType(input_types, output_types, PrintingLayoutsSample);

//     comptime {
//         @compileError(utils.ComptimeSpacedPrint(0, node_type.LayoutToString()));
//     }
// }
