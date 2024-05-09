const std = @import("std");
const meta = std.meta;
const builtin = std.builtin;
const Type = builtin.Type;
const Int = Type.Int;
const Union = Type.Union;
const UnionField = Type.UnionField;
const Enum = Type.Enum;
const EnumField = Type.EnumField;
const Declaration = Type.Declaration;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;
const expectError = testing.expectError;
const utils = @import("utils.zig");

/// The functions used to name fields must share this signature.
pub const ComptimeFieldNameFn = *const fn (comptime type, comptime usize) [:0]const u8;

/// The default index naming function for auto-unions.
/// Given a type for the field and the field index, returns a string like this:
/// "field_" + index + "_" + @typeName(T)
/// Eg: For index = 3, T = i32, the string will be "field_3_i32"
pub fn ComptimeDefaultFieldNamer(comptime T: type, comptime index: usize) [:0]const u8 {
    return std.fmt.comptimePrint("field_{d}_{s}", .{ index, @typeName(T) });
}

/// A naming function that names the field just the type name of the field.
/// The 'index' parameter is discarded, but is required for the ABI.
/// Used for optimal auto unions, where there's only one field for each type.
pub fn ComptimeTypeNameFieldNamer(comptime T: type, comptime index: usize) [:0]const u8 {
    _ = index;
    return std.fmt.comptimePrint("{s}", .{@typeName(T)});
}

///////////////////////////////////////////////////////////////////////////////////
// Types
///////////////////////////////////////////////////////////////////////////////////

/// Ease of use wrapper that returns an auto-union type with the default field naming function.
pub fn AutoUnionTypeDefault(comptime Types: []const type) type {
    return AutoUnionTypeCustom(Types, null);
}

/// Creates a union type with fields that are named with the format given by the
/// index naming function with the types given by 'Types' and the alignment of them.
/// If no field naming function is given, the default one ('ComptimeDefaultFieldNamer') will be used.
pub fn AutoUnionTypeCustom(comptime Types: []const type, comptime FieldNamingFunction: ?ComptimeFieldNameFn) type {
    // Exit early if there's no types.
    if (Types.len == 0) @compileError("Cannot create AutoUnionType from an array of types that is zero-size! [Types.len == 0]");

    // If a naming function is given, use it, otherwise use the default.
    const FieldNamer = if (FieldNamingFunction) |F| F else ComptimeDefaultFieldNamer;

    comptime var union_fields: [Types.len]UnionField = undefined;
    comptime var enum_literals: [Types.len]EnumField = undefined;

    // Building the fields of the union from the type array.
    for (Types, 0..) |T, i| {
        enum_literals[i] = EnumField{
            .name = FieldNamer(T, i),
            .value = i,
        };
        union_fields[i] = UnionField{
            .name = FieldNamer(T, i),
            .type = T,
            .alignment = @alignOf(T),
        };
    }

    // Generating the optimal enum tag type
    const optimal_tag_definition = Int{
        .bits = std.math.log2_int_ceil(u16, enum_literals.len),
        .signedness = .unsigned,
    };

    // This will be used as the tag for the enum
    const optimal_tag_int = @Type(Type{ .Int = optimal_tag_definition });

    // The actual std.builtin.Type.Enum definition for the backing enum, to be reified into a real type.
    const enum_definition = Enum{
        .fields = &enum_literals,
        .decls = &[0]Declaration{},
        .is_exhaustive = true,
        .tag_type = optimal_tag_int,
    };

    const enum_reified = @Type(Type{ .Enum = enum_definition });

    // The actual std.builtin.Type.Union definition, to be reified into a real type.
    const union_definition = Union{
        .layout = .Auto,
        .tag_type = enum_reified,
        .fields = &union_fields,
        .decls = &[0]Declaration{},
    };

    const union_reified = @Type(Type{ .Union = union_definition });
    return union_reified;
}

/// Makes an auto-union with a reduced variant of the 'Types' array list.
/// The names for each field are just the names of the type associated with it.
pub fn OptimalAutoUnion(comptime Types: []const type) type {
    comptime {
        const reduced_types = utils.ReduceTypeArray(Types);
        return AutoUnionTypeCustom(reduced_types, ComptimeTypeNameFieldNamer);
    }
}

/// Holds an OptimalAutoUnion type, and offers some API to interact with it
/// and with other OptimalAutoUnion types, including cross-type operations,
/// such as converting a value from one union to another, with error checking.
pub fn OptimalAutoUnionWrapper(comptime Types: []const type) type {
    return struct {
        inner: OAUType,

        /// The internal optimal auto union type generated.
        /// Written here for ease of use and access.
        pub const OAUType = OptimalAutoUnion(Types);
        /// An embedding of the types that generated this one for reflection.
        pub const BaseTypes = Types;
        /// The naming function used for the optimal auto union wrapper
        /// is always going to be the comptime type name field namer
        /// so that reflection is really straight forward with builtins.
        pub const FieldNamingFunction = ComptimeTypeNameFieldNamer;

        /// The error set for this type.
        pub const Errors = error{
            TypeNotFoundInTarget,
            TypeNotFoundInSelf,
        };

        /// Creates an instance of this type.
        /// Does no type checking whatsoever.
        /// Use at your own peril.
        pub fn CreateUnsafe(T: type, value: T) @This() {
            return .{ .inner = @unionInit(OAUType, FieldNamingFunction(T, 0), value) };
        }

        /// Creates an instance of this type.
        /// Checks whether the type exists.
        /// If not, it will return an error.
        pub fn Create(T: type, value: T) !@This() {
            inline for (BaseTypes) |BT| {
                if (BT == T) {
                    return CreateUnsafe(T, value);
                } else return Errors.TypeNotFoundInSelf;
            }
        }

        /// Gets the value of the current active union field given by the type specified.
        /// If the active field is of the type given, the value will be returned.
        /// Otherwise, the code is unreachable. Use at your own peril.
        pub fn GetValueUnsafe(self: @This(), T: type) T {
            switch (self.inner) {
                inline else => |self_value| {
                    const self_type = @TypeOf(self_value);
                    if (self_type == T) {
                        return self_value;
                    }
                    unreachable;
                },
            }
        }

        /// Gets the value of the current active union field given by the type specified.
        /// If the active field is of the type given, the value will be returned.
        /// Otherwise, an error will be returned.
        pub fn GetValue(self: @This(), T: type) !T {
            switch (self.inner) {
                inline else => |self_value| {
                    const self_type = @TypeOf(self_value);
                    if (self_type == T) {
                        return self_value;
                    }
                    return Errors.TypeNotFoundInSelf;
                },
            }
        }

        /// Returns a pointer to the current active union field given by the type specified.
        /// If the active field is of the type given, a pointer to it will be returned.
        /// Otherwise, the code is unreachable. Use at your own peril.
        pub fn GetReferenceUnsafe(self: *@This(), T: type) *T {
            switch (self.inner) {
                inline else => |*self_value| {
                    const self_type = @TypeOf(self_value.*);
                    if (self_type == T) {
                        return self_value;
                    }
                    unreachable;
                },
            }
        }

        /// Returns a pointer to the current active union field given by the type specified.
        /// If the active field is of the type given, a pointer to it will be returned.
        /// Otherwise an error will be returned.
        pub fn GetReference(self: *@This(), T: type) !*T {
            switch (self.inner) {
                inline else => |*self_value| {
                    const self_type = @TypeOf(self_value.*);
                    if (self_type == T) {
                        return self_value;
                    }
                    return Errors.TypeNotFoundInSelf;
                },
            }
        }

        /// Converts another union's active field to the current one's, in type and value.
        /// This does not do any checking. Use at your own peril.
        pub fn ConvertToThisUnsafe(self: @This(), OtherBaseTypes: []const type, OtherUnion: *OptimalAutoUnionWrapper(OtherBaseTypes)) void {
            switch (self.inner) {
                inline else => |self_value| {
                    const self_type = @TypeOf(self_value);
                    const OtherUnionType = OptimalAutoUnionWrapper(OtherBaseTypes);
                    OtherUnion.*.inner = @unionInit(OtherUnionType.OAUType, FieldNamingFunction(self_type, 0), self_value);
                    return;
                },
            }
        }

        /// Converts another union's active field to the current one's, in type and value.
        /// This is a safe variant that checks if the conversion is possible.
        /// Will return an error if the conversion is not possible, due to the other union not having
        /// a field of the type of the current union's active field.
        pub fn ConvertToThis(self: @This(), OtherBaseTypes: []const type, OtherUnion: *OptimalAutoUnionWrapper(OtherBaseTypes)) !void {
            switch (self.inner) {
                inline else => |self_value| {
                    const self_type = @TypeOf(self_value);
                    inline for (OtherBaseTypes) |other_type| {
                        if (self_type == other_type) {
                            const OtherUnionType = OptimalAutoUnionWrapper(OtherBaseTypes);
                            OtherUnion.*.inner = @unionInit(OtherUnionType.OAUType, FieldNamingFunction(self_type, 0), self_value);
                            return;
                        }
                    }
                    return Errors.TypeNotFoundInTarget;
                },
            }
        }
    };
}

///////////////////////////////////////////////////////////////////////////////////
// Utility functions
///////////////////////////////////////////////////////////////////////////////////

/// Checks whether the two unions are the same internally.
/// Will emit a compileError in case of any mismatch.
/// NOTE: This does not check for declarations.
pub fn ComptimeCheckUnions(comptime Union_A: Union, comptime Union_B: Union) void {
    if (Union_A.tag_type) |Union_A_Tag| {
        if (Union_B.tag_type) |Union_B_Tag| {
            ComptimeCheckEnums(Union_A_Tag, Union_B_Tag);
        } else {
            @compileError("Union_B has no backing tag, but Union_A does!");
        }
    } else {
        if (Union_B.tag_type.?) {
            @compileError("Union_A has no backing tag, but Union_B does!");
        }
    }

    comptime if (Union_A.layout != Union_B.layout)
        @compileError(std.fmt.comptimePrint( //
            "Unions do not have the same layout!" ++
            "\n\tUnion_A.layout = '{s}'" ++
            "\n\tUnion_B.layout = '{s}'", .{ @tagName(Union_A.layout), @tagName(Union_B.layout) }));

    ComptimeCheckUnionFields(Union_A.fields, Union_B.fields);
}

/// Checks that both the two types are the same enums.
/// Checks that all entries of the enums are the same, in the same order.
pub fn ComptimeCheckEnums(comptime Enum_A: type, comptime Enum_B: type) void {
    comptime {
        const Enum_A_TypeInfo = @typeInfo(Enum_A);
        if (Enum_A_TypeInfo != .Enum) @compileError("Enum_A is not an enum!");
        const Enum_B_TypeInfo = @typeInfo(Enum_B);
        if (Enum_B_TypeInfo != .Enum) @compileError("Enum_B is not an enum!");

        const Enum_A_Specs = Enum_A_TypeInfo.Enum;
        const Enum_B_Specs = Enum_B_TypeInfo.Enum;
        if (Enum_A_Specs.fields.len != Enum_B_Specs.fields.len) {
            @compileError(std.fmt.comptimePrint( //
                "Enum_A and Enum_B have a different amount of fields!" ++
                "\n\tEnum_A field count: {d}" ++
                "\n\tEnum_B field count: {d}]", .{ Enum_A_Specs.fields.len, Enum_B_Specs.fields.len }));
        }

        for (Enum_A_Specs.fields, 0..) |A_Field, i| {
            const B_Field_name = Enum_B_Specs.fields[i].name;
            if (!std.mem.eql(u8, A_Field.name, B_Field_name)) {
                @compileError(std.fmt.comptimePrint( //
                    "Enum_A and Enum_B have different entries at the same index!" ++
                    "\n\tEnum_A[{d}] = '{s}'" ++
                    "\n\tEnum_B[{d}] = '{s}'", .{ i, A_Field.name, i, B_Field_name }));
            }
        }
    }
}

/// Check that two union field arrays are the same.
/// Will emit a compileError in case of any mismatch.
pub fn ComptimeCheckUnionFields(comptime UnionField_A: []const UnionField, comptime UnionField_B: []const UnionField) void {
    comptime {
        if (UnionField_A.len != UnionField_B.len)
            @compileError(std.fmt.comptimePrint( //
                "Union field arrays do not have the same size!" ++
                "\n\tUnionField_A: [{d}]const UnionField" ++
                "\n\tUnionField_B: [{d}]const UnionField", .{ UnionField_A.len, UnionField_B.len }));

        for (0..UnionField_A.len) |i| {
            if (!std.mem.eql(u8, UnionField_A[i].name, UnionField_B[i].name))
                @compileError(std.fmt.comptimePrint( //
                    "Union fields at index {d} do not have the same name!" ++
                    "\n\tUnionField_A[{d}].name = '{s}'" ++
                    "\n\tUnionField_B[{d}].name = '{s}'", .{ i, i, UnionField_A[i].name, i, UnionField_B[i].name }));

            if (UnionField_A[i].type != UnionField_B[i].type)
                @compileError(std.fmt.comptimePrint( //
                    "Union fields at index {d} do not have the same type!" ++
                    "\n\tUnionField_A[{d}].type = '{s}'" ++
                    "\n\tUnionField_B[{d}].type = '{s}'", .{ i, i, @typeName(UnionField_A[i].type), i, @typeName(UnionField_B[i].type) }));

            if (UnionField_A[i].alignment != UnionField_B[i].alignment)
                @compileError(std.fmt.comptimePrint( //
                    "Union fields at index {d} do not have the same alignment!" ++
                    "\n\tUnionField_A[{d}].alignment = {d}" ++
                    "\n\tUnionField_B[{d}].alignment = {d}", .{ i, i, UnionField_A[i].alignment, i, UnionField_B[i].alignment }));
        }
    }
}

/// Useful for debugging.
/// Will output the layout of a std.builtin.Type.Union as a string
pub fn UnionLayoutToString(comptime U: Union) []const u8 {
    comptime {
        var text: []const u8 = undefined;
        text = std.fmt.comptimePrint( //
            "layout = '{s}'\n" ++
            "tag_type = '{s}'\n" ++
            "field_count = {d}", .{ //
            @tagName(U.layout),
            if (U.tag_type) |tag| @typeName(tag) else "null",
            U.fields.len,
        });

        for (U.fields, 0..) |field, i| {
            text = std.fmt.comptimePrint( //
                "{s}" ++
                "\nfield[{d}]:" ++
                "\n\tname = '{s}'" ++
                "\n\ttype = '{s}'" ++
                "\n\talignment = {d}", .{ //
                text,
                i,
                field.name,
                @typeName(field.type),
                field.alignment,
            });
        }

        text = std.fmt.comptimePrint("{s}\ndeclaration_count = {d}", .{ text, U.decls.len });

        for (U.decls, 0..) |decl, i| {
            text = std.fmt.comptimePrint( //
                "{s}", "\ndecl[{d}]:" ++
                "\n\tname = '{s}'", .{ //
                text,
                i,
                decl.name,
            });
        }

        return text;
    }
}

///////////////////////////////////////////////////////////////////////////////////
// TEST-ZONE
///////////////////////////////////////////////////////////////////////////////////

// This test should be kept up to date with the comptime default field naming function.
test "AutoUnion should return the expected union." {
    const types = [4]type{ i32, i64, usize, f32 };
    const expected_union_tag = enum {
        field_0_i32,
        field_1_i64,
        field_2_usize,
        field_3_f32,
    };
    const expected_union = union(expected_union_tag) {
        // expected layout
        field_0_i32: i32,
        field_1_i64: i64,
        field_2_usize: usize,
        field_3_f32: f32,
    };
    const generated_union = AutoUnionTypeDefault(&types);

    // Uncomment for more specific debugging!
    // @compileError( //
    //     UnionLayoutToString(@typeInfo(expected_union).Union) ++ "\n\n" ++
    //     UnionLayoutToString(@typeInfo(generated_union).Union));

    comptime ComptimeCheckUnions(@typeInfo(expected_union).Union, @typeInfo(generated_union).Union);
}

test "OptimalAutoUnionWrapper conversion tests" {
    const o1 = OptimalAutoUnionWrapper(&[_]type{ i32, i64 });
    const o2 = OptimalAutoUnionWrapper(&[_]type{ i64, i32, f32 });
    const o3 = OptimalAutoUnionWrapper(&[_]type{[]const u8});

    const val_1 = o1{ .inner = o1.OAUType{ .i32 = 10 } };
    var val_2 = o2{ .inner = o2.OAUType{ .i64 = 2 } };
    var val_3 = o3{ .inner = o3.OAUType{ .@"[]const u8" = "AAA" } };

    // val_1's i32 entry should be able to fit inside val_2
    // because val_2 has an i32 entry.
    try val_1.ConvertToThis(o2.BaseTypes, &val_2);

    // these should be possible because these are the active fields
    const good_value_2 = try val_2.GetValue(i32);
    const good_value_1 = try val_1.GetValue(i32);

    // this should fail because i64 is not the active field.
    try expectError(o1.Errors.TypeNotFoundInSelf, val_1.GetValue(i64));

    // this should pass because the conversion should have worked.
    try expect(good_value_1 == good_value_2);

    // Can't convert the i32 from val_1 to any of the types of val_3
    // because val_3 can only hold a []const u8, and not an i32.
    try expectError(o1.Errors.TypeNotFoundInTarget, val_1.ConvertToThis(o3.BaseTypes, &val_3));

    // this should not fail, val_2 has an i32 as its active field
    // a reference to it should be possible to acquire
    const val_2_ref = try val_2.GetReference(i32);

    // modifying the value of the i32 active field of val_2 through this reference
    val_2_ref.* = 20;

    // the value should be modified now
    const modified_value_2 = try val_2.GetValue(i32);

    // and it should be 20.
    try expect(modified_value_2 == 20);
}
