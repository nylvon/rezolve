const std = @import("std");
const testing = std.testing;
const expect = testing.expect;

/// Converts an array of types to an array of types in (non-const) pointer form.
/// Eg: [i32, i64, f32] -> [*i32, *i64, *f32]
pub fn TypeArrayToPointerArray(comptime Types: []const type) []const type {
    comptime {
        var pointers: [Types.len]type = undefined;
        for (Types, 0..) |Type, i| {
            pointers[i] = *Type;
        }
        const const_ptrs = pointers;
        return &const_ptrs;
    }
}

test "TypeArrayToPointerArray" {
    // Some example types to convert to their pointer form
    const sample_types = [_]type{ i32, *i64, **f32 };
    // The expected transformation
    const expected_types = [_]type{ *i32, **i64, ***f32 };
    // The actual transformation
    const generated_types = TypeArrayToPointerArray(&sample_types);

    // They should be the same length
    try expect(generated_types.len == expected_types.len);

    // Check for each in particular.
    inline for (expected_types, 0..) |ET, i| {
        try expect(ET == generated_types[i]);
    }
}

/// Converts an optional array of types to an array of types in (non-const) pointer form.
/// If the array is null, the result will also be null.
/// Used in node.zig for consistency.
pub fn TypeArrayToPointerArrayOptional(comptime Types: ?[]const type) ?[]const type {
    comptime {
        if (Types) |Ts| {
            return TypeArrayToPointerArray(Ts);
        } else return null;
    }
}

test "TypeArrayToPointerArrayOptional" {
    // Some example types to convert to their pointer form
    const sample_types = [_]type{ i32, *i64, **f32 };
    // The expected transformation
    const expected_types = [_]type{ *i32, **i64, ***f32 };
    // The actual transformation
    const generated_types = TypeArrayToPointerArrayOptional(&sample_types);

    // Should be able to capture it, since it should not be null
    if (generated_types) |GT| {
        // They should be the same length
        try expect(GT.len == expected_types.len);

        // Check for each in particular.
        inline for (expected_types, 0..) |ET, i| {
            try expect(ET == GT[i]);
        }
    } else {
        return error.TypeArrayIsNullWhenItReallyShouldNotBe;
    }

    // Should return null for a null parameter
    try expect(TypeArrayToPointerArrayOptional(null) == null);
}

/// Converts an array of types to an array of optionals.
/// Eg: [i32, *i64, *f32] -> [?i32, ?*i64, ?*f32]
pub fn TypeArrayToOptionalArray(comptime Types: []const type) []const type {
    comptime {
        var optionals: [Types.len]type = undefined;
        for (Types, 0..) |Type, i| {
            optionals[i] = ?Type;
        }
        const const_opts = optionals;
        return &const_opts;
    }
}

test "TypeArrayToOptionalArray" {
    // Some example types to convert to their optional form
    const sample_types = [_]type{ i32, *i64, **f32 };
    // The expected transformation
    const expected_types = [_]type{ ?i32, ?*i64, ?**f32 };
    // The actual transformation
    const generated_types = TypeArrayToOptionalArray(&sample_types);

    // They should be the same length
    try expect(generated_types.len == expected_types.len);

    // Check for each in particular.
    inline for (expected_types, 0..) |ET, i| {
        try expect(ET == generated_types[i]);
    }
}

/// Returns an array of types that is a version of the array of types 'Types'
/// but with all duplicates removed.
pub fn ReduceTypeArray(comptime Types: []const type) []const type {
    comptime {
        // Guaranteed to have at least one element.
        var reduced_types: []const type = &[1]type{Types[0]};

        // Assume there are duplicates.
        var need_to_check = true;

        while (need_to_check) {
            // Assume no more duplicates.
            need_to_check = false;

            // Check if there are duplicates.
            for (Types) |New_Type| {
                var found = false;
                // Try to find a dupicate.
                for (reduced_types) |Known_type| {
                    if (New_Type == Known_type) {
                        // Duplicate spotted.
                        found = true;
                    }
                }

                // If it was not a duplicate, add it to the 'reduced_types' array.
                // otherwise don't add it.
                if (!found) {
                    reduced_types = ComptimeArrayAppend(type, reduced_types, New_Type);
                    // There may be more duplicates, loop again.
                    need_to_check = true;
                    break;
                }
            }
        }

        const const_reduced_types = reduced_types;
        return const_reduced_types;
    }
}

test "ReduceTypeArray" {
    // An example array with multiple entries that are the same
    const redundant_array = [_]type{ i32, i32, i32, f32, f32 };
    // The same array, but without the redundant entries
    const expected_array = [_]type{ i32, f32 };
    // The one with its redundancy hopefully removed
    const generated_array = ReduceTypeArray(&redundant_array);

    // They should be the same length
    try expect(generated_array.len == expected_array.len);

    // Check for each in particular.
    inline for (expected_array, 0..) |ET, i| {
        try expect(ET == generated_array[i]);
    }
}

/// Returns a string with the type names of every type in the array, separated by a comma and a space.
pub fn ComptimeTypeArrayToString(comptime Types: []const type) []const u8 {
    comptime {
        var text: []const u8 = std.fmt.comptimePrint("{s}", .{@typeName(Types[0])});

        for (1..Types.len) |i| {
            text = std.fmt.comptimePrint("{s}, {s}", .{ text, @typeName(Types[i]) });
        }

        const const_text = text;
        return const_text;
    }
}

test "ComptimeTypeArrayToString" {
    // An example array
    const example_array = [_]type{ i32, f32, []const u8 };
    // The expected output
    const expected_string = "i32, f32, []const u8";
    // The generated one
    const generated_string = comptime ComptimeTypeArrayToString(&example_array);

    // They should be the same length
    try expect(generated_string.len == expected_string.len);

    // Check for each in particular.
    inline for (generated_string, 0..) |ExpectedChar, i| {
        try expect(ExpectedChar == generated_string[i]);
    }
}

/// Appends one entry of type T to an array of T at compile-time
/// by creating a new array with the items of the given array, plus the new one.
pub fn ComptimeArrayAppend(comptime T: type, comptime target: []const T, comptime add: T) []const T {
    comptime var result = [1]T{undefined} ** (target.len + 1);
    comptime for (target, 0..) |part, i| {
        result[i] = part;
    };
    result[target.len] = add;

    const const_result = result;
    return &const_result;
}

test "ComptimeArrayAppend" {
    // Some example array to be appended to an example integer
    const array_1 = [_]i32{ 10, 20 };
    const example_value: i32 = 30;
    // The expected appended-to array
    const expected_append = [_]i32{ 10, 20, 30 };
    // The generated appended-to array
    const generated_append = comptime ComptimeArrayAppend(i32, &array_1, example_value);

    // They should be the same length
    try expect(generated_append.len == expected_append.len);

    // Check for each in particular.
    inline for (expected_append, 0..) |ExpectedInteger, i| {
        try expect(ExpectedInteger == generated_append[i]);
    }
}

/// Merges two arrays of type T by creating a new array that is equal in size
/// to the sum of their lengths and with the items of the 'target' array, followed by
/// the items of the 'add' array, in the same order they were in their original arrays.
pub fn ComptimeArrayMerge(comptime T: type, comptime target: []const T, comptime add: []const T) []const T {
    comptime var result = [1]T{undefined} ** (target.len + add.len);
    comptime for (target, 0..) |part, i| {
        result[i] = part;
    };
    comptime for (add, 0..) |part, i| {
        result[i + target.len] = part;
    };

    const const_result = result;
    return &const_result;
}

test "ComptimeArrayMerge" {
    // Some example arrays to be merged
    const array_1 = [_]i32{ 10, 20 };
    const array_2 = [_]i32{ 30, 40 };
    // The expected merged array
    const expected_merge = [_]i32{ 10, 20, 30, 40 };
    // The generated merged array
    const generated_merge = comptime ComptimeArrayMerge(i32, &array_1, &array_2);

    // They should be the same length
    try expect(generated_merge.len == expected_merge.len);

    // Check for each in particular.
    inline for (expected_merge, 0..) |ExpectedInteger, i| {
        try expect(ExpectedInteger == generated_merge[i]);
    }
}

/// The functions used to name fields must share this signature.
pub const ComptimeFieldNameFn = *const fn (comptime type, comptime usize) [:0]const u8;

/// The default index naming function for auto containers.
/// Given a type for the field and the field index, returns a string like this:
/// "field_" + index + "_" + @typeName(T)
/// Eg: For index = 3, T = i32, the string will be "field_3_i32"
pub fn ComptimeDefaultFieldNamer(comptime T: type, comptime index: usize) [:0]const u8 {
    return std.fmt.comptimePrint("field_{d}_{s}", .{ index, @typeName(T) });
}

test "ComptimeDefaultFieldNamer" {
    // Example field entry
    const example_type = i32;
    const example_index = 0;
    // The expected generated string for this case
    const expected_string = "field_0_i32";
    // The generated string for this case
    const generated_string = comptime ComptimeDefaultFieldNamer(example_type, example_index);

    // They should be the same length
    try expect(generated_string.len == expected_string.len);

    // Check for each in particular.
    inline for (generated_string, 0..) |ExpectedChar, i| {
        try expect(ExpectedChar == generated_string[i]);
    }
}

/// A naming function that names the field just the type name of the field.
/// The 'index' parameter is discarded, but it is still here for compatibility.
/// Used for optimal auto containers, where there's only one field for each type.
pub fn ComptimeTypeNameFieldNamer(comptime T: type, comptime index: usize) [:0]const u8 {
    _ = index;
    return std.fmt.comptimePrint("{s}", .{@typeName(T)});
}

test "ComptimeTypeNameFieldNamer" {
    // Example field entry
    const example_type = i32;
    const example_index = 0;
    // The expected generated string for this case
    const expected_string = "i32";
    // The generated string for this case
    const generated_string = comptime ComptimeTypeNameFieldNamer(example_type, example_index);

    // They should be the same length
    try expect(generated_string.len == expected_string.len);

    // Check for each in particular.
    inline for (generated_string, 0..) |ExpectedChar, i| {
        try expect(ExpectedChar == generated_string[i]);
    }
}

/// A naming function that names the field just the index of it.
/// The 'T' parameter is discarded, but it is still here for compatibility.
/// Used for optimal auto containers, where there's multiple fields with the same type.
pub fn ComptimeIndexFieldNamer(comptime T: type, comptime index: usize) [:0]const u8 {
    _ = T;
    return std.fmt.comptimePrint("{d}", .{index});
}

test "ComptimeIndexFieldNamer" {
    // Example field entry
    const example_type = i32;
    const example_index = 0;
    // The expected generated string for this case
    const expected_string = "0";
    // The generated string for this case
    const generated_string = comptime ComptimeIndexFieldNamer(example_type, example_index);

    // They should be the same length
    try expect(generated_string.len == expected_string.len);

    // Check for each in particular.
    inline for (generated_string, 0..) |ExpectedChar, i| {
        try expect(ExpectedChar == generated_string[i]);
    }
}

/// Merges multiple lines into one, adding a predefined space before each new line character.
pub fn ComptimeSpacedPrint(comptime spaces: usize, comptime lines: []const [:0]const u8) [:0]const u8 {
    comptime {
        var collapsed: [:0]const u8 = "";
        for (lines) |line| {
            collapsed = std.fmt.comptimePrint("{s}", .{collapsed});
            for (0..spaces) |i| {
                _ = i;
                collapsed = std.fmt.comptimePrint("{s} ", .{collapsed});
            }
            collapsed = std.fmt.comptimePrint("{s}{s}\n", .{ collapsed, line });
        }

        const const_collapsed = collapsed;
        return const_collapsed;
    }
}

test "ComptimeSpacedPrint - Spacing = 0" {
    // Example lines to be merged
    const example_spacing = 0;
    const example_lines = [_][:0]const u8{ "ABC", "DEF", "XYZ" };
    // The expected generated string for this case
    const expected_string = "ABC\nDEF\nXYZ\n";
    // The generated string for this case
    const generated_string = comptime ComptimeSpacedPrint(example_spacing, &example_lines);

    // They should be the same length
    try expect(generated_string.len == expected_string.len);

    // Check for each in particular.
    inline for (generated_string, 0..) |ExpectedChar, i| {
        try expect(ExpectedChar == generated_string[i]);
    }
}

test "ComptimeSpacedPrint - Spacing = 4" {
    // Example lines to be merged
    const example_spacing = 4;
    const example_lines = [_][:0]const u8{ "ABC", "DEF", "XYZ" };
    // The expected generated string for this case
    const expected_string = "    ABC\n    DEF\n    XYZ\n";
    // The generated string for this case
    const generated_string = comptime ComptimeSpacedPrint(example_spacing, &example_lines);

    // They should be the same length
    try expect(generated_string.len == expected_string.len);

    // Check for each in particular.
    inline for (generated_string, 0..) |ExpectedChar, i| {
        try expect(ExpectedChar == generated_string[i]);
    }
}
