const std = @import("std");

/// Converts an array of types to an array of types in (non-const) pointer form.
/// Eg: [i32, i64, f32] -> [*i32, *i64, *f32]
pub fn TypeArrayToPointerArray(comptime Types: []const type) []const type {
    comptime {
        var pointers = [1]type{undefined} ** Types.len;
        for (Types, 0..) |Type, i| {
            pointers[i] = *Type;
        }
        return &pointers;
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

        return reduced_types;
    }
}

/// Returns a string with the type names of every type in the array, separated by a comma and a space.
pub fn ComptimeTypeArrayToString(comptime Types: []const type) []const u8 {
    comptime {
        var text: []const u8 = std.fmt.comptimePrint("{s}", .{@typeName(Types[0])});

        for (1..Types.len) |i| {
            text = std.fmt.comptimePrint("{s}, {s}", .{ text, @typeName(Types[i]) });
        }

        return text;
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
    return &result;
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
    return &result;
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

/// A naming function that names the field just the type name of the field.
/// The 'index' parameter is discarded, but it is still here for compatibility.
/// Used for optimal auto containers, where there's only one field for each type.
pub fn ComptimeTypeNameFieldNamer(comptime T: type, comptime index: usize) [:0]const u8 {
    _ = index;
    return std.fmt.comptimePrint("{s}", .{@typeName(T)});
}

/// A naming function that names the field just the index of it.
/// The 'T' parameter is discarded, but it is still here for compatibility.
/// Used for optimal auto containers, where there's multiple fields with the same type.
pub fn ComptimeIndexFieldNamer(comptime T: type, comptime index: usize) [:0]const u8 {
    _ = T;
    return std.fmt.comptimePrint("{d}", .{index});
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
        return collapsed;
    }
}
