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