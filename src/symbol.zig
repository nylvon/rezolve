const std = @import("std");

/// Symbols are a way to refer to some data.
pub const SymbolicType = struct {
    /// The actual representation underlying it.
    representation: RepresentationType,
    allocated: bool,

    /// The type that is going to be used for representing symbols.
    pub const RepresentationType = []const u8;

    /// For custom hash-map definition.
    pub const Context = struct {
        pub fn hash(self: @This(), s: SymbolicType) u64 {
            _ = self;
            return std.hash_map.hashString(s.representation);
        }
        pub fn eql(self: @This(), a: SymbolicType, b: SymbolicType) bool {
            _ = self;
            return std.hash_map.eqlString(a.representation, b.representation);
        }
    };

    /// Ease-of-use printing function, that uses an external context for printing.
    /// The writer context must implement the following function:
    ///     fn print(<self type>, comptime []const u8, anytype)
    pub fn tryPrint(self: *SymbolicType, writer_ctx: anytype) !void {
        try writer_ctx.print("{s}", .{self.representation});
    }

    /// Initializes a symbol.
    pub fn init(rep: RepresentationType, isAllocated: bool) SymbolicType {
        return SymbolicType{
            .representation = rep,
            .allocated = isAllocated,
        };
    }

    /// Initializes a symbol and marks its representation as not being allocated.
    /// The intended way of use.
    pub fn initNoAlloc(rep: RepresentationType) SymbolicType {
        return SymbolicType.init(rep, false);
    }

    /// Initializes a symbol and marks its representation as being allocated.
    /// This is mostly used internally. User use of this is discouraged.
    pub fn initFromAlloc(rep: RepresentationType) SymbolicType {
        return SymbolicType.init(rep, true);
    }

    /// De-initalizes the symbol. Also does the memory clean-up.
    pub fn deinit(self: *SymbolicType, allocator: std.mem.Allocator) void {
        if (self.allocated == true) {
            allocator.free(self.representation);
        }
    }
};
