const std = @import("std");

/// Represents an equation by a tree of operations,
/// and possesses a store of data that includes parameters,
/// constants and cached operation results.
/// TODO: Equation is currently managed. Maybe make it unmanaged
/// TODO: and derive the managed type from it.
pub const EquationUnmanaged = struct {
    /// Defined parameters, constants and cached operation results.
    data_store: DataStoreType,
    /// Root equation node, very first one.
    root_node: ?NodeType,

    /// Initializes the equation and the data store.
    pub fn init(allocator: std.mem.Allocator) EquationUnmanaged {
        return EquationUnmanaged{
            .root_node = null,
            .data_store = DataStoreType.init(allocator),
        };
    }

    /// Recursively de-initializes the nodes and the data store.
    pub fn deinit(self: *EquationUnmanaged, allocator: std.mem.Allocator) void {
        if (self.root_node) |*root| {
            root.deinit(allocator);
        }
        var iter = self.data_store.iterator();
        while (iter.next()) |*entry| {
            entry.key_ptr.deinit(allocator);
            entry.value_ptr.deinit(allocator);
        }
        self.data_store.deinit();
    }

    /// This function will insert a new root node with the data given.
    /// If the equation already has a root, it will reset the entire operation tree.
    /// (This will delete the operation results in the data store, but will not clear
    /// the parameters, constants and other data points not affiliated with operations)
    /// TODO: Maybe reconsider the reset. Option for it?
    pub fn changeRoot(self: *EquationUnmanaged, node_data: ?InnerNodeType, allocator: std.mem.Allocator) !*NodeType {
        if (self.root_node) |*r| {
            r.deinit(allocator);
        }
        self.root_node = NodeType.initFull(node_data, allocator);
        return &self.root_node.?;
    }

    pub const EquationErrors = error{SymbolAlreadyDefined};

    /// TODO: Wrap the entire thing in some API.
    /// TODO: With stuff like: addOperator(), defineOperator(), addReference(), etc...

    //
    ///
    pub fn tryDefineData(self: *EquationUnmanaged, data: *DataType, override_if_exists: bool) !void {
        if (self.data_store.getEntry(data.getSymbol())) |*entry| {
            if (override_if_exists) {
                entry.value_ptr.* = data.*;
            } else {
                return EquationErrors.SymbolAlreadyDefined;
            }
        } else {
            try self.data_store.put(data.getSymbol(), data.*);
        }
    }

    /// Attaches an already existent node to a parent node in the equation tree.
    pub fn tryAttachNode(self: *EquationUnmanaged, parent: *NodeType, new_child: *NodeType) !*NodeType {
        _ = self; // Using self to keep API consistent. Not actually using it here, though. Syntactic sugar.
        return try parent.addChildByNode(new_child);
    }

    /// Creates a new node and attaches it to a parent node in the equation tree.
    pub fn tryAddNode(self: *EquationUnmanaged, parent: *NodeType, new_child_data: InnerNodeType, allocator: std.mem.Allocator) !*NodeType {
        _ = self;
        return try parent.addChildByData(new_child_data, allocator);
    }

    pub fn tryAddReference(self: *EquationUnmanaged, parent: *NodeType, new_child_reference_symbol: SymbolicType, force_define_if_not_defined: bool, allocator: std.mem.Allocator) !*NodeType {
        // try to find what it is referencing
        if (self.tryFindSymbol(new_child_reference_symbol)) |symbol_definition| {
            // if found, make a reference node pointing to it
            const new_reference = SymbolicReferenceType.initFull(symbol_definition.getSymbol());
            const symbol_node = InnerNodeType{ .Reference = new_reference };
            return try parent.addChildByData(symbol_node, allocator);
        } else if (force_define_if_not_defined) {
            // not referencing anything, so we'll make a new constant for it to reference for now.
            const definition = SymbolicBaseDataType.initFull(new_child_reference_symbol, .{ .NotComputed = DataType.UnknownValue });
            const wrapped_definition = DataType{ .Constant = definition };
            try self.data_store.put(new_child_reference_symbol, wrapped_definition);

            // Then define a reference to this new constant.
            const new_reference = SymbolicReferenceType.init(new_child_reference_symbol.representation, false);
            const wrapped_reference = DataType{ .Reference = new_reference };

            // name the reference something else
            const allocated_symbol_rep = try std.fmt.allocPrint(allocator, "ref({s})", .{new_child_reference_symbol.representation});
            try self.data_store.put(SymbolicType.init(allocated_symbol_rep, true), wrapped_reference);
            const new_reference_node = InnerNodeType{ .Reference = new_reference };
            return try parent.addChildByData(new_reference_node, allocator);
        } else return SymbolicReferenceType.ReferenceError.NotFound;
    }

    ///
    pub fn tryFindSymbol(self: *EquationUnmanaged, symbol: SymbolicType) ?*DataType {
        if (self.data_store.getEntry(symbol)) |*symbol_definition| {
            return symbol_definition.value_ptr;
        }
        return null;
    }

    /// Ease-of-use printing function, that uses an external context for printing.
    /// The writer context must implement the following function:
    ///     fn print(<self type>, comptime []const u8, anytype)
    /// TODO: Check that writer_ctx implements the required interface.
    pub fn tryPrint(self: *EquationUnmanaged, writer_ctx: anytype, strict: bool) !void {
        var iter = self.data_store.iterator();
        while (iter.next()) |*entry| {
            try entry.key_ptr.tryPrint(writer_ctx);
            try writer_ctx.print(" >> ", .{});
            try entry.value_ptr.tryPrint(writer_ctx);
            try writer_ctx.print("\n", .{});
        }
        if (self.root_node) |*root| {
            try root.tryPrint(writer_ctx, 0, strict);
        }
    }

    //
    /// The type used to hold the data associated with the equation.
    /// TODO: Maybe rethink this? Hash maps are cool, but are they the proper choice?
    /// TODO: Probably for large equations, but for small ones?
    /// TODO: What about a customizable one? Auto-set the type.
    /// TODO: Commute from some simple structure, like an array of string!datatype entries
    /// TODO: Into the string hashmap of symbols. Or maybe not?
    ///
    pub const DataStoreType = std.HashMap(SymbolicType, DataType, SymbolicType.Context, std.hash_map.default_max_load_percentage);

    /// Delimitates what type of data node one entry into the data store is.
    pub const DataSource = enum { Constant, Reference, OperationResult };

    /// Holds information about the equation's data.
    pub const DataType = union(DataSource) {
        Constant: SymbolicBaseDataType,
        Reference: SymbolicReferenceType,
        OperationResult: SymbolicBaseDataType,

        /// The namespace used for all data errors.
        /// TODO: Currently not using these. Maybe consider if they're needed at all?
        pub const DataErrors = struct {
            pub const DefinitionError = error{
                LacksValue,
            };
            pub const LookupError = error{
                NotFound,
            };
        };

        pub fn deinit(self: *DataType, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .Constant => |*c| c.deinit(allocator),
                .Reference => |*r| r.deinit(allocator),
                .OperationResult => |*opr| opr.deinit(allocator),
            }
        }

        /// Get the value of some data entry.
        /// Does not unwrap the value, or do any error reporting.
        /// Will require you to filter it according to your need.
        pub fn getValueUnsafe(self: *DataType, store: *DataStoreType) *ValueType {
            switch (self.*) {
                .Constant => |c| return c.value,
                .Reference => |*r| {
                    const result = try r.tryFind(store);
                    return try result.getValueUnsafe(store);
                },
                .OperationResult => |o| return o.value,
            }
        }

        /// Value that will be returned if a value is unknown.
        pub const UnknownValue = InnerValueType{ .Integer = 0 };

        /// Filters the value such that it makes sense.
        pub fn getValue(self: *DataType) InnerValueType {
            switch (self.getValueUnsafe().*) {
                .NotComputed => return UnknownValue,
                .NeedsRecompute => |nrc| return nrc,
                .Computed => |c| return c,
            }
        }

        /// Gets the symbol of the data.
        pub fn getSymbol(self: *DataType) SymbolicType {
            switch (self.*) {
                .Constant => |*c| return c.symbol,
                .Reference => |*r| return r.symbol_reference,
                .OperationResult => |*opr| return opr.symbol,
            }
        }

        /// Ease-of-use printing function, that uses an external context for printing.
        /// The writer context must implement the following function:
        ///     fn print(<self type>, comptime []const u8, anytype)
        /// TODO: Check that writer_ctx implements the required interface.
        pub fn tryPrint(self: *DataType, writer_ctx: anytype) !void {
            switch (self.*) {
                .Constant => |*c| {
                    try c.tryPrint(writer_ctx);
                    try writer_ctx.print(" [Constant]", .{});
                },
                .Reference => |*r| {
                    try r.tryPrint(writer_ctx);
                    try writer_ctx.print(" [Reference]", .{});
                },
                .OperationResult => |*opr| {
                    try opr.tryPrint(writer_ctx);
                    try writer_ctx.print(" [Operation Result]", .{});
                },
            }
        }
    };

    /// TODO: Think of some description for this type.
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

        pub fn init(rep: RepresentationType, isAllocated: bool) SymbolicType {
            return SymbolicType{
                .representation = rep,
                .allocated = isAllocated,
            };
        }

        pub fn deinit(self: *SymbolicType, allocator: std.mem.Allocator) void {
            if (self.allocated == true) {
                allocator.free(self.representation);
            }
        }
    };

    /// Details meta-data about the value of some value.
    pub const ValueState = enum { NotComputed, NeedsRecompute, Computed };
    /// Describes the way a value is represented.
    pub const ValueMode = enum { Integer, Real };

    /// Most values can be described as this, or as a function of one of these.
    pub const IntegerValueType = i64;
    /// Values not describable as fractions are represented like this.
    pub const RealValueType = f64;

    /// The raw representation of values is done through this type.
    pub const InnerValueType = union(ValueMode) {
        Integer: IntegerValueType,
        Real: RealValueType,

        /// Ease-of-use printing function, that uses an external context for printing.
        /// The writer context must implement the following function:
        ///     fn print(<self type>, comptime []const u8, anytype)
        /// TODO: Add resolution printing parameters for this function.
        /// TODO: Check that writer_ctx implements the required interface.
        pub fn tryPrint(self: *InnerValueType, writer_ctx: anytype) !void {
            switch (self.*) {
                .Integer => |i| try writer_ctx.print("{d}", .{i}),
                .Real => |r| try writer_ctx.print("{d:.2}", .{r}),
            }
        }
    };

    /// Wraps some value representation with meta-data pertaining to it.
    pub const ValueType = union(ValueState) {
        NotComputed: InnerValueType,
        NeedsRecompute: InnerValueType,
        Computed: InnerValueType,

        /// Ease-of-use printing function, that uses an external context for printing.
        /// The writer context must implement the following function:
        ///     fn print(<self type>, comptime []const u8, anytype)
        /// TODO: Add resolution printing parameters for this function.
        /// TODO: Check that writer_ctx implements the required interface.
        pub fn tryPrint(self: *ValueType, writer_ctx: anytype) !void {
            switch (self.*) {
                .NotComputed => try writer_ctx.print("<Unknown> [Not Computed]", .{}),
                .NeedsRecompute => |*nrc| {
                    try nrc.tryPrint(writer_ctx);
                    try writer_ctx.print(" [Needs Recompute]", .{});
                },
                .Computed => |*c| {
                    try c.tryPrint(writer_ctx);
                    try writer_ctx.print(" [Up-to-date]", .{});
                },
            }
        }
    };

    /// The type used to represent symbolic data.
    pub const SymbolicBaseDataType = struct {
        /// Each symbol has an associated representation.
        symbol: SymbolicType,
        /// The value associated with the symbol.
        value: ValueType,

        pub fn initFull(symbol: SymbolicType, value: ValueType) SymbolicBaseDataType {
            return SymbolicBaseDataType{
                .symbol = symbol,
                .value = value,
            };
        }

        pub fn init(rep: SymbolicType.RepresentationType, isAllocated: bool, value: ValueType) SymbolicBaseDataType {
            return SymbolicBaseDataType{
                .symbol = SymbolicType.init(rep, isAllocated),
                .value = value,
            };
        }

        /// Ease-of-use printing function, that uses an external context for printing.
        /// The writer context must implement the following function:
        ///     fn print(<self type>, comptime []const u8, anytype)
        /// TODO: Add resolution printing parameters for this function.
        /// TODO: Check that writer_ctx implements the required interface.
        pub fn tryPrint(self: *SymbolicBaseDataType, writer_ctx: anytype) !void {
            try self.symbol.tryPrint(writer_ctx);
            try writer_ctx.print(": ", .{});
            try self.value.tryPrint(writer_ctx);
        }

        pub fn deinit(self: *SymbolicBaseDataType, allocator: std.mem.Allocator) void {
            self.symbol.deinit(allocator);
        }
    };

    /// A reference links a symbol to some symbolic data.
    pub const SymbolicReferenceType = struct {
        /// The symbolic representation that this reference links to.
        symbol_reference: SymbolicType,

        /// Error set for symbolic references.
        pub const ReferenceError = error{
            /// The symbol was not found during the look-up.
            NotFound,
        };

        pub fn initFull(symbol: SymbolicType) SymbolicReferenceType {
            return SymbolicReferenceType{
                .symbol_reference = symbol,
            };
        }

        pub fn init(rep: SymbolicType.RepresentationType, isAllocated: bool) SymbolicReferenceType {
            return SymbolicReferenceType{
                .symbol_reference = SymbolicType.init(rep, isAllocated),
            };
        }

        pub fn deinit(self: *SymbolicReferenceType, allocator: std.mem.Allocator) void {
            self.symbol_reference.deinit(allocator);
        }

        /// Tries to find the symbolic data in a data store.
        pub fn tryFind(self: *SymbolicReferenceType, store: *DataStoreType) !*DataType {
            if (store.get(self.symbol_reference.representation)) |*found| {
                return found;
            }
            return ReferenceError.NotFound;
        }

        /// Try to get the value associated with a symbol.
        /// Will not unwrap the value type.
        /// Offers more granularity.
        pub fn tryGetValueUnsafe(self: *SymbolicReferenceType, store: *DataStoreType) !ValueType {
            var found = try self.tryFind(store);
            return try found.getValueUnsafe();
        }

        /// Try to get the value associated with a symbol.
        /// Will unwrap the value type.
        pub fn tryGetValue(self: *SymbolicReferenceType, store: *DataStoreType) !InnerValueType {
            var found = try self.tryFind(store);
            return found.getValue();
        }

        /// Ease-of-use printing function, that uses an external context for printing.
        /// The writer context must implement the following function:
        ///     fn print(<self type>, comptime []const u8, anytype)
        /// TODO: Check that writer_ctx implements the required interface.
        pub fn tryPrint(self: *SymbolicReferenceType, writer_ctx: anytype) !void {
            try writer_ctx.print("-> ", .{});
            try self.symbol_reference.tryPrint(writer_ctx);
        }
    };

    /// Lays out the types of nodes that can be used.
    pub const NodeTag = enum {
        /// Data nodes can store symbolic constants, variables, and more.
        Data,
        /// References nodes store symbolic references to data.
        Reference,
        /// Not yet implemented, but an operation node would execute some operation on nodes.
        Operation,
    };

    /// Maps the type of node with a node definition.
    pub const InnerNodeType = union(NodeTag) {
        Data: SymbolicBaseDataType,
        Reference: SymbolicReferenceType,
        Operation: void, // <--- Not yet implemented.

        /// Lays out the possible errors for this type.
        pub const InnerNodeError = error{
            /// This type of node is not yet implemented
            NotImplemented,
        };

        pub fn deinit(self: *InnerNodeType, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .Data => |*d| d.deinit(allocator),
                .Reference => |*r| r.deinit(allocator),
                .Operation => return,
            }
        }

        /// Ease-of-use printing function, that uses an external context for printing.
        /// The writer context must implement the following function:
        ///     fn print(<self type>, comptime []const u8, anytype)
        /// TODO: Add resolution printing parameters for this function.
        /// TODO: Check that writer_ctx implements the required interface.
        pub fn tryPrint(self: *InnerNodeType, writer_ctx: anytype) !void {
            switch (self.*) {
                .Data => |*d| try d.tryPrint(writer_ctx),
                .Reference => |*r| try r.tryPrint(writer_ctx),
                .Operation => return InnerNodeError.NotImplemented,
            }
        }
    };

    /// The actual type that represents the entries in the equation tree.
    pub const NodeType = struct {
        /// The actual data behind the node.
        /// An operation, a definition of data, a reference, etc.
        /// TODO: Should data be able to be null? Probably not.
        data: ?InnerNodeType,
        /// The children nodes that branch out from this one.
        children: NodeStoreType,

        /// Type used for the children node storage inside each node.
        pub const NodeStoreType = std.ArrayList(NodeType);

        /// Initializes the node with no data.
        pub fn init(allocator: std.mem.Allocator) NodeType {
            return initFull(null, allocator);
        }

        /// Initializes the node with some data, specified.
        /// If data is null, this function acts as 'init'.
        pub fn initFull(data: ?InnerNodeType, allocator: std.mem.Allocator) NodeType {
            return NodeType{
                .data = if (data) |d| d else null,
                .children = NodeStoreType.init(allocator),
            };
        }

        /// This will recursively deinitialize the node and its children.
        /// TODO: Implement a 'pop' function that will return the children
        /// TODO: so the pointers won't be dangling.
        pub fn deinit(self: *NodeType, allocator: std.mem.Allocator) void {
            for (self.children.items) |*child| {
                child.deinit(allocator);
            }
            if (self.data) |*d| d.deinit(allocator);
            self.children.deinit();
        }

        /// Constructs a node with the data given and attaches it to this one.
        pub fn addChildByData(self: *NodeType, child_data: ?InnerNodeType, allocator: std.mem.Allocator) !*NodeType {
            try self.children.append(initFull(child_data, allocator));
            return &self.children.items[self.children.items.len - 1];
        }

        /// Attaches a node to this one.
        /// TODO: Implement a mechanism in which one can't make loops in the equation tree.
        pub fn addChildByNode(self: *NodeType, child: *NodeType) !*NodeType {
            try self.children.append(child);
            return &self.children.items[self.children.items.len - 1];
        }

        /// Checks whether an index is within the bounds of the list of child nodes.
        /// TODO: Get this error out into its own error set.
        pub fn checkIndex(self: *NodeType, index: usize) !void {
            if (index >= self.children.items.len or index < 0) return error.OutOfBounds;
        }

        /// Tries to remove the child at the given index, if the index is valid.
        /// This will also shift all the child nodes so the gap is filled.
        pub fn removeChildOrdered(self: *NodeType, index: usize) !NodeType {
            try self.checkIndex(index);
            return self.children.orderedRemove(index);
        }

        /// Tries to remove the child at the given index, if the index is valid.
        /// This will not fill the gap caused by the removal.
        pub fn removeChild(self: *NodeType, index: usize) !NodeType {
            try self.checkIndex(index);
            return self.children.swapRemove(index);
        }

        /// Ease-of-use printing function, that uses an external context for printing.
        /// The writer context must implement the following function:
        ///     fn print(<self type>, comptime []const u8, anytype)
        /// TODO: Add resolution printing parameters for this function.
        /// TODO: Check that writer_ctx implements the required interface.
        /// TODO: Turn the 'strict' parameter as a field for the printing parameters type that will be created.
        pub fn tryPrint(self: *NodeType, writer_ctx: anytype, level: usize, strict: bool) !void {
            try print_padding(writer_ctx, level);

            if (self.data) |*d| {
                try writer_ctx.print("{s}: ", .{@tagName(std.meta.activeTag(d.*))});
                try d.tryPrint(writer_ctx);
                try writer_ctx.print("\n", .{});
            } else if (strict) {
                try writer_ctx.print("EMPTY NODE\n", .{});
            }

            for (self.children.items) |*child| {
                try child.tryPrint(writer_ctx, level + 1, strict);
            }
        }
    };
};

/// Ease-of-use printing function, that uses an external context for printing.
/// The writer context must implement the following function:
///     fn print(<self type>, comptime []const u8, anytype)
/// TODO: Check that writer_ctx implements the required interface.
pub fn print_padding(writer_ctx: anytype, level: usize) !void {
    for (0..level) |_| {
        try writer_ctx.print("\t", .{});
    }
}
