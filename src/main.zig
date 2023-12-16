const std = @import("std");

const llvm = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/ExecutionEngine.h");
    @cInclude("llvm-c/Target.h");
});

// Enum for binary operations
pub const BinaryOp = enum {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equals,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
};

// Enum for unary operations
pub const UnaryOp = enum {
    Negate,
    Not,
};

// Struct for literals (we'll just use integers for simplicity)
pub const Literal = union(enum) {
    i32: i32,
    f64: f64,
    i64: i64,

    pub fn gen(self: Literal) !llvm.LLVMValueRef {
        return switch (self) {
            .i32 => llvm.LLVMConstInt(llvm.LLVMInt32Type(), self.i32, false),
            .f64 => llvm.LLVMConstReal(llvm.LLVMFloatType(), self.f64),
            .i64 => llvm.LLVMConstInt(llvm.LLVMInt64Type(), self.i64, false),
        };
    }
};

// Struct for identifiers
pub const Identifier = struct {
    name: []const u8,

    pub fn gen(
        self: Identifier,
        module: *llvm.LLVMModuleRef,
        builder: *llvm.LLVMBuilderRef,
        ctx: *llvm.LLVMContextRef,
        func: *?llvm.LLVMValueRef,
    ) !llvm.LLVMValueRef {
        // if we have a functionCtx we want to add the identifier to the functionCtx
        // otherwise we want to add it to the global context

        if (func != null) {
            const alloca = llvm.LLVMBuildAlloca(builder, llvm.LLVMInt32TypeInContext(ctx), self.name);
            _ = llvm.LLVMBuildStore(builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(ctx), 0, false), alloca);
            return alloca;
        }

        const global = llvm.LLVMAddGlobal(module, llvm.LLVMInt32TypeInContext(ctx), self.name);
        _ = llvm.LLVMSetInitializer(global, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(ctx), 0, false));
        _ = llvm.LLVMSetLinkage(global, llvm.LLVMExternalLinkage);
        return global;
    }
};

// Struct for assignments
pub const Assignment = struct {
    identifier: Identifier,
    expr: ?*Expr,

    pub fn gen(self: Assignment, builder: *llvm.LLVMBuilderRef, ctx: *llvm.LLVMContextRef) !void {
        const ident = self.identifier.name;
        const expr = try self.expr.gen(builder);

        const alloca = llvm.LLVMBuildAlloca(builder, llvm.LLVMInt32TypeInContext(ctx), ident);
        _ = llvm.LLVMBuildStore(builder, expr, alloca);
        return;
    }
};

// Struct for function definitions
pub const FuncDef = struct {
    name: Identifier,
    params: []*TypeDef,
    return_type: ?*TypeDef,
    body: ?*Expr,

    pub fn gen(self: FuncDef, builder: *llvm.LLVMBuilderRef, ctx: *llvm.LLVMContextRef, module: *llvm.LLVMModuleRef) !void {

        // we need to check our expr for loop because we need to provide loop.exit
        const name = self.name.name;

        const return_type = self.return_type.gen(builder);

        const func_type = llvm.LLVMFunctionType(return_type, null, 0, false);
        const func = llvm.LLVMAddFunction(module, name, func_type);

        const entry = llvm.LLVMAppendBasicBlockInContext(ctx, func, "function.entry");
        _ = llvm.LLVMPositionBuilderAtEnd(builder, entry);

        const alloca = llvm.LLVMBuildAlloca(builder, llvm.LLVMInt32TypeInContext(ctx), name);
        _ = llvm.LLVMBuildStore(builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(ctx), 0, false), alloca);

        _ = llvm.LLVMBuildRet(builder, llvm.LLVMBuildLoad(builder, llvm.LLVMInt32TypeInContext(ctx), alloca));
        _ = llvm.LLVMVerifyFunction(func, llvm.LLVMPrintMessageAction);
        return;
    }
};

// Struct for type definitions
pub const TypeDef = struct {
    name: Identifier,
    type_expr: ?*Expr,

    pub fn gen() !llvm.LLVMTypeRef {}
};

// Struct for if statements
pub const IfStatement = struct {
    condition: ?*Expr,
    then_branch: []*Expr,

    pub fn gen(self: IfStatement, builder: *llvm.LLVMBuilderRef, ctx: *llvm.LLVMContextRef, func: *llvm.LLVMValueRef, exit_block: *llvm.LLVMBasicBlockRef) !void {
        // if
        //     cond
        //     then
        //     exit

        _ = llvm.LLVMAppendBasicBlockInContext(ctx, func, "if");
        _ = llvm.LLVMAppendBasicBlockInContext(ctx, func, "if.cond");
        _ = llvm.LLVMAppendBasicBlockInContext(ctx, func, "if.then");
        _ = llvm.LLVMAppendBasicBlockInContext(ctx, func, "if.exit");

        const if_cond = self.condition.gen(builder);
        const if_then = self.then_branch.gen(builder);

        _ = llvm.LLVMBuildBr(builder, if_cond);
        _ = llvm.LLVMPositionBuilderAtEnd(builder, if_cond);

        _ = llvm.LLVMBuildCondBr(builder, if_cond, if_then, exit_block);

        return;
    }
};

// Struct for loops
pub const Loop = struct {
    condition: ?*Expr,
    body: []*Expr,

    pub fn gen(self: Loop, builder: *llvm.LLVMBuilderRef, ctx: *llvm.LLVMContextRef, func: *llvm.LLVMValueRef, exit_block: *llvm.LLVMBasicBlockRef) !void {
        // loop
        //     cond
        //     body
        //     exit

        _ = llvm.LLVMAppendBasicBlockInContext(ctx, func, "loop");
        _ = llvm.LLVMAppendBasicBlockInContext(ctx, func, "loop.cond");
        _ = llvm.LLVMAppendBasicBlockInContext(ctx, func, "loop.body");
        _ = llvm.LLVMAppendBasicBlockInContext(ctx, func, "loop.exit");

        const loop_cond = self.condition.gen(builder);
        const loop_body = self.body.gen(builder);

        _ = llvm.LLVMBuildBr(builder, loop_cond);
        _ = llvm.LLVMPositionBuilderAtEnd(builder, loop_cond);

        _ = llvm.LLVMBuildCondBr(builder, loop_cond, loop_body, exit_block);
        _ = llvm.LLVMPositionBuilderAtEnd(builder, loop_body);

        _ = llvm.LLVMBuildBr(builder, loop_cond);
        _ = llvm.LLVMPositionBuilderAtEnd(builder, exit_block);

        return;
    }
};

pub const Binary = struct {
    op: BinaryOp,
    left: ?*Expr,
    right: ?*Expr,

    pub fn gen(self: Binary, builder: *llvm.LLVMBuilderRer) !llvm.LLVMValueRef {
        const lhs = try self.left.gen(builder);
        const rhs = try self.right.gen(builder);
        return switch (self.op) {
            .Add => llvm.LLVMBuildAdd(builder, lhs, rhs, "add_tmp"),
            .Subtract => llvm.LLVMBuildSub(builder, lhs, rhs, "sub_tmp"),
            .Multiply => llvm.LLVMBuildMul(builder, lhs, rhs, "mul_tmp"),
            .Divide => llvm.LLVMBuildSDiv(builder, lhs, rhs, "div_tmp"),
            .Equals => llvm.LLVMBuildICmp(builder, llvm.LLVMIntEQ, lhs, rhs, "eq_tmp"),
            .LessThan => llvm.LLVMBuildICmp(builder, llvm.LLVMIntSLT, lhs, rhs, "lt_tmp"),
            .GreaterThan => llvm.LLVMBuildICmp(builder, llvm.LLVMIntSGT, lhs, rhs, "gt_tmp"),
            .LessThanEquals => llvm.LLVMBuildICmp(builder, llvm.LLVMIntSLE, lhs, rhs, "le_tmp"),
            .GreaterThanEquals => llvm.LLVMBuildICmp(builder, llvm.LLVMIntSGE, lhs, rhs, "ge_tmp"),
        };
    }
};

pub const Unary = struct {
    op: UnaryOp,
    expr: ?*Expr,
};

// Enum for expressions
pub const Expr = union(enum) {
    Binary: Binary,
    Unary: Unary,
    Literal: Literal,
    Identifier: Identifier,

    Assignment: Assignment,
    FuncDef: FuncDef,
    TypeDef: TypeDef,
    IfStatement: IfStatement,
    Loop: Loop,
    Program: []*Expr,
    Stmt: []*Expr,

    pub fn gen(self: Expr, builder: *llvm.LLVMBuilderRef, ctx: *llvm.LLVMContextRef, module: *llvm.LLVMModuleRef, symbol_table: *std.StringArrayHashMap(?llvm.LLVMValueRef)) !?llvm.LLVMValueRef {
        _ = symbol_table;
        _ = module;
        switch (self) {
            .Binary => return self.Binary.gen(builder),
            .Unary => return self.Unary.gen(builder),
            .Literal => return self.Literal.gen(),
            .Identifier => return llvm.LLVMBuildLoad(builder, llvm.LLVMInt32TypeInContext(ctx), self.Identifier.name),
            .Assignment => return self.Assignment.gen(builder, ctx),
            .FuncDef => return null,
            .TypeDef => return null,
            .IfStatement => return null,
            .Loop => return null,
            .Program => return null,
            .Stmt => return null,
        }
    }
};

pub fn main() !void {
    var a = Identifier{ .name = "a" };
    var a_expr = Expr{ .Identifier = a };
    var one = Expr{ .Literal = Literal{ .i32 = 1 } };
    var a_assign = Expr{ .Assignment = Assignment{ .identifier = a, .expr = &one } };
    var a_plus = Expr{
        .Binary = Binary{
            .op = BinaryOp.Add,
            .left = &a_expr,
            .right = &one,
        },
    };
    var a_assign_a_plus = Expr{
        .Assignment = Assignment{ .identifier = a, .expr = &a_plus },
    };

    var loop_body = [_]*Expr{&a_assign_a_plus};
    var loop_condition = Expr{ .Binary = Binary{ .op = BinaryOp.LessThanEquals, .left = &a_expr, .right = &one } };
    var loop_expr = Expr{ .Loop = Loop{ .condition = &loop_condition, .body = &loop_body } };
    var program_exprs = [_]*Expr{ &a_assign, &a_assign_a_plus, &loop_expr };

    var program = Expr{ .Program = &program_exprs };

    // Initialize LLVM
    if (llvm.LLVMInitializeNativeTarget() != 0) {
        std.log.err("Failed to initialize native target\n", .{});
        return error.Unreachable;
    }
    if (llvm.LLVMInitializeNativeAsmPrinter() != 0) {
        std.log.err("Failed to initialize native asm printer\n", .{});
        return error.Unreachable;
    }
    if (llvm.LLVMInitializeNativeAsmParser() != 0) {
        std.log.err("Failed to initialize native asm parser\n", .{});
        return error.Unreachable;
    }

    // Create a new LLVM module
    const context = llvm.LLVMGetGlobalContext();
    const module = llvm.LLVMModuleCreateWithNameInContext("main", context);
    const builder = llvm.LLVMCreateBuilderInContext(context);
    defer {
        llvm.LLVMDisposeBuilder(builder);
        llvm.LLVMDisposeModule(module);
        // llvm.LLVMContextDispose(context);
        llvm.LLVMShutdown();
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    // Declare and initialize the hashmap
    var symbol_table = std.StringArrayHashMap(
        ?llvm.LLVMValueRef,
    ).init(allocator);
    defer symbol_table.deinit();

    var ref = try program.gen(builder, context, module, &symbol_table);
    _ = ref;
    llvm.LLVMDumpModule(module);
}
