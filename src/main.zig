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
};

// Struct for identifiers
pub const Identifier = struct {
    name: []const u8,
};

const GroupOp = enum {
    Paren,
    Bracket,
    Brace,
};

// Struct for group expressions
pub const Group = struct {
    op: GroupOp,
    expr: *Expr,
};

// Struct for assignments
pub const Assignment = struct {
    identifier: Identifier,
    expr: *Expr,
};

// Struct for function definitions
pub const FuncDef = struct {
    name: Identifier,
    params: []Expr,
    return_type: *Expr,
    body: *Expr,
};

// Struct for type definitions
pub const TypeDef = struct {
    name: Identifier,
    type_expr: *Expr,
};

// Struct for if statements
pub const IfStatement = struct {
    condition: *Expr,
    then_branch: []Expr,
    else_branch: []Expr,
};

// Struct for loops
pub const Loop = struct {
    condition: *Expr,
    body: []Expr,
};

pub const Binary = struct {
    op: BinaryOp,
    left: *Expr,
    right: *Expr,
};

pub const Unary = struct {
    op: UnaryOp,
    expr: *Expr,
};

// Enum for expressions
pub const Expr = union(enum) {
    Binary: Binary,
    Unary: Unary,
    Literal: Literal,
    Identifier: Identifier,
    Group: Group,
    Assignment: Assignment,
    FuncDef: FuncDef,
    TypeDef: TypeDef,
    IfStatement: IfStatement,
    Loop: Loop,
};

pub fn codegen(
    expr: *Expr,
    module: llvm.LLVMModuleRef,
    builder: llvm.LLVMBuilderRef,
    context: llvm.LLVMContextRef,
    symbolTable: std.StringArrayHashMap(*llvm.LLVMValueRef),
    allocator: std.mem.Allocator,
) !llvm.LLVMValueRef {
    return try switch (expr.*) {
        .Binary => |binary| {
            const left = try codegen(binary.left, module, builder, context, symbolTable, allocator);
            const right = try codegen(binary.right, module, builder, context, symbolTable, allocator);
            return switch (binary.op) {
                .Add => llvm.LLVMBuildAdd(builder, left, right, "addtmp"),
                .Subtract => llvm.LLVMBuildSub(builder, left, right, "subtmp"),
                .Multiply => llvm.LLVMBuildMul(builder, left, right, "multmp"),
                .Divide => llvm.LLVMBuildSDiv(builder, left, right, "divtmp"),
            };
        },
        .Unary => |unary| {
            const operand = try codegen(unary.expr, module, builder, context, symbolTable, allocator);
            return switch (unary.op) {
                .Negate => llvm.LLVMBuildNeg(builder, operand, "negtmp"),
                .Not => llvm.LLVMBuildNot(builder, operand, "nottmp"),
            };
        },
        .Literal => |literal| {
            return switch (literal) {
                .i32 => |i| llvm.LLVMConstInt(
                    llvm.LLVMInt32Type(),
                    @as(c_ulonglong, @intCast(i)),
                    @as(llvm.LLVMBool, 0),
                ),
                .i64 => |i| llvm.LLVMConstInt(llvm.LLVMInt64Type(), @as(c_ulonglong, @intCast(i)), 0),
                .f64 => |f| llvm.LLVMConstReal(llvm.LLVMFloatType(), f),
            };
        },
        .Identifier => |identifier| {
            // Assuming you have a symbol table to look up variable values
            const value = symbolTable.get(identifier.name);
            if (value) |v| {
                const varValue = llvm.LLVMBuildLoad2(
                    builder,
                    // get the value type from llvm symbol
                    llvm.LLVMTypeOf(v.*),
                    v.*,
                    identifier.name.ptr,
                );
                return varValue;
            } else unreachable; // Or handle error
        },
        .Group => |group| codegen(group.expr, module, builder, context, symbolTable, allocator),
        .Assignment => |assignment| {
            const value = try codegen(assignment.expr, module, builder, context, symbolTable, allocator);
            const variable = symbolTable.get(assignment.identifier.name);
            if (variable) |*v| {
                return llvm.LLVMBuildStore(builder, value, v.*.*);
            } else unreachable; // Or handle error
        },
        .FuncDef => |funcDef| {
            // Create a function type

            var paramsRef = try allocator.alloc(llvm.LLVMValueRef, funcDef.params.len);

            for (funcDef.params, 0..) |*param, i| {
                paramsRef[i] = try codegen(param, module, builder, context, symbolTable, allocator);
            }

            var paramTypes = try allocator.alloc(llvm.LLVMTypeRef, funcDef.params.len);

            for (paramsRef, 0..) |paramRef, i| {
                paramTypes[i] = llvm.LLVMTypeOf(paramRef);
            }

            var returnType = try codegen(funcDef.return_type, module, builder, context, symbolTable, allocator);
            _ = returnType;

            const funcType = llvm.LLVMFunctionType(
                llvm.LLVMInt32Type(),
                paramTypes.ptr,
                @as(c_uint, @intCast(funcDef.params.len)),
                @as(llvm.LLVMBool, 0),
            );

            // Create the function
            const func = llvm.LLVMAddFunction(module, "func", funcType);
            // Create a basic block
            const entry = llvm.LLVMAppendBasicBlock(func, "entry");
            _ = entry;
            // Create a builder for the basic block
            const funcBuilder = llvm.LLVMCreateBuilder();

            var body = try codegen(funcDef.body, module, funcBuilder, context, symbolTable, allocator);
            _ = body;

            return func;
        },
        .Loop => |loop| {
            const funcType = llvm.LLVMFunctionType(
                llvm.LLVMInt32Type(),
                null,
                0,
                0,
            );

            const func = llvm.LLVMAddFunction(module, "loopFunc", funcType);

            // Create basic blocks for the loop header, loop body, and after loop
            const loopHeader = llvm.LLVMAppendBasicBlockInContext(context, func, "loopHeader");
            const loopBody = llvm.LLVMAppendBasicBlockInContext(context, func, "loopBody");
            const afterLoop = llvm.LLVMAppendBasicBlockInContext(context, func, "afterLoop");

            // Create a counter variable
            const counter = llvm.LLVMBuildAlloca(builder, llvm.LLVMInt32TypeInContext(context), "counter");
            _ = llvm.LLVMBuildStore(builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(context), 0, 0), counter);

            // Jump to the loop header
            _ = llvm.LLVMBuildBr(builder, loopHeader);

            // Start inserting at loopHeader
            llvm.LLVMPositionBuilderAtEnd(builder, loopHeader);
            const counterVal = llvm.LLVMBuildLoad2(builder, llvm.LLVMTypeOf(counter), counter, "counterVal");
            const endCondition = llvm.LLVMBuildICmp(builder, llvm.LLVMIntSLT, // signed less than
                counterVal, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(context), @as(c_ulonglong, @intCast(loop.condition.Literal.i32)), 0), // compare with loop.condition
                "endcond");
            // Branch to either the loop body or after loop depending on the end condition
            _ = llvm.LLVMBuildCondBr(builder, endCondition, loopBody, afterLoop);

            _ = // Start inserting at loopBody
                llvm.LLVMPositionBuilderAtEnd(builder, loopBody);
            for (loop.body) |*body_expr| {
                _ = try codegen(body_expr, module, builder, context, symbolTable, allocator);
            }
            const add = llvm.LLVMBuildAdd(builder, counterVal, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(context), 1, 0), // increment counter
                "add");
            _ = llvm.LLVMBuildStore(builder, add, counter);
            // Jump back to the loop header
            _ = llvm.LLVMBuildBr(builder, loopHeader);

            // Start inserting at afterLoop
            llvm.LLVMPositionBuilderAtEnd(builder, afterLoop);

            // TODO work out return type for loop
            return llvm.LLVMConstInt(llvm.LLVMInt32Type(), @as(c_longlong, 0), 0);
        },
        else => unreachable,
    };
}

pub fn main() !void {
    var a = Identifier{ .name = "a" };
    var a_expr = Expr{ .Identifier = a };

    var ten = Expr{ .Literal = Literal{ .i32 = 10 } };
    var a_assign_10 = Expr{ .Assignment = Assignment{ .identifier = a, .expr = &ten } };
    var a_plus_10 = Expr{
        .Binary = Binary{
            .op = BinaryOp.Add,
            .left = &a_expr,
            .right = &ten,
        },
    };
    var a_assign_a_plus_10 = Expr{
        .Assignment = Assignment{ .identifier = a, .expr = &a_plus_10 },
    };
    var program_exprs = [_]*Expr{ &a_assign_10, &a_assign_a_plus_10 };
    var group_expr = Expr{
        .Group = Group{
            .op = GroupOp.Paren,
            .expr = program_exprs[0],
        },
    };
    var program = Expr{
        .Group = Group{
            .op = GroupOp.Paren,
            .expr = &group_expr,
        },
    };

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
    defer {
        llvm.LLVMShutdown();
    }

    // Create a new LLVM module
    const module = llvm.LLVMModuleCreateWithName("main");
    const builder = llvm.LLVMCreateBuilder();
    const context = llvm.LLVMGetGlobalContext();
    defer {
        llvm.LLVMDisposeBuilder(builder);
        llvm.LLVMDisposeModule(module);
        llvm.LLVMContextDispose(context);
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Declare and initialize the hashmap
    var symbol_table = std.StringArrayHashMap(
        *llvm.LLVMValueRef,
    ).init(allocator);
    defer symbol_table.deinit();

    var ref = try codegen(&program, module, builder, context, symbol_table, allocator);
    _ = ref;
}
