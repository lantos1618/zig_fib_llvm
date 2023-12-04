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

// Struct for assignments
pub const Assignment = struct {
    identifier: Identifier,
    expr: ?*Expr,
};

// Struct for function definitions
pub const FuncDef = struct {
    name: Identifier,
    params: []Expr,
    return_type: ?*Expr,
    body: ?*Expr,
};

// Struct for type definitions
pub const TypeDef = struct {
    name: Identifier,
    type_expr: ?*Expr,
};

// Struct for if statements
pub const IfStatement = struct {
    condition: ?*Expr,
    then_branch: []Expr,
    else_branch: []Expr,
};

// Struct for loops
pub const Loop = struct {
    condition: ?*Expr,
    body: []Expr,
};

pub const Binary = struct {
    op: BinaryOp,
    left: ?*Expr,
    right: ?*Expr,
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
    Stmt: []Expr,
};

pub fn codegen(
    expr: ?*Expr,
    module: llvm.LLVMModuleRef,
    builder: llvm.LLVMBuilderRef,
    context: ?llvm.LLVMContextRef,
    symbol_table: *std.StringArrayHashMap(?llvm.LLVMValueRef),
    allocator: std.mem.Allocator,
) !llvm.LLVMValueRef {
    // unwrap the expression
    const t_expr = expr.?.*;

    return try switch (t_expr) {
        .Binary => |binary| {
            const left = try codegen(binary.left, module, builder, context, symbol_table, allocator);
            const right = try codegen(binary.right, module, builder, context, symbol_table, allocator);
            return switch (binary.op) {
                .Add => llvm.LLVMBuildAdd(builder, left, right, "addtmp"),
                .Subtract => llvm.LLVMBuildSub(builder, left, right, "subtmp"),
                .Multiply => llvm.LLVMBuildMul(builder, left, right, "multmp"),
                .Divide => llvm.LLVMBuildSDiv(builder, left, right, "divtmp"),
            };
        },
        .Unary => |unary| {
            const operand = try codegen(unary.expr, module, builder, context, symbol_table, allocator);
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
            const table_res = symbol_table.get(identifier.name);
            if (table_res) |v| {
                // check to see if the value is not null
                if (v == null) {
                    return error.NullValue;
                }
                const varValue = llvm.LLVMBuildLoad2(
                    builder,
                    // get the value type from llvm symbol
                    llvm.LLVMTypeOf(v.?),
                    v.?,
                    identifier.name.ptr,
                );
                return varValue;
            } else unreachable; // Or handle error
        },

        .Assignment => |assignment| {
            const value = try codegen(assignment.expr, module, builder, context, symbol_table, allocator);
            // RHS should be a llvmValueRef
            if (value == null) {
                return error.NullValue;
            }
            // check if the variable exists then update it and return
            const variable = symbol_table.get(assignment.identifier.name);
            if (variable) |v| {
                if (v == null) {
                    return error.NullValue;
                }
                return llvm.LLVMBuildStore(builder, value, v.?);
            }

            // If the variable doesn't exist, create it
            const ty = llvm.LLVMTypeOf(value);
            const name = assignment.identifier.name;

            std.log.info("Creating variable name: {s}, type: {any}\n", .{ name, ty });
            const alloca_variable = llvm.LLVMBuildAlloca(builder, ty, name.ptr);
            if (alloca_variable == null) {
                return error.NullAlloca;
            }
            // Store the variable in the symbol table
            _ = try symbol_table.put(assignment.identifier.name, alloca_variable);
            // Store the value in the variable
            return llvm.LLVMBuildStore(builder, value, alloca_variable);
        },
        .FuncDef => |funcDef| {
            // Create a function type
            var paramsRef = try allocator.alloc(llvm.LLVMValueRef, funcDef.params.len);

            for (funcDef.params, 0..) |*param, i| {
                paramsRef[i] = try codegen(param, module, builder, context, symbol_table, allocator);
            }

            var paramTypes = try allocator.alloc(llvm.LLVMTypeRef, funcDef.params.len);

            for (paramsRef, 0..) |paramRef, i| {
                paramTypes[i] = llvm.LLVMTypeOf(paramRef);
            }

            var returnType = try codegen(funcDef.return_type, module, builder, context, symbol_table, allocator);
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

            var body = try codegen(funcDef.body, module, funcBuilder, context, symbol_table, allocator);
            _ = body;

            return func;
        },
        .Program => |program| {
            // create main function
            const mainFuncType = llvm.LLVMFunctionType(
                llvm.LLVMInt32Type(),
                null,
                0,
                @as(llvm.LLVMBool, 0),
            );
            const mainFunc = llvm.LLVMAddFunction(module, "main", mainFuncType);
            const entry = llvm.LLVMAppendBasicBlock(mainFunc, "entry");
            const mainBuilder = llvm.LLVMCreateBuilder();
            llvm.LLVMPositionBuilderAtEnd(mainBuilder, entry);

            for (program) |program_expr| {
                _ = try codegen(program_expr, module, mainBuilder, context, symbol_table, allocator);
            }
            return null;
        },
        // .Loop => |loop| {
        //     _ = loop;
        //     // create a body, latch and exit block to context

        // },
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
    const module = llvm.LLVMModuleCreateWithNameInContext("test", context);
    const builder = llvm.LLVMCreateBuilderInContext(context);
    // defer {
    //     llvm.LLVMDisposeBuilder(builder);
    //     llvm.LLVMDisposeModule(module);
    //     llvm.LLVMContextDispose(context);
    //     llvm.LLVMShutdown();
    // }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {}
    // Declare and initialize the hashmap
    var symbol_table = std.StringArrayHashMap(
        ?llvm.LLVMValueRef,
    ).init(allocator);
    defer symbol_table.deinit();

    var ref = try codegen(&program, module, builder, context, &symbol_table, allocator);
    _ = ref;
    llvm.LLVMDumpModule(module);
}

// lets build tests to test the codegen

// pub fn test_setup() !type {
//     // Initialize LLVM
//     if (llvm.LLVMInitializeNativeTarget() != 0) {
//         std.log.err("Failed to initialize native target\n", .{});
//         return error.Unreachable;
//     }
//     if (llvm.LLVMInitializeNativeAsmPrinter() != 0) {
//         std.log.err("Failed to initialize native asm printer\n", .{});
//         return error.Unreachable;
//     }
//     if (llvm.LLVMInitializeNativeAsmParser() != 0) {
//         std.log.err("Failed to initialize native asm parser\n", .{});
//         return error.Unreachable;
//     }

//     // Create a new LLVM module
//     const context = llvm.LLVMGetGlobalContext();
//     const module = llvm.LLVMModuleCreateWithNameInContext("test", context);
//     const builder = llvm.LLVMCreateBuilderInContext(context);

//     return .{ .context = context, .module = module, .builder = builder };
// }

// fn test_teardown(context: llvm.LLVMContextRef, module: llvm.LLVMModuleRef, builder: llvm.LLVMBuilderRef) void {
//     llvm.LLVMShutdown();

//     llvm.LLVMDisposeBuilder(builder);
//     llvm.LLVMDisposeModule(module);
//     llvm.LLVMContextDispose(context);
// }

// test "binary" {
//     const ctx = try test_setup();
//     defer test_teardown(ctx.context, ctx.module, ctx.builder);

//     // Your implementation for the "binary" test goes here
//     const a = llvm.LLVMConstInt(llvm.LLVMInt32Type(), 5, false);
//     const b = llvm.LLVMConstInt(llvm.LLVMInt32Type(), 10, false);
//     const expectedAST = llvm.LLVMBuildAdd(ctx.builder, a, b, "addtmp");

//     // Add the AST and result comparison
//     const expr = Expr{
//         .Binary = Binary{
//             .op = BinaryOp.Add,
//             .left = Expr{ .Literal = Literal{ .i32 = 5 } },
//             .right = Expr{ .Literal = Literal{ .i32 = 10 } },
//         },
//     };

//     const actualAST = try codegen(expr, ctx.module, ctx.builder, ctx.context, null, null);

//     // Compare the AST and result
//     if (actualAST == expectedAST) {
//         std.testing.expect(actualAST == expectedAST);
//     } else {
//         std.testing.expect(false);
//     }
// }
