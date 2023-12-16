const std = @import("std");

const llvm = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/ExecutionEngine.h");
    @cInclude("llvm-c/Target.h");
});

pub fn main() !void {
    if (llvm.LLVMInitializeNativeTarget() != 0) {
        std.log.err("Failed to initialize native target", .{});
        return;
    }
    if (llvm.LLVMInitializeNativeAsmPrinter() != 0) {
        std.log.err("Failed to initialize native asm printer", .{});
        return;
    }
    if (llvm.LLVMInitializeNativeAsmParser() != 0) {
        std.log.err("Failed to initialize native asm parser", .{});
        return;
    }

    var context = llvm.LLVMContextCreate();
    var module = llvm.LLVMModuleCreateWithNameInContext("fib_module", context);
    var builder = llvm.LLVMCreateBuilderInContext(context);

    defer {
        llvm.LLVMDisposeBuilder(builder);
        llvm.LLVMDisposeModule(module);
        llvm.LLVMContextDispose(context);
    }

    // Create Function Prototype: int fib(int)
    // [*c]LLVMTypeRef
    const allocator = std.heap.page_allocator;

    // Create a single-element array of LLVMTypeRef for the parameter type
    var param_types = try allocator.alloc(llvm.LLVMTypeRef, 1);
    defer allocator.free(param_types);

    // Define the parameter type as LLVMInt32Type
    param_types[0] = llvm.LLVMInt32TypeInContext(context); // Assuming 'context' is defined

    // Define the return type as LLVMInt32Type
    const return_type = llvm.LLVMInt32TypeInContext(context);

    // Create the function type for the Fibonacci function
    const fib_func_type = llvm.LLVMFunctionType(
        return_type,
        param_types.ptr,
        1,
        0,
    );

    var fib_func = llvm.LLVMAddFunction(
        module,
        "fib",
        fib_func_type,
    );

    // Create Basic Blocks
    var entry = llvm.LLVMAppendBasicBlockInContext(
        context,
        fib_func,
        "entry",
    );
    var loop_header = llvm.LLVMAppendBasicBlockInContext(
        context,
        fib_func,
        "loop_header",
    );
    var iterative_case = llvm.LLVMAppendBasicBlockInContext(
        context,
        fib_func,
        "iterative_case",
    );

    // New exit block
    var exit = llvm.LLVMAppendBasicBlockInContext(
        context,
        fib_func,
        "exit",
    );

    // Function Arguments
    var n = llvm.LLVMGetParam(fib_func, 0);

    llvm.LLVMPositionBuilderAtEnd(builder, entry);

    // Initialize variables
    var fib_1 = llvm.LLVMBuildAlloca(
        builder,
        llvm.LLVMInt32TypeInContext(context),
        "fib_1",
    );
    _ = llvm.LLVMBuildStore(
        builder,
        llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(context), 1, 0),
        fib_1,
    );
    var fib_2 = llvm.LLVMBuildAlloca(
        builder,
        llvm.LLVMInt32TypeInContext(context),
        "fib_2",
    );
    _ = llvm.LLVMBuildStore(
        builder,
        llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(context), 1, 0),
        fib_2,
    );

    // Loop Setup
    var i = llvm.LLVMBuildAlloca(builder, llvm.LLVMInt32TypeInContext(context), "i");
    _ = llvm.LLVMBuildStore(
        builder,
        llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(context), 2, 0),
        i,
    ); // Start from 2

    // Branch to loop_header
    _ = llvm.LLVMBuildBr(builder, loop_header);

    // Loop Header
    _ = llvm.LLVMPositionBuilderAtEnd(builder, loop_header);
    var loop_condition = llvm.LLVMBuildICmp(
        builder,
        llvm.LLVMIntSLT,
        llvm.LLVMBuildLoad2(builder, llvm.LLVMInt32TypeInContext(context), i, "i"),
        n,
        "loop_cond",
    );
    _ = llvm.LLVMBuildCondBr(builder, loop_condition, iterative_case, exit); // Branch to exit block when loop condition is false

    // Iterative Case Block
    _ = llvm.LLVMPositionBuilderAtEnd(builder, iterative_case);

    // Calculate Fibonacci for the current iteration
    var fib_current = llvm.LLVMBuildAdd(
        builder,
        llvm.LLVMBuildLoad2(builder, llvm.LLVMInt32TypeInContext(context), fib_1, ""),
        llvm.LLVMBuildLoad2(builder, llvm.LLVMInt32TypeInContext(context), fib_2, ""),
        "fib_current",
    );

    // Update fib_1 and fib_2 for the next iteration
    _ = llvm.LLVMBuildStore(
        builder,
        llvm.LLVMBuildLoad2(builder, llvm.LLVMInt32TypeInContext(context), fib_2, ""),
        fib_1,
    );
    _ = llvm.LLVMBuildStore(builder, fib_current, fib_2);

    // Update the loop counter
    var current_i = llvm.LLVMBuildLoad2(builder, llvm.LLVMInt32TypeInContext(context), i, "current_i");
    var next_i = llvm.LLVMBuildAdd(
        builder,
        current_i,
        llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(context), 1, 0),
        "next_i",
    );

    _ = llvm.LLVMBuildStore(builder, next_i, i);

    // Branch back to the loop header
    _ = llvm.LLVMBuildBr(builder, loop_header);

    // Return the result
    llvm.LLVMPositionBuilderAtEnd(builder, exit); // Position builder at the end of exit block
    _ = llvm.LLVMBuildRet(
        builder,
        llvm.LLVMBuildLoad2(
            builder,
            llvm.LLVMInt32TypeInContext(context),
            fib_2,
            "",
        ),
    );

    // Verify the function
    {
        var err: c_int = llvm.LLVMVerifyFunction(fib_func, llvm.LLVMPrintMessageAction);
        if (err != 0) {
            defer llvm.LLVMDisposeMessage(err);
            std.log.err("LLVMVerifyFunction failed: {any}", .{err});
            return;
        }
    }
    // Dump the module
    llvm.LLVMDumpModule(module);

    llvm.LLVMLinkInMCJIT();
    if (llvm.LLVMInitializeNativeTarget() != 0) {
        std.log.err("Failed to initialize native target", .{});
        return;
    }
    // Create Execution Engine

    var engine: llvm.LLVMExecutionEngineRef = null;

    {
        var errs = try allocator.alloc([*c]u8, 0);
        defer allocator.free(errs);
        if (llvm.LLVMCreateExecutionEngineForModule(&engine, module, errs.ptr) != 0) {
            defer {
                for (errs) |err| {
                    llvm.LLVMDisposeMessage(err);
                }
            }
            std.log.err("Failed to create execution engine\n", .{});
            return;
        }
    }
    const fib_n = 30;
    const args = try allocator.alloc(llvm.LLVMGenericValueRef, 1);
    defer allocator.free(args);
    args[0] = llvm.LLVMCreateGenericValueOfInt(llvm.LLVMInt32TypeInContext(context), fib_n, 0);

    const jit_result: llvm.LLVMGenericValueRef = llvm.LLVMRunFunction(engine, fib_func, 1, args.ptr);
    const jit_result_int = llvm.LLVMGenericValueToInt(jit_result, 0);
    std.log.info("fib({any})= {any}", .{ fib_n, jit_result_int });
}
