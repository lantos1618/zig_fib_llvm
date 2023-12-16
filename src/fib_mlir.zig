const std = @import("std");

const mlir = @cImport({
    @cInclude("mlir-c/IR.h");
    @cInclude("mlir-c/Support.h");
    @cInclude("mlir-c/ExecutionEngine.h");
    @cInclude("mlir-c/RegisterEverything.h");
});

pub fn main() !void {
    var context = try mlir.mlirContextCreate();
    defer mlir.mlirContextDestroy(context);

    var module = try mlir.mlirModuleCreateEmpty(context);
    defer mlir.mlirModuleDestroy(module);

    var engine = try mlir.mlirExecutionEngineCreate(module);
    defer mlir.mlirExecutionEngineDestroy(engine);

    var funcType = try mlir.mlirFunctionTypeGet(
        context,
        2,
        .{ mlir.mlirIntegerTypeGet(context, 32), mlir.mlirIntegerTypeGet(context, 32) },
        1,
        .{mlir.mlirIntegerTypeGet(context, 32)},
    );
    var func = try mlir.mlirFuncOpCreate("add", funcType, context);
    defer mlir.mlirFunctionDestroy(func);

    var result = try mlir.mlirExecutionEngineInvoke(func, null, 0, null, 0);
    std.debug.print("result: {}\n", .{result});
}
