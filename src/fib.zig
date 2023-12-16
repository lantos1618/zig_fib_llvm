const std = @import("std");

fn fibIterative(n: u64) u64 {
    if (n <= 1) return n;
    var prev: u64 = 0;
    var current: u64 = 1;
    var i: u64 = 2;
    while (i <= n) : (i += 1) {
        const next = prev + current;
        prev = current;
        current = next;
    }
    return current;
}

fn fibRecursive(n: u64) u64 {
    if (n <= 1) return n;
    return fibRecursive(n - 1) + fibRecursive(n - 2);
}

fn fibIterativeMemo(n: u64, memo: *std.ArrayList(u64)) !u64 {
    try memo.append(0);
    try memo.append(1);
    var i: u64 = 2;
    while (i <= n) : (i += 1) {
        try memo.append(memo.items[i - 1] + memo.items[i - 2]);
    }
    return memo.items[n];
}

fn fibRecursiveMemo(n: u64, memo: *std.ArrayList(u64)) !u64 {
    if (memo.items.len > n and memo.items[n] != 0) return memo.items[n];
    if (n <= 1) return n;
    const value = try fibRecursiveMemo(n - 1, memo) + try fibRecursiveMemo(n - 2, memo);

    try memo.append(value);
    return value;
}

pub fn main() !void {
    const n: u64 = 50; // Test with different Fibonacci numbers

    var timer_iter = try std.time.Timer.start();
    const iterativeResult = fibIterative(n);
    const iterativeTime = timer_iter.read();
    std.debug.print("Iterative({any}): {any}, Time: {any} ms\n", .{ n, iterativeResult, iterativeTime });

    var timer_rec = try std.time.Timer.start();
    const recursiveResult = fibRecursive(n);
    const recursiveTime = timer_rec.read();
    std.debug.print("Recursive({any}): {any}, Time: {any} ms\n", .{ n, recursiveResult, recursiveTime });

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit();
        //fail test; can't try in defer as defer is executed after we return
        if (deinit_status == .leak) {
            std.debug.print("Allocator leaked memory\n", .{});
        }
    }

    var memo_iter = std.ArrayList(u64).init(allocator);
    defer memo_iter.deinit();

    var timer_iter_memo = try std.time.Timer.start();
    const iterativeMemoResult = fibIterativeMemo(n, &memo_iter);
    const iterativeMemoTime = timer_iter_memo.read();
    std.debug.print("Iterative Memo({any}): {any}, Time: {any} ms\n", .{ n, iterativeMemoResult, iterativeMemoTime });

    var memo_rec = std.ArrayList(u64).init(allocator);
    defer memo_rec.deinit();

    var timer_rec_memo = try std.time.Timer.start();
    const recursiveMemoResult = fibRecursiveMemo(n, &memo_rec);
    const recursiveMemoTime = timer_rec_memo.read();
    std.debug.print("Recursive Memo({any}): {any}, Time: {any} ms\n", .{ n, recursiveMemoResult, recursiveMemoTime });
}
