#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Iterative Fibonacci
unsigned long long fib_iterative(int n) {
    if (n <= 1) return n;
    unsigned long long prev = 0, current = 1;
    for (int i = 2; i <= n; i++) {
        unsigned long long next = prev + current;
        prev = current;
        current = next;
    }
    return current;
}

// Recursive Fibonacci
unsigned long long fib_recursive(int n) {
    if (n <= 1) return n;
    return fib_recursive(n - 1) + fib_recursive(n - 2);
}

// Iterative Fibonacci with Memoization
unsigned long long fib_iterative_memo(int n) {
    unsigned long long memo[n + 1];
    memo[0] = 0;
    memo[1] = 1;
    for (int i = 2; i <= n; i++) {
        memo[i] = memo[i - 1] + memo[i - 2];
    }
    return memo[n];
}

// Recursive Fibonacci with Memoization
unsigned long long fib_recursive_memo_helper(int n, unsigned long long memo[]) {
    if (memo[n] != 0) return memo[n];
    if (n <= 1) return n;
    memo[n] = fib_recursive_memo_helper(n - 1, memo) + fib_recursive_memo_helper(n - 2, memo);
    return memo[n];
}

unsigned long long fib_recursive_memo(int n) {
    unsigned long long memo[n + 1];
    for (int i = 0; i <= n; i++) {
        memo[i] = 0;
    }
    return fib_recursive_memo_helper(n, memo);
}

// Benchmarking function
void benchmark_fib(int n, unsigned long long (*fib_function)(int), const char* function_name) {
    clock_t start_time = clock();
    unsigned long long result = fib_function(n);
    clock_t end_time = clock();
    double time_taken = ((double) (end_time - start_time)) / CLOCKS_PER_SEC * 1000.0; // Convert to milliseconds
    printf("%s(%d) = %llu, Time taken: %f milliseconds\n", function_name, n, result, time_taken);
}

// clang -O3 src/fib.c -o bin/fib && ./bin/fib
int main() {
    int n = 50; // Change this value to test different Fibonacci numbers

    // Iterative(50) = 12586269025, Time taken: 0.000005 seconds
    // Recursive(50) = 12586269025, Time taken: 33.731565 seconds
    // Iterative Memoization(50) = 12586269025, Time taken: 0.000001 seconds
    // Recursive Memoization(50) = 12586269025, Time taken: 0.000001 seconds

    benchmark_fib(n, fib_iterative, "Iterative");
    benchmark_fib(n, fib_recursive, "Recursive");
    benchmark_fib(n, fib_iterative_memo, "Iterative Memoization");
    benchmark_fib(n, fib_recursive_memo, "Recursive Memoization");

    return 0;
}
