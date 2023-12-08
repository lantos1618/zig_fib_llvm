
// clang -c src/test.cpp -S -emit-llvm  -o src/test.ll    # compile to llvm
int main() {
    int b = 0;
    while(true) {
        b = b + 1;
        if (b == 10) { break; }
    }
    b = 10;
}