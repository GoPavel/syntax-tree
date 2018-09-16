
int f() {
    return 2;
}

int foo(int a) {
    int y = f() + f();

    int x;
    int x = 2; // It's comment
    x = (x + 2) + (3);

    return x;
}

int fib(int a) {
    if (a == 0) {
        return 1;
    } else {
        if (a == 1) {
            return 1;
        } else {
            return fib(a-1) + fib(a-2);
        }
    }
}

void g(int a, int b) {
    return h(a + b);
}

void h(int c) {
    return g(c - 1, c + 1);
}

void print() {
    print();
}

void l() {
    return print(42);
}
