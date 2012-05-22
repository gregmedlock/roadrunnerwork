#include <stdio.h>
#include "hello.h"

int main() {
    Greeter* g = create_greeter();
    print_greeting(g, "World");
    printf("things greeted: %d\n", get_count(g));
    delete_greeter(g);
    return 0;
}
