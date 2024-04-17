#include "../include/constant_table.h"
#include "criterion/criterion.h"

ConstTable table;

void setup(void) {
    init_const_table(&table);
}

void teardown(void) {
    free(table.arr);
}

TestSuite(constant_table_tests, .init=setup, .fini=teardown);

Test(constant_table_tests, test_insertion) {
    size_t prev_size = table.size;
    Const* c;
    ALLOC_CONST(c);
    c->type = INT_CONST;
    c->as_int_const = 4;
    insert_const(&table, c);
    cr_expect(table.size == prev_size + 1, "insertion error");
}

Test(constant_table_tests, insert_multiple) {
    int n = 100;
    size_t prev_size = table.size;
    for (int i = 0; i < n; i++) {
        Const* c;
        ALLOC_CONST(c);
        c->as_int_const = i;
        c->type = INT_CONST;
        insert_const(&table, c);
    }
    cr_expect(table.size == prev_size + n, "some constants were wrongly inserted or overwritten");
}

Test(constant_table_tests, test_reshash) {
    size_t prev_size = table.size;
    rehash_consts(&table);
    cr_expect(prev_size == table.size, "rehashing failed, size difference recorded");
    
}
