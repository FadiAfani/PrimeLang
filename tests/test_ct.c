#include "../include/hash_table.h"
#include "../include/constant_table.h"
#include "criterion/criterion.h"

HashTable table;

void setup(void) {
    init_hash_table(&table);
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
    insert(&table, &c->as_int_const, c, sizeof(int));
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
        insert(&table, &c->as_int_const, c, sizeof(int));
    }

    printf("size: %d", table.size);
    cr_expect(table.size == prev_size + n, "some constants were wrongly inserted or overwritten");
}

Test(constant_table_tests, test_reshash) {
    size_t prev_size = table.size;
    rehash(&table);
    cr_expect(prev_size == table.size, "rehashing failed, size difference recorded");
    
}

Test(constant_table_tests, test_lookup) {
    int key = 32;
    Const* c;
    ALLOC_CONST(c);
    c->type = INT_CONST;
    c->as_int_const = key;
    insert(&table, &key, c, sizeof(int)); 
    Const* res = lookup(&table, &key, sizeof(int));
    cr_expect(res != NULL, "lookup function failed");
}
