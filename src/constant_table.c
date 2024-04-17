#include "../include/constant_table.h"
#include <string.h>

#define LOAD_FACTOR 0.8

static inline float get_load_factor(ConstTable* table) {
    return (float) table->size / table->capacity;
}

static void clear_table(ConstTable* table) {
    Const** arr = table->arr;
    for (size_t i = 0; i < table->capacity; i++) {
        arr[i] = NULL;
    }
}

void rehash_consts(ConstTable* table) {
    table->size = 0;
    ConstTable nt;
    ALLOCATE(nt.arr, Const*, table->capacity);
    nt.size = 0;
    nt.capacity = table->capacity;
    clear_table(&nt);

    for (size_t i = 0; i < table->capacity; i++) {
        Const** arr = (Const**) table->arr;
        if (arr[i] != NULL) {
            insert_const(&nt, arr[i]);
            arr[i] = NULL;
        }
    }
    memcpy(table, &nt, sizeof(ConstTable));
    //TODO: free old table
}

void init_const_table(ConstTable* table) {
    INIT_VECTOR((*table), Const*);
    clear_table(table);

}

void insert_const(ConstTable* table, Const* c) {
    printf("---------size: %ld------\n", table->size);
    for(size_t l = 0; l < table->capacity; l++) {
        Const* c = INDEX_VECTOR((*table), Const*, l);
        if (c != NULL) {
            printf("value: %d\n", c->as_int_const);
        }
    }

    printf("---------------\n");
    size_t i = 0;
    switch(c->type) {
        case INT_CONST:
            i = c->as_int_const % table->capacity;
            break;
        case DOUBLE_CONST:
            i = (int) c->as_double_const % table->capacity;
            break;
        case FUNC_CONST:
        case STRING_CONST:
            i = strlen(c->as_string_const) % table->capacity;
            break;
    }
    //printf("load_factor: %f\n", get_load_factor(table));

    if (get_load_factor(table) >= LOAD_FACTOR) {
        size_t old_cap = table->capacity;
        REALLOCATE(table->arr, table->capacity, Const*);
        table->capacity *= SCALE_FACTOR;
        Const** arr = table->arr;
        for (size_t k = old_cap; k < table->capacity; k++) {
            arr[k] = NULL;
        }
        rehash_consts(table);
    }

 

    size_t j = i;
    size_t idx = j;
    while (INDEX_VECTOR((*table), Const*, idx) != NULL) {
        j++;
        idx = j % table->capacity;
    }
    c->const_index = table->size + 1;
    INSERT_AT((*table), c, Const*, idx);

}

Const* lookup_int_const(ConstTable* table, int n ) {
    size_t i = n % table->size;
    for (size_t j = i; j < table->capacity; j++) {
        Const* c = INDEX_VECTOR((*table), Const*, j);
        if (c->type == INT_CONST && c->as_int_const == n) return c;
    }
    return NULL;
}

Const* lookup_double_const(ConstTable* table, double n ) {
    size_t i = (int) n % table->size;
    for (size_t j = i; j < table->capacity; j++) {
        Const* c = INDEX_VECTOR((*table), Const*, j);
        if (c->type == DOUBLE_CONST && c->as_double_const == n) return c;
    }
    return NULL;
}

Const* lookup_string_const(ConstTable* table, char* str) {
    size_t i = strlen(str) % table->size;
    for (size_t j = i; j < table->capacity; j++) {
        Const* c = INDEX_VECTOR((*table), Const*, j);
        if ((c->type == STRING_CONST || c->type == FUNC_CONST) && strcmp(str, c->as_string_const) == 0) return c;
    }
    return NULL;
}
