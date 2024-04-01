#include "../include/constant_table.h"
#include <string.h>

void insert_const(ConstTable* table, Const* c) {
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

    for (size_t j = i; j < table->capacity; j++) {
        Const* stored_const = INDEX_VECTOR((*table), Const* ,j);
        if (stored_const == NULL) {
            uint16_t idx = table->size + 1; // reserve 0 index for the main entry point
            INSERT_AT((*table), c, Const*, j);
            c->const_index = idx;
            break;
        }
    }

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
