#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "value.h"
#include "memory.h"
#include "prime_file_parser.h"
#include "hash_table.h"

#define READ_FILE_16(file) (fgetc(file) << 8 | fgetc(file))

static inline uint8_t byte_to_uint(char c) {
    return c - '0';
}

static Value parse_number_val(FILE* file) {
    double d;
    fread(&d, 1, sizeof(double), file);
    return (Value) {.number = d, NUMBER};
}

static StringObj* parse_string_object(FILE* file) {
    
    uint16_t n_bytes = fgetc(file) << 8 | fgetc(file);
    char* str;
    ALLOCATE(str, char, n_bytes);
    fread(str, 1, n_bytes, file);
    StringObj* obj = init_string_obj(str, n_bytes);
    return obj;
}

static ClosureObj* parse_closure_object(FILE* file, ClosureObj* parent) {

    uint8_t arity = fgetc(file);
    uint16_t n_locs = fgetc(file) << 8 | fgetc(file);
    uint16_t n_bytes = fgetc(file) << 8 | fgetc(file);
    uint16_t n_keys = READ_FILE_16(file);
    uint8_t* clsr_insts;
    ALLOCATE(clsr_insts, uint8_t, n_bytes);
    fread(clsr_insts, 1, n_bytes, file);
    uint16_t n_consts = READ_FILE_16(file);
    /* read the closure's constants */
    Value* consts;
    ALLOCATE(consts, Value, n_consts);
    ClosureObj* clsr_obj = init_closure(clsr_insts, consts, n_locs, arity);
    /* initialize hash table */
    HashTable* table = init_hash_table(n_keys);

    for (int i = 0; i < n_consts; i++) {
        clsr_obj->consts[i] = parse_val(file, clsr_obj);
        //printf("parsed number: %f\n", clsr_obj->consts[i].number);
    }

    clsr_obj->parent = parent;
    clsr_obj->outerVals = table;
    return clsr_obj;
}


Value parse_val(FILE* file, ClosureObj* parent) {
    uint8_t val_type = fgetc(file);
    Value val;
    switch(val_type) {
        case 0: 
            val = parse_number_val(file);
            break;
        case 1: 
            val = (Value) {.ref = parse_string_object(file), OBJECT};
            break;
        case 2: 
            val = (Value) {.ref = parse_closure_object(file, parent), OBJECT};
            break;
        default:
            printf("Unrecognized value type: %d\n", val_type);
            exit(EXIT_FAILURE);
    }
    return val;
}

Value parse_file_into_main_closure_obj(const char* file_path) {

    FILE* file = fopen(file_path, "rb");
    if (file == NULL) {
        printf("Failed to open the binary file");
        exit(1);
    }

    Value main_obj = parse_val(file, NULL);
    return main_obj;
}

