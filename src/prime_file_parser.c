#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "value.h"
#include "memory.h"
#include "prime_file_parser.h"
#include "codegen.h"

#define READ_FILE_16(file) (fgetc(file) | fgetc(file) << 8)


/* return a pointer to a constant table */
Value* parse_prime_file(const char* path) {
    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        printf("cannot read file contents\n");
        exit(EXIT_FAILURE);
    }
    Value* consts;
    uint16_t num_consts = READ_FILE_16(file); /* constant-table size / data section size */
    ALLOCATE(consts, Value, num_consts);
    for (int i = 0; i < num_consts; i++) {
        uint8_t tag = fgetc(file);
        uint16_t idx = READ_FILE_16(file);
        Value v;
        switch(tag) {
            case INT_TAG:
            {
                int x;
                fread(&x, sizeof(int), 1, file);
                v = (Value) {.as_int = x};
                break;
            }
            case DOUBLE_TAG:
            {
                double x;
                fread(&x, sizeof(double), 1, file);
                v = (Value) {.as_double = x};
                break;
            }
            case FUNC_TAG:
            {
                FuncObj* func;
                ALLOCATE(func, FuncObj, 1);
                func->obj = (Obj) {OBJ_FUNC};
                func->arity = (uint8_t) fgetc(file);
                func->loc_cnt = (uint16_t) READ_FILE_16(file);
                uint32_t insts_size;
                fread(&insts_size, sizeof(uint32_t), 1, file);
                ALLOCATE(func->insts, uint8_t, insts_size);
                fread(func->insts, insts_size, 1, file);
                v = (Value) {.as_ref = func};
                break;
            }
        }
        consts[idx] = v;
    }



    return consts;
}
