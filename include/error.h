#ifndef ERROR_H
#define ERROR_H

#include "token.h"
#include <stdio.h>

typedef enum ErrorType {
   SYNTAX_ERROR,
   TYPE_ERROR,
   COMPILATION_ERROR,
   UNDEFINED_SYMBOL_ERROR
}ErrorType;

typedef struct Error {
    ErrorType type;
    size_t src_start_pos;
    Position pos;
    int err_len;
    char* err_msg;
}Error;

#define ALLOC_ERROR(ptr) (ALLOCATE(ptr, Error, 1))

void print_error(Error error, char* filename, char* src);

#endif
