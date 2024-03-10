#ifndef ERROR_H
#define ERROR_H

#include "token.h"
#include <stdio.h>

typedef enum ErrorType {
   SYNTAX_ERROR,
   TYPE_ERROR,
   COMPILATION_ERROR
}ErrorType;

typedef struct Error {
    ErrorType type;
    size_t src_start_pos;
    size_t src_end_pos;
    Position pos;
    char* err_msg;
}Error;

void print_error(Error error, char* filename, char* src);

#endif
