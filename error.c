#include "error.h"

void print_error(Error error, char* filename, char* src) {
    size_t num_lines = 0;
    switch(error.type) {
        case SYNTAX_ERROR:
            printf("SyntaxError:%d:%d: %s\n", error.pos.row, error.pos.col, error.err_msg);
            break;
        case COMPILATION_ERROR: 
            printf("SyntaxError:%d:%d: %s\n", error.pos.row, error.pos.col, error.err_msg);
            break;
        default: 
            printf("Unkown Error Type\n");
            exit(EXIT_FAILURE);
    }

    printf("%d | \t", error.pos.row);
    for (size_t i = 0; i < error.src_end_pos - error.src_start_pos + 1; i++) {
        if (src[i] == '\n' || src[i] == '\r') {
            num_lines++;
            printf("\n%d | \t", error.pos.row + num_lines);
        } else {
            printf("%c", src[i]);
        }
    }
}

void set_error(Error* err, ErrorType err_type, size_t start_pos, size_t end_pos, Position pos, const char* msg) {
    err->type = err_type;
    err->src_start_pos = start_pos;
    err->src_end_pos = end_pos;
    err->pos = pos;
    err->err_msg = msg;
}
