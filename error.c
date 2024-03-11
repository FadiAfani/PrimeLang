#include "error.h"
/**
 * formats an error message
*/
void print_error(Error error, char* filename, char* src) {
    size_t num_lines = 0;
    switch(error.type) {
        case SYNTAX_ERROR:
            printf("SyntaxError:%d:%d in %s: %s\n", error.pos.row, error.pos.col, filename, error.err_msg);
            break;
        case COMPILATION_ERROR: 
            printf("SyntaxError:%d:%d: %s\n", error.pos.row, error.pos.col, error.err_msg);
            break;
        default: 
            printf("Unkown Error Type\n");
            exit(EXIT_FAILURE);
    }

    /**
     * starts from the very beginning of the row and column of where the error ocurred
    */

    printf("%d |  ", error.pos.row);
    for (size_t i = error.src_start_pos; i < error.src_end_pos; i++) {
        printf("%c", src[i]);
    }
    printf("\n  |  ");
    for (size_t i = 0; i < error.src_start_pos - error.pos.col; i ++) {
        printf(" ");
    }
    printf("^");
    for (size_t i = 0; i < error.src_end_pos - error.pos.col; i++) {
        printf("~");
    }

    printf("\n");
   
}


