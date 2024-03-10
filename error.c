#include "error.h"
/**
 * formats an error message
*/
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

    /**
     * starts from the very beginning of the row and column of where the error ocurred
    */

    printf("%ld |  ", error.pos.row);
    for (size_t i = error.src_start_pos - error.pos.col + 1; i < error.src_end_pos; i++) {
        if (src[i] == '\n' || src[i] == '\r') {
            num_lines++;
            printf("%ld |  \n", error.pos.row + num_lines);
        } else if (i == error.src_start_pos) {
            printf("%c\n", src[i]);
            printf("%ld |  ", error.pos.row + num_lines + 1);
            for (size_t i = 0; i < error.pos.col - 1; i++) {
                printf(" ");
            }
            printf("^");
            for (int i = error.src_start_pos; i < error.src_end_pos - 1; i++) {
                printf("~");
            }
            printf("\n");
        } else {
            printf("%c", src[i]);
        }
    }
   
}


