#include "error.h"
/**
 * formats an error message
*/
void print_error(Error error, char* filename, char* src) {
    size_t line_digits = 0;
    size_t counter = error.pos.row;
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
    printf("\n");
    while(counter > 0) {
        printf(" ");
        counter /= 10;
    }
    printf(" |  ");
    for (size_t i = 0; i < error.pos.col - 1; i ++) {
        printf(" ");
    }
    printf("^");
    for (int i = 0; i < error.err_len - 1; i++) {
        printf("~");
    }

    printf("\n");
   
}


