#include "../include/error.h"
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
        case TYPE_ERROR:
            printf("TypeError:%d:%d: %s\n", error.pos.row, error.pos.col, error.err_msg);
            break;
        case UNDEFINED_SYMBOL_ERROR:
            printf("UndefinedSymbol:%d:%d: %s\n", error.pos.row, error.pos.col, error.err_msg);
            break;
        default: 
            printf("Unkown Error Type: %d\n", error.type);
            exit(EXIT_FAILURE);
    }

    /**
     * starts from error.src_start_pos
     * stops when it encounters a line character
    */


    printf("%d |  ", error.pos.row);
    size_t i = error.src_start_pos;
    char c;
    while ((c = src[i++]) != '\0' && c != '\r' && c != '\n') {
        printf("%c", c);
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


