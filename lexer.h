#include "token.h"

typedef struct Lexer {
    Position pos;
    Vector* tokens;
    char* file_name;
    int cursor;
    char* src;
}Lexer;

Lexer* init_lexer();

