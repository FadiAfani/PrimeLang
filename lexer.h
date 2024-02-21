#include "token.h"

typedef struct Lexer {
    Position pos;
    Vector* tokens;
    char* filename;
    int cursor;
    char* src;
}Lexer;

Lexer* init_lexer();
void load_file_into_memory(Lexer* lexer, char* filename);

