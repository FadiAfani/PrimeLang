#ifndef LEXER_H
#define LEXER_H

#include "token.h"

typedef struct Lexer {
    Position pos;
    Vector tokens;
    char* filename;
    int cursor;
    char* src;
}Lexer;

void init_lexer(Lexer* lexer);
void load_file_into_memory(Lexer* lexer, char* filename);
void print_lexer(Lexer* lexer);
void tokenize(Lexer* lexer);
#endif

