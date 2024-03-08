#include "lexer.h"
#include "vector.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <string.h>
#include <stdbool.h>

#define INIT_TOKEN_SIZE 2048
#define INIT_ID_TOKEN_SIZE 16
#define READ_CHAR(lexer) (lexer->src[lexer->cursor])


void load_file_into_memory(Lexer* lexer, char* filename) {
    FILE* file = fopen(filename, "r");
    assert(file != NULL);
    fseek(file, 0, SEEK_END);
    long len = ftell(file);
    rewind(file);
    char* src;
    ALLOCATE(src, char, len + 1); // extra slot for eof terminator
    fread(src, 1, len, file);
    fclose(file);
    src[len] = '\0';

    lexer->filename = filename;
    lexer->src = src;

}

void init_lexer(Lexer* lexer) {
    lexer->pos.row = 1;
    lexer->pos.col = 1;
    lexer->filename = NULL;
    lexer->src = NULL;
    lexer->cursor = 0;
    lexer->tokens = (Vector) {INIT_VECTOR_CAP, 0, NULL};
    ALLOCATE(lexer->tokens.arr, Token, INIT_VECTOR_CAP);
}

void print_lexer(Lexer* lexer) {
    print_position(lexer->pos);
    printf("cursor: %d\n", lexer->cursor);
    printf("num_tokens: %d\n", lexer->tokens.size);
    for (int i = 0; i < lexer->tokens.size; i++) {
        printf("------------------\n");
        print_token(( (Token*) lexer->tokens.arr)[i]);
        printf("------------------\n");
    }
}

static inline bool is_whitespace(char c) {
    switch (c) {
        case '\n':
        case '\t':
        case ' ':
        case '\r':
            return true;
    }
    return false;
}

static inline bool read_whitespace(Lexer* lexer) {
    switch (READ_CHAR(lexer)) {
        case '\r':
        case '\n':
            lexer->pos.row++;
            lexer->pos.col = 1;
            break;
        case '\t':
            lexer->pos.col += 4;
            break;
        case ' ':
            lexer->pos.col++;
            break;
        default: return false;
    }
    lexer->cursor++;
    return true;
}

// consumes whitespace until there are none
static inline void skip_whitespace(Lexer* lexer) {
    while(read_whitespace(lexer));
}

static void tokenize_id(Lexer* lexer, Token* tok) {

    ALLOCATE(tok->value.arr, char, INIT_VECTOR_CAP);
    char c;
    while ((c = READ_CHAR(lexer)) != '\0' && isalpha(c)) {
        lexer->cursor++;
        APPEND(tok->value, c, char);
    }
    APPEND(tok->value, '\0', char);
    // TODO: change this to a hash table
    const char* value = (const char*) tok->value.arr;
    if (strcmp(value, "false") == 0) {
        tok->type = BOOL_LIT;
    } else if (strcmp(value, "true") == 0) {
        tok->type = BOOL_LIT;
    } else if (strcmp(value, "if") == 0) {
        tok->type = IF;
    } else if (strcmp(value, "else") == 0) {
        tok->type = ELSE;
    } else if (strcmp(value, "elif") == 0) {
        tok->type = ELIF;
    } else if (strcmp(value, "while") == 0) {
        tok->type = WHILE;
    } else if (strcmp(value, "for") == 0) {
        tok->type = FOR;
    } else if (strcmp(value, "then") == 0) {
        tok->type = THEN;
    } else if (strcmp(value, "break") == 0) {
        tok->type = BREAK;
    } else if (strcmp(value, "type") == 0) {
        tok->type = KEYWORD_TYPE;
    } 
    else {
        tok->type = IDENTIFIER;
    } 
    lexer->pos.col += tok->value.size;

}

static void tokenize_int(Lexer* lexer, Token* tok) {
    ALLOCATE(tok->value.arr, char, INIT_VECTOR_CAP);
    char c;
    while((c = READ_CHAR(lexer)) != '\0' && isdigit(c)) {
        lexer->cursor++;
        APPEND(tok->value, c, char);
    }
    APPEND(tok->value, '\0', char);
    tok->type = INT_LIT;
    lexer->pos.col += tok->value.size;

}

static void tokenize_string(Lexer* lexer, Token* tok) {
    ALLOCATE(tok->value.arr, char, INIT_VECTOR_CAP);
    char c;
    while ((c = READ_CHAR(lexer)) != '\0' && c != '"') {
        lexer->cursor++;
        APPEND(tok->value, c, char);
    }
    APPEND(tok->value, '\0', char);
    tok->type = STRING_LIT;
    lexer->pos.col += tok->value.size;
}


void tokenize(Lexer* lexer) {
    char c;
    while((c = READ_CHAR(lexer)) != '\0') {
        Token tok;
        tok.index = lexer->cursor;
        tok.pos = lexer->pos;
        tok.value = (Vector) {INIT_VECTOR_CAP, 0, NULL};
        switch(c) {
            case '+':
                lexer->cursor++;
                lexer->pos.col++;
                if (READ_CHAR(lexer) == '=') {
                    tok.type = PLUS_EQ;
                    lexer->cursor++;
                    lexer->pos.col++;
                    tok.value = (Vector) {2, 2, "+="};
                } else {
                    tok.type = PLUS;
                } 
                tok.value = (Vector) {1, 1, "+"};
                APPEND(lexer->tokens, tok, Token);
                break;
            case '-':
                lexer->cursor++;
                lexer->pos.col++;
                if (READ_CHAR(lexer) == '=') {
                    tok.type = MINUS_EQ;
                    lexer->cursor++;
                    tok.value = (Vector) {2, 2, "-="};
                    lexer->pos.col++;
                } else if(READ_CHAR(lexer) == '>') {
                    tok.type = ARROW;
                    tok.value = (Vector) {2, 2, "->"};
                    lexer->cursor++;
                    lexer->pos.col++;
                }
                else {
                    tok.type = MINUS;
                    tok.value = (Vector) {1, 1, "-"};
                }
                APPEND(lexer->tokens, tok, Token);
                break;
            case '*':
                lexer->cursor++;
                lexer->pos.col++;
                if (READ_CHAR(lexer) == '=') {
                    tok.type = MULT_EQ;
                    lexer->cursor++;
                    lexer->pos.col++;
                    tok.value = (Vector) {2, 2, "*="};
                } else {
                    tok.type = MULT;
                    tok.value = (Vector) {1, 1, "*"};
                } 
                APPEND(lexer->tokens, tok, Token);
                break;
            case '/':
                lexer->cursor++;
                lexer->pos.col++;
                if (READ_CHAR(lexer) == '=') {
                    tok.type = DIV_EQ;
                    lexer->cursor++;
                    lexer->pos.col++;
                    tok.value = (Vector) {2, 2, "/="};
                } else {
                    tok.type = DIV;
                    tok.value = (Vector) {1, 1, "/"};
                } 
                APPEND(lexer->tokens, tok, Token);
                break;
            case ';':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = SEMICOLON;
                tok.value = (Vector) {1, 1, ";"};
                APPEND(lexer->tokens, tok, Token);
                break;
            case ':':
                lexer->cursor++;
                lexer->pos.col++;
                if (READ_CHAR(lexer) == ':') {
                    tok.type = DOUBLE_COLON;
                    lexer->cursor++;
                    lexer->pos.col++;
                    tok.value = (Vector) {2, 2, "::"};
                } else {
                    tok.type = COLON;
                    tok.value = (Vector) {1, 1, ":"};
                }
                APPEND(lexer->tokens, tok, Token);
                break;
            case ',':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = COMMA;
                tok.value = (Vector) {1, 1, ","};
                APPEND(lexer->tokens, tok, Token);
                break;
            case '(':
                lexer->cursor++;
                lexer->pos.col++;
                if (READ_CHAR(lexer) == ')') {
                    tok.type = UNIT;
                    lexer->cursor++;
                    lexer->pos.col++;
                    tok.value = (Vector) {2, 2, "()"};
                } else {
                    tok.type = LPAREN;
                    tok.value = (Vector) {1, 1, "("};
                }
                APPEND(lexer->tokens, tok, Token);
                break;
            case ')':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = RPAREN;
                tok.value = (Vector) {1, 1, ")"};
                APPEND(lexer->tokens, tok, Token);
                break;
            case '{':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = LCURLY;
                tok.value = (Vector) {1, 1, "{"};
                APPEND(lexer->tokens, tok, Token);
                break;
            case '}':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = RCURLY;
                tok.value = (Vector) {1, 1, "}"};
                APPEND(lexer->tokens, tok, Token);
                break;
            case '[':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = LBRAC;
                tok.value = (Vector) {1, 1, "["};
                APPEND(lexer->tokens, tok, Token);
                break;
            case ']':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = RBRAC;
                tok.value = (Vector) {1, 1, "}"};
                APPEND(lexer->tokens, tok, Token);
                break;
            case '.':
                lexer->cursor++;
                lexer->pos.col++; 
                if (READ_CHAR(lexer) == '.') {
                    tok.type = RANGE;
                    lexer->cursor++;
                    lexer->pos.col++;
                    tok.value = (Vector) {2, 2, ".."};
                    APPEND(lexer->tokens, tok, Token);
                    break;
                }
                tok.type = DOT;
                tok.value = (Vector) {1, 1, "."};
                APPEND(lexer->tokens, tok, Token);
                break;

            case '!':
                lexer->cursor++;
                lexer->pos.col++;
                if (READ_CHAR(lexer) == '=') {
                    tok.type = NEQ;
                    lexer->cursor++;
                    lexer->pos.col++;
                    tok.value = (Vector) {2, 2, "!="};
                } else {
                    tok.type = NOT;
                    tok.value = (Vector) {1, 1, "!"};
                }
                APPEND(lexer->tokens, tok, Token);
                break;

            case '%':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = MOD;
                tok.value = (Vector) {1, 1, "%"};
                APPEND(lexer->tokens, tok, Token);
                break;

            case '_':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = UNDERSCORE;
                tok.value = (Vector) {1, 1, "_"};
                APPEND(lexer->tokens, tok, Token);
                break;
            
            case '|':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = PIPE;
                tok.value = (Vector) {1, 1, "|"};
                APPEND(lexer->tokens, tok, Token);
                break;
            case '=':
                lexer->cursor++;
                lexer->pos.col++;
                if (READ_CHAR(lexer) == '=') {
                    tok.type = DEQ;
                    lexer->cursor++;
                    lexer->pos.col++;
                    tok.value = (Vector) {2, 2, "=="};
                } else {
                    tok.type = EQ;
                    tok.value = (Vector) {1, 1, "="};
                }
                APPEND(lexer->tokens, tok, Token);
                break;

            case '"':
                {
                    lexer->cursor++;
                    tok.type = STRING;
                    tok.value = (Vector) {INIT_VECTOR_CAP, 0, NULL};
                    tokenize_string(lexer, &tok);
                    APPEND(lexer->tokens, tok, Token);
                    break;
                }

            default:
                if (isdigit(c)) {
                    tokenize_int(lexer, &tok);
                    if (READ_CHAR(lexer) == '.') {
                        lexer->cursor++;
                        Token frac;
                        frac.value = (Vector) {INIT_VECTOR_CAP, 0, NULL};
                        tokenize_int(lexer, &frac);
                        strcat(tok.value.arr, ".");
                        strcat(tok.value.arr, frac.value.arr);

                    }
                    APPEND(lexer->tokens, tok, Token);
                } else if (isalpha(c)) {
                    tokenize_id(lexer, &tok);
                    APPEND(lexer->tokens, tok, Token);
                } else if (is_whitespace(c)) {
                    skip_whitespace(lexer); 
                } else {
                    fprintf(stderr, "Unrecognized Symbol: %d", c);
                    exit(EXIT_FAILURE);
                }
                break;
        }
    }
    Token eof = {lexer->pos, TOK_EOF, {0}, lexer->cursor};
    APPEND(lexer->tokens, eof, Token);
}

/*void main(int argc, char** argv) {
    Lexer* lexer;
    init_lexer(lexer);
    load_file_into_memory(lexer, argv[1]);
    tokenize(lexer);
    print_lexer(*lexer);
}
*/
