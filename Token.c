#include "token.h"
#include "memory.h"

void print_position(Position pos) {
    printf("pos: (%d, %d)\n", pos.row, pos.col);
}
static void print_token_type(TokenType type) {
    switch(type) {
        case IDENTIFIER: printf("IDENTIFIER\n"); break;
        case LBRAC: printf("LBRAC\n"); break;
        case RBRAC: printf("RBRAC\n"); break;
        case LPAREN: printf("LPAREN\n"); break;
        case RPAREN: printf("RPAREN\n"); break;
        case SEMICOLON: printf("SEMICOLON\n"); break;
        case COLON: printf("COLON\n"); break;
        case COMMA: printf("COMMA\n"); break;
        case MULT: printf("MULT\n"); break;
        case DIV: printf("DIV\n"); break;
        case PLUS: printf("PLUS\n"); break;
        case MINUS: printf("MINUS\n"); break;
        case EQ: printf("EQ\n"); break;
        case DEQ: printf("DEQ\n"); break;
        case NOT: printf("NOT\n"); break;
        case NEQ: printf("NEQ\n"); break;
        case BT: printf("BT\n"); break;
        case ST: printf("ST\n"); break;
        case BTE: printf("BTE\n"); break;
        case STE: printf("STE\n"); break;
        case ARROW: printf("ARROW\n"); break;
        case LCURLY: printf("LCURLY\n"); break;
        case RCURLY: printf("RCURLY\n"); break;
        case MOD: printf("MOD\n"); break;
        case AND: printf("AND\n"); break;
        case OR: printf("OR\n"); break;
        case PLUS_EQ: printf("PLUS_EQ\n"); break;
        case MINUS_EQ: printf("MINUS_EQ\n"); break;
        case MULT_EQ: printf("MULT_EQ\n"); break;
        case DIV_EQ: printf("DIV_EQ\n"); break;
        case HASHTAG: printf("HASHTAG\n"); break;
        case TYPE_INT: printf("TYPE_INT\n"); break;
        case TYPE_DOUBLE: printf("TYPE_DOUBLE\n"); break;
        case TYPE_STRING: printf("TYPE_STRING\n"); break;
        case TYPE_BYTE: printf("TYPE_BYTE\n"); break;
        case TYPE_CHAR: printf("TYPE_CHAR\n"); break;
        case TYPE_BOOL: printf("TYPE_BOOL\n"); break;
        case FALSE: printf("FALSE\n"); break;
        case TRUE: printf("TRUE\n"); break;
        case IF: printf("IF\n"); break;
        case ELIF: printf("ELIF\n"); break;
        case ELSE: printf("ELSE\n"); break;
        case FOR: printf("FOR\n"); break;
        case WHILE: printf("WHILE\n"); break;
        case UNDERSCORE: printf("UNDERSCORE\n"); break;
        case UNIT: printf("UNIT\n"); break;
        case DOT: printf("DOT\n"); break;
        case EOF: printf("EOF\n"); break;
        default: printf("UNKNOWN_TOKEN_TYPE\n");
    }
}
void print_token(Token token) {
    print_position(token.pos);
    printf("type: ");
    print_token_type(token.type);
    if (token.value) {
        printf("value: %s\n", (char*) token.value->arr);
    }
}
