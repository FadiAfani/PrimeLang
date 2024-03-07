#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "symbol.h"
#include "ast_node.h"



typedef struct Parser {
    Lexer lexer;
    ASTNode* root;
    Vector parsing_errors;
    size_t cursor;
}Parser;

ASTNode* parse_identifier_literal(Parser* parser);
ASTNode* parse_int_literal(Parser* parser);
ASTNode* parse_double_literal(Parser* parser);
ASTNode* parse_string_literal(Parser* parser);
ASTNode* parse_literal(Parser* parser);
ASTNode* parse_factor(Parser* parser);
ASTNode* parse_term(Parser* parser);
ASTNode* parse_and(Parser* parser);
ASTNode* parse_or(Parser* parser);
ASTNode* parse_assignment(Parser* parser);
ASTNode* parse_expr(Parser* parser);
ASTNode* parse_group_expr(Parser* parser);
ASTNode* parse_func_call(Parser* parser);
ASTNode* parse_primary(Parser* parser);
ASTNode* parse_statement(Parser* parser);
ASTNode* parse_compound(Parser* parser);
ASTNode* parse_list_index(Parser* parser);
ASTNode* parse_block(Parser* parser);
ASTNode* parse_if(Parser* parser);
ASTNode* parse_elif(Parser* parser);

void print_node(ASTNode* node, int depth);
void free_ast_node(ASTNode* node);


#endif
