#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "symbol.h"
#include "ast_node.h"
#include "scope.h"



typedef struct Parser {
    Lexer lexer;
    ASTNode* root;
    Vector parsing_errors;
    size_t cursor;
    ScopeStack scopes;
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
ASTNode* parse_grouped_expr(Parser* parser);
ASTNode* parse_func_call(Parser* parser);
ASTNode* parse_unary_expr(Parser* parser);
ASTNode* parse_primary(Parser* parser);
ASTNode* parse_statement(Parser* parser);
ASTNode* parse_compound(Parser* parser);
ASTNode* parse_list_index(Parser* parser);
ASTNode* parse_block(Parser* parser);
ASTNode* parse_if(Parser* parser);
ASTNode* parse_elif(Parser* parser);
ASTNode* parse_func_decl(Parser* parser);
ASTNode* parse_comparison(Parser* parser);
ASTNode* parse_block_based_expr(Parser* parser);
ASTNode* parse_break_expr(Parser* parser);
ASTNode* parse_program(Parser* parser);
ASTNode* parse_param(Parser* parser);
PrimeType* parse_type_annot(Parser* parser);
PrimeType*   parse_type(Parser* parser);
void register_pattern(Parser* parser, ASTNode* node);
void parse_type_annotation(Parser* parser, ASTNode* node);
void parse_type_constructor(Parser* parser, ASTNode* node);



void print_node(ASTNode* node, int depth);
void free_ast_node(ASTNode* node);
void init_parser(Parser* parser);


#endif
