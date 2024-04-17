#include "../include/parser.h"
#include "../include/memory.h"
#include "../include/symbol_table.h"
#include "../include/vector.h"
#include "../include/error.h"
#include "../include/type.h"
#include "../include/scope.h"
#include <stdbool.h>

#define READ_TOKEN(parser) (&(INDEX_VECTOR(parser->lexer.tokens, Token, parser->cursor)))
#define PEEK_AT(parser, i) (&(INDEX_VECTOR(parser->lexer.tokens, Token, parser->cursor + 1)))
#define PEEK_NEXT(parser) (PEEK_AT(parser, 1))


void init_parser(Parser* parser) {
    Lexer lexer;
    init_lexer(&lexer);
    parser->lexer = lexer;
    parser->root = NULL;
    parser->parsing_errors = (Vector) {INIT_VECTOR_CAP, 0, NULL};
    ALLOCATE(parser->parsing_errors.arr, Error, INIT_VECTOR_CAP);
    parser->cursor = 0;
    parser->scopes.sp = -1;
}

void free_ast_node(ASTNode* node) {
    if (node == NULL) return;

    switch(node->type) {
        case LITERAL_EXPR: break;
        case BIN_EXPR:
            free_ast_node(node->as_bin_expr.left);
            free_ast_node(node->as_bin_expr.right);
            break;
        case LIST_EXPR:
            for (size_t i = 0; i < node->as_list_expr.items.size; i++) {
                free_ast_node(INDEX_VECTOR(node->as_list_expr.items, ASTNode*, i));
            }
            break;
        default:
            return;
    }
    free(node);
}

void print_node(ASTNode* node, int depth) {
    if (node == NULL) return;

    for (int i = 0; i < depth; i++) {
        printf("\t");
    }
    switch(node->type) {
        case LITERAL_EXPR:
            printf("<literal>: %s\n", (char*) node->as_literal_expr->value.arr);
            if (node->as_literal_expr->type == IDENTIFIER && node->as_id_literal.id_type != NULL) {
                for (size_t i = 0; i < node->as_id_literal.id_type->size; i++) {
                    print_node(INDEX_VECTOR((*node->as_id_literal.id_type), ASTNode*, i), depth + 1);
                }
            }
            break;
        case BIN_EXPR:
            printf("<binary-expr>: %s\n", CAST_VECTOR(node->as_bin_expr.op->value, char));
            print_node(node->as_bin_expr.left, depth + 1);
            print_node(node->as_bin_expr.right, depth + 1);
            break;
        case LIST_EXPR:
            printf("<list-expr>\n");
            for (size_t i = 0; i < node->as_list_expr.items.size; i++) {
                print_node(INDEX_VECTOR(node->as_list_expr.items, ASTNode*, i), depth + 1);
            }
            break;
        case TUPLE_EXPR:
            printf("<tuple-expr>\n");
            for (size_t i = 0; i < node->as_list_expr.items.size; i++) {
                print_node(INDEX_VECTOR(node->as_list_expr.items, ASTNode*, i), depth + 1);
            }
            break;
        case COMPOUND_STMT:
            printf("<compound-statement>\n");
            for (size_t i = 0; i < node->as_compound_statements.size; i++) {
                print_node(INDEX_VECTOR(node->as_compound_statements, ASTNode*, i), depth + 1);
            }
            break;
        case BLOCK_EXPR:
            printf("<block-expr>\n");
            for (size_t i = 0; i < node->as_compound_statements.size; i++) {
                print_node(INDEX_VECTOR(node->as_compound_statements, ASTNode*, i), depth + 1);
            }
            break;
        case LIST_INDEX_EXPR:
            printf("<list-index>\n");
            print_node(node->as_list_index_expr.index, depth + 1);
            print_node(node->as_list_index_expr.list_id, depth + 1);
            break;
        case IF_EXPR:
            printf("<if-expr>\n");
            print_node(node->as_if_expr.cond, depth + 1);
            print_node(node->as_if_expr.expr, depth + 1);
            print_node(node->as_if_expr.else_expr, depth + 1);
            for (size_t i = 0; i < node->as_if_expr.else_ifs.size; i++) {
                print_node(INDEX_VECTOR(node->as_if_expr.else_ifs, ASTNode*, i), depth + 1);
            }
            break;

        case TYPE_DECL:
            printf("<type-decl>\n");
            for(int i = 0; i < depth + 1; i++) {
                printf("\t");
            }
            //print_symbol(node->as_type_decl.symbol);
            break;

        case FUNC_DECL:
            printf("<func-decl>\n");
            Vector vec = node->as_func_decl.parameters;
            for(size_t i = 0; i < vec.size; i++) {
                //print_node((INDEX_VECTOR(vec, ASTNode*, i)), depth + 1);
            }
            print_node(node->as_func_decl.block, depth + 1);
            break;
        case FUNC_CALL_EXPR:
            printf("<func-call>\n");
            print_token(*node->as_func_call.func_id);
            for (size_t i = 0; i < node->as_func_call.params.size; i++) {
                print_node(INDEX_VECTOR(node->as_func_call.params, ASTNode*, i), depth + 1);
            }

            break;

        case PREDEFINED_TYPE:
            printf("<predefined-type>:");
            switch(node->as_type->type) {
                case UNIT:
                    printf("()\n");
                    break;
                case TYPE_BOOL:
                    printf("bool\n");
                    break;
                case TYPE_BYTE:
                    printf("byte\n");
                    break;
                case TYPE_INT:
                    printf("int\n");
                    break;
                case TYPE_DOUBLE:
                    printf("double\n");
                    break;
                case TYPE_CHAR:
                    printf("char\n");
                    break;
                case TYPE_STRING:
                    printf("string\n");
                    break;
                default:
                    printf("%s", (char*) node->as_type->value.arr);
                    break;
            }
            break;

        default:
            printf("print method is not implemented for this node type: %d\n", node->type);
            exit(EXIT_FAILURE);

    }
}

static bool consume(Parser* parser, TokenType token_type) {
    Token* tok = READ_TOKEN(parser);
    if (tok->type == token_type) {
       parser->cursor++;
       return true;
    } 

    return false;
}


ASTNode* parse_identifier_literal(Parser* parser) {

    Token* tok = READ_TOKEN(parser);
    consume(parser,IDENTIFIER);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->start_tok = tok;
    node->end_tok = tok;
    node->as_id_literal.id_token = tok;
    node->as_id_literal.id_type = NULL;
    node->type = LITERAL_EXPR;

    Token* colon = READ_TOKEN(parser);

    return node;
}

ASTNode* parse_int_literal(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    if (consume(parser, INT_LIT)) {
        ASTNode* node;
        ALLOCATE(node, ASTNode, 1);
        node->as_literal_expr = tok;
        node->type = LITERAL_EXPR;
        node->start_tok = tok;
        node->end_tok = tok;
        return node;
    } 
    
    return NULL;
}

ASTNode* parse_double_literal(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    if (consume(parser, DOUBLE_LIT)) {
        ASTNode* node;
        ALLOCATE(node, ASTNode, 1);
        node->as_literal_expr = tok;
        node->type = LITERAL_EXPR;
        node->start_tok = tok;
        node->end_tok = tok;
        return node;
    } 

    return NULL;
}

ASTNode* parse_string_literal(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    if (consume(parser, STRING_LIT)) {
        ASTNode* node;
        ALLOCATE(node, ASTNode, 1);
        node->as_literal_expr = tok;
        node->type = LITERAL_EXPR;
        node->start_tok = tok;
        node->end_tok = tok;
        return node;
    } 

    return NULL;
}

ASTNode* parse_literal(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    switch(tok->type) {
        case IDENTIFIER:
            return parse_identifier_literal(parser);
        case INT_LIT:
            return parse_int_literal(parser);
        case DOUBLE_LIT:
            return parse_double_literal(parser);
        case STRING_LIT: 
            return parse_string_literal(parser);
        case BOOL_LIT: break;
        case TOK_EOF: break;
        default: 
            parser->cursor++;
            APPEND(parser->parsing_errors,
                ((Error) {SYNTAX_ERROR,
                tok->index - tok->pos.col + 1, 
                tok->pos, 
                tok->value.size, 
                "unexpected symbol"}),
                Error
            );
            break;
    }
    return NULL;
}

ASTNode* parse_unary_expr(Parser* parser) {
    Token* op = READ_TOKEN(parser);
    if (!consume(parser, MINUS)) return NULL;
    ASTNode* expr = parse_expr(parser);
    if (expr == NULL) return NULL;
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->type = UNARY_EXPR;
    node->as_un_expr.op = op;
    node->as_un_expr.operand = expr;
    return node;
    
}

// <factor> := <literal> ( [ * | \ ] <literal> )*
ASTNode* parse_factor(Parser* parser) {
    ASTNode* node = parse_primary(parser);
    if (node == NULL) return NULL;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        Token* tok = READ_TOKEN(parser);
        if (consume(parser, MULT) || consume(parser, DIV)) {
            ASTNode* new_node;
            ALLOCATE(new_node, ASTNode, 1);
            new_node->as_bin_expr.op = tok;
            new_node->as_bin_expr.left = node;
            new_node->as_bin_expr.right = parse_literal(parser);
            new_node->type = BIN_EXPR;

            new_node->start_tok = new_node->as_bin_expr.left->start_tok;
            if (new_node->as_bin_expr.right == NULL) {
                new_node->end_tok = tok;
                APPEND(
                    parser->parsing_errors,
                    ((Error) {SYNTAX_ERROR,
                    node->start_tok->index - node->start_tok->pos.col + 1, 
                    node->start_tok->pos, 
                    tok->index - node->start_tok->index + 1,
                    tok->type == MULT ? "expected an expression after '*'" :"expected an expression after '/'" }),
                    Error
                );

                /**
                 * free and return
                 * freeing new_node should also free its child nodes
                */
                free_ast_node(new_node);
                return NULL;

                    
            } else {
                new_node->end_tok = new_node->as_bin_expr.right->end_tok;
            }

            node = new_node;
        } else {break;}
    }

    return node;

}

ASTNode* parse_term(Parser* parser) {
    ASTNode* node = parse_factor(parser);
    if (node == NULL) return NULL;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        Token* tok = READ_TOKEN(parser);
        if (consume(parser, PLUS) || consume(parser, MINUS)) {
            ASTNode* new_node;
            ALLOCATE(new_node, ASTNode, 1);
            new_node->as_bin_expr.op = tok;
            new_node->as_bin_expr.left = node;
            new_node->as_bin_expr.right = parse_factor(parser);
            new_node->type = BIN_EXPR;

            new_node->start_tok = new_node->as_bin_expr.left->start_tok;
            if (new_node->as_bin_expr.right == NULL) {
                new_node->end_tok = tok;
                APPEND(
                    parser->parsing_errors,
                    ((Error) {SYNTAX_ERROR,
                    node->start_tok->index - node->start_tok->pos.col + 1, 
                    node->start_tok->pos, 
                    tok->index - node->start_tok->index + 1,
                    tok->type == PLUS ? "expected an expression after '+'" :"expected an expression after '-'" }),
                    Error
                );

                // free and return
                free_ast_node(new_node);
                return NULL;
                    
            } else {
                new_node->end_tok = new_node->as_bin_expr.right->end_tok;
            }

            node = new_node;
        } else {break;}
    }
    return node;
}

ASTNode* parse_and(Parser* parser) {
    ASTNode* node = parse_term(parser);
    if (node == NULL) return NULL;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        Token* tok = READ_TOKEN(parser);
        if (consume(parser, AND)) {
            ASTNode* new_node;
            ALLOCATE(new_node, ASTNode, 1);
            new_node->as_bin_expr.op = tok;
            new_node->as_bin_expr.left = node;
            new_node->as_bin_expr.right = parse_term(parser);
            new_node->type = BIN_EXPR;

            new_node->start_tok = new_node->as_bin_expr.left->start_tok;
            if (new_node->as_bin_expr.right == NULL) {
                new_node->end_tok = tok;
                APPEND(
                    parser->parsing_errors,
                    ((Error) {SYNTAX_ERROR,
                    node->start_tok->index - node->start_tok->pos.col + 1, 
                    node->start_tok->pos, 
                    tok->index - node->start_tok->index + 1,
                    "expected an expression after '&&'"}),
                    Error
                );
                // free and return
                free_ast_node(new_node);
                return NULL;
                    
            } else {
                new_node->end_tok = new_node->as_bin_expr.right->end_tok;
            }

            node = new_node;
        } else {break;}
    }
    return node;
}

ASTNode* parse_or(Parser* parser) {
    ASTNode* node = parse_and(parser);
    if (node == NULL) return NULL;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        Token* tok = READ_TOKEN(parser);
        if (consume(parser, OR)) {
            ASTNode* new_node;
            ALLOCATE(new_node, ASTNode, 1);
            new_node->as_bin_expr.op = tok;
            new_node->as_bin_expr.left = node;
            new_node->as_bin_expr.right = parse_and(parser);
            new_node->type = BIN_EXPR;

            new_node->start_tok = new_node->as_bin_expr.left->start_tok;
            if (new_node->as_bin_expr.right == NULL) {
                new_node->end_tok = tok;
                APPEND(
                    parser->parsing_errors,
                    ((Error) {SYNTAX_ERROR,
                    node->start_tok->index - node->start_tok->pos.col + 1, 
                    node->start_tok->pos, 
                    tok->index - node->start_tok->index + 1,
                    "expected an expression after '||'"}),
                    Error
                );
                // free and return
                free_ast_node(new_node);
                return NULL;
                    
            } else {
                new_node->end_tok = new_node->as_bin_expr.right->end_tok;
            }

            node = new_node;
        } else {break;}
    }
    return node;
}

ASTNode* parse_list(Parser* parser) {

    Token* tok = READ_TOKEN(parser);
    consume(parser, LBRAC);

    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->as_list_expr.items = (Vector) {INIT_VECTOR_CAP, 0, NULL};
    ALLOCATE(node->as_list_expr.items.arr, ASTNode*, INIT_VECTOR_CAP);
    APPEND(node->as_list_expr.items, parse_expr(parser), ASTNode*);
    node->type = LIST_EXPR;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        Token* tok = READ_TOKEN(parser);
        if (consume(parser, COMMA)) {
            ASTNode* item = parse_expr(parser);
            if (item != NULL) {
                APPEND(node->as_list_expr.items, parse_expr(parser), ASTNode*);
            } else {
                APPEND(parser->parsing_errors,
                    ((Error) {SYNTAX_ERROR,
                    tok->index - tok->pos.col + 1, 
                    tok->pos, 
                    tok->value.size,
                    "expected an expression after ','"}),
                    Error
                    );

                // free node and return
                free_ast_node(node);
                return NULL;
            }
        } else {
            break;
        }
    }

    // store starting token and ending token

    node->start_tok = tok;
    if (node->as_list_expr.items.size > 0) {
        node->end_tok = INDEX_VECTOR(node->as_list_expr.items, ASTNode*, node->as_list_expr.items.size - 1)->end_tok;
    } else {
        node->end_tok = tok;
    }

    Token* brac = READ_TOKEN(parser);
    if (!consume(parser, RBRAC)) {
        // report error and free node memory

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            node->start_tok->index - node->start_tok->pos.col + 1, 
            tok->pos, 
            node->start_tok->value.size,
            "unclosed delimiter, missing ']' at the end of the list literal"}),
            Error
            );

        free_ast_node(node);
        return NULL;
    }

    return node;

}
// TODO: add start_tok and end_tok to the node
ASTNode* parse_tuple(Parser* parser) {

    consume(parser, LPAREN);
    ASTNode* expr = parse_expr(parser);
    if (READ_TOKEN(parser)->type == RPAREN) {
        consume(parser, RPAREN);
        return expr;
    }

    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->as_list_expr.items = (Vector) {INIT_VECTOR_CAP, 0, NULL};
    ALLOCATE(node->as_list_expr.items.arr, ASTNode*, INIT_VECTOR_CAP);
    APPEND(node->as_list_expr.items, expr, ASTNode*);
    node->type = TUPLE_EXPR;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        if (consume(parser, COMMA)) {
            APPEND(node->as_list_expr.items, parse_expr(parser), ASTNode*);
        } else {
            break;
        }
    }

    Token* brac = READ_TOKEN(parser);
    if (!consume(parser, RPAREN)) {
        // report error and free node memory

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            brac->index, 
            brac->pos, 
            brac->value.size, "missing ')' symbol"}),
            Error
            );

        free_ast_node(node);


        return NULL;
    }

    return node;

}


ASTNode* parse_func_call(Parser* parser) {
    Token* id_tok = READ_TOKEN(parser);
    consume(parser, IDENTIFIER);

    Token* lparen = READ_TOKEN(parser);

    ASTNode* node = NULL;
    ALLOCATE(node, ASTNode, 1);
    node->start_tok = id_tok;
    node->end_tok = node->start_tok;
    node->type = FUNC_CALL_EXPR;
    node->as_func_call.func_id = id_tok;
    INIT_VECTOR(node->as_func_call.params, ASTNode*);
    ASTNode* arg = parse_grouped_expr(parser);
    if (arg == NULL) {
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            node->start_tok->index - node->start_tok->pos.col + 1, 
            lparen->pos, 
            lparen->value.size, 
            "expected an expression"}),
            Error
            );
        free_ast_node(node);
        return NULL;
    }
    APPEND(node->as_func_call.params, arg, ASTNode*);

    while(READ_TOKEN(parser)->type == LPAREN) {
        Token* tok = READ_TOKEN(parser);
        arg = parse_grouped_expr(parser);
        APPEND(node->as_func_call.params, arg, ASTNode*);
    }


    return node;

}

PrimeType* parse_type_annot(Parser* parser) {
    PrimeType* t = parse_type(parser);
    if (t == NULL) {
        //TODO: report error and free
        return NULL;
    }
    FuncType* ft;
    ALLOCATE(ft, FuncType, 1);
    ft->pt = t;
    while(READ_TOKEN(parser)->type == ARROW) {
        consume(parser, ARROW);
        ft->pt = parse_type(parser);
    }

    t->type_kind = FUNC_KIND;
    t->as_func_type = ft;

    return t;

}

ASTNode* parse_func_decl(Parser* parser) {
    Token* id = READ_TOKEN(parser);
    if (!consume(parser, IDENTIFIER)) return NULL;
    if (!consume(parser, DOUBLE_COLON)) return NULL;

    ASTNode* node;
    ALLOC_NODE(node);
    INIT_VECTOR(node->as_func_decl.parameters, ASTNode*);
    node->as_func_decl.sym_id = id;
    node->type = FUNC_DECL;
    node->p_type = NULL;

    // init FuncType
    PrimeType* func_t;
    ALLOC_TYPE(func_t);
    ALLOCATE(func_t->as_func_type, FuncType, 1);

    // parse parameters
    ASTNode* p = parse_param(parser);
    APPEND(node->as_func_decl.parameters, p, ASTNode*);
    while (READ_TOKEN(parser)->type == ARROW && PEEK_NEXT(parser)->type == LPAREN) {
        consume(parser, ARROW);
        p = parse_param(parser);
        APPEND(node->as_func_decl.parameters, p, ASTNode*);
    }

    if (!consume(parser, ARROW)) {
        // TODO: report error and free memory
        return NULL;
    }

    PrimeType* ret_t = parse_type_annot(parser);
    if (ret_t == NULL) {
        //TODO: report error
        return NULL;
    }
    node->as_func_decl.block = parse_block(parser);
    Symbol* sym;
    ALLOC_SYMBOL(sym);
    INIT_FUNC_SYMBOL(sym->as_func_symbol);
    sym->type = SYMBOL_FUNCTION;
    insert_top(&parser->scopes, (char*) id->value.arr, sym);
    FuncType* cur_ft = func_t->as_func_type;
    for (size_t i = 0; i < node->as_func_decl.parameters.size; i++) {
        Symbol* ps;
        ALLOC_SYMBOL(ps);
        ps->type = SYMBOL_PARAMETER;
        ASTNode* id_node = INDEX_VECTOR(node->as_func_decl.parameters, ASTNode*, i);
        ps->as_var_symbol = id_node->p_type;
        FuncType* next;
        ALLOCATE(next, FuncType, 1);
        next->pt = id_node->p_type;
        cur_ft->next = next;
        cur_ft = next;
        insert(&node->as_func_decl.block->as_block_expr.table, (char*) id_node->as_literal_expr->value.arr, ps);

    }
    cur_ft->next = NULL;
    func_t->type_kind = FUNC_KIND;
    sym->as_func_symbol.func_type = func_t;

    return node;

    }


ASTNode* parse_list_index(Parser* parser) {
    ASTNode* id = parse_identifier_literal(parser);
    Token* tok = READ_TOKEN(parser);
    if (!consume(parser, LBRAC)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            tok->index, 
            tok->pos, 
            tok->value.size, 
            "missing ')' symbol"}),
            Error
            );
        free_ast_node(id);
        return NULL;
    }
    ASTNode* expr = parse_expr(parser);

    if (!consume(parser, RBRAC)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            tok->index, 
            tok->pos, 
            tok->value.size, 
            "missing ')' symbol"}),
            Error
            );
        free_ast_node(id);
        free_ast_node(expr);
        return NULL;
    }
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->type = LIST_INDEX_EXPR;
    node->as_list_index_expr.index = expr;
    node->as_list_index_expr.list_id = id;

    return node;
}

ASTNode* parse_primary(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    switch(READ_TOKEN(parser)->type) {
        case IDENTIFIER:
            if (PEEK_NEXT(parser)->type == LPAREN) {
                return parse_func_call(parser);
            } else if (PEEK_NEXT(parser)->type == LBRAC) {
                return parse_list_index(parser);
            }
            return parse_identifier_literal(parser);
        case INT_LIT: return parse_int_literal(parser);
        case LPAREN: return parse_tuple(parser);
        case LBRAC: return parse_list(parser);
        case LCURLY: return parse_block(parser);
        case IF: return parse_if(parser);
        case BLOCK_EXPR: return parse_block(parser);
        case BREAK: return parse_break_expr(parser);
        case TOK_EOF: break;

        default: 
            parser->cursor++;
            APPEND(parser->parsing_errors,
                ((Error) {SYNTAX_ERROR,
                tok->index - tok->pos.col + 1, 
                tok->pos, 
                tok->value.size, 
                "unexpected symbol"}),
                Error
            );
            break;
    }
    return NULL;

}


ASTNode* parse_block(Parser* parser) {
    Token* lcurly = READ_TOKEN(parser);
    consume(parser, LCURLY);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    INIT_VECTOR(node->as_compound_statements, ASTNode*);
    node->type = BLOCK_EXPR;
    node->start_tok = lcurly;
    init_symbol_table(&node->as_block_expr.table);
    push_scope(&parser->scopes, &node->as_block_expr.table);
    bool unclosed = true;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        Token* rcurly = READ_TOKEN(parser);
        if (consume(parser, RCURLY)) {
            node->end_tok = rcurly;
            unclosed = false;
            break;
        }
        ASTNode* stmt = parse_statement(parser);
        if (stmt == NULL) return NULL;
        APPEND(node->as_compound_statements, stmt, ASTNode*);
        
    }

    Token* eof_tok = READ_TOKEN(parser);
    if (unclosed && !consume(parser, RCURLY)) {
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            lcurly->index - lcurly->pos.col + 1, 
            lcurly->pos, 
            lcurly->value.size, 
            "unclosed delimiter, missing '}' after a block expression"}),
            Error
            );
        free_ast_node(node);
        return NULL;
    }

    pop_scope(&parser->scopes);


    return node;

}

ASTNode* parse_elif(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    consume(parser, ELIF);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    INIT_VECTOR(node->as_if_expr.else_ifs, ASTNode*);
    node->as_if_expr.cond = parse_expr(parser);
    node->start_tok = tok;
    node->end_tok = tok;
    node->type = IF_EXPR;
    if (node->as_if_expr.cond == NULL) {

        Position err_pos = {tok->pos.row, tok->pos.col + 3}; // point to the end of 'elif'
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            node->start_tok->index - node->start_tok->pos.col + 1, 
            err_pos, 
            0, "expected an expression after 'elif'"}),
            Error
        );
        free_ast_node(node);
        return NULL;
    }

    tok = READ_TOKEN(parser);
    if (!consume(parser, THEN)) {

        // tok->value.size - 1 throws away the 0 character
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            node->start_tok->index - node->start_tok->pos.col + 1, 
            node->as_if_expr.cond->end_tok->pos, 
            node->as_if_expr.cond->end_tok->value.size - 1, "expected 'then' after elif condition"}),
            Error
        );
        free_ast_node(node);
        return NULL;
    }
    node->as_if_expr.expr= parse_expr(parser);
    return node;
}

ASTNode* parse_if(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    consume(parser, IF);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    INIT_VECTOR(node->as_if_expr.else_ifs, ASTNode*);
    node->as_if_expr.cond = parse_expr(parser);
    node->start_tok = tok;
    node->end_tok = tok;
    node->type = IF_EXPR;
    if (node->as_if_expr.cond == NULL) {
        Position err_pos = {tok->pos.row, tok->pos.col + 1}; // point to the end of 'if'
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            node->start_tok->index - node->start_tok->pos.col + 1, 
            err_pos, 
            0, "expected an expression after 'if'"}),
            Error
        );
        free_ast_node(node);
        return NULL;
    }

    tok = READ_TOKEN(parser);
    if (!consume(parser, THEN)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            tok->index - tok->pos.col + 1, 
            tok->pos, 
            tok->value.size - 1, 
            "expected 'then' after an if condition"}),
            Error
            );

        free_ast_node(node);
        return NULL;
    }
    node->as_if_expr.expr = parse_expr(parser);
    node->as_if_expr.else_expr = NULL;
    while(READ_TOKEN(parser)->type == ELIF) {
        APPEND(node->as_if_expr.else_ifs, parse_elif(parser), ASTNode*);
    }

    if (consume(parser, ELSE)) {
        node->as_if_expr.else_expr = parse_expr(parser);
    } 

    if (node->as_if_expr.else_expr != NULL) {
        node->end_tok = node->as_if_expr.else_expr->end_tok;
    } else if (node->as_if_expr.else_ifs.size > 0) {
        ASTNode* last_elif = INDEX_VECTOR(node->as_if_expr.else_ifs, ASTNode*, node->as_if_expr.else_ifs.size - 1);
        if (last_elif != NULL) {
            node->end_tok = last_elif->end_tok;
        }
    }

    return node;
}


ASTNode* parse_range(Parser* parser) {
    ASTNode* left = parse_or(parser);
    if (left == NULL) return NULL;
    Token* tok = READ_TOKEN(parser);
    if (!consume(parser, RANGE)) return left;
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->as_bin_expr.left = left;
    node->as_bin_expr.right = parse_or(parser);
    node->type = BIN_EXPR;
    node->as_bin_expr.op = tok;
    node->start_tok = node->as_bin_expr.left->start_tok;
    node->end_tok = node->start_tok;

    if (node->as_bin_expr.right == NULL) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            left->start_tok->index - left->start_tok->pos.col + 1, 
            tok->pos, 
            tok->value.size, 
            "expected an expression after '..'"}),
            Error
        );

        free_ast_node(node);
        return NULL;
    }

    node->end_tok = node->as_bin_expr.right->end_tok;
    return node;
}

ASTNode* parse_comparison(Parser* parser) {
    ASTNode* left = parse_range(parser);
    if (left == NULL) return NULL;

    Token* tok = READ_TOKEN(parser);
    if (!consume(parser, DEQ) 
        && !consume(parser, NEQ) 
        && !consume(parser, BTE) 
        && !consume(parser, BT) 
        && !consume(parser, LTE) 
        && !consume(parser, LT)
        ) return left;

    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->as_bin_expr.left = left;
    node->as_bin_expr.op = tok;
    node->as_bin_expr.right = parse_range(parser);
    node->type= BIN_EXPR;
    node->start_tok = node->as_bin_expr.left->start_tok;

    if (node->as_bin_expr.right == NULL) {

        char* err_msg;
        switch(tok->type) {
            case DEQ:
                err_msg = "expected an expression after '=='";
                break;
            case NEQ:
                err_msg = "expected an expression after '!='";
                break;
            case BTE:
                err_msg = "expected an expression after '>='";
                break;
            case BT:
                err_msg = "expected an expression after '>'";
                break;
            case LTE:
                err_msg = "expected an expression after '<='";
                break;
            case LT:
                err_msg = "expected an expression after '<'";
                break;
            default:
                err_msg = "unexpected symbol";
                break;

        }

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            left->start_tok->index - left->start_tok->pos.col + 1, 
            tok->pos, 
            tok->value.size, 
            err_msg}),
            Error
        );

        free_ast_node(node);
        node->end_tok = node->start_tok;
        return NULL;
    }

    node->end_tok = node->as_bin_expr.right->end_tok;
    return node;
}

PrimeType* parse_type(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    PrimeType* t;
    ALLOC_TYPE(t);
    switch (tok->type) {
        case TYPE_INT:
            t->as_built_in_type = INT_TYPE;
            t->type_kind = BUILT_IN;
            consume(parser, TYPE_INT);
            break;
        case TYPE_DOUBLE:
            t->as_built_in_type = DOUBLE_TYPE;
            t->type_kind = BUILT_IN;
            consume(parser, TYPE_DOUBLE);
            break;
        case TYPE_STRING:
            t->as_built_in_type = STRING_TYPE;
            t->type_kind = BUILT_IN;
            consume(parser, TYPE_STRING);
            break;
        case TYPE_BOOL:
            t->as_built_in_type = BOOL_TYPE;
            t->type_kind = BUILT_IN;
            consume(parser, TYPE_BOOL);
            break;
        case UNIT:
            t->as_built_in_type = UNIT_TYPE;
            t->type_kind = BUILT_IN;
            consume(parser, UNIT);
            break;
        case IDENTIFIER:
            t->as_user_defined_type = (char*) tok->value.arr;
            t->type_kind = USER_DEFINED;
            consume(parser, IDENTIFIER);
            break;
        case TOK_EOF: return NULL;
        default:

            APPEND(parser->parsing_errors,
                ((Error) {SYNTAX_ERROR,
                tok->index - tok->pos.col + 1, 
                tok->pos, 
                tok->value.size, 
                "unexpected symbol"}),
                Error
            );
            parser->cursor++;
            return NULL;
    }
    return t;
}


ASTNode* parse_param(Parser* parser) {
    if (!consume(parser, LPAREN)) return NULL;
    Token* t = READ_TOKEN(parser);
    if (!consume(parser, IDENTIFIER)) {
        //TODO: report an error
        return NULL;
    }
    if (!consume(parser, COLON)) {
        //TODO: report an error
        return NULL;
    }

    PrimeType* pt = parse_type(parser);
    if (!consume(parser, RPAREN)) {
        //TODO: report error and free pt
        return NULL;
    }

    ASTNode* node;
    ALLOC_NODE(node);
    node->as_literal_expr= t;
    node->p_type = pt;
    node->type = IDENTIFIER;

    return node;
}


ASTNode* parse_assignment(Parser* parser) {
    ASTNode* left = parse_comparison(parser);
    if (left == NULL) return NULL;
    if (consume(parser, COLON)) {
        left->p_type = parse_type(parser);
    }
    Token* eq = READ_TOKEN(parser);
    if (!consume(parser, EQ)) {
        return left;
    }
    ASTNode* right = parse_comparison(parser);
    if (right == NULL) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            eq->pos.col + 1, 
            eq->pos, 
            eq->value.size, 
            "expected an expression after '='"}),
            Error
        );
        free_ast_node(left);
        return NULL;
    }
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->as_bin_expr.op = eq;
    node->as_bin_expr.left = left;
    node->as_bin_expr.right = right;
    node->type = BIN_EXPR;

    node->start_tok = node->as_bin_expr.left->start_tok;
    node->end_tok = node->as_bin_expr.right->end_tok;
    return node;
}



ASTNode* parse_expr(Parser* parser) {
    ASTNode* node = parse_assignment(parser);
    return node;
}

ASTNode* parse_grouped_expr(Parser* parser) {
    if (!consume(parser, LPAREN)) return NULL;
    ASTNode* expr = parse_expr(parser);
    if (!consume(parser, RPAREN)) {
        free_ast_node(expr);
        return NULL;
    }
    return expr;
}

ASTNode* parse_break_expr(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    consume(parser, BREAK);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->type = BREAK_EXPR;
    node->as_break_expr = parse_expr(parser);
    node->start_tok = tok;
    if (node->as_break_expr == NULL) {
        node->end_tok = tok;
    } else {
        node->end_tok = node->as_break_expr->end_tok;
    }

    return node;
}



ASTNode* parse_statement(Parser* parser) {
    if (READ_TOKEN(parser)->type == IDENTIFIER && PEEK_NEXT(parser)->type == DOUBLE_COLON) {
        return parse_func_decl(parser);
    }
   
    ASTNode* node = parse_expr(parser);
    return node;
}


ASTNode* parse_compound(Parser* parser) {
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    INIT_VECTOR(node->as_compound_statements, ASTNode);
    node->type = COMPOUND_STMT;
    while(READ_TOKEN(parser)->type != TOK_EOF) {
        APPEND(node->as_compound_statements, parse_statement(parser), ASTNode*);
    }

    return node;
}
// this might not be needed
ASTNode* parse_program(Parser* parser) {
    ASTNode* node = parse_compound(parser);
    node->type = PROGRAM;
    return node;
}


