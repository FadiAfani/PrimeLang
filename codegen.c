#include "codegen.h"
#include "vector.h"
#include  <stdlib.h>
#include <string.h>

void compile_literal(ASTNode* node, Compiler* compiler, SymbolTable* syms) {
    Const* c;
    ALLOC_CONST(c);
    switch(node->as_literal_expr->type) {
        case INT_LIT:
        {
            int num = (int) atof(node->as_literal_expr->value.arr);
            APPEND((*compiler->code), OP_CONST, uint8_t);
            if (compiler->data->capacity - compiler->data->size < sizeof(int)) {
                REALLOC_VECTOR((*compiler->data), uint8_t);
            }
            uint8_t* data = compiler->data->arr;
            memcpy(data + compiler->data->size, &num, sizeof(int));
            compiler->data->size += sizeof(int);

            c->type = INT_CONST;
            c->as_int_const = num;
            insert_const(compiler->consts, c);
            uint8_t msbs = 0xFF00 & c->const_index;
            uint8_t lsbs = 0XFF & c->const_index;
            APPEND((*compiler->code), msbs, uint8_t);
            APPEND((*compiler->code), lsbs, uint8_t);
            break;
        }
        case DOUBLE_LIT:
        {
            double num = atof(node->as_literal_expr->value.arr);
            APPEND((*compiler->code), OP_CONST, uint8_t);
            if (compiler->data->capacity - compiler->data->size < sizeof(double)) {
                REALLOC_VECTOR((*compiler->data), uint8_t);
            }

            uint8_t* data = compiler->data->arr;
            memcpy(data + compiler->data->size, &num, sizeof(double));
            compiler->data->size += sizeof(double);

            c->type = DOUBLE_CONST;
            c->as_int_const = num;
            insert_const(compiler->consts, c);
            uint8_t msbs = 0xFF00 & c->const_index;
            uint8_t lsbs = 0XFF & c->const_index;
            APPEND((*compiler->code), msbs, uint8_t);
            APPEND((*compiler->code), lsbs, uint8_t);
            break;
        }
        case STRING_LIT:
        {
            char* str;
            APPEND((*compiler->code), OP_CONST, uint8_t);
            uint8_t* data = compiler->data->arr;
            size_t len = strlen((char*) compiler->data->arr);
            if (compiler->data->capacity - compiler->data->size < len) {
                REALLOC_VECTOR((*compiler->data), uint8_t);
            }
            memcpy(compiler->data->arr + compiler->data->size, str, len);
            compiler->code->size += len;

            c->type = STRING_CONST;
            c->as_string_const = str;
            insert_const(compiler->consts, c);
            uint8_t msbs = 0xFF00 & c->const_index;
            uint8_t lsbs = 0XFF & c->const_index;
            APPEND((*compiler->code), msbs, uint8_t);
            APPEND((*compiler->code), lsbs, uint8_t);
            break;
        }
        case IDENTIFIER:
        {
            Symbol* sym = lookup(syms, (char*) node->as_id_literal.id_token->value.arr);
            if (sym->outer_index > 0) {
                APPEND((*compiler->code), OP_LOAD_OUTER, uint8_t);
                uint8_t msbs = 0xFF00 & sym->outer_index;
                uint8_t lsbs = 0XFF & sym->outer_index;
                APPEND((*compiler->code), msbs, uint8_t);
                APPEND((*compiler->code), lsbs, uint8_t);
            } else {
                APPEND((*compiler->code), OP_LOADL, uint8_t);
                uint8_t msbs = 0xFF00 & sym->local_index;
                uint8_t lsbs = 0XFF & sym->local_index;
                APPEND((*compiler->code), msbs, uint8_t);
                APPEND((*compiler->code), lsbs, uint8_t);
            }
            break;

        }
        default: break;
    }
}

void compile_func_call(ASTNode* node, Compiler* compiler, SymbolTable* syms) {
    if (strcmp((char*) node->as_func_call.func_id->value.arr, "print") == 0) {
        APPEND((*compiler->code), OP_CALL_NATIVE, uint8_t);
        APPEND((*compiler->code), 0, uint8_t); /* native function index */
        return;
    }
    Symbol* sym = lookup(syms, (char*) node->as_func_call.func_id->value.arr);
    for (size_t i = 0; i < node->as_func_call.params.size; i++) {
        compile_expr(INDEX_VECTOR(node->as_func_call.params, ASTNode*, i), compiler, syms);
    }
    if (sym->outer_index > 0) {
        APPEND((*compiler->code), OP_LOAD_OUTER, uint8_t);
        uint8_t msbs = 0xFF00 & sym->outer_index;
        uint8_t lsbs = 0XFF & sym->outer_index;
        APPEND((*compiler->code), msbs, uint8_t);
        APPEND((*compiler->code), lsbs, uint8_t);
    } else {
        APPEND((*compiler->code), OP_LOADL, uint8_t);
        uint8_t msbs = 0xFF00 & sym->local_index;
        uint8_t lsbs = 0XFF & sym->local_index;
        APPEND((*compiler->code), msbs, uint8_t);
        APPEND((*compiler->code), lsbs, uint8_t);
    }
    APPEND((*compiler->code), OP_CALL, uint8_t);
}

void compile_block_expr(ASTNode* node, Compiler* compiler, SymbolTable* syms) {
    for (size_t i = 0; i < node->as_block_expr.statements.size; i++) {
        compile_statement(
                INDEX_VECTOR(node->as_block_expr.statements, ASTNode*, i),
                compiler,
                &node->as_block_expr.table
                );
    }
}

void compile_binary_expr(ASTNode* node, Compiler* compiler, SymbolTable* syms) {
    switch(node->as_bin_expr.op->type) {
        case MULT:
            APPEND((*compiler->code), OP_MULT, uint8_t);
            break;
        case PLUS:
            APPEND((*compiler->code), OP_ADD, uint8_t);
            break;
        case MINUS:
            APPEND((*compiler->code), OP_SUB, uint8_t);
            break;
        case DIV:
            APPEND((*compiler->code), OP_DIV, uint8_t);
            break;
        default: return;

    }
    compile_expr(node->as_bin_expr.left, compiler, syms);
    compile_expr(node->as_bin_expr.right, compiler, syms);
}

//TODO: implement
void compile_expr(ASTNode* node, Compiler* compiler, SymbolTable* syms) {
    switch(node->type) {
        case LITERAL_EXPR: 
            compile_literal(node, compiler, syms);
            break;
        case BIN_EXPR: 
            compile_binary_expr(node, compiler, syms);
            break;
        case FUNC_CALL_EXPR: 
            compile_func_call(node, compiler, syms);
            break;
        case BLOCK_EXPR: 
            compile_block_expr(node, compiler, syms);
            break;
        default:
            printf("compiler->code generation is not yet implemented for this node type\n");
            break;
    }
}

void compile_function(ASTNode* node, Compiler* compiler, SymbolTable* syms) {
    return;
}

void compile_statement(ASTNode* node, Compiler* compiler, SymbolTable* syms) {
    return;
}

void write_compiler_data(char* filename, Compiler* compiler) {
    FILE* out = fopen(filename, "wb");
    if (out == NULL) return;
    uint8_t* data = compiler->data->arr;
    uint8_t* code = compiler->code->arr; 
    fwrite(data, sizeof(uint8_t), sizeof(data), out);
    fwrite(code, sizeof(uint8_t), sizeof(code), out);

}

void init_compiler(Compiler* compiler) {
    ALLOCATE(compiler->data, Vector, 1);
    ALLOCATE(compiler->code, Vector, 1);
    ALLOCATE(compiler->consts, ConstTable, 1);
    INIT_VECTOR((*compiler->data), uint8_t);
    INIT_VECTOR((*compiler->code), uint8_t);
    INIT_VECTOR((*compiler->consts), uint8_t);
    
}
