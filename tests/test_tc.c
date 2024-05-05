#include "../include/semantics.h"
#include "../include/token.h"
#include "../include/parser.h"
#include "criterion/criterion.h"

TypeChecker tc;
Parser parser;
SymbolTable globals;

void setup(void) {
    init_type_checker(&tc);
    init_parser(&parser);
    init_symbol_table(&globals);
    push_scope(&parser.scopes, &globals);
    push_scope(&tc.scopes, &globals);

}

void teardown(void) {
    return;
}


TestSuite(typechecker_tests, .init=setup, .fini=teardown);

Test(typechecker_tests, test_simple_literal_expr) {
    parser.lexer.src = "3 * 10";
    tokenize(&parser.lexer);
    ASTNode* root = parse_program(&parser);
    ASTNode* bin_expr = INDEX_VECTOR(root->as_compound_statements, ASTNode*, 0);
    cr_assert(parser.parsing_errors.size == 0, "parsing failures occured");
    cr_assert(root != NULL, "root is null");
    cr_assert(bin_expr->type == BIN_EXPR, "node type should be BIN_EXPR");
    infer_program(&tc, root);
    cr_assert(tc.semantic_errors.size == 0, "semantic failures");
    cr_assert(bin_expr->p_type != NULL, "type is null");
    cr_assert(bin_expr->p_type->type_kind == INT_TYPE, "resulting type should be an int");
}


Test(typechecker_tests, test_var_literal) {
    parser.lexer.src = "x = 3";
    tokenize(&parser.lexer);
    ASTNode* root = parse_program(&parser);
    ASTNode* bin_expr = INDEX_VECTOR(root->as_compound_statements, ASTNode*, 0);
    cr_assert(root != NULL, "root is null");
    cr_assert(bin_expr != NULL, "assignment node is null");
    cr_assert(bin_expr->type == BIN_EXPR, "node type should be BIN_EXPR");
    infer_program(&tc, root);
    cr_assert(bin_expr->p_type != NULL, "assignment node is has a null type");
    cr_assert(bin_expr->p_type->as_built_in_type == UNIT_TYPE, "assginment type should be UNIT");


    // variable analysis
    ASTNode* var = bin_expr->as_bin_expr.left;
    Symbol* sym = lookup(&globals.ht, var->as_literal_expr->value.arr, var->as_literal_expr->value.size);
    cr_assert(sym != NULL, "symbol was not found in the symbol table");
    cr_assert(var->p_type != NULL && var->p_type->type_kind == INT_TYPE, "expected int type for 'x'");

}

Test(typechecker_tests, test_block_type) {
    parser.lexer.src = "{x = 3 \n 1 * 7}";
    tokenize(&parser.lexer);
    ASTNode* root = parse_program(&parser);
    ASTNode* block = INDEX_VECTOR(root->as_compound_statements, ASTNode*, 0);
    cr_assert(parser.parsing_errors.size == 0, "parsing failures occured");
    cr_assert(root != NULL, "root is null");
    cr_assert(block->type == BLOCK_EXPR, "node type should be BIN_EXPR");
    infer_program(&tc, root);
    cr_assert(tc.semantic_errors.size == 0, "semantic failures");
    cr_assert(block->p_type != NULL, "type is null");
    cr_assert(block->p_type->type_kind == INT_TYPE, "resulting type should be an int");
}


Test(typechecker_tests, test_function_currying) {
    parser.lexer.src = "f :: (x: int) -> (y: int) -> int {x * y} \n z = f (3)";
    tokenize(&parser.lexer);
    ASTNode* root = parse_program(&parser);
    ASTNode* bin_expr = INDEX_VECTOR(root->as_compound_statements, ASTNode*, 1);
    ASTNode* var = bin_expr->as_bin_expr.left;
    cr_assert(parser.parsing_errors.size == 0, "parsing failures occured");
    cr_assert(root != NULL, "root is null");
    infer_program(&tc, root);
    cr_assert(tc.semantic_errors.size == 0, "semantic failures");
    cr_assert(var->p_type != NULL, "type is null");
    cr_assert(var->p_type->type_kind == FUNC_KIND, "'x' should be a FUNC_KIND");
    Symbol* sym = lookup(&globals.ht, var->as_literal_expr->value.arr, var->as_literal_expr->value.size);
    cr_assert(sym != NULL, "'x' symbol was not registered correctly");
    cr_assert(sym->as_func_symbol.pc == 1, "incorrect parameter count");

}

Test(typechecker_tests, test_basic_int_if_type) {
    parser.lexer.src = "if 2 > 1 then 5 * 6";
    tokenize(&parser.lexer);
    ASTNode* root = parse_program(&parser);
    ASTNode* if_expr = INDEX_VECTOR(root->as_compound_statements, ASTNode*, 0);
    infer_program(&tc, root);
    cr_assert(if_expr->as_if_expr.cond->p_type->as_built_in_type == INT_TYPE);
    cr_assert(if_expr->p_type->as_built_in_type == INT_TYPE);

}

Test(typechecker_tests, test_if_type_with_else) {
    parser.lexer.src = "if 2 > 1 then 5 * 6 else 2 * 10";
    tokenize(&parser.lexer);
    ASTNode* root = parse_program(&parser);
    ASTNode* if_expr = INDEX_VECTOR(root->as_compound_statements, ASTNode*, 0);
    infer_program(&tc, root);
    cr_assert(if_expr->as_if_expr.cond->p_type->as_built_in_type == INT_TYPE);
    cr_assert(if_expr->p_type->as_built_in_type == INT_TYPE);
}

Test(type_checker_tests, test_int_double_mult) {
    parser.lexer.src = "1.5 * 4";
    tokenize(&parser.lexer);
    ASTNode* root = parse_program(&parser);
    ASTNode* bin_expr = INDEX_VECTOR(root->as_compound_statements, ASTNode*, 0);
    cr_assert(parser.parsing_errors.size == 0, "parsing failures occured");
    cr_assert(root != NULL, "root is null");
    cr_assert(bin_expr->type == BIN_EXPR, "node type should be BIN_EXPR");
    infer_program(&tc, root);
    cr_assert(tc.semantic_errors.size == 0, "semantic failures");
    cr_assert(bin_expr->p_type != NULL, "type is null");
    cr_assert(bin_expr->p_type->type_kind == INT_TYPE, "resulting type should be a double");
}
