#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <time.h>
#include <string.h>

#include "vm.h"
#include "parser.h"
#include "vector.h"
#include "codegen.h"
#include "error.h"
#include "prime_file_parser.h"
#include "semantics.h"
#include <assert.h>

int main(int argc, char* argv[]) {
	Parser* parser;
    ALLOCATE(parser, Parser, 1);
    init_parser(parser);
    load_file_into_memory(&(parser->lexer), "ctest.txt");
    tokenize(&(parser->lexer));
    ASTNode* root = parse_program(parser);
    //print_lexer(&parser->lexer);
	assert(root != NULL);
    infer_program(&parser->global_table, &parser->parsing_errors, root);
    for (size_t i = 0; i < parser->parsing_errors.size; i ++) {
        print_error(INDEX_VECTOR(parser->parsing_errors, Error, i), parser->lexer.filename, parser->lexer.src);
    }
    //print_node(root, 0);
    Compiler* compiler;
    ALLOCATE(compiler, Compiler, 1);
    init_compiler(compiler);
    compile_program(root, compiler, &parser->global_table);
    write_compiler_data("out.bin", compiler, &parser->global_table);

	VM* vm = init_VM();
    vm->mem = parse_prime_file("out.bin");
	uint16_t consts_size = sizeof(vm->mem) / sizeof(Value);
	uint16_t main_entry = consts_size - 1;
    uint8_t insts[5] = {OP_CONST, 0, 0, OP_CALL, OP_HALT};
    vm->code = insts;
    run(vm);

	return 0;

}
