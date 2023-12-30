#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <time.h>

#include "lexer.h"
#include "vm.h"

int main(int argc, char* argv[]) {
	char* filename = argv[1];
	FILE* file = fopen(filename, "rb");
	size_t len;
	char* buffer;

	if (file) {
		fseek(file, 0, SEEK_END);
		len = ftell(file);
		fseek(file, 0, SEEK_SET);
		buffer = malloc(len * sizeof(char));
		if (buffer) {
			fread(buffer, 1, len, file);
		} else {
			printf("malloc failed");
			exit(EXIT_FAILURE);
		}
		fclose(file);
	} else {
		printf("Failed to open file");
		exit(EXIT_FAILURE);
	}

	// test VM
	//VM vm;
	//init_VM(&vm);
	//free(vm.pool.values);
	//Value vals[] = {{.number = 3}, {.number = 1}, {.number = 9}};
	//ConstPool p = {vals, 5, 2};
	//vm.pool = p;
	//uint8_t bytes[] = {OP_CONST, 1, 0, OP_CONST, 1, 2, OP_ADD, OP_RETURN};
	//Chunk chunk = {bytes, 7, 7};
	//disas(&chunk, vm);
	//run(&vm, &chunk);

	
	Lexer lexer = {0,0,NULL};
	init_lexer(&lexer);
	clock_t t_start,t_end;
	t_start = clock();
	tokenize(&lexer, buffer);
	t_end = clock();
	double time_elapsed = ((double)(t_end - t_start))/CLOCKS_PER_SEC;
	print_lexer(&lexer);
	printf("\nelapsed time: %f\n", time_elapsed);
	//print_lexer(&lexer);
	return 0;

}
