#include "memory.h"

#include <stdlib.h>

void free_chunk(Chunk* chunk) {
	free(chunk->bytecode);
	chunk->size = 0;
	chunk->capacity = 0;
}

void free_pool(ConstPool* pool) {
	free(pool->values);
	pool->size = 0;
	pool->capacity = 0;
}

void free_VM(VM* vm) {
	free_pool(&vm->pool);
	vm->ip = 0;
	// Reset stack
}
