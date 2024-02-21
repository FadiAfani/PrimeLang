#include "memory.h"

#include <stdlib.h>


inline void free_vector(Vector* vec) {
    free(vec->arr);
    free(vector);
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
