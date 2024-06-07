#ifndef MEMORY_H
#define MEMORY_H

#include <stdio.h>


#define SCALE_FACTOR 2

#define CHECK_FAILED_ALLOCATION(ptr) ({\
		if (!ptr) {\
		    printf("Failure in memory allocation"); \
			exit(EXIT_FAILURE); \
		} \
})

#define ALLOCATE(ptr, type, capacity) ({\
    ptr = malloc(sizeof(type) * capacity); \
    CHECK_FAILED_ALLOCATION(ptr); \
})

#define REALLOCATE(ptr, prev_size, type) ({\
	type* new_ptr = realloc(ptr, SCALE_FACTOR * sizeof(type) * prev_size); \
    if (!new_ptr) { \
		    printf("Realloc failed\n"); \
			free(ptr); \
			exit(EXIT_FAILURE); \
		} \
	ptr = new_ptr; \
})

#define FREE_VECTOR(vec) ({ \
    free(vec->arr); \
    free(vec); \
})

void* reverse_memcpy(void* dest, void* src, size_t len);

#endif 
