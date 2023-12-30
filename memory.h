#ifndef MEMORY_H
#define MEMORY_H

#include <stdio.h>

#include "vm.h"

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

#define BUFF_DYN_ARR(ptr, prev_size, type) ({\
	type* new_ptr = realloc(ptr, 2  * sizeof(type) * prev_size); \
    if (!new_ptr) { \
		    printf("Realloc failed\n"); \
			free(ptr); \
			exit(EXIT_FAILURE); \
		} \
	ptr = new_ptr; \
})

#define INIT_DYN_ARR(ptr, type, capacity) (BUFF_DYN_ARR(ptr, capacity/2, type))

#endif 
