#ifndef VECTOR_H
#define VECTOR_H

#include "memory.h"
#include <stdlib.h>

#define INIT_VECTOR_CAP 16

#define CAST_VECTOR(vector, type) ((type*) vector.arr)
#define INDEX_VECTOR(vector, type, i) (CAST_VECTOR(vector, type)[i])

#define APPEND(vector, elem, type) ({ \
    if (vector.size >= vector.capacity) { \
        REALLOCATE(vector.arr, vector.capacity, type); \
        vector.capacity *= 2; \
    } \
    ((type*) vector.arr)[vector.size++] = elem; \
})

#define INIT_VECTOR(vector, type) ({ \
    vector->capacity = INIT_VECTOR_CAP; \
    vector->size = 0; \
    ALLOCATE(vector->arr, type, INIT_VECTOR_CAP); \
})

typedef struct Vector{
    int capacity;
    int size;
    void* arr;
}Vector;

void init_vector(Vector* vector);

#endif
