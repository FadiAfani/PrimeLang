#ifndef VECTOR_H
#define VECTOR_H

#include "memory.h"
#include <stdlib.h>
#include <string.h>

#define INIT_VECTOR_CAP 4

#define CAST_VECTOR(vector, type) ((type*) vector.arr)
#define INDEX_VECTOR(vector, type, i) (CAST_VECTOR(vector, type)[i])



#define APPEND(vector, elem, type) ({ \
    if (vector.size >= vector.capacity) { \
        REALLOCATE(vector.arr, vector.capacity, type); \
        vector.capacity *= 2; \
    } \
    ((type*) vector.arr)[vector.size++] = elem; \
})

#define INSERT_AT(vector, elem, type, idx) ({ \
    if (vector.size >= vector.capacity) { \
        REALLOCATE(vector.arr, vector.capacity, type); \
        vector.capacity *= 2; \
    } \
    if (idx < vector.capacity) { \
        ((type*) vector.arr)[idx] = elem; \
        vector.size++; \
    } \
})

#define REALLOC_VECTOR(vector, type) ({ \
    REALLOCATE(vector.arr, vector.capacity, type); \
    vector.capacity *= SCALE_FACTOR; \
})

#define INIT_VECTOR(vector, type) ({ \
    vector.capacity = INIT_VECTOR_CAP; \
    vector.size = 0; \
    ALLOCATE(vector.arr, type, INIT_VECTOR_CAP); \
})

#define MEMCPY_VECTOR(dest_vec, src, n_bytes, type) ({ \
    if (dest_vec.capacity - dest_vec.size <= n_bytes) { \
        REALLOC_VECTOR(dest_vec, type); \
    } \
    type* arr = (unsigned char*) dest_vec.arr; \
    memcpy(arr + dest_vec.size, src, n_bytes); \
    dest_vec.size += n_bytes; \
})



#define MEMCPY_VECTOR(dest_vec, src, n_bytes, type) ({ \
    if (dest_vec.capacity - dest_vec.size <= n_bytes) { \
        REALLOC_VECTOR(dest_vec, type); \
    } \
    type* arr = (unsigned char*) dest_vec.arr; \
    memcpy(arr + dest_vec.size, src, n_bytes); \
    dest_vec.size += n_bytes; \
})

#define ALLOC_VECTOR(ptr) (ALLOCATE(ptr, Vector, 1))

#define REALLOC_VECTOR_INIT(vec, type) ({ \
    size_t prev_size = vec.size; \
    REALOC_VECTOR(vec, type); \
    memset(vec.arr + prev_size, 0, vec.capacity - prev_size); \
})


#define MEMCPY_VECTOR_INIT(vec, src, n_bytes, type) ({ \
    if (vec.capacity - vec.size <= n_bytes) { \
        size_t prev_size = vec.size; \
        REALLOC_VECTOR(vec, type); \
        memset(vec.arr + prev_size, 0, vec.capacity - prev_size); \
    } \
    type* arr = (unsigned char*) vec.arr; \
    memcpy(arr + vec.size, src, n_bytes); \
    vec.size += n_bytes; \
})

typedef struct Vector{
    size_t capacity;
    size_t size;
    void* arr;
}Vector;


void init_vector(Vector* vector);
void free_vector(Vector* vector);

#endif
