#include "vector.h"
#include "memory.h"

void free_vector(Vector* vec) {
    free(vec->arr);
    free(vec);
}




