#include "memory.h"

#include <stdlib.h>
#include <string.h>

void* reverse_memcpy(void* dest, void* src, size_t len) {
    uint8_t* src_as_bytes = (uint8_t*) src;
    uint8_t* dest_as_bytes = (uint8_t*) dest;
    for (int i = 0; i < len; i++) {
        dest_as_bytes[len - 1 - i] = src_as_bytes[i];
    }

    return dest_as_bytes;
}
