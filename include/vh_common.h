#ifndef _VH_COMMON_H
#define _VH_COMMON_H

#include <stdint.h>
#include <stdbool.h>
/* value-hash_table common header */

typedef enum ValueType {
	STRING,
	NUMBER,
    OBJECT,
    RET_ADDR,
    BOOL,
    REFERENCE,
    EMPTY
}ValueType;


typedef union {	
    double as_double;
    int as_int;
    void* as_ref;
    bool as_bool;
}Value;


typedef struct {
    int key;
    Value val;
}HashTableEntry;

typedef struct {
    uint16_t capacity;
    HashTableEntry* table;
}HashTable;

#endif 
