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

typedef struct Value {
	union {	
		double number;
        int ret_addr;
        void* ref;
        bool as_bool;
	};
	ValueType type;
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
