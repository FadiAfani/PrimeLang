#ifndef TYPE_H
#define TYPE_H

typedef enum BuiltInType {
    INT_TYPE,
    DOUBLE_TYPE,
    CHAR_TYPE,
    BOOL_TYPE,
    BYTE_TYPE,
    STRING_TYPE,
    UNIT_TYPE,

}BuiltInType;

typedef enum PrimeTypeKind {
    BUILT_IN, 
    USER_DEFINED,
}PrimeTypeKind;

typedef struct PrimeType {
    union {
        BuiltInType as_built_in_type;
        char* as_user_defined_type;
    };
    PrimeTypeKind type_kind;
}PrimeType;

#endif