#include "type.h"

void print_type(PrimeType* type) {
    if (type == NULL) return;

    switch(type->type_kind) {
        case BUILT_IN:
            switch(type->as_built_in_type) {
                case INT_TYPE:
                    printf("type: INT_TYPE\n");
                    break;
                case DOUBLE_TYPE: 
                    printf("type: DOUBLE_TYPE\n");
                    break;
                case CHAR_TYPE:
                    printf("type: CHAR_TYPE\n");
                    break;
                case BOOL_TYPE:
                    printf("type: BOOL_TYPE\n");
                    break;
                case BYTE_TYPE:
                    printf("type: BYTE_TYPE\n");
                    break;
                case STRING_TYPE:
                    printf("type: STRING_TYPE\n");
                    break;
                case UNIT_TYPE:
                    printf("type: UNIT_TYPE\n");
                    break;
                default:
                    printf("not a type\n");
                    break;
            }
            break;
        case USER_DEFINED:
            printf("type: %s\n", type->as_user_defined_type);
            break;
        default:
            printf("not a type\n");
            break;
    }
}
