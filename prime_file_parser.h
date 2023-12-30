#include <stdio.h>
#include "value.h"

Value parse_file_into_main_closure_obj(const char* file_path);
Value parse_val(FILE* file, ClosureObj* parent);
