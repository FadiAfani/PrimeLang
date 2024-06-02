#include "../include/vm.h"

#define READ_F_16(fd) (fgetc(fd) | fgetc(fd) << 8)
void disassemble(VM* vm);
