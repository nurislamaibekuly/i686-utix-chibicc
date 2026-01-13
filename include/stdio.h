#pragma once
#include <stdint.h>
#include <stddef.h>

void syscallsend(uint8_t type, const char* msg, size_t len);
size_t syscallread(uint8_t* out, size_t len);
void printf(const char* fmt, ...);
