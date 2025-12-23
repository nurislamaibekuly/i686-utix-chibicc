#pragma once
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <ram.h>
#include <file.h>
#define HEAP_START 0x100000

void* malloc(uint32_t size);
void free(void* ptr);
void printf(const char* fmt, ...);
void* aligned_malloc(size_t alignment, size_t size);
FILE* fopen(const char* name, const char *mode);
