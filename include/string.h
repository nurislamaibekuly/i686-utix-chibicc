#pragma once

#include <stddef.h>

// String functions
int strcmp(const char* a, const char* b);
void strcpy(char* dst, const char* src);
size_t strlen(const char* str);
int strncmp(const char* a, const char* b, size_t n);
char* strncpy(char* dest, const char* src, size_t n);

// Memory functions
void* memcpy(void* dest, const void* src, size_t n);
int memcmp(const void* a, const void* b, size_t n);
void* memset(void* ptr, int value, size_t num);
