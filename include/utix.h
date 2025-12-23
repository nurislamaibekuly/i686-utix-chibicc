#pragma once
#include <stdint.h>

static uint32_t totalmem() {
    return *((uint32_t*)0x8000);
}

int mkdir(const char* path);
