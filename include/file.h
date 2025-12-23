#pragma once
#include <stdint.h>
#include <stddef.h>
#include <string.h>

#define MAX_FILE_NAME 64
#define MAX_OPEN_FILES   16
#define MAX_FILE_SIZE    65536

typedef struct FILE {
    void* fs_data;  // backend-specific pointer (ram_file*, sata_file*, etc.)

    // Backend-specific operations
    int  (*read)(struct FILE* f, uint8_t* buf, uint32_t count);
    int  (*write)(struct FILE* f, const uint8_t* buf, uint32_t count);
    int  (*close)(struct FILE* f);
    int  (*seek)(struct FILE* f, uint32_t pos);

    uint32_t pos;   // current file position
    int mode;       // 0 = read, 1 = write
    char name[64];  // optional file name
} FILE;
