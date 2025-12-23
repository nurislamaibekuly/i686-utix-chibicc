#include <file.h>
    #pragma once
    #include <stdint.h>

    #define MAX_RAM_FILES    512
    #define RAMFS_DATA_BASE ((uint8_t*)0x200000)

    typedef enum { RAM_FILE, RAM_DIR } ram_type;

    typedef struct {
        char name[MAX_FILE_NAME];
        uint8_t data[MAX_FILE_SIZE];
        uint32_t size;
        uint32_t offset;
        ram_type type;
        uint32_t parent;
    } ram_file;

    typedef struct {
        ram_file files[MAX_RAM_FILES];
        uint32_t count;
    } ram_fs;

    typedef struct {
        void* data;       // pointer to the ram_file
        uint32_t pos;     // current read position
    } file_descriptor;

    extern ram_fs ramfs;

    int ram_read(FILE* f, uint8_t* buf, uint32_t count);
    int ram_write(FILE* f, const uint8_t* buf, uint32_t count);
    int ram_close(FILE* f);
    int ram_seek(FILE* f, uint32_t pos);
    ram_file* ramfsopen(const char* path);
    FILE* ram_fopen(const char* name, int mode);
    void ramfscreate(const char* name, uint8_t* data, uint32_t size);
    void ramfs_mkdir(const char* path, int parent);
    uint8_t* ramfs_get_data_ptr(const char* path);
