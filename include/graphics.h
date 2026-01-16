#pragma once
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>

static uint8_t dummy = 0;
static uint32_t fb_width = 0;
static uint32_t fb_height = 0;

static void graphicsclear() {
    syscallsend(4, (const char*)&dummy, 0);
}

static void graphicsflush() {
    syscallsend(7, (const char*)&dummy, 0);
}

static void graphicsdraw(uint32_t x, uint32_t y, uint32_t color) {
    uint8_t buf[12];
    buf[0] = (x >> 0) & 0xFF;
    buf[1] = (x >> 8) & 0xFF;
    buf[2] = (x >> 16) & 0xFF;
    buf[3] = (x >> 24) & 0xFF;

    buf[4] = (y >> 0) & 0xFF;
    buf[5] = (y >> 8) & 0xFF;
    buf[6] = (y >> 16) & 0xFF;
    buf[7] = (y >> 24) & 0xFF;

    buf[8]  = (color >> 0) & 0xFF;
    buf[9]  = (color >> 8) & 0xFF;
    buf[10] = (color >> 16) & 0xFF;
    buf[11] = (color >> 24) & 0xFF;

    syscallsend(3, (const char*)buf, 12);
}

static uint32_t graphicswidth() {
	if (!fb_width) {
		syscallsend(5, (const char*)&dummy, 0);
		syscallread((uint8_t*)&fb_width, sizeof(fb_width));
	}

	return fb_width;
}

static uint32_t graphicsheight() {
	if (!fb_height) {
		syscallsend(6, (const char*)&dummy, 0);
		syscallread((uint8_t*)&fb_height, sizeof(fb_height));
	}

	return fb_height;
}
