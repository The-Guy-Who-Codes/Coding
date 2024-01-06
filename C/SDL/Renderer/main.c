#pragma once
#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "Random.h"
#include "General.h"
#include "maths.h"


const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;


SDL_Window* window = NULL;
SDL_Renderer* renderer = NULL;
SDL_Texture* texture = NULL;

uint32_t seed = 0x01a35f4;

typedef struct Pixel {
    float x;
    float y;
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t a;

} Pixel;

void RenderPixels(Pixel* pixels, int count) {

    //SDL_SetRenderDrawColor(renderer, random_uint8(&seed), random_uint8(&seed), random_uint8 (&seed), 255);
    //SDL_RenderDrawPoint(renderer, pixel->x, pixel->y);

    for (int i = 0; i < count; i++) {

        if ((pixels[i].x) * (pixels[i].x) + (pixels[i].y) * (pixels[i].y) <= 0.25f) {
            pixels[i].a = 255;
            pixels[i].r = 255;
            pixels[i].g = 0;
            pixels[i].b = 0;
        } else {
            pixels[i].a = 255;
            pixels[i].r = 0;
            pixels[i].g = 0;
            pixels[i].b = 0;

        }
    }
}

void CreateBuffer(uint32_t* screen, Pixel* pixels, int count) {
    // using argb format
    for (int i = 0; i < count; i++) {
        // create 32 bit argb pixel colour for screen
        screen[i] = (((((pixels[i].a << 8) + pixels[i].r) << 8) + pixels[i].g) << 8) + pixels[i].b;

    }
    
}


int main(int argc, char **argv) {


    // create pixel array and screen buffer
    uint64_t PixelCount = SCREEN_WIDTH * SCREEN_HEIGHT;
    int pitch = 4 * SCREEN_WIDTH; // 4 is the number of bytes used for colour information per pixel
    
    Pixel *pixels = NULL;
    uint32_t* screen = NULL;

    pixels = malloc(sizeof(Pixel) * PixelCount);
    screen = malloc(sizeof(uint32_t) * PixelCount);
    
    // give the pixel coordinates and sets the y axis from -1 to 1 and x axis is created to preserve aspect ratio
    float max = (float) SCREEN_WIDTH / (float) SCREEN_HEIGHT;
    for (int i = 0; i < PixelCount; i++) {
        pixels[i].x = (float)(i % SCREEN_WIDTH) / (float) SCREEN_HEIGHT * 2.0f - max;
        pixels[i].y = -((float)(i / SCREEN_WIDTH) / (float) SCREEN_HEIGHT * 2.0f - 1);
    }

    if (!init(&window, &renderer, &texture, SCREEN_WIDTH, SCREEN_HEIGHT)) {
        printf("Failed to initialise\n");
    }

   

    SDL_Event e;
    SDL_PollEvent(&e);

    while (e.type != SDL_QUIT) {
        // clear the screen to black
        //SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
        //SDL_RenderClear( renderer );

        RenderPixels(pixels, PixelCount);

        CreateBuffer(screen, pixels, PixelCount);

        // draw the pixel buffer

        SDL_LockTexture(texture, NULL, &screen, &pitch);

        SDL_UnlockTexture(texture);

        SDL_RenderCopy(renderer, texture, NULL, NULL);

        // Update screen
        SDL_RenderPresent( renderer );

        SDL_PollEvent(&e);
    }

    ex(window, renderer, texture);
    free(screen);
    free(pixels);

    screen = NULL;
    pixels = NULL;

    return 0;
}