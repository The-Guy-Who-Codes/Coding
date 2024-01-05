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
SDL_Texture *texture = NULL;

uint32_t seed = 0x01a35f4;

typedef struct Pixel {
    uint16_t x;
    uint16_t y;
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t a;

} Pixel;

void RenderPixel(Pixel *pixel) {
    //SDL_SetRenderDrawColor(renderer, random_uint8(&seed), random_uint8(&seed), random_uint8 (&seed), 255);
    //SDL_RenderDrawPoint(renderer, pixel->x, pixel->y);
    SDL_SetRenderDrawColor(renderer, 255, 0, 0, 255);
    if ((exponent(pixel->x - 320, 2) + exponent(pixel->y - 240, 2)) <= 2500) {
        SDL_RenderDrawPoint(renderer, pixel->x, pixel->y);
    }
}


int main(int argc, char **argv) {

    uint64_t PixelCount = SCREEN_WIDTH * SCREEN_HEIGHT;
    Pixel *screen = malloc(sizeof(Pixel) * PixelCount);

    for (int i = 0; i < PixelCount; i++) {
        screen[i].x = (uint16_t)(i % SCREEN_WIDTH);
        screen[i].y = (uint16_t)(i / SCREEN_HEIGHT);
    }

    if (!init(&window, &renderer, &texture, SCREEN_WIDTH, SCREEN_HEIGHT)) {
        printf("Failed to initialise\n");
    }

   

    SDL_Event e;
    SDL_PollEvent(&e);

    while (e.type != SDL_QUIT) {
        SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
        SDL_RenderClear( renderer );

        /*for (int x = 0; x < SCREEN_WIDTH; x++) {
            for (int y = 0; y < SCREEN_HEIGHT; y++) {
                SDL_SetRenderDrawColor(renderer, random_uint8(&seed), random_uint8(&seed), random_uint8 (&seed), 255);
                SDL_RenderDrawPoint(renderer, x, y);
            }
        }*/
        for (int i = 0; i < PixelCount; i++) {
            RenderPixel(&screen[i]);
        }


        //Update screen
        SDL_RenderPresent( renderer );

        SDL_PollEvent(&e);
    }

    ex(window, renderer, texture);
    free(screen);

    return 0;
}