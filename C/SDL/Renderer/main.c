#pragma once
#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <omp.h>
#include <float.h>
#include "Random.h"
#include "General.h"
#include "vectors.h"

// map argb where values range from 0 to 1 with return of 32 bit argb value
#define ConvertToARGB_Clamp(a, r, g, b) (((uint8_t) (clamp(a, 0, 1) * 255.0f)) << 24 | ((uint8_t) (clamp(r, 0, 1) * 255.0f)) << 16 | ((uint8_t) (clamp(g, 0, 1) * 255.0f)) << 8 | ((uint8_t) (clamp(b, 0, 1) * 255.0f)))
#define ConvertToARGB(a, r, g, b) (((uint8_t) (a * 255.0f)) << 24 | ((uint8_t) (r * 255.0f)) << 16 | ((uint8_t) (g * 255.0f)) << 8 | ((uint8_t) (b * 255.0f)))
#define AlbedoToARGB(albedo) (((uint8_t) (albedo[0] * 255.0f)) << 24 | ((uint8_t) (albedo[1] * 255.0f)) << 16 | ((uint8_t) (albedo[2] * 255.0f)) << 8 | ((uint8_t) (albedo[3] * 255.0f)))
#define SCREEN_WIDTH 640
#define SCREEN_HEIGHT 480
#define SPHERE_COUNT 2
#define pixelsPerUnit ((float) SCREEN_HEIGHT / 2.0f)

#define XToUV(x) (((float) x - SCREEN_WIDTH / 2.0f) / pixelsPerUnit)
#define YToUV(y) ((SCREEN_HEIGHT / 2.0f - (float) y) / pixelsPerUnit)


SDL_Window* window = NULL;
SDL_Renderer* renderer = NULL;
SDL_Texture* texture = NULL;


uint32_t* seed = 0x01a35f4;

typedef struct Pixel {
    float x;
    float y;
    uint32_t argb;

} Pixel;


// main pixel shader
void PixelShader(Pixel* pixels, int count, Sphere* spheres) {

    Vector rayOrigin = {0, 0, -1.0f};

    Vector lightSource = normalize((Vector) {1, 1, -1});

    Vector normal;
    float lightScale;

    Vector rayDirection;
    
    Vector hitPoint;

    Vector translate;


    float radius;
    float t, a, b, c, discriminant, closestT;
    int closestShpere;
    int hit;



    #pragma omp parallel for private(t, a, b, c, discriminant, hitPoint, rayDirection, lightScale, normal, radius, closestShpere, closestT, translate, hit) shared( rayOrigin, lightSource)
    for (int i = 0; i < count; i++) {

        rayDirection = (Vector) {pixels[i].x, pixels[i].y, 1.0f};
        rayDirection = normalize(rayDirection);


        closestT = FLT_MAX;
        hit = 0;

        for (int x = 0; x < SPHERE_COUNT; x++) {
            radius = spheres[x].r;
            translate = Vminus(rayOrigin, spheres[x].origin);

            // solve for whether there is an intersection of the ray and the sphere
            a = dot(rayDirection, rayDirection);
            b = 2 * dot(rayDirection, translate);
            c = dot(translate, translate) - radius * radius;

            discriminant = b * b - 4 * a * c;
            
            if (discriminant >= 0) {
                // normalised distance from ray source to sphere
                t = (-b - sqrt(discriminant)) / (2.0f * a);
                if (t < 0) {
                    break;
                }
                hit = 1;

                if (t < closestT) {
                    closestT = t;
                    closestShpere = x;
                }

            }
        }

        // if there is an intersection
        if (hit == 1) {

            radius = spheres[closestShpere].r;
            // calculate where on the sphere the ray hit
            hitPoint = (Vector) {rayOrigin.x + rayDirection.x * closestT, rayOrigin.y + rayDirection.y * closestT, rayOrigin.z + rayDirection.z * closestT};

            // calculate the normal of the sphere surface where the ray hit
            normal = normalize(Vminus(hitPoint, spheres[closestShpere].origin));

            // calculate the dot product (cos(angle)) between the light source and the surface normal
            lightScale = max(dot(normal, lightSource), 0.0f);

            // shade the sphere using the dot product as a shading constant
            float colour[4] = {spheres[closestShpere].albedo[0], spheres[closestShpere].albedo[1] * lightScale, spheres[closestShpere].albedo[2] * lightScale, spheres[closestShpere].albedo[3] * lightScale};
            pixels[i].argb = AlbedoToARGB(colour);  //ConvertToARGB(1, 1 * lightScale, 0, 1 * lightScale);

        } else {
            pixels[i].argb = ConvertToARGB(1, 0, 0, 0);
        }

    }

}



int main(int argc, char **argv) {

    printf("test\n");
    //Vector random = {random_float(seed), random_float(seed), random_float(seed)};
    //printf("%f, %f, %f\n", random.x, random.y, random.z);
    //printf("%f\n", random_float(seed));


    // create pixel array and screen buffer
    uint64_t PixelCount = SCREEN_WIDTH * SCREEN_HEIGHT;
    int pitch = 4 * SCREEN_WIDTH; // 4 is the number of bytes used for colour information per pixel
    
    Pixel *pixels = NULL;
    uint32_t* screen = NULL;
    Sphere* spheres;

    pixels = malloc(sizeof(Pixel) * PixelCount);
    screen = malloc(sizeof(uint32_t) * PixelCount);
    spheres = malloc(sizeof(Sphere) * SPHERE_COUNT);

    spheres[0] = (Sphere) {{0, 0, 0}, 0.5f, {1, 1, 0, 1}};
    spheres[1] = (Sphere) {{0, -100.5, 0}, 100, {1, 0, 1, 0}};
    
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

    // for frame rate
    uint64_t start, end;
    float elapsed;
    
    while (e.type != SDL_QUIT) {
        
        // calculate frame rate
        start = SDL_GetPerformanceCounter();


        // clear the screen to black
        //SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
        //SDL_RenderClear( renderer );

        PixelShader(pixels, PixelCount, spheres);

        // create 32 bit argb pixel colour for screen
        for (int i = 0; i < PixelCount; i++) {
            screen[i] = pixels[i].argb;
        }

        // draw the pixel buffer

        SDL_LockTexture(texture, NULL, &screen, &pitch);

        SDL_UnlockTexture(texture);

        SDL_RenderCopy(renderer, texture, NULL, NULL);

        // Update screen
        SDL_RenderPresent( renderer );

        SDL_PollEvent(&e);

        // calculate frame rate
        end = SDL_GetPerformanceCounter();
        elapsed = (end - start) / (float)SDL_GetPerformanceFrequency();
        //printf("%f ms\n", elapsed * 1000);
    }

    ex(window, renderer, texture);
    free(screen);
    free(pixels);
    free(spheres);

    screen = NULL;
    pixels = NULL;
    spheres = NULL;

    return 0;
}
