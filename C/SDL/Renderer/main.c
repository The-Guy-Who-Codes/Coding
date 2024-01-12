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
#define VectorToARGB(albedo) ((0xff) << 24 | ((uint8_t) (albedo.x * 255.0f)) << 16 | ((uint8_t) (albedo.y * 255.0f)) << 8 | ((uint8_t) (albedo.z * 255.0f)))
#define VectorToARGB_Clamp(albedo) ((0xff) << 24 | ((uint8_t) (clamp(albedo.x, 0, 1) * 255.0f)) << 16 | ((uint8_t) (clamp(albedo.y, 0, 1) * 255.0f)) << 8 | ((uint8_t) (clamp(albedo.z, 0, 1) * 255.0f)))
#define SCREEN_WIDTH 640
#define SCREEN_HEIGHT 480
#define SPHERE_COUNT 9
#define RAYS_PER_PIXEL 100
#define MAX_REFLECTIONS 4
#define pixelsPerUnit ((float) SCREEN_HEIGHT / 2.0f)

#define XToUV(x) (((float) x - SCREEN_WIDTH / 2.0f) / pixelsPerUnit)
#define YToUV(y) ((SCREEN_HEIGHT / 2.0f - (float) y) / pixelsPerUnit)


SDL_Window* window = NULL;
SDL_Renderer* renderer = NULL;
SDL_Texture* texture = NULL;


uint32_t seed = 0x01a35f4;

typedef struct Pixel {
    float x;
    float y;
    uint32_t argb;

} Pixel;

typedef struct HitPayload {
    float HitDistance;
    Vector WorldPosition;
    Vector WorldNormal;
    uint32_t ObjectIndex;
} HitPayload;

Vector miss(Ray ray) {
    Vector unit_dir = normalize(ray.Direction);
    float a = 0.5f * (unit_dir.y + 1.0f);
    Vector colour = {(1.0f - a) + a * 0.5f, (1.0f - a) + a * 0.7f, (1.0f - a)+ a * 1.0f};
    return  colour;
}

HitPayload ClosestHit(Sphere* spheres, Ray ray, float Distance, uint32_t ObjectIndex) {
    
    HitPayload payload;
    payload.HitDistance = Distance;


    // if the ray intersected a sphere
    if (payload.HitDistance == FLT_MAX) {
    
        return payload;

    } else {

        payload.ObjectIndex = ObjectIndex;
        
        // calculate where on the sphere the ray hit
        payload.WorldPosition = (Vector) {ray.Origin.x + ray.Direction.x * payload.HitDistance, ray.Origin.y + ray.Direction.y * payload.HitDistance, ray.Origin.z + ray.Direction.z * payload.HitDistance};
    
         // calculate the normal of the sphere surface where the ray hit
        payload.WorldNormal = normalize(Vminus(payload.WorldPosition, spheres[payload.ObjectIndex].origin));

        return payload;
    
    }

}

HitPayload TraceRay(Sphere* spheres, Ray ray) {

    float closestT = FLT_MAX;
    uint32_t index = UINT32_MAX;

    for (int x = 0; x < SPHERE_COUNT; x++) {

        float radius = spheres[x].r;
        Vector translate = Vminus(ray.Origin, spheres[x].origin);

        // solve for whether there is an intersection of the ray and the sphere
        float a = dot(ray.Direction, ray.Direction);
        float b = 2 * dot(ray.Direction, translate);
        float c = dot(translate, translate) - radius * radius;

        float discriminant = b * b - 4 * a * c;
        
        if (discriminant >= 0) {
            // normalised distance from ray source to sphere
            float t = (-b - sqrt(discriminant)) / (2.0f * a);

            // find the sphere which the ray origin is closest to
            if (t < closestT && t > 0) {
                closestT = t;
                index = x;
            }

        }
    }
    return ClosestHit(spheres, ray, closestT, index);
}

uint32_t PerPixel(float x, float y, Sphere* spheres, Vector lightSource, uint64_t frameCount, Vector* realColour) {

    *realColour = (realColour->x == FLT_MAX) ? (Vector) {0, 0, 0} : *realColour;

    //for (int z = 0; z < RAYS_PER_PIXEL; z++) {
        // define the ray comming from the pixel
        Ray ray;
        ray.Origin = (Vector) {0, 0.5, -1.5};
        ray.Direction = normalize((Vector) {x + random_float(&seed) * 0.007f, y + random_float(&seed) * 0.007f, 1.0f});

        float multiplier = 1.0f;
        Vector colour = {0, 0, 0};

        for (int i = 0; i < MAX_REFLECTIONS; i++) {
            HitPayload payload = TraceRay(spheres, ray);

            // if the ray did not hit a sphere
            if (payload.HitDistance == FLT_MAX) {  
                Vector skyColour = miss(ray);
                skyColour = Vscale(skyColour, multiplier);
                colour = Vsum(colour, skyColour);
                break;
            
            } else {

                Vector sphereColour = spheres[payload.ObjectIndex].albedo;

                // calculate the dot product (cos(angle)) between the light source and the surface normal
                float lightScale = max(dot(payload.WorldNormal, lightSource), 0.0f);

                sphereColour = Vscale(sphereColour, lightScale);

                sphereColour = Vscale(sphereColour, multiplier);

                // shade the sphere using the dot product as a shading constant            
                colour = Vsum(colour, sphereColour);

                ray.Origin.x = payload.WorldPosition.x + payload.WorldNormal.x * 0.000001f; // to avoid ray starting on the surface of the sphere
                ray.Origin.y = payload.WorldPosition.y + payload.WorldNormal.y * 0.000001f;
                ray.Origin.z = payload.WorldPosition.z + payload.WorldNormal.z * 0.000001f;

                Vector normal = {random_float(&seed) - 0.5, random_float(&seed) - 0.5, random_float(&seed) - 0.5};
                normal = Vscale(normal , spheres[payload.ObjectIndex].roughness);
                normal = Vsum(normal, payload.WorldNormal);
                ray.Direction = reflect(ray.Direction, normal);
            }

            multiplier *= 0.4f;
        }
        *realColour = (Vector) {realColour->x + colour.x, realColour->y + colour.y, realColour->z + colour.z};
        
    //}

    float a = 1.0f / (float) frameCount;
    return VectorToARGB_Clamp(((Vector) {realColour->x * a, realColour->y * a, realColour->z * a}));

}



int main(int argc, char **argv) {

    // create pixel array and screen buffer
    uint64_t PixelCount = SCREEN_WIDTH * SCREEN_HEIGHT;
    int pitch = 4 * SCREEN_WIDTH; // 4 is the number of bytes used for colour information per pixel
    
    // linked lists for the screen coordinates and the colours (screen)
    float* pixelsX = NULL;
    float* pixelsY = NULL;
    uint32_t* screen = NULL;
    
    Sphere* spheres = NULL;

    pixelsX = malloc(sizeof(float) * PixelCount);
    pixelsY = malloc(sizeof(float) * PixelCount);
    screen = malloc(sizeof(uint32_t) * PixelCount);
    
    spheres = malloc(sizeof(Sphere) * SPHERE_COUNT);

    spheres[0] = (Sphere) {{0, 0, 0}, 0.5f, {0.8, 0.8, 0.8}, 1.0f, 0.0f};
    spheres[1] = (Sphere) {{0, -100.5, 0}, 100, {0, 0, 0}, 0.0f, 0.0f};
    spheres[2] = (Sphere) {{0, 0.75, 0}, 0.4f, {0.8, 0.8, 0.8}, 1.0f, 0.0f};
    spheres[3] = (Sphere) {{0.1, 0.74, -0.4}, 0.07f, {0.1, 0.1, 0.1}, 1.0f, 0.0f};
    spheres[4] = (Sphere) {{-0.1, 0.74, -0.4}, 0.07f, {0.1, 0.1, 0.1}, 1.0f, 0.0f};

    spheres[5] = (Sphere) {{1, 0, 1.5}, 0.5f, {0.8, 0.8, 0.8}, 1.0f, 0.0f};
    spheres[6] = (Sphere) {{1, 0.75, 1.5}, 0.4f, {0.8, 0.8, 0.8}, 1.0f, 0.0f};
    spheres[7] = (Sphere) {{1.1, 0.74, 1.1}, 0.07f, {0.1, 0.1, 0.1}, 1.0f, 0.0f};
    spheres[8] = (Sphere) {{0.9, 0.74, 1.1}, 0.07f, {0.1, 0.1, 0.1}, 1.0f, 0.0f};


    Vector* realColours = malloc(sizeof(Vector) * PixelCount);

    // give the pixel coordinates and sets the y axis from -1 to 1 and x axis is created to preserve aspect ratio
    float max = (float) SCREEN_WIDTH / (float) SCREEN_HEIGHT;
    for (int i = 0; i < PixelCount; i++) {
        pixelsX[i] = (float)(i % SCREEN_WIDTH) / (float) SCREEN_HEIGHT * 2.0f - max;
        pixelsY[i] = -((float)(i / SCREEN_WIDTH) / (float) SCREEN_HEIGHT * 2.0f - 1);
        realColours[i] = (Vector) {FLT_MAX, FLT_MAX, FLT_MAX};
    }

    if (!init(&window, &renderer, &texture, SCREEN_WIDTH, SCREEN_HEIGHT)) {
        printf("Failed to initialise\n");
    }

   

    SDL_Event e;
    SDL_PollEvent(&e);

    // for frame rate
    uint64_t start, end;
    float elapsed;
    
    // define the light source
    Vector lightSource = normalize((Vector) {1, 0.5f, -1.5});



    uint64_t frameCount = 0;

    while (e.type != SDL_QUIT) {
        
        // calculate frame rate
        start = SDL_GetPerformanceCounter();

        // run pixel shader
        #pragma omp parallel for shared(pixelsX, pixelsY, lightSource, spheres, screen)
        for (int i = 0; i < PixelCount; i++) {

            screen[i] = PerPixel(pixelsX[i], pixelsY[i], spheres, lightSource, frameCount, &(realColours[i]));
                
        }

        // draw the pixel buffer

        SDL_LockTexture(texture, NULL, &screen, &pitch);

        SDL_UnlockTexture(texture);

        SDL_RenderCopy(renderer, texture, NULL, NULL);

        // Update screen
        SDL_RenderPresent( renderer );

        SDL_PollEvent(&e);

        frameCount++;

        // calculate frame rate
        end = SDL_GetPerformanceCounter();
        elapsed = (end - start) / (float)SDL_GetPerformanceFrequency();
        printf("%f ms\n", elapsed * 1000);
    }

    ex(window, renderer, texture);

    free(screen);
    free(pixelsX);
    free(pixelsY);
    free(spheres);

    screen = NULL;
    pixelsX = NULL;
    pixelsY = NULL;
    spheres = NULL;

    return 0;
}
