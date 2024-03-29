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
#include "Objects.h"
#include <time.h>

// map argb where values range from 0 to 1 with return of 32 bit argb value
#define ConvertToARGB_Clamp(a, r, g, b) (((uint8_t) (clamp(a, 0, 1) * 255.0f)) << 24 | ((uint8_t) (clamp(r, 0, 1) * 255.0f)) << 16 | ((uint8_t) (clamp(g, 0, 1) * 255.0f)) << 8 | ((uint8_t) (clamp(b, 0, 1) * 255.0f)))
#define ConvertToARGB(a, r, g, b) (((uint8_t) (a * 255.0f)) << 24 | ((uint8_t) (r * 255.0f)) << 16 | ((uint8_t) (g * 255.0f)) << 8 | ((uint8_t) (b * 255.0f)))
#define VectorToARGB(albedo) ((0xff) << 24 | ((uint8_t) (albedo.x * 255.0f)) << 16 | ((uint8_t) (albedo.y * 255.0f)) << 8 | ((uint8_t) (albedo.z * 255.0f)))
#define VectorToARGB_Clamp(albedo) ((0xff) << 24 | ((uint8_t) (clamp(albedo.x, 0, 1) * 255.0f)) << 16 | ((uint8_t) (clamp(albedo.y, 0, 1) * 255.0f)) << 8 | ((uint8_t) (clamp(albedo.z, 0, 1) * 255.0f)))
#define SCREEN_WIDTH 640//1024 //640
#define SCREEN_HEIGHT 480//720 //480
#define MAX_REFLECTIONS 10
#define SPHERE_COUNT 4
#define pixelsPerUnit ((float) SCREEN_HEIGHT / 2.0f)

#define XToUV(x) (((float) x - SCREEN_WIDTH / 2.0f) / pixelsPerUnit)
#define YToUV(y) ((SCREEN_HEIGHT / 2.0f - (float) y) / pixelsPerUnit)


SDL_Window* window = NULL;
SDL_Renderer* renderer = NULL;
SDL_Texture* texture = NULL;


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

uint32_t PerPixel(float x, float y, Sphere* spheres, Vector lightSource, uint64_t frameCount, Vector* realColour, uint32_t* seed, Material* materials) {

    *realColour = (realColour->x == FLT_MAX) ? (Vector) {0, 0, 0} : *realColour;

        // define the ray comming from the pixel
        Ray ray;
        // FRONT VIEW
        ray.Origin = (Vector) {0, 0.5, -1.5};
        ray.Direction = normalize((Vector) {x + random_float(seed) * 0.005f, y + random_float(seed) * 0.005f, 1.0f});
        
        // SIDE VIEW
        //ray.Origin = (Vector) {1.5, 0.5, -0.5};
        //ray.Direction = normalize((Vector) {-1.0f, y + random_float(seed) * 0.007f, x + random_float(seed) * 0.007f});

        Vector contribution = {1, 1, 1};
        Vector light = {0, 0, 0};

        for (int i = 0; i < MAX_REFLECTIONS; i++) {
            HitPayload payload = TraceRay(spheres, ray);

            // if the ray did not hit a sphere
            if (payload.HitDistance == FLT_MAX) {  
                Vector skyColour = miss(ray);
                skyColour = Vtimes(skyColour, contribution);
                light = Vsum(light, skyColour);
                break;
            
            } else {

                Vector sphereColour = materials[spheres[payload.ObjectIndex].MaterialIndex].albedo;


                // metallic 
                if (materials[spheres[payload.ObjectIndex].MaterialIndex].type == METALLIC) {
                    
                    // generates a new ray with a "pure" reflection added to a random "fuzzyness" vector defined by the met_roughness constant
                    ray.Direction = reflect(ray.Direction, payload.WorldNormal);
                    Vector tmp = normalize(((Vector) {2.0 * (random_float(seed) - 0.5), 2.0 * (random_float(seed) - 0.5), 2.0 * (random_float(seed) - 0.5)}));
                    tmp = Vscale(tmp, materials[spheres[payload.ObjectIndex].MaterialIndex].met_roughness);
                    ray.Direction = Vsum(ray.Direction, tmp);

                    contribution = Vtimes(sphereColour, contribution);

                    ray.Origin.x = payload.WorldPosition.x + payload.WorldNormal.x * 0.000001f; // to avoid ray starting on the surface of the sphere
                    ray.Origin.y = payload.WorldPosition.y + payload.WorldNormal.y * 0.000001f;
                    ray.Origin.z = payload.WorldPosition.z + payload.WorldNormal.z * 0.000001f;


                } else if(materials[spheres[payload.ObjectIndex].MaterialIndex].type == DIELECTRIC){
                    
                    ray.Direction = refract(normalize(ray.Direction), payload.WorldNormal, 1 / materials[spheres[payload.ObjectIndex].MaterialIndex].ref_index, seed);

                    ray.Origin.x = payload.WorldPosition.x - payload.WorldNormal.x * 0.000001f; // to avoid ray starting on the surface of the sphere
                    ray.Origin.y = payload.WorldPosition.y - payload.WorldNormal.y * 0.000001f;
                    ray.Origin.z = payload.WorldPosition.z - payload.WorldNormal.z * 0.000001f;


                // diffuse
                } else {
                    // complete random reat direction biased by the normal
                    ray.Direction = Vsum(normalize(((Vector) {2.0 * (random_float(seed) - 0.5), 2.0 * (random_float(seed) - 0.5), 2.0 * (random_float(seed) - 0.5)})), payload.WorldNormal);
                    
                    contribution = Vtimes(sphereColour, contribution);


                    // calculate the increase in light from emissive objects
                    Vector tmp = Vscale(materials[spheres[payload.ObjectIndex].MaterialIndex].EmissionColour, materials[spheres[payload.ObjectIndex].MaterialIndex].EmissionPower);
                    light = Vsum(light, tmp);

                    ray.Origin.x = payload.WorldPosition.x + payload.WorldNormal.x * 0.000001f; // to avoid ray starting on the surface of the sphere
                    ray.Origin.y = payload.WorldPosition.y + payload.WorldNormal.y * 0.000001f;
                    ray.Origin.z = payload.WorldPosition.z + payload.WorldNormal.z * 0.000001f;

                }
            }

        }
        *realColour = (Vector) {realColour->x + light.x, realColour->y + light.y, realColour->z + light.z};
        

    float a = 1.0f / (float) frameCount;
    return VectorToARGB_Clamp(((Vector) {realColour->x * a, realColour->y * a, realColour->z * a}));

}

float randColour() {
    return (float) rand() / (float) RAND_MAX;
}


int main(int argc, char **argv) {


    Material materials[] = {{{1, 1, 1}, {0, 0, 0}, 0, METALLIC, 0.0, FLT_MAX}, // 0: mirror
    {{0.8, 0.8, 0.8}, {0.8, 0.8, 0.8}, 0, LAMBERTIAN, FLT_MAX, FLT_MAX}, // 1: matte white
    {{0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}, 0, LAMBERTIAN, FLT_MAX, FLT_MAX}, // 2: matte black
    {{0.77, 0.36, 0.15}, {0.77, 0.36, 0.15}, 0, LAMBERTIAN,FLT_MAX, FLT_MAX}, // 3: matte orange
    {{0.8, 0.5, 0.2}, {0.8, 0.5, 0.2}, 2.0, LAMBERTIAN, FLT_MAX, FLT_MAX},  // 4: orange lightsource
    {{0.2, 0.3, 0.7}, {0.2, 0.3, 0.7}, 0, LAMBERTIAN, FLT_MAX, FLT_MAX}, // 5: blueish
    {{0.2, 0.3, 0.7}, {0.2, 0.3, 0.7}, 0, DIELECTRIC, FLT_MAX, 1.5} // 6: dielectric glass
    };



    // create pixel array and screen buffer
    uint64_t PixelCount = SCREEN_WIDTH * SCREEN_HEIGHT;
    int pitch = 4 * SCREEN_WIDTH; // 4 is the number of bytes used for colour information per pixel
    
    // linked lists for the screen coordinates and the colours (screen)
    float* pixelsX = NULL;
    float* pixelsY = NULL;
    uint32_t* screen = NULL;
    
    Sphere* spheres = NULL;

    //Material* materials = NULL;

    pixelsX = malloc(sizeof(float) * PixelCount);
    pixelsY = malloc(sizeof(float) * PixelCount);
    screen = malloc(sizeof(uint32_t) * PixelCount);
    
    spheres = malloc(sizeof(Sphere) * SPHERE_COUNT);
    //materials = malloc(sizeof(Material) * SPHERE_COUNT);

    spheres[0] = (Sphere) {{0, -1000, 0}, 1000, 1};
    spheres[1] = (Sphere) {{-1, 0.5, 0}, 0.5, 0};
    spheres[2] = (Sphere) {{60, 100, -60}, 50, 4};
    spheres[3] = (Sphere) {{0, 0.5, 0}, 0.5, 5};
    //Snowman(spheres + 3, (Vector) {0.0f, 0.0f, 0.0f});
    
    uint32_t seed = 0x01a35f4;
    
    //RandomSpheres(spheres, materials);

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
        #pragma omp parallel for shared(pixelsX, pixelsY, lightSource, spheres, screen, materials) private(seed)
        for (int i = 0; i < PixelCount; i++) {

            screen[i] = PerPixel(pixelsX[i], pixelsY[i], spheres, lightSource, frameCount, &(realColours[i]), &seed, materials);
                
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

    //free(screen); // screen creates seg fault
    free(pixelsX);
    free(pixelsY);
    free(spheres);
    //free(materials);

    screen = NULL;
    pixelsX = NULL;
    pixelsY = NULL;
    spheres = NULL;
    //materials = NULL;

    return 0;
}
