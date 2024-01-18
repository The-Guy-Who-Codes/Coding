#pragma once
#include "vectors.h"


// Requires a buffered array of 11 sphere structs
// Center argument is where the center of the main body sphere lies
void Snowman(Sphere* spheres, Vector center) {
    spheres[0] = (Sphere) {{0 + center.x, 0 + center.y, 0 + center.z}, 0.5f, 1};
    spheres[1] = (Sphere) {{0 + center.x, 0.75 + center.y, 0 + center.z}, 0.4f, 1};

    // eyes
    spheres[2] = (Sphere) {{0.1 + center.x, 0.74 + center.y, -0.4 + center.z}, 0.07f, 2};
    spheres[3] = (Sphere) {{-0.1 + center.x, 0.74 + center.y, -0.4 + center.z}, 0.07f, 2};

    // nose
    spheres[4] = (Sphere) {{0 + center.x, 0.63 + center.y, -0.4 + center.z}, 0.07f, 3};
    spheres[5] = (Sphere) {{0 + center.x, 0.63 + center.y, -0.49 + center.z}, 0.05f, 3};
    spheres[6] = (Sphere) {{0 + center.x, 0.63 + center.y, -0.55 + center.z}, 0.03f, 3};

    // buttons
    spheres[7] = (Sphere) {{0 + center.x, 0.3 + center.y, -0.405 + center.z}, 0.03f, 2};
    spheres[8] = (Sphere) {{0 + center.x, 0.2 + center.y, -0.465 + center.z}, 0.03f, 2};
    spheres[9] = (Sphere) {{0 + center.x, 0.1 + center.y, -0.49 + center.z}, 0.03f, 2};
    spheres[10] = (Sphere) {{0 + center.x, 0.0 + center.y, -0.49 + center.z}, 0.03f, 2};
}

// Requires buffered array og 202 Material and Sphere structs
void RandomSpheres(Sphere* spheres, Material* materials){
    
    uint32_t seed = 0x01a35f4;
    
    // ground
    spheres[0] = (Sphere) {{0, -1000, 0}, 1000, 0};
    materials[0] = (Material) {{0.5, 0.5, 0.5}, {0, 0, 0}, 0, LAMBERTIAN, FLT_MAX, FLT_MAX};

    materials[1] = (Material) {{0.5, 0.5, 0.5}, {0, 0, 0}, 0, DIELECTRIC, FLT_MAX, 1.5};
    spheres[1] = (Sphere) {{-2, 1, 4}, 1, 2};

    materials[2] = (Material) {{0.5, 0.5, 0.5}, {0, 0, 0}, 0, LAMBERTIAN, FLT_MAX, FLT_MAX};
    spheres[2] = (Sphere) {{0, 1, 2}, 1, 1};

    materials[3] = (Material) {{0.7, 0.6, 0.5}, {0, 0, 0}, 0, METALLIC, 0.0, FLT_MAX};
    spheres[3] = (Sphere) {{2, 1, 0}, 1, 3};

    for (int i = 4; i < 90; i++) {
        materials[i] = (Material) {{random_float(&seed), random_float(&seed), random_float(&seed)}, {0, 0, 0}, 0, METALLIC, random_float(&seed) / 2.0, FLT_MAX};
        spheres[i] = (Sphere) {{(random_float(&seed) -0.5) * 20, 0.2, (random_float(&seed)) * 10}, 0.2, i};
    }
    for (int i = 90; i < 180; i++) {
        materials[i] = (Material) {{random_float(&seed), random_float(&seed), random_float(&seed)}, {0, 0, 0}, 0, LAMBERTIAN, 0.0, FLT_MAX};
        spheres[i] = (Sphere) {{(random_float(&seed) -0.5) * 20, 0.2, (random_float(&seed)) * 10}, 0.2, i};  
    }
    for (int i = 180; i < 201; i++) {
        materials[i] = (Material) {{random_float(&seed), random_float(&seed), random_float(&seed)}, {0, 0, 0}, 0, DIELECTRIC, 0.0, 1.5};
        spheres[i] = (Sphere) {{(random_float(&seed) -0.5) * 20, 0.2, (random_float(&seed)) * 10}, 0.2, i};  
    }

    materials[201] = (Material) {{0.8, 0.5, 0.2}, {0.8, 0.5, 0.2}, 2.0, LAMBERTIAN, FLT_MAX, FLT_MAX};
    spheres[201] = (Sphere) {{60, 100, -60}, 50, 201};
}