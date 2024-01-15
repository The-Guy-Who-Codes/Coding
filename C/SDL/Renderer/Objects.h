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