#pragma once
#include <math.h>
#include <float.h>
#include "Random.h"


#define max(a, b) ((a > b) ? a : b)
#define min(a, b) ((a < b) ? a : b)

#define dot(a, b) (a.x * b.x + a.y * b.y + a.z * b.z)
#define modulus(vector) (sqrt(dot(vector, vector)))
#define Vsum(a, b)((Vector) {a.x + b.x, a.y + b.y, a.z + b.z})
#define Vminus(a, b) ((Vector) {a.x - b.x, a.y - b.y, a.z - b.z})
#define Vscale(vec, a) ((Vector) {vec.x * a, vec.y * a, vec.z * a})
#define Vtimes(a, b) ((Vector) {a.x * b.x, a.y * b.y, a.z * b.z})

#define LAMBERTIAN 0
#define METALLIC 1
#define DIELECTRIC 2

const double pi = 3.141592654;

typedef struct Vector {
    double x;
    double y;
    double z;
} Vector;

typedef struct Ray {
    Vector Origin;
    Vector Direction;
} Ray;

typedef struct Material {
    Vector albedo;
    Vector EmissionColour;
    float EmissionPower;
    uint32_t type;
    float met_roughness;
    float ref_index;
} Material;

typedef struct Sphere {
    Vector origin;
    double r;
    uint32_t MaterialIndex;

} Sphere;


/*Material materials[] = {{{1, 1, 1}, {0, 0, 0}, 0, METALLIC, 0.0, FLT_MAX}, // 0: mirror
{{0.8, 0.8, 0.8}, {0.8, 0.8, 0.8}, 0, LAMBERTIAN, FLT_MAX, FLT_MAX}, // 1: matte white
{{0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}, 0, LAMBERTIAN, FLT_MAX, FLT_MAX}, // 2: matte black
{{0.77, 0.36, 0.15}, {0.77, 0.36, 0.15}, 0, LAMBERTIAN,FLT_MAX, FLT_MAX}, // 3: matte orange
{{0.8, 0.5, 0.2}, {0.8, 0.5, 0.2}, 2.0, LAMBERTIAN, FLT_MAX, FLT_MAX},  // 4: orange lightsource
{{0.2, 0.3, 0.7}, {0.2, 0.3, 0.7}, 0, LAMBERTIAN, FLT_MAX, FLT_MAX}, // 5: blueish
{{0.2, 0.3, 0.7}, {0.2, 0.3, 0.7}, 0, DIELECTRIC, FLT_MAX, 1.5} // 6: dielectric glass
};*/




double clamp(double d, double min, double max) {
  const double t = d < min ? min : d;
  return t > max ? max : t;
}

Vector normalize(Vector vector) {
    Vector output;
    double mod = modulus(vector);
    output.x = vector.x / mod;
    output.y = vector.y / mod;
    output.z = vector.z / mod;
    return output;
}

Vector reflect(Vector incomming, Vector normal) {
    // d-2(d dot n)n where n is normal and d is incomming ray
    // https://math.stackexchange.com/questions/13261/how-to-get-a-reflection-vector
    double scale = dot(incomming, normal) * 2.0f;
    Vector tmp = Vscale(normal, scale);
    return Vminus(incomming, tmp);

}

// Schlick’s approximation for whether incomming light reflects of a dielectric
static double reflectance(double cosine, double ref_idx) {
    // Use Schlick's approximation for reflectance.
    auto r0 = (1-ref_idx) / (1+ref_idx);
    r0 = r0*r0;
    return r0 + (1-r0)*pow((1 - cosine),5);
}

// inputted vectors MUST be normalised
// refractiveRatio = (ref_index of incomming medium) / (ref_index of outgoing medium)
Vector refract(Vector incidentRay, Vector normal, float refractiveRatio, uint32_t* seed) {

    float tmp = dot(normal, Vscale(incidentRay, -1));

    if (refractiveRatio * sqrt(fabs(1 - tmp * tmp)) > 1.0 || reflectance(tmp, refractiveRatio) > random_float(seed)) {
        // due to snells law if the refractiveRatio * sin(theta) > 1 then the incomming light must reflect
        return reflect(incidentRay, normal);
    } else {
        // can refract
        Vector refracted_ray_perp = Vscale(normal, tmp);
        refracted_ray_perp = Vsum(refracted_ray_perp, incidentRay);
        refracted_ray_perp = Vscale(refracted_ray_perp, refractiveRatio);

        tmp = modulus(refracted_ray_perp);
        Vector refracted_ray_par = Vscale(normal, -sqrt(fabs(1 - tmp * tmp)));

        return Vsum(refracted_ray_perp, refracted_ray_par);
    }

    }