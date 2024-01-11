#pragma once
#include <math.h>


#define max(a, b) ((a > b) ? a : b)
#define min(a, b) ((a < b) ? a : b)

#define dot(a, b) (a.x * b.x + a.y * b.y + a.z * b.z)
#define modulus(vector) (sqrt(dot(vector, vector)))
#define Vsum(a, b)((Vector) {a.x + b.x, a.y + b.y, a.z + b.z})
#define Vminus(a, b) ((Vector) {a.x - b.x, a.y - b.y, a.z - b.z})
#define Vscale(vec, a) ((Vector) {vec.x * b, vec.y * b, vec.z * b})

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

typedef struct Sphere {
    Vector origin;
    double r;
    uint8_t albedo[4];
} Sphere;

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
