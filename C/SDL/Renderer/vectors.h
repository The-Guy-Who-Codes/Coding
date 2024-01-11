#pragma once
#include <math.h>


#define max(a, b) ((a > b) ? a : b)
#define min(a, b) ((a < b) ? a : b)

#define dot(a, b) (a.x * b.x + a.y * b.y + a.z * b.z)
#define modulus(vector) (sqrt(dot(vector, vector)))
#define Vsum(a, b)((Vector) {a.x + b.x, a.y + b.y, a.z + b.z})
#define Vminus(a, b) ((Vector) {a.x - b.x, a.y - b.y, a.z - b.z})
#define Vscale(vec, a) ((Vector) {vec.x * a, vec.y * a, vec.z * a})

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
    Vector albedo;
    float roughness;
    float metallic;
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

Vector reflect(Vector incomming, Vector normal) {
    // d-2(d dot n)n where n is normal and d is incomming ray
    // https://math.stackexchange.com/questions/13261/how-to-get-a-reflection-vector
    double scale = dot(incomming, normal) * 2.0f;
    Vector tmp = Vscale(normal, scale);
    return Vminus(incomming, tmp);

}