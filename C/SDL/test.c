#include <stdio.h>
#include <stdint.h>
#include <math.h>

#define ConvertToARGB(a, r, g, b) ((((uint8_t) (a * 255.0f)) << 24) | (((uint8_t) (r * 255.0f)) << 16) | (((uint8_t) (g * 255.0f)) << 8) | ((uint8_t) (b * 255.0f)))
#define ConvertToARGB_Clamp(a, r, g, b) ((((uint8_t) (clamp(a, 0, 1) * 255.0f)) << 24) | (((uint8_t) (clamp(r, 0, 1) * 255.0f))) << 16 | (((uint8_t) (clamp(g, 0, 1) * 255.0f)) << 8) | ((uint8_t) (clamp(b, 0, 1) * 255.0f)))

#define dot(a, b) (a.x * b.x + a.y * b.y + a.z * b.z)
#define Vminus(a, b) ((Vector) {a.x - b.x, a.y - b.y, a.z - b.z})
#define modulus(vector) (sqrt(dot(vector, vector)))

typedef struct Vector {
    double x;
    double y;
    double z;
} Vector;

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

    
int main() {
    Vector b = {1, 1, 1};
    b = (Vector) {b.x* 0.5 + 0.5, b.y* 0.5 + 0.5, b.z* 0.5 + 0.5};
    //b = normalize(b);

    printf("(%f, %f, %f)\n", b.x, b.y, b.z);
    printf("0x%x\n", ConvertToARGB(1, b.x, b.y, b.z));
    printf("0x%x\n", ConvertToARGB_Clamp(1, b.x, b.y, b.z));
    printf("0x%.8x\n", (uint32_t) (((uint8_t) (b.x * 255.0f)) << 16));
    return 0;
}