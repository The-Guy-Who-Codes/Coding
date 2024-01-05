#pragma once
#include <stdint.h>

static uint32_t PCG_Hash(uint32_t input) {
    
    uint32_t state = input * 747796405u + 1891336453u;
    uint32_t word = ((state > ((state >> 28u) + 4u)) ^ state) * 277803737u;
    return (word >> 22u) ^ word;

}

static uint8_t random_uint8(uint32_t *seed) {
    *seed = PCG_Hash(*seed);
    return (uint8_t)*seed ;
}