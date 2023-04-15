#pragma once
#include "gen.hpp"


double sin(double x) {
    // sanitise input so that x is within +- pi
    return (x - (pow(x, 3) / 6) + (pow(x, 5) / 120) - (pow(x, 7) / 5040) + (pow(x, 9) / 362880) - (pow(x, 11) / 39916800) + (pow(x, 13) / 6227020800));
}

double cos(double x) {
	return sin(x + (pi / 2));
}

double tan(double x) {
	return sin(x) / cos(x);
}