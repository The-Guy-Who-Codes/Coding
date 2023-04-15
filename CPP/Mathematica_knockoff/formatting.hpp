#pragma once
#include <vector>
#include <cstdio>

void to_frac(double x) {
	std::vector<int> ints;
	double left = x;
	double tmp1;
	int tmp2;
	tmp1 = x;
	while (tmp1 < 1000) {
		tmp2 = (int) tmp1;
		ints.push_back(tmp2);
		tmp1 -= tmp2;
		tmp1 = 1 / tmp1;
	}
	int a, b;
	a = ints[ints.size() - 1];
	b = 1;
	for (int i = ints.size() - 2; i >= 0; i--) {
		tmp1 = a;
		a = b;
		b = tmp1;
		a += b * ints[i];		
	}
	printf("%i / %i \n", a, b);
}