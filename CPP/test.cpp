#include <cstdio>
#include <cstdlib>
#include <vector>
#include <iostream>


double pi = 3.141592654;

void debugI(int a) {
    printf("DEBUG: %i\n", a);
}

void to_frac(double x) {
	std::vector<int> ints;
	double left = x;
	double tmp1;
	int tmp2;
	tmp1 = x;
	while (1 / tmp1 > 0.00000001) {
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

double pow(double a, int b) {
    if (b == 0) {
        return 1;
    }
    double tmp = a;
    for (int i = 0; i < b - 1; i++) {
        a *= tmp;
    }
    return a;
}

int factorial(int a) {
    int sum = a;
    for (int i = a - 1; i > 0; i--) {
        sum *= i;
    }
    return sum;
}

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

class Matrix {
        int w, h;
    
    // enter key has ascii value dec. 10

    public:
        Matrix (int, int);

        //int* pmatrix = (int*) malloc(w * h * sizeof(int));
        int* pmatrix = new int(w * h);

        int inp_int() {
                    int num = 0;
                    int inp;
                    std::vector<int> inps;
                    inp = getchar();
                    while (inp != 10){
                        inps.push_back(inp - 48);
                        inp = getchar();
                    };
                    int len = inps.size();
                    for (int i = 0; i < len; i++) {
                        num += inps[i] * pow(10, len - (i + 1));
                    }
                    return num;
        }

        void init() {
            printf("Test\n[%i, %i]\n", w, h);

            for (int i = 0; i < (w * h); i++) {
                printf("Enter Value %i:\n", i + 1);
                pmatrix[i] = inp_int();

            }
            printf("Matrix initialised\n");
        }

        void printMat() {
            for (int i = 0; i < h; i++) {
                for (int x = 0; x < w; x++) {
                    printf(" %i ", pmatrix[i * w + x]);
                }
                printf("\n");
            }
            printf("\n");

        }

        void scalar(int k) {
            for (int i = 0; i < h; i++) {
                for (int x = 0; x < w; x++) {
                    pmatrix[i * w + x] *= k;
                }
            }
        }

};




Matrix::Matrix (int a, int b) {
    w = a;
    h = b;
}


int main(int argc, char* argv[]){
    //Matrix mat (2, 2);
    //mat.init();
    //mat.printMat();
   // mat.scalar(2);
    //mat.printMat();
    	to_frac(88.3820224719);
    	return 0;
}
