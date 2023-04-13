#include <cstdio>
#include <cstdlib>
#include <vector>
#include <cmath>


class Matrix {
        int w, h;
    
    // enter has ascii value dec. 10

    public:
        Matrix (int, int);

        //int* pmatrix = (int*) malloc(w * h * sizeof(int));
        int* pmatrix = new int(w * h);

        int inp_int() {
                    int num = 0;
                    int inp;
                    std::vector<int> inps;
                    inp = getchar();
                    while(inp != 10){
                        inps.push_back(inp - 48);
                        inp = getchar();
                    };
                    int len = inps.size();
                    for(int i = 0; i < len; i++) {
                        num += inps[i] * pow(10, len - (i + 1));
                    }
                    return num;
        }

        void init() {
            printf("Test\n[%i, %i]\n", w, h);

            for(int i = 0; i < (w * h); i++) {
                printf("Enter Value %i:\n", i + 1);
                pmatrix[i] = inp_int();

            }
            printf("Matrix initialised\n");
        }

        void printMat() {
            for(int i = 0; i < h; i++) {
                for(int x = 0; x < w; x++) {
                    printf(" %i ", pmatrix[i * w + x]);
                }
                printf("\n");
            }
        }

};

Matrix::Matrix (int a, int b) {
    w = a;
    h = b;
}


int main() {
    Matrix mat (2, 2);
    mat.init();
    mat.printMat();
    //printf("%i\n", (inp_int() - 5));
    return 0;
}