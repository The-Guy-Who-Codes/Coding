#pragma once
#include <cstdio>
#include <stdlib.h>
#include <vector>
#include "gen.hpp"
#include "trig.hpp"

int inp_int(std::vector<char> params) {
    int num = 0;
    int len = params.size();
    for (int i = 0; i < len; i++) {
        params[i] -= 48;
        num += params[i] * pow(10, len - (i + 1));
    }
    return num;
}

int num_params(std::vector<char> params) {
    if (params.size() == 0) {
        return 0;
    } 
    int count = 0;
    for (int i = 0; i < params.size(); i++) {
        if (params[i] == ',') { // is ","
            count++;
        }
    }
    return count + 1;
}

int contains(std::vector<char> input, char value) {
    int count = 0;
    for (int i = 0; i < input.size(); i++) {
        if (input[i] == value) {
            count++;
        }
    }
    return count;
}


int correct_num_params(int a, int b) {
    if (a != b) {
        printf("Incorrect number of parameters\n");
        return 0;
    }
    return 1;
}

int is_int(std::vector<char> param) {
    for (int i = 0; i < param.size(); i++) {
        if (param[i] == '.') { // "."
            return 0;
        }
    }
    return 1;
}

std::vector<double> get_params(std::vector<char> params, int num) {
    int pointer = 0;
    std::vector<double> ret_params;
    for (int i = 0; i < num; i++) {
        std::vector<char> tmp;
        while (params[pointer] != 0x2C && pointer < params.size()) { // not ","
            tmp.push_back(params[pointer]);
            pointer++;
        }
        pointer++;
        if (is_int(tmp)) {
            ret_params.push_back(inp_int(tmp));
        }
    }
    return ret_params;
}



int execute() {
    int a = getchar();
    int sum, bopen, bclose;
    std::vector<char> params, input;
    std::vector<int> operations;

    while (a != 0x0A) {
        if (a != ' ') {
            printf("Debug: %c\n", a);
            input.push_back(a);
            }
        a = getchar();
    }
    input.push_back(';');

    int num_ops = contains(input, '(');
    int pointer = 0;
    sum = 0;
    bopen = 0;
    bclose = 0;
    a = input[pointer];
    while (a != ';') {
        printf("%c\n", a);
        //printf("%i\n", pointer);
        if (a != '(' && bopen == 0 && bclose ==  0) { // not "(", defines the operator
            sum += a;
        } else if (a == '(') {
            bopen = 1;
        } else if (a != ')' && a != ' ' && bopen == 1 && bclose == 0) { // not ")" or " ", defines the parameters
            params.push_back(a);
            //printf("Debug: %c\n", a);
        } else if (a == ')') {
            bclose = 1;
        }
        pointer++;
        a = input[pointer];
    }
    printf("0x%x\n", sum);    

    int num = num_params(params);
    int num_params;
    double ret;
    std::vector<double> values;
    switch (sum) {

        case (0x156): // pow(a, b)
            num_params = 2;
            if (correct_num_params(num, num_params) == 0) {
                return 0;
            } else {
                values = get_params(params, num_params);
                ret = pow(values[0], values[1]);
            }
            break;
        case (0x1CA): // sqrt(a)
            num_params = 1;
            if (correct_num_params(num, num_params) == 0) {
                return 0;
            } else {
                values = get_params(params, num_params);
                ret = sqrt(values[0]);
            }
            break;
        case (0x1C4): // root(a)
            num_params = 2;
            if (correct_num_params(num, num_params) == 0) {
                return 0;
            } else {
                values = get_params(params, num_params);
                ret = root(values[0], values[1]);
            }
            break;
        case (0x14A): // sin(a)
            num_params = 1;
            if (correct_num_params(num, num_params) == 0) {
                return 0;
            } else {
                values = get_params(params, num_params);
                ret = sin(values[0]);
            }
            break;
        case (0x145): // cos(a)
            num_params = 1;
            if (correct_num_params(num, num_params) == 0) {
                return 0;
            } else {
                values = get_params(params, num_params);
                ret = cos(values[0]);
            }
            break;
        case (0x143): // tan(a)
            num_params = 1;
            if (correct_num_params(num, num_params) == 0) {
                return 0;
            } else {
                values = get_params(params, num_params);
                ret = tan(values[0]);
            }
            break;
    }
    printf("%f\n\n", ret);
    return 1;
}
