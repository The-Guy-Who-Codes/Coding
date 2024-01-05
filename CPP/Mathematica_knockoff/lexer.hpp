#pragma once
#include <cstdio>
#include <stdlib.h>
#include <vector>
#include <string>
#include <iostream>
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

int get_num_params(std::vector<char> params) {
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

int find(std::vector<std::string> input, std::string value) {
    for (int i = 0; i < input.size(); i++) {
        if (input[i] == value) {
            return i;
        }
    }
    return -1;
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

void push_token(std::string* token, std::string* token_val, std::vector<std::string>* output) {
    output->push_back(*token);
    output->push_back(*token_val);
    token_val->erase(0, token_val->size());
    token->erase(0, token->size());
}

std::vector<std::string> tokenise(std::string input) {
    std::string token, token_val, number;
    int is_float = 0;
    std::vector<std::string> output;
    int i = 0;
    int open = 0;
    while (i < input.size() - 1) {
        if (input[i] > 'A' && input[i] < 'z') {
            token_val.append(1, input[i]);

            if (!(input[i + 1] > 'A' && input[i + 1] < 'z') && input[i + 1] != '{') {
                token = "_var";
                push_token(&token, &token_val, &output);
            }

        } else if (input[i] == '{') {
            open++;
            token = "_func";
            push_token(&token, &token_val, &output);
            token = "_param";
            i++;
            while (open != 0) {
                if (input[i] == '{') {
                    open++;
                } else if (input[i] == '}') {
                    open--;
                }
                token_val.append(1, input[i]);
                i++;
            }
            token_val.pop_back();
            push_token(&token, &token_val, &output);
            i--;
            token = "_eparam";
            output.push_back(token);
            token.erase(0, token.size());

        } else if (input[i] == ',') {
            token = "_nparam";
            output.push_back(token);
            token.erase(0, token.size());

        } else if (((input[i] - 48) >= 0 && (input[i] - 48) <= 9) || input[i] == '.') {
            if (input[i] == '.') {
                is_float = 1;
                number.append(1, input[i]);
            } else {
                number.append(1, input[i]);
            }
            if (!((input[i + 1] - 48) >= 0 && (input[i + 1] - 48) <= 9) && input[i + 1] != '.') {
                if (is_float == 1) {
                    token = "_float";
                } else {
                    token = "_int";
                }
                push_token(&token, &number, &output);
                is_float = 0;
            }

        } else if (input[i] == '+' || input[i] == '-' || input[i] == '*' || input[i] == '/' || input[i] == '!') {
            token = "_op";
            token_val = input[i];
            push_token(&token, &token_val, &output);

        } else if (input[i] == '(') {
            token = "_obrack";
            output.push_back(token);
            token.erase(0, token.size());

        } else if (input[i] == ')') {
            token = "_cbrack";
            output.push_back(token);
            token.erase(0, token.size());

        }
        i++;
    }

    return output;
}

std::vector<std::string> tokenise_input() {
    std::string input;
    std::vector<std::string> output, tmp;
    char a = getchar();
    while (a != 0x0A) {
    if (a != ' ') {
        input.append(1, a);
        }
    a = getchar();
    }
    input.append(";");
    output = tokenise(input);
    int i = find(output, "_param");
    
    while (i != -1) {
        tmp = tokenise(output[i + 1] + ';');
        output.erase(output.begin() + i, output.begin() + i + 2);
        for (int j = 0; j < tmp.size(); j++) {
            output.insert(output.begin() + i + j, tmp[j]);
        }
        i = find(output, "_param");

    }
    
    std::cout << '\n';  
    for (int j = 0; j < output.size(); j++) {
        std::cout << output[j] << ", ";       
    }
    std::cout << '\n';

    return output;

    

}

