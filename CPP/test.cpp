int execute() {
    int a = getchar();
    int sum, bopen, bclose;
    std::vector<char> params, input;
    std::vector<int> operations;

    while (a != 0x0A) {
        if (a != ' ') {
            input.push_back(a);
            }
        a = getchar();
    }
    input.push_back(';');

    int num_ops = contains(input, '{');
    int pointer = 0;
    sum = 0;
    bopen = 0;
    bclose = 0;
    a = input[pointer];
    while (a != ';') {
        if (a != '{' && bopen == 0 && bclose ==  0) { // not "(", defines the operator
            sum += a;
        } else if (a == '{') {
            bopen = 1;
        } else if (a != '}' && a != ' ' && bopen == 1 && bclose == 0) { // not ")" or " ", defines the parameters
            params.push_back(a);
        } else if (a == '}') {
            bclose = 1;
        }
        pointer++;
        a = input[pointer];
    }

    int num = get_num_params(params);
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
