#include <iostream>
#include <string>

class LOG {
public:
    const int INFO_LEVEL = 0;
    const int WARN_LEVEL = 1;
    const int ERRO_LEVEL = 2;
    int LOG_LEVEL = 0;

    void INFO(std::string x) {
        if (LOG_LEVEL <= 0){
            std::cout << "[INFO]: " << x << std::endl;
        }
    }

    void WARN(std::string x) {
        if (LOG_LEVEL <= 1) {
            std::cout << "[WARNING]: " << x << std::endl;
        }
    }

    void ERROR(std::string x) {
        std::cout << "[ERROR]: " << x << std::endl;
    }

};