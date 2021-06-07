#include <iostream>
#include <strings.h>
#include "test.h"

class Person {
public:
    std::string name;
    int age;

    Person(std::string iname, int iage) {
        age = iage;
        name = iname;
    }

    void birthday() {
        age++;
        std::cout << "Happy Birthday\n";
    }
};


int main() {
    LOG LOG;
    Person Borris("Borris", 24);
    std::cout << Borris.age << '\n';
    Borris.birthday();
    std::cout << Borris.age << '\n';
    LOG.INFO(Borris.name + " is my name");
    return 0;
}