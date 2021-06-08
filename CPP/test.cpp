#include <iostream>
#include <string>
#include "test.hpp"

class Person {
public:
    std::string name;
    int age;

    Person(std::string iname, int iage) {
        age = iage;
        name = iname;
    }

    ~Person() {
        std::cout << "Oh No, You died!!!\n";
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