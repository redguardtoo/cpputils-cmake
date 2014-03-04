#include "moreclasses.h"

#include <iostream>

moremyclass::moremyclass() {}
moremyclass::~moremyclass() {}

void moremyclass::anothermethod(int a, int b) const {
    std::cout << "hello method" << std::endl;
}
