#include "classes.h"

#include <iostream>

myclass::myclass() {}
myclass::~myclass() {}
    
void myclass::method(int a, int b) const {
    std::cout << "hello method" << std::endl;
}
