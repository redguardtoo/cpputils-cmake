cmake_minimum_required(VERSION 2.8)
project(baselib) # Note that this project name will come before subproject ("baselib" < "subproject")

add_library(mylib SHARED
  src/classes.cpp)