cmake_minimum_required(VERSION 2.8)
project(project_a) # Note that this project name will come before subproject ("project_a" < "project_x")

add_library(mylib1 SHARED
  src/classes.cpp)
