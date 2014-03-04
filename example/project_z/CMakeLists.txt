cmake_minimum_required(VERSION 2.8)
project(project_z) # Note that this project name will come after subproject ("project_x" < "project_z")

add_library(mylib2 SHARED
  src/moreclasses.cpp)
