cmake_minimum_required(VERSION 2.8)
project(project_a)

add_subdirectory(subproject)
include_directories(subproject/src)
include_directories(src)

add_executable(project_a_exe
  src/main.cpp)

target_link_libraries(project_a_exe mylib)
