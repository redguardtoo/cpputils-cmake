cmake_minimum_required(VERSION 2.8)
project(project_x)

add_subdirectory(project_a)
include_directories(project_a/src)
add_subdirectory(project_z)
include_directories(project_z/src)
include_directories(src)

add_executable(project_x_exe
  src/main.cpp)

target_link_libraries(project_x_exe mylib1 mylib2)
