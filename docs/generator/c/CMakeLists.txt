cmake_minimum_required(VERSION 4.1)
project(untitled C)

set(CMAKE_C_STANDARD 90)

add_executable(engine ../engine/c/engine_main.c ../engine/c/engine.c)
target_include_directories(engine PRIVATE ../engine/c)