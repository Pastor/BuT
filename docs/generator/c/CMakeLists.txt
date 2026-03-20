cmake_minimum_required(VERSION 4.1)
project(untitled C)

set(CMAKE_C_STANDARD 90)

add_executable(engine ../engine/c/engine_main.c ../engine/c/engine.c)
target_include_directories(engine PRIVATE ../engine/c)

add_executable(position ../position/c/position_main.c ../position/c/position.c)
target_include_directories(position PRIVATE ../position/c)