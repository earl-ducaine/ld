

add_definitions(-DSTDC_HEADERS)
add_definitions(-DHAVE_REGEX_H)
add_definitions(-DHAVE_SYS_WAIT_H)
add_definitions(-DM4=m4)
add_definitions(-DHAVE_LIMITS_H)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=gnu99 -Wall -Wextra -Wno-packed-bitfield-compat -ggdb -ggdb -Og")
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -fPIC -Og")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fPIC -Og")

# cmake -DCONFIG_2X2=ON SDL_VERSION=1 .

option(CONFIG_2X2 "CONFIG_2X2" ON)
option(SDL_VERSION "SDL_VERSION" 2)

cmake_minimum_required(VERSION 2.8.9)
project(ecl_implemented_sh)
set(CMAKE_BUILD_TYPE Release)

add_subdirectory(src)
