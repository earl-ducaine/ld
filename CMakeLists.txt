

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


#set (CMAKE_SHARED_LINKER_FLAGS "--sysroot=${INTEL_GRAPHICS_SDK_BASE}/sdk")

# set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-array-bounds")

cmake_minimum_required(VERSION 2.8.9)
project(ecl_implemented_sh)
set(CMAKE_BUILD_TYPE Release)
# find_package(ecl)

# find_package(SDL REQUIRED)


#SET(ECL_LIB /usr/lib)
#SET(BOOST_INCLUDEDIR /usr/local/Cellar/boost/1.58.0/include)
#find_package(ecl REQUIRED)

#Bring the headers, such as Student.h into the project

add_subdirectory(src)


# file(GLOB SOURCES "src/*.c")
# file(GLOB SOURCES "src/main.c")

#file(GLOB flex_SOURCES "src/*.c")
# file(flex_SOURCES "src/main.c")



#Generate the shared library from the sources
# add_library(test_ecl_implemented_sh SHARED ${LIB_SOURCES})

#Set the location for library installation -- i.e., /usr/lib in this case
# not really necessary in this example. Use "sudo make install" to apply
# install(TARGETS test_ecl_implemented_sh DESTINATION /usr/lib)

# add_executable(ecl_implemented_sh "src/main.c")
#link_directories(  )


#target_link_libraries(ecl_implemented_sh test_ecl_implemented_sh m ${SDL2_LIBRARIES})

# target_link_libraries(ecl_implemented_sh m)
