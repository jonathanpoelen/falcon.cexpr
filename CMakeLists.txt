project(cstmt)
cmake_minimum_required(VERSION 2.8)


include(CMakeDefinitions.txt)

# if (NOT "${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
#   set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -iquote ${PROJECT_SOURCE_DIR}/")
# endif()

# set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g")
# set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE}")

include_directories(include/)
include_directories(modules/falcon.cxx/include/)

# aux_source_directory(. SRC_LIST)
# file(GLOB headers_SRC "*.hpp")


# Tests
function(add_executable_test name)
  add_executable(${name}_test test/${name}_test.cpp)
  add_test(all_tests ${name}_test)
endfunction()

add_executable_test(cstmt)

enable_testing()


# install(DIRECTORY ${PROJECT_SOURCE_DIR}/include/cstmt DESTINATION .)
