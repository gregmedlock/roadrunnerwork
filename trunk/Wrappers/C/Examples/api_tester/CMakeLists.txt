CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
set(target api_tester)

add_executable( ${target} main.cpp)
target_link_libraries (${target} rr_c_api)

install(
        TARGETS ${target} 		
        DESTINATION bin 
        COMPONENT compiled_examples)
