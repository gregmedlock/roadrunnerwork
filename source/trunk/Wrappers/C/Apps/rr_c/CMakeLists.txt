cmake_minimum_required(VERSION 2.8)
set(target rr_c)

add_executable(		   ${target} 
main.cpp 
Args.cpp
GetOptions.cpp
)

target_link_libraries (${target} rr_c_api)

install(    TARGETS ${target} 		
            DESTINATION bin 
            COMPONENT apps)
