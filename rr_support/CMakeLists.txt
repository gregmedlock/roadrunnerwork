cmake_minimum_required(VERSION 2.8)
project(rr_support)
set(SUPPORT_FOLDER 			${CMAKE_CURRENT_SOURCE_DIR})

#=== Install support code for on the fly compilation
FILE(GLOB rr_support *.*)
install(FILES ${rr_support} 
        DESTINATION rr_support
        COMPONENT rr_core) 

