project(pugixml)

cmake_minimum_required(VERSION 2.6)
#set(BUILD_SHARED_LIBS OFF CACHE BOOL "Build shared instead of static library")

set(HEADERS src/pugixml.hpp src/pugiconfig.hpp)
set(SOURCES ${HEADERS} src/pugixml.cpp)

add_library(pugixml         SHARED ${SOURCES})
add_library(pugixml-static  STATIC ${SOURCES})

set_target_properties(pugixml PROPERTIES 
                        VERSION 1.2 SOVERSION 1.2)
set_target_properties(pugixml-static PROPERTIES VERSION 1.2 SOVERSION 1.2)

install(TARGETS pugixml 
        LIBRARY DESTINATION lib 
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin)

install(TARGETS pugixml-static 
        LIBRARY DESTINATION lib 
        ARCHIVE DESTINATION lib)

install(FILES ${HEADERS} 
        DESTINATION include/pugi)
