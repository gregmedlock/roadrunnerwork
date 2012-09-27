CMAKE_MINIMUM_REQUIRED(VERSION 2.8)
project(poco-foundation)

# Option that allows users to build static and/or shared libraries
# ----------------------------------------------------------------
OPTION(POCO_BUILD_STATIC_LIBS "Build static libraries" ON)
OPTION(POCO_BUILD_SHARED_LIBS "Build shared libraries" OFF)
set(target poco_foundation) 

set(poco_include_folder ${CMAKE_CURRENT_SOURCE_FOLDER}../include)
set(sources 
Thread
Runnable
Exception
Mutex
ErrorHandler
Bugcheck
ThreadLocal
Debugger
)


if(${BORLAND})
set(SUPPRESS_WARNINGS "-w-8128 -w-8057 -w-8012")
endif()

if(${MSVC})
#set(....
endif()

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${SUPPRESS_WARNINGS}")

include_directories(
${poco_include_folder}
)
#add_library(${target} SHARED ${sources})
#
#set_property(TARGET ${target}  
#             PROPERTY  COMPILE_DEFINITIONS 
#                       WIN32
#                       SHARED_NLEQ
#             ) 
#
#Then the static version
add_library(${target}-static STATIC ${sources})
set_property(TARGET ${target}-static  
             PROPERTY  COMPILE_DEFINITIONS 
                       WIN32
                       STATIC_POCO_FOUNDATION
             ) 

link_directories(				    
                 ${THIRD_PARTY_BUILD_FOLDER}/lib
                 )

#libraries to link with

#Install required files
install(FILES ${headers} 
        DESTINATION include/poco)

#install(TARGETS ${target} 
#        LIBRARY DESTINATION lib 
#        ARCHIVE DESTINATION lib
#        RUNTIME DESTINATION bin)
#
install(TARGETS ${target}-static 
        LIBRARY DESTINATION lib 
        ARCHIVE DESTINATION lib)

