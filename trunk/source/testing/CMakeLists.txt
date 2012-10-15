CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(RR_CXX_API_Tests)

set(target cxx_api_tests)
set(tests
core/base
)
add_executable( ${target} 
main.cpp
${tests}
)

target_link_libraries (${target} 
roadrunner-static
unit_test-static
)

link_directories(
${THIRD_PARTY_INSTALL_FOLDER}/lib
)


include_directories(
${RR_ROOT}
${THIRD_PARTY_FOLDER}/unit_test/src
)

#Have the test automatically run as part of the build
#This took 2 days to figure out :(
set( resultFileFolder "${CMAKE_SOURCE_DIR}/wiki/reports")

add_custom_target(cxx_test ALL
    COMMAND ${CMAKE_BINARY_DIR}/trunk/bin/${target}.exe ${resultFileFolder}
)
    
    
install(
        TARGETS ${target} 		
        DESTINATION bin 
        COMPONENT testing)


