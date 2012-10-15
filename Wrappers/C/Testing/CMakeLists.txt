CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(RR_C_API_Tests)

set(target c_api_tests)
set(tests
core/base
)
add_executable( ${target} 
main.cpp
${tests}
)

target_link_libraries (${target} 
rr_c_api
unit_test-static
roadrunner
)

link_directories(
${THIRD_PARTY_INSTALL_FOLDER}/lib
)


include_directories(
${RR_ROOT}
${THIRD_PARTY_FOLDER}/unit_test/src
)

if(${BORLANDC})

#Have the test automatically run as part of the build
#This took 2 days to figure out :(.. don't work with VS..
set( resultFileFolder "${CMAKE_SOURCE_DIR}/wiki/reports")

add_custom_target(test PACKAGE
    COMMAND ${exe_path}/${target}.exe ${resultFileFolder}
)
 
endif()
    
install(
        TARGETS ${target} 		
        DESTINATION bin 
        COMPONENT testing)


