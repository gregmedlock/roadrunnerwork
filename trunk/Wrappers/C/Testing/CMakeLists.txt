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

install(
        TARGETS ${target} 		
        DESTINATION bin 
        COMPONENT testing)

#Have the test automatically run as part of the build
set( resultFileFolder ${CMAKE_CURRENT_SOURCE_DIR} )
set( resultFile ${} )
add_custom_command( OUTPUT ${resultFileFolder}/test.xml
    COMMAND touch ${resultFiler}
    COMMAND echo "POST_BUILD ${output}"
    COMMENT "Touching foo"
    )
