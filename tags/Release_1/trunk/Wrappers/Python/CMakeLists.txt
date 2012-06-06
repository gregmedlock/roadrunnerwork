FIND_PACKAGE(SWIG REQUIRED)
INCLUDE( ${SWIG_USE_FILE} )

FIND_PACKAGE(PythonLibs REQUIRED)
INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH})
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

INCLUDE_DIRECTORIES(
../../src
../C
)
SET( CMAKE_SWIG_FLAGS "" )
SET( CMAKE_SWIG_OUTDIR ${CMAKE_RUNTIME_OUTPUT_DIRECTORY} )

SET_SOURCE_FILES_PROPERTIES(rr_python.i PROPERTIES CPLUSPLUS ON)
SET_SOURCE_FILES_PROPERTIES(example.i PROPERTIES SWIG_FLAGS "-includeall")

set(target RoadRunner)

SWIG_ADD_MODULE(
${target}
python
rr_python.i 
)

SWIG_LINK_LIBRARIES(
${target}
${PYTHON_LIBRARIES}
roadrunner-cg
rr_c_api-cg
)


install (FILES ${CMAKE_SWIG_OUTDIR}/${target}.py		DESTINATION bin COMPONENT python)
install (FILES ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/_${target}.pyd		DESTINATION lib COMPONENT python)
    # set up SWIG
#    SET( CMAKE_SWIG_OUTDIR ${HELLO_RELEASE_DIR} )

    # set up project
#    INCLUDE_DIRECTORIES( ${HELLO_SOURCE_DIR} )
#    FILE( GLOB HELLO_SOURCE_FILES
#        ${HELLO_WRAPPER_DIR}/hello.i
#        ${HELLO_SOURCE_DIR}/hello.h
#        ${HELLO_SOURCE_DIR}/hello.c
#    )
#
    # build low-level Python wrapper
    # (if not adding a high-level wrapper,
    #  can name this hello instead of libhello)
#    SWIG_ADD_MODULE( RoadRunner
#        python
#        ${HELLO_SOURCE_FILES}
#    )
#    SWIG_LINK_LIBRARIES( libhello ${PYTHON_LIBRARIES} )

    # copy high-level wrapper to the release dir
    # (it's not auto-generated during a build)
#    FILE(
#        COPY        hello.py
#        DESTINATION ${HELLO_RELEASE_DIR}
#    )

