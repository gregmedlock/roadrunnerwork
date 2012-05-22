FIND_PACKAGE( SWIG       )
FIND_PACKAGE( PythonLibs )
IF( SWIG_FOUND AND PYTHONLIBS_FOUND )

    # set up SWIG
    INCLUDE_DIRECTORIES( ${PYTHON_INCLUDE_PATH} )
    INCLUDE( ${SWIG_USE_FILE} )
    SET( CMAKE_SWIG_FLAGS "" )
    SET( CMAKE_SWIG_OUTDIR ${HELLO_RELEASE_DIR} )

    # set up your project
    INCLUDE_DIRECTORIES( ${HELLO_SOURCE_DIR} )
    FILE( GLOB HELLO_SOURCE_FILES
        ${HELLO_WRAPPER_DIR}/hello.i
        ${HELLO_SOURCE_DIR}/hello.h
        ${HELLO_SOURCE_DIR}/hello.c
    )

    # build low-level Python wrapper
    # (if not adding a high-level wrapper,
    #  can name this hello instead of libhello)
    SWIG_ADD_MODULE( libhello
        python
        ${HELLO_SOURCE_FILES}
    )
    SWIG_LINK_LIBRARIES( libhello ${PYTHON_LIBRARIES} )

    # copy high-level wrapper to the release dir
    # (it's not auto-generated during a build)
    FILE(
        COPY        hello.py
        DESTINATION ${HELLO_RELEASE_DIR}
    )

ELSE( SWIG_FOUND )
    MESSAGE( "SWIG not found; unable to generate Python wrapper" )
ELSE( PYTHONLIBS_FOUND )
    MESSAGE( "Python not found; unable to generate Python wrapper" )
ENDIF()
