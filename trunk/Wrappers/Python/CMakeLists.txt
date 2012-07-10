PROJECT( HELLO )
CMAKE_MINIMUM_REQUIRED( VERSION 2.8 )

SET( HELLO_ROOT_DIR         ${CMAKE_CURRENT_SOURCE_DIR} )
SET( C_API_SOURCE_DIR       "C:/rrSource/Wrappers/C")
SET( C_API_RELEASE_DIR      ${HELLO_ROOT_DIR}/release   )
SET( EXECUTABLE_OUTPUT_PATH ${HELLO_RELEASE_DIR}        )
SET( LIBRARY_OUTPUT_PATH    ${HELLO_RELEASE_DIR}        )

#ADD_SUBDIRECTORY( wrapper )

FIND_PACKAGE( SWIG )
IF( SWIG_FOUND )
    FIND_PACKAGE( PythonLibs )
    IF( PYTHONLIBS_FOUND )

    # set up SWIG
    INCLUDE_DIRECTORIES( 
        ${PYTHON_INCLUDE_PATH} 
        "C:/rrSource/source/"
       )
    INCLUDE( ${SWIG_USE_FILE} )
    SET( CMAKE_SWIG_FLAGS "" )
    SET( CMAKE_SWIG_OUTDIR ${HELLO_RELEASE_DIR} )

    # build wrapper
    INCLUDE_DIRECTORIES( ${HELLO_SOURCE_DIR} )
    SWIG_ADD_MODULE( hello_swig
        python
        rrPython.i
        ${C_API_SOURCE_DIR}/rr_c_api.h
        ${C_API_SOURCE_DIR}/rr_c_api.cpp
    )
    SWIG_LINK_LIBRARIES( hello_swig ${PYTHON_LIBRARIES} )

    # copy the manually written object-oriented wrapper
    # to the release dir so everything's in one place
    FILE(
        COPY hello_oo.py
        DESTINATION ${C_API_RELEASE_DIR}
    )

    ELSE( PYTHONLIBS_FOUND )
        MESSAGE( "Python not found; unable to generate Python wrapper" )
    ENDIF( PYTHONLIBS_FOUND )
ELSE( SWIG_FOUND )
    MESSAGE( "SWIG not found; unable to generate Python wrapper" )
ENDIF( SWIG_FOUND )
