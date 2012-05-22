# this is unrelated to SWIG;
# it just builds a separate main program
# to verify that the C code works

INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR} )

FILE( GLOB HELLO_SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/*.h
    ${CMAKE_CURRENT_SOURCE_DIR}/*.c
)

ADD_EXECUTABLE( hello ${HELLO_SOURCE_FILES} )
