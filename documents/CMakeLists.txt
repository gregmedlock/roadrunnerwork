cmake_minimum_required(VERSION 2.8)

if(${INSTALL_CXX_DOCS})
add_subdirectory(cpp_api)
endif()

if(${INSTALL_C_DOCS})
add_subdirectory(c_api)
endif()

if(${INSTALL_PYTHON_DOCS})
add_subdirectory(python_api)
endif()

