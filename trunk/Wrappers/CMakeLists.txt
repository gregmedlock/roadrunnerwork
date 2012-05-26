CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(rrWrappers)
set(RR_INCLUDE_ROOT "../src")

# rr Includes
include_directories(${RR_INCLUDE_ROOT})
include_directories(${LIBSBML_INSTALL_FOLDER}/include)
include_directories(${LIBSBML_INSTALL_FOLDER}/include/sbml)
include_directories(${CVODE_INSTALL_FOLDER}/include)
include_directories(${CVODE_INSTALL_FOLDER}/include/cvode)

if(${MSVC})
endif(${MSVC})

if(BUILD_C_API)
set(wrapper "C")
 add_subdirectory(${wrapper})
 install (FILES ${wrapper}/Readme.txt 			DESTINATION Examples/${wrapper}	COMPONENT c_wrapper)
 install (FILES ${wrapper}/CMakeLists.txt 		DESTINATION Examples/${wrapper}	COMPONENT c_wrapper)
endif()

IF( PYTHON_WRAPPER )
set(wrapper "Python")
    add_subdirectory( ${wrapper} )
 	install (FILES ${wrapper}/Readme.txt 			DESTINATION Examples/${wrapper}	COMPONENT python_wrapper)
 	install (FILES ${wrapper}/CMakeLists.txt 		DESTINATION Examples/${wrapper}	COMPONENT python_wrapper)
ENDIF()

add_subdirectory(Delphi)
install (FILES Readme.txt 			DESTINATION Examples COMPONENT wrappers)
install (FILES CMakeLists.txt 		DESTINATION Examples COMPONENT wrappers)
