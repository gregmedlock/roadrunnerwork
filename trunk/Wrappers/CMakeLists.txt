CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(rrWrappers)
set(RR_INCLUDE_ROOT "../src")

# rr Includes
include_directories(${RR_INCLUDE_ROOT})
include_directories(${RR_INCLUDE_ROOT}/Common)
include_directories(${RR_INCLUDE_ROOT}/LibStructSupport)
include_directories(${RR_INCLUDE_ROOT}/LibStructSupport/libstruct)
include_directories(${RR_INCLUDE_ROOT}/ModelGeneration)
include_directories(${RR_INCLUDE_ROOT}/Scanner)
include_directories(${RR_INCLUDE_ROOT}/Solvers)
include_directories(${RR_INCLUDE_ROOT}/C_src)
include_directories(${SBML_INSTALL_FOLDER}/include)
include_directories(${SBML_INSTALL_FOLDER}/include/sbml)
include_directories(${CVODE_INSTALL_FOLDER}/include/cvode)


if(${MSVC})
endif(${MSVC})

set (wrappers 	C )

foreach(wrapper ${wrappers})
 add_subdirectory(${wrapper})
 install (FILES ${wrapper}/Readme.txt 			DESTINATION Examples/${wrapper}	COMPONENT wrappers_source)
 install (FILES ${wrapper}/CMakeLists.txt 		DESTINATION Examples/${wrapper}	COMPONENT wrappers_source)
endforeach(wrapper)

 install (FILES ${wrapper}/${wrapper}.cpp 		DESTINATION Examples/${wrapper}	COMPONENT wrappers_source)
#install (FILES auto_link.h 			DESTINATION Examples COMPONENT wrappers_source)
install (FILES Readme.txt 			DESTINATION Examples COMPONENT info)
#install (FILES Examples.groupproj 	DESTINATION Examples COMPONENT wrappers_source)
install (FILES CMakeLists.txt 		DESTINATION Examples COMPONENT wrappers_source)
