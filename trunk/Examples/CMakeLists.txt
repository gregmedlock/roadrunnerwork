CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(rrExamples)
set(RR_INCLUDE_ROOT "../src")

# rr Includes
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

set(examples 	
		validate_sbml
		simulate_sbml
)

foreach(example ${examples})
 add_subdirectory(${example})
# install (FILES ${example}/${example}.cbproj 	DESTINATION Examples/${example} COMPONENT examples_source)
 install (FILES ${example}/${example}.cpp 		DESTINATION Examples/${example}	COMPONENT examples_source)
 install (FILES ${example}/Readme.txt 			DESTINATION Examples/${example}	COMPONENT examples_source)
 install (FILES ${example}/CMakeLists.txt 		DESTINATION Examples/${example}	COMPONENT examples_source)
endforeach(example)

#install (FILES auto_link.h 			DESTINATION Examples COMPONENT examples_source)
install (FILES Readme.txt 			DESTINATION Examples COMPONENT info)
#install (FILES Examples.groupproj 	DESTINATION Examples COMPONENT examples_source)
install (FILES CMakeLists.txt 		DESTINATION Examples COMPONENT examples_source)
