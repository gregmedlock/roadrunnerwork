CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(rrExamples)
set(RR_INCLUDE_ROOT "../src")

# rr Includes
include_directories(${RR_INCLUDE_ROOT})
include_directories(${RR_INCLUDE_ROOT}/libstruct)
include_directories(${RR_INCLUDE_ROOT}/C_src)
include_directories(${SBML_INSTALL_FOLDER}/include)
include_directories(${SBML_INSTALL_FOLDER}/include/sbml)
include_directories(${CVODE_INSTALL_FOLDER}/include/cvode)

if(${MSVC})
endif(${MSVC})

set(examples 	
	validate_sbml
)

foreach(example ${examples})
 	add_subdirectory(${example})
	FILE (GLOB hdrs ${example}/*.h)
 	install (FILES ${hdrs} 						DESTINATION Examples/${example}	COMPONENT example_files)
	FILE (GLOB src ${example}/*.cpp)
 	install (FILES ${src} 						DESTINATION Examples/${example}	COMPONENT example_files)
 	install (FILES ${example}/Readme.txt 			DESTINATION Examples/${example}	COMPONENT example_files)
 	install (FILES ${example}/CMakeLists.txt 		DESTINATION Examples/${example}	COMPONENT example_files)
endforeach(example)

#install (FILES auto_link.h 			DESTINATION Examples COMPONENT example_files)
install (FILES Readme.txt 			DESTINATION Examples COMPONENT info)
#install (FILES Examples.groupproj 	DESTINATION Examples COMPONENT example_files)
install (FILES CMakeLists.txt 		DESTINATION Examples COMPONENT example_files)
