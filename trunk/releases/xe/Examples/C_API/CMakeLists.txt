CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

PROJECT(RR_C_API_Examples)
set(C_API_INCLUDE_ROOT "${RR_ROOT}/Wrappers/C")

# rr Includes
include_directories(${C_API_INCLUDE_ROOT})

if(${MSVC})
endif(${MSVC})

set(examples 	
		rr_c
#		simulate_sbml
)

foreach(example ${examples})
 	add_subdirectory(${example})
# install (FILES ${example}/${example}.cbproj 	DESTINATION Examples/${example} COMPONENT example_files)
	FILE (GLOB hdrs ${example}/*.h)
 	install (FILES ${hdrs} 						DESTINATION Examples/C_API/${example}	COMPONENT example_files)
	FILE (GLOB src ${example}/*.c)
 	install (FILES ${src} 						DESTINATION Examples/C_API/${example}	COMPONENT example_files)
 	install (FILES ${example}/Readme.txt 		DESTINATION Examples/C_API/${example}	COMPONENT example_files)
 	install (FILES ${example}/CMakeLists.txt 	DESTINATION Examples/C_API/${example}	COMPONENT example_files)
endforeach(example)

install (FILES Readme.txt 			DESTINATION Examples/C_API COMPONENT doc_file)
install (FILES CMakeLists.txt 		DESTINATION Examples/C_API COMPONENT example_files)
