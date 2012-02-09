CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(NOMLibExamples)

#Link with libsbml library
link_libraries(${target} libsbml-static)


set(examples 	
		Example_1
)

foreach(example ${examples})
 add_subdirectory(${example})
 install (FILES ${example}/main.cpp 			DESTINATION Examples/${example}	COMPONENT examples_source)
 install (FILES ${example}/Readme.txt 			DESTINATION Examples/${example}	COMPONENT examples_source)
 install (FILES ${example}/CMakeLists.txt 		DESTINATION Examples/${example}	COMPONENT examples_source)
endforeach(example)

install (FILES Readme.txt 			DESTINATION Examples COMPONENT info)
install (FILES CMakeLists.txt 		DESTINATION Examples COMPONENT examples_source)
