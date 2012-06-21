CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(RR_C_API_Examples)
if(${MSVC})

endif(${MSVC})

set(examples 	
        api_tester
)

foreach(example ${examples})
 	add_subdirectory(${example})
	FILE (GLOB hdrs ${example}/*.h)
 	install (FILES ${hdrs} 						
                DESTINATION Examples/C_API/${example}	
                COMPONENT example_files)

	FILE (GLOB src ${example}/*.c)
 	install (FILES ${src} 						DESTINATION Examples/C_API/${example}	COMPONENT example_files)
 	install (FILES ${example}/Readme.txt 		DESTINATION Examples/C_API/${example}	COMPONENT example_files)
 	install (FILES ${example}/CMakeLists.txt 	DESTINATION Examples/C_API/${example}	COMPONENT example_files)
endforeach(example)

install (FILES Readme.txt 			DESTINATION Examples/C_API COMPONENT doc_file)
install (FILES CMakeLists.txt 		DESTINATION Examples/C_API COMPONENT example_files)
