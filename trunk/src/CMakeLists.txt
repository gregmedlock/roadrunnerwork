include_directories()

if(${BUILD_LIBS})
add_subdirectory(Common)
endif()


set(modules Common)

foreach(module ${modules})
FILE (GLOB headers ${module}/*.h)
install (FILES ${headers}			 	DESTINATION src/${module} COMPONENT cxx_headers)
endforeach(module)

install (FILES CMakeLists.txt 			DESTINATION src/Common)
install (FILES CMakeLists.txt 			DESTINATION src)

