include_directories()

if(${BUILD_LIBS})
add_subdirectory(Common)
endif()


set(modules )

foreach(module ${modules})
FILE (GLOB headers ${module}/*.h)
install (FILES ${headers}			 	DESTINATION Source/${module} COMPONENT cxx_headers)
endforeach(module)

install (FILES CMakeLists.txt 			DESTINATION Source/Common)
install (FILES CMakeLists.txt 			DESTINATION Source)

