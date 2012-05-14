include_directories()

if(${BUILD_LIBS})
add_subdirectory(Common)
add_subdirectory(C_src)
add_subdirectory(LibStructSupport)
add_subdirectory(ModelGeneration)
add_subdirectory(Scanner)
add_subdirectory(Solvers)
endif()


set(modules Common)

foreach(module ${modules})
FILE (GLOB headers ${module}/*.h)
install (FILES ${headers}			 	DESTINATION include COMPONENT cxx_headers)
endforeach(module)

#install (FILES CMakeLists.txt 			DESTINATION src/Common)
#install (FILES CMakeLists.txt 			DESTINATION src)

