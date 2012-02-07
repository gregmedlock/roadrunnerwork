set(target NOMLib)

#Link with libsbml dynamic library
link_directories("C:/sbw_libs/libsbml/lib")
link_libraries(${target} libsbml)

add_library(${target} ${LIB_TYPE}
NOMLib.cpp
)


if(${BUILD_SHARED_LIBS})
	add_definitions(-DEXPORT_NOM)
endif()


if($BUILD_LIBS)
	install (TARGETS ${target} 	DESTINATION libs COMPONENT library_files)
endif()

install (FILES NOMLib.h	DESTINATION include COMPONENT cxx_headers)


