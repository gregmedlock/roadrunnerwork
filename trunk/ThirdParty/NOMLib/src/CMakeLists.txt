set(target NOMLib)

#Link with libsbml dynamic library
link_libraries(${target} libsbml-static)
link_libraries(${target} libxml2_xe)

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


