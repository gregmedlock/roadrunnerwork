set(target NOMLib)

#Link with libsbml libraries
link_libraries(${target} libsbml)

if(${MSVC})
link_libraries(${target} libxml2)

IF (BUILD_SHARED_LIBS)
link_libraries(${target} libsbml)
ELSE()
link_libraries(${target} libsbml-static)
add_definitions(-DNO_SBML_DLL) 
ENDIF (BUILD_SHARED_LIBS)

endif()

if(${BORLAND})
link_libraries(${target} libxml2_xe)
endif()

add_library(${target} ${LIB_TYPE}
NOMLib.cpp
)


if(${BUILD_SHARED_LIBS})
	add_definitions(-DEXPORT_NOM)
else()	
	add_definitions(-DNO_NOM_DLL) #static lib?
endif()


if($BUILD_LIBS)
	install (TARGETS ${target} 	DESTINATION libs COMPONENT library_files)
endif()

install (FILES NOMLib.h	DESTINATION include COMPONENT cxx_headers)
install (TARGETS ${target} DESTINATION bin COMPONENT compiled_library)

