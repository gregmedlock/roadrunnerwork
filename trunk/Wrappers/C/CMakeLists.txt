set(target rrC_API)

add_library(		  ${target} rrC_API.cpp)

#Link C_api dll statically to roadrunner and libsbml
target_link_libraries (${target} rr_static)
target_link_libraries (${target} libsbml-static)

add_subdirectory(Examples)

#set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${exe_path}/${target}.tds)
#=============== INSTALL =====================
install (FILES ${target}.h 					DESTINATION include COMPONENT c_wrapper)
install (FILES ${lib_path}/${target}.lib	DESTINATION link 	COMPONENT c_wrapper)
install (FILES ${exe_path}/${target}.dll	DESTINATION bin 	COMPONENT c_wrapper)

