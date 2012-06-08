set(target rr_c_api)


set(BUILD_SHARED_LIBS 	ON)

add_library(${target} 
rr_c_api.cpp
rr_dll_entry_point.cpp
rr_c_api_support.cpp
)

add_definitions(-DEXPORT_RR_C_API)

IF(${BORLAND})
target_link_libraries (${target} libxml2_xe)
ELSE()
target_link_libraries (${target} libxml2)
ENDIF()

#Link C_api dll statically to roadrunner and libsbml

target_link_libraries (${target} roadrunner-static)
#target_link_libraries (${target} libsbml-static)
#target_link_libraries (${target} sundials_cvode)
#target_link_libraries (${target} sundials_nvecserial)
#target_link_libraries (${target} blas)
#target_link_libraries (${target} lapack)
#target_link_libraries (${target} libf2c)
#
add_subdirectory(Examples)

#set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${exe_path}/${target}.tds)
#=============== INSTALL =====================
install (FILES ${target}.h 					DESTINATION include/c_api  	COMPONENT c_api)
install (FILES rr_c_types.h 				DESTINATION include/c_api  	COMPONENT c_api)
install (FILES ${lib_path}/${target}.lib	DESTINATION lib 			COMPONENT c_api)
install (FILES ${exe_path}/${target}.dll	DESTINATION bin 			COMPONENT c_api)


