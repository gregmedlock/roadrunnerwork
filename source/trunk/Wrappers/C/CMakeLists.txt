set(target rr_c_api)

add_library(${target} SHARED
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

#Link C_api dll statically to roadrunner and libsbml (don't work for MinGW)
if(${MINGW})
target_link_libraries (${target} roadrunner)
else()
target_link_libraries (${target} roadrunner-static)
endif()
#target_link_libraries (${target} libsbml-static)
#target_link_libraries (${target} sundials_cvode)
#target_link_libraries (${target} sundials_nvecserial)
#target_link_libraries (${target} blas)
#target_link_libraries (${target} lapack)
#target_link_libraries (${target} libf2c)
#
add_subdirectory(Examples)

#=============== INSTALL =====================

INSTALL(TARGETS ${target} 
            RUNTIME DESTINATION bin
            LIBRARY DESTINATION lib
            ARCHIVE DESTINATION lib
            COMPONENT c_api)

#install (   FILES ${lib_path}/${target}.lib	
#            DESTINATION lib 			
#            COMPONENT c_api)
#
#install (   FILES ${exe_path}/${target}.dll	
#            DESTINATION bin 			
#            COMPONENT c_api)
#

install (FILES ${target}.h 	rr_c_types.h 				
				
            DESTINATION include/c_api  	
            COMPONENT c_api)
            
