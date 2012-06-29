set(target rr_c_api)

set(C_API_INCLUDE_ROOT "${RR_ROOT}/Wrappers/C")
include_directories(${C_API_INCLUDE_ROOT})

add_library(${target} SHARED
rr_c_api.cpp
rr_dll_entry_point.cpp
rr_c_api_support.cpp
)

set_property(   TARGET ${target} 
                PROPERTY  COMPILE_DEFINITIONS 
                          EXPORT_RR_C_API
                          STATIC_RR             #when linking with static lib, no declspec shuld be present..
                          LIBSBML_STATIC
					      LIBLAX_STATIC
)

IF(${BORLAND})
target_link_libraries (${target} libxml2_xe)
ELSE()
target_link_libraries (${target} libxml2)
ENDIF()

#Link C_api dll statically to roadrunner and libsbml
if(${MINGW})
target_link_libraries (${target} roadrunner)
else()
target_link_libraries (${target} roadrunner-static)
target_link_libraries (${target} blas)
target_link_libraries (${target} lapack)
target_link_libraries (${target} libf2c)
endif()

INCLUDE(FindSubversion)
IF(SUBVERSION_FOUND)
    Subversion_WC_INFO(${PROJECT_SOURCE_DIR} rr)
    MESSAGE("Current revision is ${rr_WC_REVISION}")
    Subversion_WC_LOG(${PROJECT_SOURCE_DIR} rr)
    MESSAGE("Last changed log is ${rr_LAST_CHANGED_LOG}")
    MESSAGE("Last changed Author is ${rr_WC_LAST_CHANGED_AUTHOR}")

    SET(infoFile rr_svn_info)
    FILE(WRITE ${infoFile}.txt 
"#ifndef ${infoFile}H
#define ${infoFile}H
const int SVN_VERSION =  ${rr_WC_REVISION};\n
const char* SVN_LAST_COMMIT_AUTHOR =\"${rr_WC_LAST_CHANGED_AUTHOR}\";\n
#endif"
    )
#const char* SVN_LASTLOG =\"    ${rr_LAST_CHANGED_LOG}\";\n

    EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} -E copy_if_different ${infoFile}.txt ${infoFile}.h)
ENDIF(SUBVERSION_FOUND)


add_subdirectory(Apps)
add_subdirectory(Examples)

#=============== INSTALL =====================
install (   FILES ${target}.h 					
            DESTINATION include/c_api  	
            COMPONENT c_api)

install (   FILES rr_c_types.h 				
            DESTINATION include/c_api  
            COMPONENT c_api)

install (   
            TARGETS ${target}
            RUNTIME DESTINATION bin
            LIBRARY DESTINATION lib
            ARCHIVE DESTINATION lib
            COMPONENT c_api)

install (   FILES ${lib_path}/${target}.lib	
            DESTINATION lib 			
            COMPONENT c_api)

install (   FILES ${exe_path}/${target}.dll	
            DESTINATION bin 			
            COMPONENT c_api)


#set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${exe_path}/${target}.tds)
