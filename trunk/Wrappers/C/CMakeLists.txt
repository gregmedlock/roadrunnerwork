set(copy_rr_dll ${BUILD_SHARED_LIBS})

set(target rrC_API)
if(NOT BUILD_SHARED_LIBS)
set(BUILD_SHARED_LIBS 	ON)
endif()

add_library(		  ${target} rrC_API.cpp)

#Link C_api dll statically or dynamically to roadrunner and libsbml
#IF(${BUILD_SHARED_FILES})
#target_link_libraries (${target} rr)
#target_link_libraries (${target} libsbml)
#ELSE()
target_link_libraries (${target} rr-static)
target_link_libraries (${target} libsbml-static)
target_link_libraries (${target} libxml2_xe)
target_link_libraries (${target} sundials_cvode)
target_link_libraries (${target} sundials_nvecserial)
target_link_libraries (${target} blas)
target_link_libraries (${target} lapack)
target_link_libraries (${target} libf2c)
#ENDIF()

IF(${BORLAND})
add_definitions(-DEXPORT_RR_C_API)
ENDIF()

add_subdirectory(Examples)

#set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${exe_path}/${target}.tds)
#=============== INSTALL =====================
install (FILES ${target}.h 					DESTINATION include/rr_C_API  COMPONENT c_api)
install (FILES ${lib_path}/${target}.lib	DESTINATION link 			COMPONENT c_api)
install (FILES ${exe_path}/${target}.dll	DESTINATION bin 			COMPONENT c_api)


#=== dependencies
set(SBML_DEPENDENCIES libxml2.dll iconv.dll zlib1.dll)
foreach(system_file ${SBML_DEPENDENCIES})
install (FILES ${THIRD_PARTY_FOLDER}/libsbml_dependencies/bin/${system_file}	DESTINATION bin COMPONENT c_api)
endforeach()

set(SBML_DEPENDENCIES libxml2)
foreach(system_file ${SBML_DEPENDENCIES})
install (FILES ${THIRD_PARTY_FOLDER}/libsbml_dependencies/lib/${system_file}.lib	DESTINATION link COMPONENT c_api)
install (FILES ${THIRD_PARTY_FOLDER}/libsbml_dependencies/lib/${system_file}_xe.lib	DESTINATION link COMPONENT c_api)
endforeach()
