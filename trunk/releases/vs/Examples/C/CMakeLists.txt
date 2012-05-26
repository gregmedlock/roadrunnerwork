set(target rr_c_api)


if(NOT BUILD_SHARED_LIBS)
set(BUILD_SHARED_LIBS 	ON)
endif()

add_library(${target} 
rr_c_api.cpp
)

#Link C_api dll statically or dynamically to roadrunner and libsbml
IF(${BORLAND})
#target_link_libraries (${target} rr)
#target_link_libraries (${target} libsbml)
add_definitions(-DEXPORT_RR_C_API)
ELSE()
add_definitions(-DEXPORT_RR_C_API)
target_link_libraries (${target} roadrunner-static)
target_link_libraries (${target} libsbml-static)
target_link_libraries (${target} libxml2)
target_link_libraries (${target} sundials_cvode)
target_link_libraries (${target} sundials_nvecserial)
target_link_libraries (${target} blas)
target_link_libraries (${target} lapack)
target_link_libraries (${target} libf2c)
ENDIF()

#add_subdirectory(Examples)

#set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${exe_path}/${target}.tds)
#=============== INSTALL =====================
install (FILES ${target}.h 					DESTINATION include/c_api  	COMPONENT c_api)
install (FILES ${lib_path}/${target}.lib	DESTINATION lib 			COMPONENT c_api)
install (FILES ${exe_path}/${target}.dll	DESTINATION bin 			COMPONENT c_api)

#=== dependencies
set(SBML_DEPENDENCIES libxml2.dll iconv.dll zlib1.dll)
foreach(system_file ${SBML_DEPENDENCIES})
install (FILES ${THIRD_PARTY_FOLDER}/libsbml_dependencies/bin/${system_file}	DESTINATION bin COMPONENT c_api)
endforeach()

set(SBML_DEPENDENCIES libxml2)
foreach(system_file ${SBML_DEPENDENCIES})
if(${BORLAND})
install (FILES ${THIRD_PARTY_FOLDER}/libsbml_dependencies/lib/${system_file}_xe.lib	DESTINATION lib COMPONENT c_api)
else()
install (FILES ${THIRD_PARTY_FOLDER}/libsbml_dependencies/lib/${system_file}.lib	DESTINATION lib COMPONENT c_api)
endif()
endforeach()
