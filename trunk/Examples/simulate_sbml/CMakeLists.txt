set(target simulate_sbml)

add_executable(		   ${target} simulate_sbml.cpp)

IF(BUILD_SHARED_LIBS) 
target_link_libraries (${target} rr_shared)
ELSE()
target_link_libraries (${target} rr_static)
ENDIF()

target_link_libraries (${target} libsbml-static)

#target_link_libraries (${target} libxml2_xe)
target_link_libraries (${target} sundials_cvode)
target_link_libraries (${target} sundials_nvecserial)
target_link_libraries (${target} lapack)
target_link_libraries (${target} blas)
target_link_libraries (${target} libf2c)

set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${exe_path}/${target}.tds)

install (TARGETS ${target} 		DESTINATION bin COMPONENT compiled_examples)
