cmake_minimum_required(VERSION 2.8)

set(target validate_sbml)

add_executable(		   ${target} validate_sbml.cpp util.c)

target_link_libraries (${target} roadrunner)
#target_link_libraries (${target} libsbml-static)
#target_link_libraries (${target} libxml2_xe)
#
set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${exe_path}/${target}.tds)

install (TARGETS ${target} 		DESTINATION bin COMPONENT compiled_examples)
