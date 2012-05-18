set(target rr_c_api_info)

add_executable(		   ${target} rr_c_api_info.c)

target_link_libraries (${target} rr_c_api)

install (TARGETS ${target} 		DESTINATION bin COMPONENT compiled_examples)
