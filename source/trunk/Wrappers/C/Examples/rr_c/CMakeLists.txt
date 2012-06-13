set(target rr_c)

add_executable( ${target} 
main.cpp
Args.cpp
GetOptions
../../rr_c_api.cpp
../../rr_c_api_support.cpp
)

remove_definitions(
-DEXPORT_RR
#-DNO_RR_DLL 
)
add_definitions(
-DSTATIC_RR_C_API
)

target_link_libraries (${target} 
roadrunner
)

install(
        TARGETS ${target} 		
        DESTINATION bin 
        COMPONENT compiled_examples)
