CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

set(target rr_nleq) 

include_directories(
. 
${THIRD_PARTY_FOLDER}/clapack/3.2.1/INCLUDE
)

set(sources 
linalg_nleq1.c
nleq1.c
wnorm.c
zibconst.c
zibmon.c
zibsec.c
)

add_library(${target} 	SHARED ${sources})

set_property(TARGET ${target}  
                PROPERTY    COMPILE_DEFINITIONS 
                            WIN32 
                            ) 

#libraries to link with our shared lib
if(${MINGW})
else()
target_link_libraries (${target}
                                    libf2c
                      )
endif()

#libraries to link with static lib

#--- INSTALL STUFF

#=== Targets
install(    TARGETS ${target} 
            RUNTIME DESTINATION bin
            LIBRARY DESTINATION lib
            ARCHIVE DESTINATION lib
            COMPONENT rr_core
)            

if(${MINGW})
    SET(linkLibrary ${lib_path}/lib${target}.dll.a)
    SET(sharedLibrary ${exe_path}/lib${target}.dll)
else()
    SET(linkLibrary ${lib_path}/${target}.lib)
    SET(sharedLibrary ${exe_path}/${target}.dll)
endif()


#=== Shared target
    install (   FILES ${linkLibrary}	
                DESTINATION lib 
                COMPONENT rr_core)

    install (   FILES ${sharedLibrary}	
                DESTINATION bin 
                COMPONENT rr_core)

#=== HEADERS
set(subfolders 
.
)

foreach(subfolder ${subfolders})
    FILE (GLOB headers ${subfolder}/*.h)
    install (   FILES ${headers}			 		
                DESTINATION include/rr 
                COMPONENT rr_core)
endforeach(subfolder)
