cmake_minimum_required(VERSION 2.8)
project(rr)

SET(RR_MAJOR_VERSION 2012)
SET(RR_MINOR_VERSION 5)
SET(RR_BUILD_VERSION 31)
SET(RR_VERSION "${RR_MAJOR_VERSION}.${RR_MINOR_VERSION}.${RR_BUILD_VERSION}")

#These will be the defaults ...
SET(RR_BUILDS_ROOT "C:/rrBuilds")
SET(RR_INSTALLS_ROOT "C:/rrInstalls")

if(${BORLAND})
set(FOLDER_POSTFIX "xe")
else(${BORLAND})
set(FOLDER_POSTFIX "vs")
endif()

set(THIRD_PARTY_INSTALL_FOLDER ${RR_INSTALLS_ROOT}/${FOLDER_POSTFIX} CACHE PATH "Directory containing ThirdParty libraries.")
if("${THIRD_PARTY_INSTALL_FOLDER}" STREQUAL "")
  set(THIRD_PARTY_INSTALL_FOLDER ${RR_INSTALLS_ROOT}/${FOLDER_POSTFIX} CACHE PATH "Directory containing ThirdParty libraries" FORCE)
endif()

set(THIRD_PARTY_BUILD_FOLDER ${RR_BUILDS_ROOT}/${FOLDER_POSTFIX}/ThirdParty CACHE PATH "Directory containing ThirdParty build.")
if("${THIRD_PARTY_BUILD_FOLDER}" STREQUAL "")
  set(THIRD_PARTY_BUILD_FOLDER ${RR_BUILDS_ROOT}/${FOLDER_POSTFIX}/ThirdParty CACHE PATH "Directory containing ThirdParty build" FORCE)
endif()

OPTION (WRAP_TO_PYTHON 			"Generate Python wrapper" 		    ON)
OPTION (WRAP_TO_DELPHI 			"Generate/Install Delphi wrapper "  ON)
OPTION (WRAP_TO_C	 			"Create C API DLL" 					ON)
OPTION (BUILD_EXAMPLES			"Build and install Examples"		ON)
OPTION (INSTALL_STATIC_LIB		"Install RoadRunner static lib"		OFF)

#Setup so that roadrunner is always built as a dll and linked statically with 'as much as possible'
set(BUILD_SHARED_LIBS 												ON)

set(RR_ROOT 					${CMAKE_CURRENT_SOURCE_DIR})
set(RR_SOURCE_ROOT 				${CMAKE_CURRENT_SOURCE_DIR}/source)
set(THIRD_PARTY_FOLDER 			${CMAKE_CURRENT_SOURCE_DIR}/ThirdParty)

set(exe_path		 			${PROJECT_BINARY_DIR}/bin)
set(lib_path		 			${PROJECT_BINARY_DIR}/lib)

set(CMAKE_BUILD_TYPE "Debug" CACHE STRING
    "Choose the build type. The options are: None (CMAKE_CXX_FLAGS or
     CMAKE_C_FLAGS are used), Debug, Release, RelWithDebInfo, MinSizeRel.")
if("${CMAKE_BUILD_TYPE}" STREQUAL "")
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING
  "Choose the build type. The options are: None (CMAKE_CXX_FLAGS or
  CMAKE_C_FLAGS are used), Debug, Release, RelWithDebInfo, MinSizeRel." FORCE)
endif()

IF(WIN32)
    IF(${MSVC})
        IF(CMAKE_BUILD_TYPE MATCHES "Debug")
            set(exe_path ${exe_path}/Debug)
            set(lib_path ${lib_path}/Debug)
        ELSE(CMAKE_BUILD_TYPE MATCHES "Debug")
               set(exe_path ${exe_path}/Release)
               set(lib_path ${lib_path}/Release)
        ENDIF(CMAKE_BUILD_TYPE MATCHES "Debug")
    ELSE(WIN32)
   # Linux
    ENDIF(${MSVC})
ENDIF(WIN32)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/bin)

include_directories(				${RR_ROOT}
				                    ${THIRD_PARTY_FOLDER})

link_directories(				    ${LIBRARY_OUTPUT_PATH}
                                    ${THIRD_PARTY_INSTALL_FOLDER}/lib
                                    ${THIRD_PARTY_FOLDER}/libsbml_dependencies/lib
                                    ${THIRD_PARTY_BUILD_FOLDER}/lib
                 )

if(${MSVC})
add_definitions(-DCRT_NONSTDC_NO_DEPRECATE)
link_directories(					${THIRD_PARTY_BUILD_FOLDER}/SRC/Debug
                                    ${THIRD_PARTY_BUILD_FOLDER}/BLAS/SRC/Debug
                                    ${THIRD_PARTY_BUILD_FOLDER}/lib/Debug
                                    ${THIRD_PARTY_BUILD_FOLDER}/F2CLIBS/libf2c/Debug)
SET (CMAKE_CXX_FLAGS 			"${CMAKE_CXX_FLAGS} /EHsc ")
SET (CMAKE_SHARED_LINKER_FLAGS 	"${CMAKE_SHARED_LINKER_FLAGS} ") #/FORCE:MULTIPLE")
endif()

#=== COMPILER FLAGS 
if(${MINGW})
SET (CMAKE_CXX_FLAGS 			"${CMAKE_CXX_FLAGS} -std=gnu++0x")
endif()

if(${BORLAND})
link_directories(${THIRD_PARTY_FOLDER}/libsbml_xe/dependencies/lib)
add_definitions(
                                -DUSE_PCH                   #Enable pre-compiled headers
                                -H=${PROJECT_BINARY_DIR}/rr_pch.csm
                                -w-8012                     #Comparing signed /unsigned
                                -w-8057                     #Parameter never used
                                -w-8004                     #'var' is assigned a value that is never used
        )
endif()

#Common compiler definitions
add_definitions(
                    -DLIBSBML_USE_CPP_NAMESPACE 
                    -DNO_LIBSTRUCT_DLL
) 

# Allow the developer to select if Dynamic or Static libraries are built
SET (LIB_TYPE STATIC)
IF (BUILD_SHARED_LIBS)
  SET (LIB_TYPE SHARED)
ENDIF (BUILD_SHARED_LIBS)

#Source Code Folder
add_subdirectory(source)
add_subdirectory(apps)
add_subdirectory(Wrappers) 

#======================= INSTALL ROADRUNNER ========================================================
install(FILES source/rr_pch.h	
        DESTINATION include 
        COMPONENT rr_core)

if(${BUILD_EXAMPLES})
add_subdirectory(examples)
endif()

install(FILES Readme.txt License.txt	
        DESTINATION . 	
        COMPONENT info
        )

#==== SYSTEM FILES (COMPILER SPECIFICS) =================================================
if(${BORLAND})
set(CG_RUNTIMES		cc32110MT.dll)
foreach(runtime ${CG_RUNTIMES})
install (FILES 	${THIRD_PARTY_FOLDER}/runtime_dependencies/cg/xe/${runtime} DESTINATION bin COMPONENT rr_core)
endforeach()
endif()

#=== TCC ==================================================
FILE (GLOB tcc ${THIRD_PARTY_FOLDER}/tcc/*.*)
install (FILES 	${tcc} DESTINATION bin COMPONENT rr_core)

FILE (GLOB tcc ${THIRD_PARTY_FOLDER}/tcc/lib/*.*)
install (FILES 	${tcc} DESTINATION bin/lib COMPONENT rr_core)

FILE (GLOB tcc ${THIRD_PARTY_FOLDER}/tcc/include/*.*)
install (FILES 	${tcc} DESTINATION bin/include COMPONENT rr_core)

FILE (GLOB tcc ${THIRD_PARTY_FOLDER}/tcc/include/sys/*.*)
install (FILES 	${tcc} DESTINATION bin/include/sys COMPONENT rr_core)

FILE (GLOB tcc ${THIRD_PARTY_FOLDER}/tcc/include/winapi/*.*)
install (FILES 	${tcc} DESTINATION bin/include/winapi COMPONENT rr_core)
