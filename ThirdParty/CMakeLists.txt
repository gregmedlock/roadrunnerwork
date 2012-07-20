cmake_minimum_required(VERSION 2.8)
project(RRThirdPartyBuild)

#----- ThirdParties
#pre set option
set(WITH_BZIP2          OFF CACHE BOOL "test")
set(WITH_SWIG           OFF CACHE BOOL "test")
set(WITH_ZLIB           OFF CACHE BOOL "test")
set(WITH_CPP_NAMESPACE  ON  CACHE BOOL "test")

set(BUILD_CVODES        OFF CACHE BOOL "")
set(BUILD_IDA           OFF CACHE BOOL "")
set(BUILD_IDAS          OFF CACHE BOOL "")
set(BUILD_KINSOL        OFF CACHE BOOL "")

if(${BORLAND})
set(FOLDER_POSTFIX "xe")
set(LIBXML_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libsbml_xe/dependencies/include/" CACHE string "test")
set(LIBXML_LIBRARY "${CMAKE_CURRENT_SOURCE_DIR}/libsbml_xe/dependencies/lib/libxml2_xe.lib" CACHE string "test")
endif(${BORLAND})

if(${MSVC})
set(FOLDER_POSTFIX "vs")
set(LIBXML_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libsbml_dependencies/include/" CACHE string "test")
set(LIBXML_LIBRARY "${CMAKE_CURRENT_SOURCE_DIR}/libsbml_dependencies/lib/libxml2.lib" CACHE string "test")


endif()

set(THIRD_PARTY_FOLDER 			${CMAKE_CURRENT_SOURCE_DIR})

set(CMAKE_BUILD_TYPE "Debug" CACHE STRING
    "Choose the build type. The options are: None (CMAKE_CXX_FLAGS or
     CMAKE_C_FLAGS are used), Debug, Release, RelWithDebInfo, MinSizeRel.")
if("${CMAKE_BUILD_TYPE}" STREQUAL "")
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING
  "Choose the build type. The options are: None (CMAKE_CXX_FLAGS or
  CMAKE_C_FLAGS are used), Debug, Release, RelWithDebInfo, MinSizeRel." FORCE)
endif()

set(exe_path		 			${PROJECT_BINARY_DIR}/bin)
set(lib_path		 			${PROJECT_BINARY_DIR}/lib)

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
MESSAGE( STATUS "CMAKE_BUILD_TYPE: " ${CMAKE_BUILD_TYPE} )

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/bin)

if(${BORLAND})
include_directories(        
                                    ${THIRD_PARTY_FOLDER}/libsbml_xe/src
)

link_directories(${THIRD_PARTY_FOLDER}/libsbml_xe/dependencies/lib)
else()
include_directories(                ${THIRD_PARTY_FOLDER}/libsbml
                                    ${THIRD_PARTY_FOLDER}/libsbml/src
                                    ${THIRD_PARTY_FOLDER}/libsbml/src/sbml
                   )
link_directories(					${THIRD_PARTY_FOLDER}/libsbml/dependencies/lib)
endif()

if(${MSVC})
#SET (CMAKE_CXX_FLAGS 			"${CMAKE_CXX_FLAGS} /EHsc ")
#SET (CMAKE_SHARED_LINKER_FLAGS 	"${CMAKE_SHARED_LINKER_FLAGS} ") #/FORCE:MULTIPLE")
add_definitions(
-D_CRT_SECURE_NO_WARNINGS 
-D_CRT_NONSTDC_NO_DEPRECATE
"-wd4244"
"-wd4554" #use paranthesis to clarify precedence

)
endif()

#=== COMPILER FLAGS 
if(${MINGW})
SET (CMAKE_CXX_FLAGS 			"${CMAKE_CXX_FLAGS} -std=gnu++0x")
endif()

if(${BORLAND})
add_definitions(
#                                -DUSE_PCH                   #Enable pre-compiled headers
#                                -H=${PROJECT_BINARY_DIR}/rr_tp_pch.csm
                                -w-8012                     #Comparing signed /unsigned
                                -w-8057                     #Parameter never used
                                -w-8004                     #'var' is assigned a value that is never used
        )
endif()

#3rd party folder
if(${BORLAND})
add_subdirectory(libsbml_xe)
else(${BORLAND})
add_subdirectory(libsbml)
endif()

add_subdirectory(clapack/3.2.1)
add_subdirectory(sundials)

#set(LIBSBML_DEPENDENCY_DIR CACHE STRING "./libsbml_dependencies")
#set(LIBXML_INCLUDE_DIR CACHE STRING "libsbml_dependencies/include/libxml")

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


