cmake_minimum_required(VERSION 2.8)
project(RRThirdPartyBuild)

#These will be the defaults ...
SET(RR_BUILDS_ROOT "C:/rrBuilds")
SET(RR_INSTALLS_ROOT "C:/rrInstalls")

if(${BORLAND})
set(FOLDER_POSTFIX "xe")
else(${BORLAND})
set(FOLDER_POSTFIX "vs")
endif()

#Setup so that roadrunner is always built as a dll and linked statically with 'as much as possible'
#set(BUILD_SHARED_LIBS 												ON)

set(THIRD_PARTY_FOLDER 			${CMAKE_CURRENT_SOURCE_DIR})

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
MESSAGE( STATUS "CMAKE_BUILD_TYPE: " ${CMAKE_BUILD_TYPE} )

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/bin)

include_directories(                ${THIRD_PARTY_FOLDER}/libsbml
                                    ${THIRD_PARTY_FOLDER}/libsbml/src
                                    ${THIRD_PARTY_FOLDER}/libsbml/src/sbml


)

link_directories(					
                                    ${THIRD_PARTY_FOLDER}/libsbml/dependencies/lib
                 )

if(${MSVC})
#SET (CMAKE_CXX_FLAGS 			"${CMAKE_CXX_FLAGS} /EHsc ")
#SET (CMAKE_SHARED_LINKER_FLAGS 	"${CMAKE_SHARED_LINKER_FLAGS} ") #/FORCE:MULTIPLE")
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

#Source Code Folder
add_subdirectory(libsbml)
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


