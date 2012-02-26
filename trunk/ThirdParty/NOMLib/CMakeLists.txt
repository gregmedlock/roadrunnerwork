cmake_minimum_required(VERSION 2.8)
project(NOMLib)

#-----------------------------------------------------------------------------
# version number.  An even minor number is a releases.
SET(NL_MAJOR_VERSION 0)
SET(NL_MINOR_VERSION 5)
SET(NL_BUILD_VERSION 11)
SET(NL_VERSION
    "${NL_MAJOR_VERSION}.${NL_MINOR_VERSION}.${NL_BUILD_VERSION}")

OPTION (BUILD_EXAMPLES			"Build Examples"			ON)
OPTION (BUILD_LIBS				"Build Libs"				ON)
OPTION (BUILD_SHARED_LIBS 		"Build Shared Libraries" 	ON)

set(NOM_SOURCE_ROOT 		${CMAKE_CURRENT_SOURCE_DIR}/src)

set(lib_path				${PROJECT_BINARY_DIR}/link)
set(bin_path				${PROJECT_BINARY_DIR}/bin)

IF(${MSVC})
	IF(CMAKE_BUILD_TYPE MATCHES "Debug")
		set(lib_path ${lib_path}/Debug)
		set(bin_path ${bin_path}/Debug)
	ELSE(CMAKE_BUILD_TYPE MATCHES "Debug")
		set(lib_path ${lib_path}/Release)
		set(bin_path ${bin_path}/Release)
	ENDIF(CMAKE_BUILD_TYPE MATCHES "Debug")
ENDIF(${MSVC})

if(${MSVC})
#Macros
set(THIRD_PARTY_ROOT 				"C:\\rrInstalls\\vs")
add_definitions(-D_CRT_NONSTDC_NO_DEPRECATE)
add_definitions(-D_CRT_SECURE_NO_WARNINGS)

#Compiler Flags 
SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /EHsc")
endif()

set(THIRD_PARTY_ROOT 				"C:\\rrInstalls")

if(${BORLAND})
set(THIRD_PARTY_ROOT 				"C:\\rrInstalls\\xe")
endif()
#set(THIRD_PARTY_ROOT 				"C:\\rrInstalls\\xe")

#Include paths and link paths
set(LIBSBML_SOURCE_ROOT 			${THIRD_PARTY_ROOT}/libsbml)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/link)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/link)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/bin)

include_directories(				${NOM_SOURCE_ROOT})
include_directories(				${LIBSBML_SOURCE_ROOT})
include_directories(				${LIBSBML_SOURCE_ROOT}/include)
include_directories(				${LIBSBML_SOURCE_ROOT}/include/sbml)

link_directories(${THIRD_PARTY_ROOT}/libsbml/lib)


# Allow the developer to select if Dynamic or Static libraries are built (libSBML static don't work)
SET (LIB_TYPE STATIC)
IF (BUILD_SHARED_LIBS)
  SET (LIB_TYPE SHARED)
ELSE()
add_definitions(-DNO_SBML_DLL) #static lib?
add_definitions(-DNO_NOM_DLL) #static lib?
ENDIF (BUILD_SHARED_LIBS)
add_definitions(-DLIBSBML_USE_CPP_NAMESPACE)

#Source Code Folder
add_subdirectory(src)

#======================= INSTALL ========================================================
FILE (GLOB libs ${lib_path}/*.lib)
install (FILES ${libs}	DESTINATION link COMPONENT library_files)

#Install DLL's
if(${BUILD_SHARED_LIBS})
FILE (GLOB dlls ${bin_path}/*.dll)
install (FILES ${dlls}	DESTINATION bin COMPONENT library_files)
endif()

if(${BUILD_EXAMPLES})
add_subdirectory(Examples)
endif()

