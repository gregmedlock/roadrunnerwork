cmake_minimum_required(VERSION 2.8)
project(rr)

#-----------------------------------------------------------------------------
# version number.  
SET(RR_MAJOR_VERSION 0)
SET(RR_MINOR_VERSION 5)
SET(RR_BUILD_VERSION 11)
SET(RR_VERSION "${RR_MAJOR_VERSION}.${RR_MINOR_VERSION}.${RR_BUILD_VERSION}")

OPTION (BUILD_EXAMPLES			"Build Examples"			OFF)
OPTION (BUILD_LIBS				"Build Libs"				OFF)
OPTION (BUILD_SHARED_LIBS 		"Build Shared Libraries" 	OFF)

set(RR_SOURCE_ROOT 					${CMAKE_CURRENT_SOURCE_DIR}/src)
set(THIRD_PARTY_ROOT 				${CMAKE_CURRENT_SOURCE_DIR}/ThirdParty)

set(exe_path		 				${PROJECT_BINARY_DIR}/bin)
set(lib_path		 				${PROJECT_BINARY_DIR}/link)

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

IF(CMAKE_BUILD_TYPE MATCHES "Debug")
set(BUILD_TYPE "debug")
ELSE(CMAKE_BUILD_TYPE MATCHES "Debug")
set(BUILD_TYPE "release")
ENDIF(CMAKE_BUILD_TYPE MATCHES "Debug")

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/link)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/link)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/bin)

include_directories(				${RR_ROOT})
include_directories(				${CMAKE_CURRENT_SOURCE_DIR}/ThirdParty)

link_directories(					${LIBRARY_OUTPUT_PATH})

if(${BORLAND})
#include_directories(				${VTK_BINARY_ROOT}/cg-static/Rendering)
#link_directories(					${VTK_BINARY_ROOT}/cg-static/bin)
endif()

if(${MSVC})
#include_directories(				${VTK_BINARY_ROOT}/vs)
#include_directories(				${VTK_BINARY_ROOT}/vs/Rendering)
#add_definitions(-D_CRT_NONSTDC_NO_DEPRECATE)
#add_definitions(-D_CRT_SECURE_NO_WARNINGS)
#find_package(vtk)
endif()

#=== COMPILER FLAGS 
if(${BORLAND})
add_definitions(-DRR_PCH) #Enable pre-compiled headers
add_definitions(-H=${RR_ROOT}/rr_pch.csm)
endif()

if(${MSVC})
SET (CMAKE_CXX_FLAGS 			"${CMAKE_CXX_FLAGS} /EHsc")
SET (CMAKE_SHARED_LINKER_FLAGS 	"${CMAKE_SHARED_LINKR_FLAGS} /FORCE:MULTIPLE")
endif()

# Allow the developer to select if Dynamic or Static libraries are built
SET (LIB_TYPE STATIC)
IF (BUILD_SHARED_LIBS)
  SET (LIB_TYPE SHARED)
ENDIF (BUILD_SHARED_LIBS)

#Source Code Folder
add_subdirectory(src)

#======================= INSTALL ========================================================
install (FILES src/rr_pch.h	DESTINATION include COMPONENT cxx_headers)

FILE (GLOB libs ${lib_path}/*.lib)
install (FILES ${libs}	DESTINATION link COMPONENT library_files)

if(${BUILD_SHARED_LIBS})
FILE (GLOB dlls ${exe_path}/*.dll)
install (FILES ${dlls}	DESTINATION bin COMPONENT library_files)
endif()

if(${BUILD_EXAMPLES})
add_subdirectory(Examples)
endif()

install (FILES Readme.txt 		DESTINATION . 	COMPONENT info)
install (FILES License.txt 		DESTINATION . 	COMPONENT info)
#install (FILES CMakeLists.txt 	DESTINATION . 	COMPONENT build_files)

#==== INSTALL SYSTEM FILES (COMPILER SPECIFICS) =================================================
if(${BORLAND})
set(SYSTEM_FILES 		${THIRD_PARTY_ROOT}/CG/XE/cc32110MT.dll)
install (FILES 			${SYSTEM_FILES}  		DESTINATION bin COMPONENT library_files)
endif()

#================ CPACK =========================================================================
#Create distribution Pack
include (InstallRequiredSystemLibraries)


#COMPONENTS OF THE INSTALL
set(CPACK_COMPONENTS_ALL examples_source compiled_examples cxx_headers library_files build_files info)
set(CPACK_COMPONENT_EXAMPLES_SOURCE_GROUP					"Examples")
set(CPACK_COMPONENT_COMPILED_EXAMPLES_GROUP					"Examples")
set(CPACK_COMPONENT_LIBRARY_FILES_GROUP						"Development")
set(CPACK_COMPONENT_CXX_HEADERS_GROUP						"Development")
set(CPACK_COMPONENT_BUILD_FILES_GROUP						"Development")

set(CPACK_COMPONENT_BUILD_FILES_DEPENDS examples_source)
set(CPACK_COMPONENT_COMPILED_EXAMPLES_DISPLAY_NAME			"Compiled Examples")
set(CPACK_COMPONENT_EXAMPLES_SOURCE_DISPLAY_NAME			"Examples Source")
set(CPACK_COMPONENT_LIBRARY_FILES_DISPLAY_NAME				"rr Library Files")
set(CPACK_COMPONENT_CXX_HEADERS_DISPLAY_NAME				"C++ Headers")
set(CPACK_COMPONENT_BUILD_FILES_HIDDEN						"Build Files")
set(CPACK_COMPONENT_INFO_HIDDEN								"Info Files")

set(CPACK_PACKAGE_ICON										${CMAKE_CURRENT_SOURCE_DIR}\\\\rrArt.bmp)

#NSIS Specific
set(CPACK_NSIS_MUI_ICON										${CMAKE_CURRENT_SOURCE_DIR}/rr.ico)
set(CPACK_NSIS_MUI_UNIICON									${CMAKE_CURRENT_SOURCE_DIR}/rr.ico)
set(CPACK_NSIS_INSTALL_ROOT									"C:")

#COMPONENTS
set(CPACK_COMPONENT_COMPILED_EXAMPLES_DESCRIPTION			"This will install pre-compiled binaries for all examples")
set(CPACK_COMPONENT_EXAMPLES_SOURCE_DESCRIPTION				"Examples Source")
set(CPACK_COMPONENT_LIBRARY_FILES_DESCRIPTION				"Link Library files (.lib) and if dynamic disribution Dynamic Link Libraries (.dll) ")
set(CPACK_COMPONENT_CXX_HEADERS_DESCRIPTION					"rrLibrary C++ header files")
set(CPACK_COMPONENT_BUILD_FILES_DESCRIPTION					"Build Files")
set(CPACK_COMPONENT_CXX_HEADERS_DEPENDS						library_files)

set(CPACK_RESOURCE_FILE_LICENSE								"${CMAKE_CURRENT_SOURCE_DIR}/License.txt")
set(CPACK_PACKAGE_VENDOR									"Dune Scientific")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY						"rr library - headers, libraries and examples")
set(CPACK_PACKAGE_VERSION_MAJOR								"${RR_MAJOR_VERSION}")
set(CPACK_PACKAGE_VERSION_MINOR								"${RR_MINOR_VERSION}")
set(CPACK_PACKAGE_VERSION_PATCH								"${RR_BUILD_VERSION}")
set(CPACK_PACKAGE_INSTALL_DIRECTORY							"rr")

if(${BUILD_SHARED_LIBS})
set(PACKAGE_NAME_LIB_TYPE "shared")
else()
set(PACKAGE_NAME_LIB_TYPE "static")
endif()

if(${BORLAND})
set(COMPILER_FOR_PACKAGE "cg")
endif()

if(${MSVC})
set(COMPILER_FOR_PACKAGE "vs")
endif()

MESSAGE( STATUS "CMAKE_CFG_INTDIR: " ${CMAKE_CFG_INTDIR} )

if(${CMAKE_CFG_INTDIR} MATCHES "/Debug")
set(bType "debug")
else()
set(bType "release")
endif()

set(CPACK_PACKAGE_NAME										"rr-${COMPILER_FOR_PACKAGE}-${PACKAGE_NAME_LIB_TYPE}-${bType}")
set(CPACK_OUTPUT_FILE_PREFIX								"../")
include(CPack)


