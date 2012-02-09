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

set(NOM_SOURCE_ROOT 				${CMAKE_CURRENT_SOURCE_DIR}/src)
set(THIRD_PARTY_ROOT 				"C:/rrThirdPartyInstalls")
set(LIBSBML_SOURCE_ROOT 			${THIRD_PARTY_ROOT}/libsbml)

#Where to put the resulting binaries of a build
set(lib_path		 				${PROJECT_BINARY_DIR}/link)
set(bin_path		 				${PROJECT_BINARY_DIR}/bin)

#IF(WIN32)
#  IF(${MSVC})
#  IF(CMAKE_BUILD_TYPE MATCHES "Debug")
#   set(lib_path ${lib_path}/Debug)
#   set(bin_path ${bin_path}/Debug)
#  ELSE(CMAKE_BUILD_TYPE MATCHES "Debug")
#   set(lib_path ${lib_path}/Release)
#   set(bin_path ${bin_path}/Release)
#  ENDIF(CMAKE_BUILD_TYPE MATCHES "Debug")
# ELSE(WIN32)
#   # Linux ----------
# ENDIF(${MSVC})
#ENDIF(WIN32)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/link)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/link)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/bin)

include_directories(				${NOM_SOURCE_ROOT})
include_directories(				${LIBSBML_SOURCE_ROOT})
include_directories(				${LIBSBML_SOURCE_ROOT}/include)
include_directories(				${LIBSBML_SOURCE_ROOT}/include/sbml)
link_directories(${THIRD_PARTY_ROOT})

if(${MSVC})
add_definitions(-D_CRT_NONSTDC_NO_DEPRECATE)
add_definitions(-D_CRT_SECURE_NO_WARNINGS)

#=== COMPILER FLAGS 
SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /EHsc")
endif()

# Allow the developer to select if Dynamic or Static libraries are built
SET (LIB_TYPE STATIC)
IF (BUILD_SHARED_LIBS)
  SET (LIB_TYPE SHARED)
ENDIF (BUILD_SHARED_LIBS)

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


#
#install (FILES Readme.txt 		DESTINATION . 	COMPONENT info)
#install (FILES License.txt 		DESTINATION . 	COMPONENT info)
#install (FILES CMakeLists.txt 	DESTINATION . 	COMPONENT build_files)

##==== INSTALL SYSTEM FILES (COMPILER SPECIFICS) =================================================
#if(${BORLAND})
#set(SYSTEM_FILES 		${THIRD_PARTY_ROOT}/CodeGear/XE/cc32110MT.dll)
#install (FILES 			${SYSTEM_FILES}  		DESTINATION bin COMPONENT library_files)
#endif()
#
##================ CPACK =========================================================================
##Create distribution Pack
#include (InstallRequiredSystemLibraries)
#
##COMPONENTS OF THE INSTALL
#set(CPACK_COMPONENTS_ALL examples_source compiled_examples cxx_headers library_files build_files info)
#set(CPACK_COMPONENT_EXAMPLES_SOURCE_GROUP					"Examples")
#set(CPACK_COMPONENT_COMPILED_EXAMPLES_GROUP					"Examples")
#set(CPACK_COMPONENT_LIBRARY_FILES_GROUP						"Development")
#set(CPACK_COMPONENT_CXX_HEADERS_GROUP						"Development")
#set(CPACK_COMPONENT_BUILD_FILES_GROUP						"Development")
#
#set(CPACK_COMPONENT_BUILD_FILES_DEPENDS examples_source)
#set(CPACK_COMPONENT_COMPILED_EXAMPLES_DISPLAY_NAME			"Compiled Examples")
#set(CPACK_COMPONENT_EXAMPLES_SOURCE_DISPLAY_NAME			"Examples Source")
#set(CPACK_COMPONENT_LIBRARY_FILES_DISPLAY_NAME				"mtk Library Files")
#set(CPACK_COMPONENT_CXX_HEADERS_DISPLAY_NAME				"C++ Headers")
#set(CPACK_COMPONENT_BUILD_FILES_HIDDEN						"Build Files")
#set(CPACK_COMPONENT_INFO_HIDDEN								"Info Files")
#
#set(CPACK_PACKAGE_ICON										${CMAKE_CURRENT_SOURCE_DIR}\\\\mtkArt.bmp)
##NSIS Specific
#set(CPACK_NSIS_MUI_ICON										${CMAKE_CURRENT_SOURCE_DIR}/mtk.ico)
#set(CPACK_NSIS_MUI_UNIICON									${CMAKE_CURRENT_SOURCE_DIR}/mtk.ico)
#set(CPACK_NSIS_INSTALL_ROOT									"C:")
#
##COMPONENTS
#set(CPACK_COMPONENT_COMPILED_EXAMPLES_DESCRIPTION			"This will install pre-compiled binaries for all examples")
#set(CPACK_COMPONENT_EXAMPLES_SOURCE_DESCRIPTION				"Examples Source")
#set(CPACK_COMPONENT_LIBRARY_FILES_DESCRIPTION				"Link Library files (.lib) and if dynamic disribution Dynamic Link Libraries (.dll) ")
#set(CPACK_COMPONENT_CXX_HEADERS_DESCRIPTION					"mtkLibrary C++ header files")
#set(CPACK_COMPONENT_BUILD_FILES_DESCRIPTION					"Build Files")
#set(CPACK_COMPONENT_CXX_HEADERS_DEPENDS						library_files)
#
#set(CPACK_RESOURCE_FILE_LICENSE								"${CMAKE_CURRENT_SOURCE_DIR}/License.txt")
#set(CPACK_PACKAGE_VENDOR									"Dune Scientific")
#set(CPACK_PACKAGE_DESCRIPTION_SUMMARY						"mtk library - headers, libraries and examples")
#set(CPACK_PACKAGE_VERSION_MAJOR								"${NL_MAJOR_VERSION}")
#set(CPACK_PACKAGE_VERSION_MINOR								"${NL_MINOR_VERSION}")
#set(CPACK_PACKAGE_VERSION_PATCH								"${NL_BUILD_VERSION}")
#set(CPACK_PACKAGE_INSTALL_DIRECTORY							"mtk")
#
#if(${BUILD_SHARED_LIBS})
#set(PACKAGE_NAME_LIB_TYPE "shared")
#else()
#set(PACKAGE_NAME_LIB_TYPE "static")
#endif()
#
#if(${BORLAND})
#set(COMPILER_FOR_PACKAGE "cg")
#endif()
#
#if(${MSVC})
#set(COMPILER_FOR_PACKAGE "vs")
#endif()
#
#set(CPACK_PACKAGE_NAME										"mtk-${COMPILER_FOR_PACKAGE}-${PACKAGE_NAME_LIB_TYPE}")
#set(CPACK_OUTPUT_FILE_PREFIX								"../")
#include(CPack)
#
#
