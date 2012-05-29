cmake_minimum_required(VERSION 2.8)
project(rr)

SET(RR_MAJOR_VERSION 2012)
SET(RR_MINOR_VERSION 5)
SET(RR_BUILD_VERSION 26)
SET(RR_VERSION "${RR_MAJOR_VERSION}.${RR_MINOR_VERSION}.${RR_BUILD_VERSION}")


IF(${BORLAND})
set(LIBSBML_INSTALL_FOLDER ${CMAKE_SOURCE_DIR}/installs/xe CACHE PATH
    "Directory containing libsbml libraries.")
if("${LIBSBML_INSTALL_FOLDER}" STREQUAL "")
  set(LIBSBML_INSTALL_FOLDER ${CMAKE_SOURCE_DIR}/installs/xe CACHE PATH
  "Directory containing libsbml libraries" FORCE)
endif()
else(${BORLAND})
set(LIBSBML_INSTALL_FOLDER ${CMAKE_SOURCE_DIR}/installs/vs CACHE PATH
    "Directory containing libsbml libraries.")
if("${LIBSBML_INSTALL_FOLDER}" STREQUAL "")
  set(LIBSBML_INSTALL_FOLDER ${CMAKE_SOURCE_DIR}/installs/vs CACHE PATH
  "Directory containing libsbml libraries" FORCE)
endif()
endif()

IF(${BORLAND})
set(CVODE_INSTALL_FOLDER ${CMAKE_SOURCE_DIR}/installs/xe CACHE PATH
    "Directory containing cvode libraries.")
if("${CVODE_INSTALL_FOLDER}" STREQUAL "")
  set(CVODE_INSTALL_FOLDER ${CMAKE_SOURCE_DIR}/installs/xe CACHE PATH
  "Directory containing cvode libraries" FORCE)
endif()
else(${BORLAND})
set(CVODE_INSTALL_FOLDER ${CMAKE_SOURCE_DIR}/installs/vs CACHE PATH
    "Directory containing cvode libraries.")
if("${CVODE_INSTALL_FOLDER}" STREQUAL "")
  set(CVODE_INSTALL_FOLDER ${CMAKE_SOURCE_DIR}/installs/vs CACHE PATH
  "Directory containing cvode libraries" FORCE)
endif()
endif()

IF(${BORLAND})
set(CLAPACK_BUILD_FOLDER ${CMAKE_SOURCE_DIR}/builds/xe/clapack CACHE PATH
    "Directory containing clapack libraries.")
if("${CLAPACK_INSTALL_FOLDER}" STREQUAL "")
  set(CLAPACK_BUILD_FOLDER ${CMAKE_SOURCE_DIR}/builds/xe/clapack CACHE PATH
  "Directory containing clapack libraries" FORCE)
endif()
else(${BORLAND})
set(CLAPACK_BUILD_FOLDER ${CMAKE_SOURCE_DIR}/builds/vs/clapack CACHE PATH
    "Directory containing clapack libraries.")
if("${CLAPACK_BUILD_FOLDER}" STREQUAL "")
  set(CLAPACK_BUILD_FOLDER ${CMAKE_SOURCE_DIR}/builds/vs/clapack CACHE PATH
  "Directory containing clapack libraries" FORCE)
endif()
endif()

#set(CLAPACK_BUILD_FOLDER "C:/rrw/builds/xe/clapack")

OPTION (PYTHON_WRAPPER 			"Generate Python wrapper " 		    FALSE)
OPTION (BUILD_C_API	 			"Create C API DLL" 					OFF)
OPTION (BUILD_EXAMPLES			"Build and install Examples"		OFF)
OPTION (INSTALL_STATIC_LIB		"Install RoadRunner static lib"		OFF)

#Setup so that roadrunner is always built as a dll and linked statically with 'as much as possible'
set(BUILD_SHARED_LIBS 												ON)

set(RR_ROOT 					${CMAKE_CURRENT_SOURCE_DIR})
set(RR_SOURCE_ROOT 				${CMAKE_CURRENT_SOURCE_DIR}/src)
set(THIRD_PARTY_FOLDER 			${CMAKE_CURRENT_SOURCE_DIR}/ThirdParty)

set(exe_path		 			${PROJECT_BINARY_DIR}/bin)
set(lib_path		 			${PROJECT_BINARY_DIR}/lib)

#IF(WIN32)
#  IF(${MSVC})
#  IF(CMAKE_BUILD_TYPE MATCHES "Debug")
   set(exe_path ${exe_path}/Debug)
   set(lib_path ${lib_path}/Debug)
#  ELSE(CMAKE_BUILD_TYPE MATCHES "Debug")
#   set(exe_path ${exe_path}/Release)
#   set(lib_path ${lib_path}/Release)
#  ENDIF(CMAKE_BUILD_TYPE MATCHES "Debug")
# ELSE(WIN32)
   # Linux
# ENDIF(${MSVC})
#ENDIF(WIN32)
#MESSAGE( STATUS "CMAKE_BUILD_TYPE: " ${CMAKE_BUILD_TYPE} )

#IF(CMAKE_BUILD_TYPE MATCHES "Debug")
#set(BUILD_TYPE "debug")
#ELSE(CMAKE_BUILD_TYPE MATCHES "Debug")
#set(BUILD_TYPE "release")
#ENDIF(CMAKE_BUILD_TYPE MATCHES "Debug")

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY 	${PROJECT_BINARY_DIR}/bin)

include_directories(				${RR_ROOT})
include_directories(				${CMAKE_CURRENT_SOURCE_DIR}/ThirdParty)

link_directories(					${LIBRARY_OUTPUT_PATH})
link_directories(					${LIBSBML_INSTALL_FOLDER}/lib)
link_directories(					${THIRD_PARTY_FOLDER}/libsbml_dependencies/lib)
link_directories(					${CLAPACK_BUILD_FOLDER}/BLAS/SRC)
link_directories(					${CLAPACK_BUILD_FOLDER}/SRC)
link_directories(					${CLAPACK_BUILD_FOLDER}/F2CLIBS/libf2c)
link_directories(					${PROJECT_BINARY_DIR}/lib)

if(${MSVC})
link_directories(					${CLAPACK_BUILD_FOLDER}/BLAS/SRC/Debug)
link_directories(					${CLAPACK_BUILD_FOLDER}/SRC/Debug)
link_directories(					${CLAPACK_BUILD_FOLDER}/F2CLIBS/libf2c/Debug)
add_definitions(-DCRT_NONSTDC_NO_DEPRECATE)
add_definitions(-DLIBSBML_USE_CPP_NAMESPACE)
add_definitions(-DNO_LIBSTRUCT_DLL) 
SET (CMAKE_CXX_FLAGS 			"${CMAKE_CXX_FLAGS} /EHsc ")
SET (CMAKE_SHARED_LINKER_FLAGS 	"${CMAKE_SHARED_LINKR_FLAGS} /FORCE:MULTIPLE")
endif()

#=== COMPILER FLAGS 
if(${BORLAND})
add_definitions(-DUSE_PCH) #Enable pre-compiled headers
add_definitions(-DLIBSBML_USE_CPP_NAMESPACE) 
add_definitions(-DNO_LIBSTRUCT_DLL) 
add_definitions(-H=${RR_ROOT}/rr_pch.csm)
add_definitions(-w-8012)  #Comparing signed /unsigned
add_definitions(-w-8057)  #Parameter never used
add_definitions(-w-8004)  #'var' is assigned a value that is never used
endif()

# Allow the developer to select if Dynamic or Static libraries are built
SET (LIB_TYPE STATIC)
IF (BUILD_SHARED_LIBS)
  SET (LIB_TYPE SHARED)
ENDIF (BUILD_SHARED_LIBS)

#Source Code Folder
add_subdirectory(src)
add_subdirectory(Wrappers)

add_subdirectory(apps)
#======================= INSTALL ========================================================
install (FILES src/rr_pch.h	DESTINATION include COMPONENT rr_core)

if(${BUILD_EXAMPLES})
add_subdirectory(Examples)
endif()

add_subdirectory(Models)
install (FILES Readme.txt 		DESTINATION . 	COMPONENT info)
install (FILES License.txt 		DESTINATION . 	COMPONENT info)

#==== INSTALL SYSTEM FILES (COMPILER SPECIFICS) =================================================
if(${BORLAND})
set(CG_RUNTIMES		cc32110MT.dll)
foreach(runtime ${CG_RUNTIMES})
install (FILES 	${THIRD_PARTY_FOLDER}/CG/XE/${runtime} DESTINATION bin COMPONENT rr_core)
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

#================ CPACK =========================================================================
#Create distribution Pack
include (InstallRequiredSystemLibraries)

#COMPONENTS OF THE INSTALL
set(CPACK_COMPONENTS_ALL rr_core apps c_api compiled_examples example_files sbml_models delphi python)


#set(CPACK_COMPONENT_RR_CORE_GROUP							"Core")
#set(CPACK_COMPONENT_EXAMPLES_SOURCE_GROUP					"Examples")
#set(CPACK_COMPONENT_COMPILED_EXAMPLES_GROUP				"Examples")

#set(CPACK_COMPONENT_RR_CORE_REQUIRED						ON)
set(CPACK_COMPONENT_RR_CORE_DISPLAY_NAME					"Core Files")
set(CPACK_COMPONENT_APPS_DISPLAY_NAME						"Applications")
set(CPACK_COMPONENT_C_API_DISPLAY_NAME						"C API DLL")
#set(CPACK_COMPONENT_BUILD_FILES_DEPENDS example_files)
set(CPACK_COMPONENT_COMPILED_EXAMPLES_DISPLAY_NAME			"Compiled Examples")
set(CPACK_COMPONENT_EXAMPLE_FILES_DISPLAY_NAME				"Examples Source and Readme files")
set(CPACK_COMPONENT_SBML_MODELS_DISPLAY_NAME				"SBML Models")
set(CPACK_COMPONENT_DELPHI_DISPLAY_NAME						"Delphi Units")
set(CPACK_COMPONENT_PYTHON_DISPLAY_NAME						"Python Module")

#set(CPACK_PACKAGE_ICON										${CMAKE_CURRENT_SOURCE_DIR}\\\\rrArt.bmp)
#NSIS Specific
#set(CPACK_NSIS_MUI_ICON										${CMAKE_CURRENT_SOURCE_DIR}/rr.ico)
#set(CPACK_NSIS_MUI_UNIICON									${CMAKE_CURRENT_SOURCE_DIR}/rr.ico)
set(CPACK_NSIS_INSTALL_ROOT									"C:")

#COMPONENTS
set(CPACK_COMPONENT_RR_CORE_DESCRIPTION						"Headers (.h), link library files (.lib) and if a dynamic disribution, dynamic link libraries (.dll's)")
set(CPACK_COMPONENT_APPS_DESCRIPTION						"Applications using RoadRunner API")
set(CPACK_COMPONENT_C_API_DESCRIPTION						"C API Wrapper for RoadRunner")
set(CPACK_COMPONENT_COMPILED_EXAMPLES_DESCRIPTION			"This will install pre-compiled binaries for all examples")
set(CPACK_COMPONENT_EXAMPLE_FILES_DESCRIPTION				"Examples Source")
set(CPACK_COMPONENT_SBML_MODELS_DESCRIPTION					"Install various SBML Model files")
set(CPACK_COMPONENT_DELPHI_DESCRIPTION						"Installs RoadRunner Delphi Units")
set(CPACK_COMPONENT_PYTHON_DESCRIPTION						"Installs RoadRunner Python Module")

set(CPACK_RESOURCE_FILE_LICENSE								"${CMAKE_CURRENT_SOURCE_DIR}/License.txt")
set(CPACK_PACKAGE_VENDOR									"UW")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY						"rr library - headers, libraries and examples")
set(CPACK_PACKAGE_VERSION_MAJOR								"${RR_MAJOR_VERSION}")
set(CPACK_PACKAGE_VERSION_MINOR								"${RR_MINOR_VERSION}")
set(CPACK_PACKAGE_VERSION_PATCH								"${RR_BUILD_VERSION}")
set(CPACK_PACKAGE_INSTALL_DIRECTORY							"RoadRunner")

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

#MESSAGE( STATUS "CMAKE_CFG_INTDIR: " ${CMAKE_CFG_INTDIR} )

#if(${CMAKE_CFG_INTDIR} MATCHES "/Debug")
#set(bType "debug")
#else()
#set(bType "release")
#endif()

#set(CPACK_PACKAGE_NAME										"rr-${COMPILER_FOR_PACKAGE}-${PACKAGE_NAME_LIB_TYPE}-${bType}")
set(CPACK_PACKAGE_NAME										"rr-${COMPILER_FOR_PACKAGE}")
set(CPACK_OUTPUT_FILE_PREFIX								"${CMAKE_CURRENT_SOURCE_DIR}/releases")
include(CPack)


