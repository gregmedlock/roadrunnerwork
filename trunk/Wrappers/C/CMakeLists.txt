set(target rrC_API)

add_library(		  ${target} rrC_API.cpp)

IF(BUILD_SHARED_LIBS) 
target_link_libraries (${target} rr_shared)
target_link_libraries (${target} libsbml)
ELSE()
target_link_libraries (${target} rr_static)
target_link_libraries (${target} libsbml-static)
ENDIF()

#set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${exe_path}/${target}.tds)
#=============== INSTALL =====================
install (FILES ${target}.h 		DESTINATION include COMPONENT include_files)
install (FILES ${lib_path}/${target}.lib	DESTINATION link COMPONENT library_files)

if(BUILD_SHARED_LIBS)
install (FILES ${exe_path}/${target}.dll	DESTINATION bin COMPONENT library_files)
endif()

#install dependencies
set(SBML_DEPENDENCIES libxml2.dll iconv.dll zlib1.dll)
set(C_API_DEPENDENCIES libxml2.dll iconv.dll zlib1.dll)

foreach(system_file ${SBML_DEPENDENCIES})
install (FILES ${SBML_INSTALL_FOLDER}/bin/${system_file}	DESTINATION bin COMPONENT DLLs)
endforeach()

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

#set(CPACK_PACKAGE_ICON										${CMAKE_CURRENT_SOURCE_DIR}\\\\rrArt.bmp)

#NSIS Specific
#set(CPACK_NSIS_MUI_ICON										${CMAKE_CURRENT_SOURCE_DIR}/rr.ico)
#set(CPACK_NSIS_MUI_UNIICON									${CMAKE_CURRENT_SOURCE_DIR}/rr.ico)
set(CPACK_NSIS_INSTALL_ROOT									"C:")

#COMPONENTS
set(CPACK_COMPONENT_COMPILED_EXAMPLES_DESCRIPTION			"This will install pre-compiled binaries for all examples")
set(CPACK_COMPONENT_EXAMPLES_SOURCE_DESCRIPTION				"Examples Source")
set(CPACK_COMPONENT_LIBRARY_FILES_DESCRIPTION				"Link Library files (.lib) and if dynamic disribution Dynamic Link Libraries (.dll) ")
set(CPACK_COMPONENT_CXX_HEADERS_DESCRIPTION					"rrLibrary C++ header files")
set(CPACK_COMPONENT_BUILD_FILES_DESCRIPTION					"Build Files")
set(CPACK_COMPONENT_CXX_HEADERS_DEPENDS						library_files)

set(CPACK_RESOURCE_FILE_LICENSE								"${CMAKE_CURRENT_SOURCE_DIR}/License.txt")
set(CPACK_PACKAGE_VENDOR									"Systems Biology - University of Washington")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY						"rr library - headers, libraries and examples")
set(CPACK_PACKAGE_VERSION_MAJOR								"${RR_MAJOR_VERSION}")
set(CPACK_PACKAGE_VERSION_MINOR								"${RR_MINOR_VERSION}")
set(CPACK_PACKAGE_VERSION_PATCH								"${RR_BUILD_VERSION}")
set(CPACK_PACKAGE_INSTALL_DIRECTORY							"rr")

if(BUILD_SHARED_LIBS)
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

