cmake_minimum_required(VERSION 2.8)
project(RoadRunner)

add_subdirectory(Models)
add_subdirectory(trunk)

#------ RELEASES FOLDER
if(${BORLAND})
set(RELEASE_FOLDER ${CMAKE_CURRENT_SOURCE_DIR}/releases/xe CACHE PATH "Directory containing roadrunner releases.")
if("${RELEASE_FOLDER}" STREQUAL "")
  set(LIBSBML_INSTALL_FOLDER ${CMAKE_CURRENT_SOURCE_DIR}/releases/xe CACHE PATH "Directory containing roadrunner releases" FORCE)
endif()
else(${BORLAND})
set(RELEASE_FOLDER ${CMAKE_CURRENT_SOURCE_DIR}/releases/vs CACHE PATH "Directory containing roadrunner releases.")
if("${RELEASE_FOLDER}" STREQUAL "")
  set(RELEASE_FOLDER ${CMAKE_CURRENT_SOURCE_DIR}/releases/vs CACHE PATH "Directory containing roadrunner releases" FORCE)
endif()
endif()

#================ CPACK =========================================================================
#Create distribution Pack
include (InstallRequiredSystemLibraries)

#COMPONENTS OF THE INSTALL
set(CPACK_COMPONENTS_ALL rr_core apps c_api compiled_examples example_files sbml_models Delphi Python rr_docs)


#set(CPACK_COMPONENT_RR_CORE_GROUP							"Core")
#set(CPACK_COMPONENT_EXAMPLES_SOURCE_GROUP					"Examples")
#set(CPACK_COMPONENT_COMPILED_EXAMPLES_GROUP				"Examples")

#set(CPACK_COMPONENT_RR_CORE_REQUIRED						ON)
set(CPACK_COMPONENT_RR_CORE_DISPLAY_NAME					"Core Files")
set(CPACK_COMPONENT_APPS_DISPLAY_NAME						"Applications")
set(CPACK_COMPONENT_C_API_DISPLAY_NAME						"C API DLL")
#set(CPACK_COMPONENT_BUILD_FILES_DEPENDS example_files)
set(CPACK_COMPONENT_COMPILED_EXAMPLES_DISPLAY_NAME			"Compiled Examples")
set(CPACK_COMPONENT_EXAMPLE_FILES_DISPLAY_NAME				"Examples Source")
set(CPACK_COMPONENT_SBML_MODELS_DISPLAY_NAME				"SBML Models")
set(CPACK_COMPONENT_DELPHI_DISPLAY_NAME						"Delphi Units")
set(CPACK_COMPONENT_PYTHON_DISPLAY_NAME						"Python Module")
set(CPACK_COMPONENT_RR_DOCS_DISPLAY_NAME					"Doxygen Documentation")

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
set(CPACK_COMPONENT_RR_DOCS_DESCRIPTION						"Installs RoadRunner Doxygen Documentation")

set(CPACK_RESOURCE_FILE_LICENSE								"${CMAKE_CURRENT_SOURCE_DIR}/trunk/License.txt")
set(CPACK_PACKAGE_VENDOR									"UW")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY						"rr library - headers, libraries and examples")
set(CPACK_PACKAGE_VERSION_MAJOR								"${RR_MAJOR_VERSION}")
set(CPACK_PACKAGE_VERSION_MINOR								"${RR_MINOR_VERSION}")
set(CPACK_PACKAGE_VERSION_PATCH								"${RR_BUILD_VERSION}")
set(CPACK_PACKAGE_INSTALL_DIRECTORY							"RoadRunner")

if(${BORLAND})
set(COMPILER_FOR_PACKAGE "cg")
set(CPACK_PACKAGE_FILE_NAME "rr-cg_xe")
endif()

if(${MSVC})
set(COMPILER_FOR_PACKAGE "vs")
set(CPACK_PACKAGE_FILE_NAME "rr-vs_2010")
endif()

if(${MINGW})
set(COMPILER_FOR_PACKAGE "mingw")
set(CPACK_PACKAGE_FILE_NAME "rr-mingw")
endif()

#MESSAGE( STATUS "CMAKE_CFG_INTDIR: " ${CMAKE_CFG_INTDIR} )
MESSAGE( STATUS "PACKAGE NAME: " ${CPACK_PACKAGE_NAME})

#set(CPACK_PACKAGE_NAME										"rr-${COMPILER_FOR_PACKAGE}")
set(CPACK_OUTPUT_FILE_PREFIX								"${CMAKE_CURRENT_SOURCE_DIR}/releases/${COMPILER_FOR_PACKAGE}")
include(CPack)

#======= DOXYGEN
#add_subdirectory(docs)


