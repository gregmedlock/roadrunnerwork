cmake_minimum_required(VERSION 2.8)
project(rr)

#These will be the defaults ...
SET(RR_BUILDS_ROOT      "c:/builds")
SET(RR_INSTALLS_ROOT    "c:/installs")

if(${BORLAND})
set(FOLDER_POSTFIX "xe")
else(${BORLAND})
set(FOLDER_POSTFIX "vs")
endif()

set(THIRD_PARTY_INSTALL_FOLDER ${RR_INSTALLS_ROOT}/${FOLDER_POSTFIX}/release CACHE PATH "Directory containing ThirdParty libraries.")
if("${THIRD_PARTY_INSTALL_FOLDER}" STREQUAL "")
  set(THIRD_PARTY_INSTALL_FOLDER ${RR_INSTALLS_ROOT}/${FOLDER_POSTFIX}/release CACHE PATH "Directory containing ThirdParty libraries" FORCE)
endif()

#set(THIRD_PARTY_BUILD_FOLDER ${RR_BUILDS_ROOT}/${FOLDER_POSTFIX}/ThirdParty CACHE PATH "Directory containing ThirdParty build.")
#if("${THIRD_PARTY_BUILD_FOLDER}" STREQUAL "")
#  set(THIRD_PARTY_BUILD_FOLDER ${RR_BUILDS_ROOT}/${FOLDER_POSTFIX}/ThirdParty CACHE PATH "Directory containing ThirdParty build" FORCE)
#endif()

OPTION (INSTALL_CXX_API 		"Package c++ api" 	        	    ON)
OPTION (INSTALL_PYTHON 			"Package Python wrapper" 		    ON)
OPTION (INSTALL_DELPHI  		"Package Delphi wrapper "           OFF)
OPTION (INSTALL_C_API	 		"Generate and Package C API" 		ON)
OPTION (INSTALL_APPS	    	"Build and install Apps"	    	OFF)
OPTION (INSTALL_EXAMPLES		"Build and install Examples"		OFF)
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
                                    ${THIRD_PARTY_FOLDER}/dependencies/libsbml/lib
                                    ${THIRD_PARTY_INSTALL_FOLDER}/lib
                 )

if(${MSVC})
add_definitions(-DCRT_NONSTDC_NO_DEPRECATE)
SET (CMAKE_CXX_FLAGS 			"${CMAKE_CXX_FLAGS} /EHsc ")
SET (CMAKE_SHARED_LINKER_FLAGS 	"${CMAKE_SHARED_LINKER_FLAGS} ") #/FORCE:MULTIPLE")
endif()

#=== COMPILER FLAGS 
if(${MINGW})
SET (CMAKE_CXX_FLAGS 			"${CMAKE_CXX_FLAGS} -std=gnu++0x")
endif()

if(${BORLAND})
link_directories(${THIRD_PARTY_FOLDER}/dependencies/libsbml/lib)
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
) 

#How do we move forward from this folder? Moving on to subfolders
#Source Code Folder

add_subdirectory(source)        # We alway have to build the roadrunner core

if(INSTALL_CXX_API)

if(INSTALL_APPS)
add_subdirectory(apps)
endif()

if(INSTALL_EXAMPLES)
add_subdirectory(examples)
endif()
endif()

add_subdirectory(Wrappers)
add_subdirectory(compilers)
add_subdirectory(rr_support)

#======================= INSTALL ROADRUNNER ========================================================
if(INSTALL_CXX_API)
install(FILES source/rr_pch.h	
        DESTINATION include 
        COMPONENT rr_core)
endif()

install(FILES Readme.txt License.txt	
        DESTINATION . 	
        COMPONENT info
        )

#=== ThirdParties
if(${BORLAND})
set(CG_RUNTIMES		cc32110MT.dll)
foreach(runtime ${CG_RUNTIMES})
install (FILES 
            ${THIRD_PARTY_FOLDER}/dependencies/cg/xe/${runtime} 
            DESTINATION bin 
            COMPONENT rr_core)
endforeach()
endif()


#One day these may be statically linked
set(RUNTIMES libxml2.DLL iconv.dll zlib1.dll)
foreach(runtime ${RUNTIMES})
install(    FILES 	${THIRD_PARTY_FOLDER}/dependencies/libsbml/bin/${runtime} 
            DESTINATION bin 
            COMPONENT rr_core)
endforeach()

