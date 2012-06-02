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
set(LIBSBML_INSTALL_FOLDER ${RR_INSTALLS_ROOT}/xe CACHE PATH "Directory containing libsbml libraries.")
if("${LIBSBML_INSTALL_FOLDER}" STREQUAL "")
  set(LIBSBML_INSTALL_FOLDER ${RR_INSTALLS_ROOT}/xe CACHE PATH "Directory containing libsbml libraries" FORCE)
endif()
else(${BORLAND})
set(LIBSBML_INSTALL_FOLDER ${RR_INSTALLS_ROOT}/vs CACHE PATH "Directory containing libsbml libraries.")
if("${LIBSBML_INSTALL_FOLDER}" STREQUAL "")
  set(LIBSBML_INSTALL_FOLDER ${RR_INSTALLS_ROOT}/vs CACHE PATH "Directory containing libsbml libraries" FORCE)
endif()
endif()

if(${BORLAND})
set(CVODE_INSTALL_FOLDER ${RR_INSTALLS_ROOT}/xe CACHE PATH "Directory containing cvode libraries.")
if("${CVODE_INSTALL_FOLDER}" STREQUAL "")
  set(CVODE_INSTALL_FOLDER ${RR_INSTALLS_ROOT}/xe CACHE PATH "Directory containing cvode libraries" FORCE)
endif()
else(${BORLAND})
set(CVODE_INSTALL_FOLDER ${RR_INSTALLS_ROOT}/vs CACHE PATH "Directory containing cvode libraries.")
if("${CVODE_INSTALL_FOLDER}" STREQUAL "")
  set(CVODE_INSTALL_FOLDER ${RR_INSTALLS_ROOT}/vs CACHE PATH "Directory containing cvode libraries" FORCE)
endif()
endif()

if(${BORLAND})
set(CLAPACK_BUILD_FOLDER ${RR_BUILDS_ROOT}/xe/clapack CACHE PATH "Directory containing clapack libraries.")
if("${CLAPACK_BUILD_FOLDER}" STREQUAL "")
  set(CLAPACK_BUILD_FOLDER ${RR_BUILDS_ROOT}/xe/clapack CACHE PATH "Directory containing clapack libraries" FORCE)
endif()
else(${BORLAND})
set(CLAPACK_BUILD_FOLDER ${RR_BUILDS_ROOT}/vs/clapack CACHE PATH "Directory containing clapack libraries.")
if("${CLAPACK_BUILD_FOLDER}" STREQUAL "")
  set(CLAPACK_BUILD_FOLDER ${RR_BUILDS_ROOT}/vs/clapack CACHE PATH "Directory containing clapack libraries" FORCE)
endif()
endif()

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

IF(WIN32)
    IF(${MSVC})
        IF(CMAKE_BUILD_TYPE MATCHES "Debug")
            set(exe_path ${exe_path}/Debug)
            set(lib_path ${lib_path}/Debug)
        ELSE(CMAKE_BUILD_TYPE MATCHES "Debug")
            #   set(exe_path ${exe_path}/Release)
            #   set(lib_path ${lib_path}/Release)
        ENDIF(CMAKE_BUILD_TYPE MATCHES "Debug")
    ELSE(WIN32)
   # Linux
    ENDIF(${MSVC})
ENDIF(WIN32)
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
link_directories(					${PROJECT_BINARY_DIR}/lib)
link_directories(					${LIBSBML_INSTALL_FOLDER}/lib)
link_directories(					${THIRD_PARTY_FOLDER}/libsbml/dependencies/lib)
link_directories(					${CLAPACK_BUILD_FOLDER}/SRC)
link_directories(					${CLAPACK_BUILD_FOLDER}/BLAS/SRC)
link_directories(					${CLAPACK_BUILD_FOLDER}/F2CLIBS/libf2c)

if(${MSVC})
link_directories(					${CLAPACK_BUILD_FOLDER}/SRC/Debug)
link_directories(					${CLAPACK_BUILD_FOLDER}/BLAS/SRC/Debug)
link_directories(					${CLAPACK_BUILD_FOLDER}/F2CLIBS/libf2c/Debug)
add_definitions(-DCRT_NONSTDC_NO_DEPRECATE)
SET (CMAKE_CXX_FLAGS 			"${CMAKE_CXX_FLAGS} /EHsc ")
SET (CMAKE_SHARED_LINKER_FLAGS 	"${CMAKE_SHARED_LINKR_FLAGS} /FORCE:MULTIPLE")
endif()

#=== COMPILER FLAGS 
if(${BORLAND})
link_directories(${THIRD_PARTY_FOLDER}/libsbml_xe/dependencies/lib)
add_definitions(-DUSE_PCH) #Enable pre-compiled headers
add_definitions(-H=${PROJECT_BINARY_DIR}/rr_pch.csm)
add_definitions(-w-8012)  #Comparing signed /unsigned
add_definitions(-w-8057)  #Parameter never used
add_definitions(-w-8004)  #'var' is assigned a value that is never used
endif()

#Common compiler definitions
add_definitions(-DLIBSBML_USE_CPP_NAMESPACE) 
add_definitions(-DNO_LIBSTRUCT_DLL) 
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
install (FILES 	${THIRD_PARTY_FOLDER}/runtime_dependencies/cg/xe/${runtime} DESTINATION bin COMPONENT rr_core)
endforeach()
endif()

##=== SBML dependencies
#set(SBML_DEPENDENCIES libxml2.dll iconv.dll zlib1.dll)
#foreach(system_file ${SBML_DEPENDENCIES})
#install (FILES ${THIRD_PARTY_FOLDER}/libsbml_dependencies/bin/${system_file}	DESTINATION bin COMPONENT c_api)
#endforeach()
#
#set(SBML_DEPENDENCIES libxml2)
#foreach(system_file ${SBML_DEPENDENCIES})
#if(${BORLAND})
#install (FILES ${THIRD_PARTY_FOLDER}/libsbml_dependencies/lib/${system_file}_xe.lib	DESTINATION lib COMPONENT c_api)
#else()
#install (FILES ${THIRD_PARTY_FOLDER}/libsbml_dependencies/lib/${system_file}.lib	DESTINATION lib COMPONENT c_api)
#endif()
#endforeach()

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


