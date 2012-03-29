# Install script for directory: C:/rrw/ThirdParty/libsbml

# Set the install prefix
IF(NOT DEFINED CMAKE_INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX "c:/rrw/installs/vs-2010")
ENDIF(NOT DEFINED CMAKE_INSTALL_PREFIX)
STRING(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
IF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  IF(BUILD_TYPE)
    STRING(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  ELSE(BUILD_TYPE)
    SET(CMAKE_INSTALL_CONFIG_NAME "Release")
  ENDIF(BUILD_TYPE)
  MESSAGE(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
ENDIF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)

# Set the component getting installed.
IF(NOT CMAKE_INSTALL_COMPONENT)
  IF(COMPONENT)
    MESSAGE(STATUS "Install component: \"${COMPONENT}\"")
    SET(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  ELSE(COMPONENT)
    SET(CMAKE_INSTALL_COMPONENT)
  ENDIF(COMPONENT)
ENDIF(NOT CMAKE_INSTALL_COMPONENT)

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE PROGRAM FILES
    "C:/Program Files (x86)/Microsoft Visual Studio 10.0/VC/redist/x86/Microsoft.VC100.CRT/msvcp100.dll"
    "C:/Program Files (x86)/Microsoft Visual Studio 10.0/VC/redist/x86/Microsoft.VC100.CRT/msvcr100.dll"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE FILE FILES
    "C:/rrw/ThirdParty/libsbml/dependencies/bin/bzip2.dll"
    "C:/rrw/ThirdParty/libsbml/dependencies/bin/iconv.dll"
    "C:/rrw/ThirdParty/libsbml/dependencies/bin/libcheck.dll"
    "C:/rrw/ThirdParty/libsbml/dependencies/bin/libexpat.dll"
    "C:/rrw/ThirdParty/libsbml/dependencies/bin/libxml2.dll"
    "C:/rrw/ThirdParty/libsbml/dependencies/bin/MSVCRT.DLL"
    "C:/rrw/ThirdParty/libsbml/dependencies/bin/xerces-c_3_1.dll"
    "C:/rrw/ThirdParty/libsbml/dependencies/bin/zlib1.dll"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE FILE FILES
    "C:/rrw/ThirdParty/libsbml/dependencies/lib/bzip2.lib"
    "C:/rrw/ThirdParty/libsbml/dependencies/lib/iconv.lib"
    "C:/rrw/ThirdParty/libsbml/dependencies/lib/libcheck.lib"
    "C:/rrw/ThirdParty/libsbml/dependencies/lib/libexpat.lib"
    "C:/rrw/ThirdParty/libsbml/dependencies/lib/libxml2.lib"
    "C:/rrw/ThirdParty/libsbml/dependencies/lib/libxml2_xe.lib"
    "C:/rrw/ThirdParty/libsbml/dependencies/lib/xerces-c_3.lib"
    "C:/rrw/ThirdParty/libsbml/dependencies/lib/xerces-c_3D.lib"
    "C:/rrw/ThirdParty/libsbml/dependencies/lib/xerces-c_static_3.lib"
    "C:/rrw/ThirdParty/libsbml/dependencies/lib/xerces-c_static_3D.lib"
    "C:/rrw/ThirdParty/libsbml/dependencies/lib/zdll.lib"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/." TYPE FILE FILES
    "C:/rrw/ThirdParty/libsbml/COPYING.txt"
    "C:/rrw/ThirdParty/libsbml/FUNDING.txt"
    "C:/rrw/ThirdParty/libsbml/LICENSE.txt"
    "C:/rrw/ThirdParty/libsbml/NEWS.txt"
    "C:/rrw/ThirdParty/libsbml/README.txt"
    "C:/rrw/ThirdParty/libsbml/VERSION.txt"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  INCLUDE("C:/rrw/builds/vs-2010/libsbml/src/cmake_install.cmake")

ENDIF(NOT CMAKE_INSTALL_LOCAL_ONLY)

IF(CMAKE_INSTALL_COMPONENT)
  SET(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
ELSE(CMAKE_INSTALL_COMPONENT)
  SET(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
ENDIF(CMAKE_INSTALL_COMPONENT)

FILE(WRITE "C:/rrw/builds/vs-2010/libsbml/${CMAKE_INSTALL_MANIFEST}" "")
FOREACH(file ${CMAKE_INSTALL_MANIFEST_FILES})
  FILE(APPEND "C:/rrw/builds/vs-2010/libsbml/${CMAKE_INSTALL_MANIFEST}" "${file}\n")
ENDFOREACH(file)
