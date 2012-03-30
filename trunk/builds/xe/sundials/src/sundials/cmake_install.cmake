# Install script for directory: C:/rrw/ThirdParty/sundials/src/sundials

# Set the install prefix
IF(NOT DEFINED CMAKE_INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX "C:/rrw/installs/xe/sundials")
ENDIF(NOT DEFINED CMAKE_INSTALL_PREFIX)
STRING(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
IF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  IF(BUILD_TYPE)
    STRING(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  ELSE(BUILD_TYPE)
    SET(CMAKE_INSTALL_CONFIG_NAME "Debug")
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
  MESSAGE("
Install shared components
")
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/sundials" TYPE FILE FILES
    "C:/rrw/ThirdParty/sundials/include/sundials/sundials_band.h"
    "C:/rrw/ThirdParty/sundials/include/sundials/sundials_dense.h"
    "C:/rrw/ThirdParty/sundials/include/sundials/sundials_direct.h"
    "C:/rrw/ThirdParty/sundials/include/sundials/sundials_iterative.h"
    "C:/rrw/ThirdParty/sundials/include/sundials/sundials_math.h"
    "C:/rrw/ThirdParty/sundials/include/sundials/sundials_nvector.h"
    "C:/rrw/ThirdParty/sundials/include/sundials/sundials_fnvector.h"
    "C:/rrw/ThirdParty/sundials/include/sundials/sundials_spbcgs.h"
    "C:/rrw/ThirdParty/sundials/include/sundials/sundials_spgmr.h"
    "C:/rrw/ThirdParty/sundials/include/sundials/sundials_sptfqmr.h"
    "C:/rrw/ThirdParty/sundials/include/sundials/sundials_types.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

