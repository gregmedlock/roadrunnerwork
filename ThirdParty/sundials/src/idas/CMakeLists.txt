# ---------------------------------------------------------------
# $Revision$
# $Date$
# ---------------------------------------------------------------
# Programmer:  Radu Serban @ LLNL
# ---------------------------------------------------------------
# Copyright (c) 2007, The Regents of the University of California.
# Produced at the Lawrence Livermore National Laboratory.
# All rights reserved.
# For details, see the LICENSE file.
# ---------------------------------------------------------------
# CMakeLists.txt file for the IDAS library

INSTALL(CODE "MESSAGE(\"\nInstall IDAS\n\")")

# Add variable idas_SOURCES with the sources for the IDAS library
SET(idas_SOURCES
  idas.c
  idaa.c
  idas_io.c
  idas_ic.c
  idaa_io.c
  idas_direct.c
  idas_band.c
  idas_dense.c
  idas_spils.c
  idas_spbcgs.c
  idas_spgmr.c
  idas_sptfqmr.c
  idas_bbdpre.c
  )

# Add variable shared_SOURCES with the common SUNDIALS sources which will
# also be included in the IDAS library
SET(shared_SOURCES
  sundials_nvector.c
  sundials_math.c
  sundials_direct.c
  sundials_band.c
  sundials_dense.c
  sundials_iterative.c
  sundials_spbcgs.c
  sundials_spgmr.c
  sundials_sptfqmr.c
  )

# Add prefix with complete path to the common SUNDIALS sources
ADD_PREFIX(${sundials_SOURCE_DIR}/src/sundials/ shared_SOURCES)

# Add variable idas_HEADERS with the exported IDAS header files
SET(idas_HEADERS
  idas_band.h
  idas_bbdpre.h
  idas_dense.h
  idas_direct.h
  idas.h
  idas_spbcgs.h
  idas_spgmr.h
  idas_spils.h
  idas_sptfqmr.h
  )

# Add prefix with complete path to the IDAS header files
ADD_PREFIX(${sundials_SOURCE_DIR}/include/idas/ idas_HEADERS)

# If Blas/Lapack support was enabled, set-up additional file lists
IF(LAPACK_FOUND)
  SET(idas_BL_SOURCES idas_lapack.c)
  SET(idas_BL_HEADERS idas_lapack.h)
  ADD_PREFIX(${sundials_SOURCE_DIR}/include/idas/ idas_BL_HEADERS)
ELSE(LAPACK_FOUND)
  SET(idas_BL_SOURCES "")
  SET(idas_BL_HEADERS "")
ENDIF(LAPACK_FOUND)

# Add source directories to include directories for access to
# implementation only header files.
INCLUDE_DIRECTORIES(.)
INCLUDE_DIRECTORIES(../sundials)

# Define C preprocessor flag -DBUILD_SUNDIALS_LIBRARY 
ADD_DEFINITIONS(-DBUILD_SUNDIALS_LIBRARY)

# Build the static library
IF(BUILD_STATIC_LIBS)

  # Add the build target for the static IDAS library
  ADD_LIBRARY(sundials_idas_static STATIC 
    ${idas_SOURCES} ${idas_BL_SOURCES}  ${shared_SOURCES})

  # Set the library name and make sure it is not deleted
  SET_TARGET_PROPERTIES(sundials_idas_static
    PROPERTIES OUTPUT_NAME sundials_idas CLEAN_DIRECT_OUTPUT 1)

  # Install the IDA library
  INSTALL(TARGETS sundials_idas_static DESTINATION lib)

ENDIF(BUILD_STATIC_LIBS)

# Build the shared library
IF(BUILD_SHARED_LIBS)

  # Add the build target for the IDAS library
  ADD_LIBRARY(sundials_idas_shared SHARED 
    ${idas_SOURCES}  ${idas_BL_SOURCES}  ${shared_SOURCES})

  # Set the library name and make sure it is not deleted
  SET_TARGET_PROPERTIES(sundials_idas_shared
    PROPERTIES OUTPUT_NAME sundials_idas CLEAN_DIRECT_OUTPUT 1)

  # Set VERSION and SOVERSION for shared libraries
  SET_TARGET_PROPERTIES(sundials_idas_shared
    PROPERTIES VERSION ${idaslib_VERSION} SOVERSION ${idaslib_SOVERSION})

  # Install the IDAS library
  INSTALL(TARGETS sundials_idas_shared DESTINATION lib)

ENDIF(BUILD_SHARED_LIBS)

# Install the IDAS header files
INSTALL(FILES ${idas_HEADERS} ${idas_BL_HEADERS} DESTINATION include/idas)

# Install the IDAS implementation header file
INSTALL(FILES idas_impl.h DESTINATION include/idas)

#
MESSAGE(STATUS "Added IDAS module")
