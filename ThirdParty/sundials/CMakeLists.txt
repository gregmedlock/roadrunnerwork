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
# Top level CMakeLists.txt for SUNDIALS (for cmake build system)


# -------------------------------------------------------------
# Initial commands
# -------------------------------------------------------------

# Require a fairly recent cmake version

CMAKE_MINIMUM_REQUIRED(VERSION 2.2)

# Project SUNDIALS (initially only C supported)

PROJECT(sundials C)

# Set some variables with info on the SUNDIALS project

SET(PACKAGE_BUGREPORT "radu@llnl.gov")
SET(PACKAGE_NAME "SUNDIALS")
SET(PACKAGE_STRING "SUNDIALS 2.4.0")
SET(PACKAGE_TARNAME "sundials")
SET(PACKAGE_VERSION "2.4.0")

# Prohibit in-source build

IF("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")
  MESSAGE(FATAL_ERROR "In-source build prohibited.")
ENDIF("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")

# Hide/show some cache variables

MARK_AS_ADVANCED(EXECUTABLE_OUTPUT_PATH LIBRARY_OUTPUT_PATH)
MARK_AS_ADVANCED(CLEAR
  CMAKE_C_COMPILER
  CMAKE_C_FLAGS
  CMAKE_C_FLAGS_DEBUG
  CMAKE_C_FLAGS_MINSIZEREL
  CMAKE_C_FLAGS_RELEASE
  CMAKE_C_FLAGS_RELWITHDEB)

# Specify the VERSION and SOVERSION for shared libraries

SET(cvodelib_VERSION "1.0.0")
SET(cvodelib_SOVERSION "1")

SET(cvodeslib_VERSION "2.0.0")
SET(cvodeslib_SOVERSION "2")

SET(idalib_VERSION "2.0.0")
SET(idalib_SOVERSION "2")

SET(idaslib_VERSION "0.0.0")
SET(idaslib_SOVERSION "0")

SET(kinsollib_VERSION "1.0.0")
SET(kinsollib_SOVERSION "1")

SET(cpodeslib_VERSION "0.0.0")
SET(cpodeslib_SOVERSION "0")

SET(nveclib_VERSION "0.0.2")
SET(nveclib_SOVERSION "0")

# Specify the location of additional CMAKE modules

SET(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/config)

# -------------------------------------------------------------
# MACRO definitions
# -------------------------------------------------------------

# Macros to hide/show cached variables.
# These two macros can be used to "hide" or "show" in the
# list of cached variables various variables and/or options 
# that depend on other options.
# Note that once a variable is modified, it will preserve its
# value (hidding it merely makes it internal)

MACRO(HIDE_VARIABLE var)
  IF(DEFINED ${var})
    SET(${var} "${${var}}" CACHE INTERNAL "")
  ENDIF(DEFINED ${var})
ENDMACRO(HIDE_VARIABLE)

MACRO(SHOW_VARIABLE var type doc default)
  IF(DEFINED ${var})
    SET(${var} "${${var}}" CACHE "${type}" "${doc}" FORCE)
  ELSE(DEFINED ${var})
    SET(${var} "${default}" CACHE "${type}" "${doc}")
  ENDIF(DEFINED ${var})
ENDMACRO(SHOW_VARIABLE)

# Macros to append a common suffix or prefix to the elements of a list

MACRO(ADD_SUFFIX rootlist suffix)
  SET(outlist )
  FOREACH(root ${${rootlist}})
    LIST(APPEND outlist ${root}${suffix})
  ENDFOREACH(root)
  SET(${rootlist} ${outlist})
ENDMACRO(ADD_SUFFIX)

MACRO(ADD_PREFIX prefix rootlist)
  SET(outlist )
  FOREACH(root ${${rootlist}})
    LIST(APPEND outlist ${prefix}${root})
  ENDFOREACH(root)
  SET(${rootlist} ${outlist})
ENDMACRO(ADD_PREFIX)

# Macro to print warning that some features will be disabled
# due to some failure.

MACRO(PRINT_WARNING message action)
  MESSAGE("WARNING: ${message}.\n   ${action}.")
ENDMACRO(PRINT_WARNING)

# Returns an unquoted string.  Note that CMake will readily turn such
# strings back into lists, due to the duality of lists and
# semicolon-separated strings.  So be careful how you use it.

MACRO(LIST2STRING alist astring)
  FOREACH(elem ${${alist}})
   SET(${astring} "${${astring}} ${elem}")
  ENDFOREACH(elem)
ENDMACRO(LIST2STRING)

# -------------------------------------------------------------
# Which modules to build?
# -------------------------------------------------------------

# For each SUNDIALS solver available (i.e. for which we have the
# sources), give the user the option of enabling/disabling it.

IF(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/cvode")
  OPTION(BUILD_CVODE  "Build the CVODE library"  ON)
ELSE(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/cvode")
  SET(BUILD_CVODE OFF)
ENDIF(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/cvode")

IF(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/cvodes")
  OPTION(BUILD_CVODES  "Build the CVODES library"  ON)
ELSE(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/cvodes")
  SET(BUILD_CVODES OFF)
ENDIF(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/cvodes")

IF(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/ida")
  OPTION(BUILD_IDA  "Build the IDA library"  ON)
ELSE(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/ida")
  SET(BUILD_IDA OFF)
ENDIF(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/ida")

IF(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/idas")
  OPTION(BUILD_IDAS  "Build the IDAS library"  ON)
ELSE(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/idas")
  SET(BUILD_IDAS OFF)
ENDIF(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/idas")

IF(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/kinsol")
  OPTION(BUILD_KINSOL  "Build the KINSOL library"  ON)
ELSE(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/kinsol")
  SET(BUILD_KINSOL OFF)
ENDIF(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/kinsol")

IF(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/cpodes")
  OPTION(BUILD_CPODES  "Build the CPODES library"  ON)
ELSE(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/cpodes")
  SET(BUILD_CPODES OFF)
ENDIF(IS_DIRECTORY "${sundials_SOURCE_DIR}/src/cpodes")

# -------------------------------------------------------------
# Other configuration options
# -------------------------------------------------------------

# Option that allows users to build static and/or shared libraries
# ----------------------------------------------------------------

OPTION(BUILD_STATIC_LIBS "Build static libraries" ON)
OPTION(BUILD_SHARED_LIBS "Build shared libraries" OFF)

# Prepare substitution variable SUNDIALS_EXPORT for sundials_config.h
# When building shared SUNDIALS libraries under Windows, use
#      #define SUNDIALS_EXPORT __declspec(dllexport)
# When linking to shared SUNDIALS libraries under Windows, use
#      #define SUNDIALS_EXPORT __declspec(dllimport)
# In all other cases (other platforms or static libraries
# under Windows), the SUNDIALS_EXPORT macro is empty

IF(BUILD_SHARED_LIBS AND WIN32)
  SET(SUNDIALS_EXPORT
    "#ifdef BUILD_SUNDIALS_LIBRARY
#define SUNDIALS_EXPORT __declspec(dllexport)
#else
#define SUNDIALS_EXPORT __declspec(dllimport)
#endif")
ELSE(BUILD_SHARED_LIBS AND WIN32)
  SET(SUNDIALS_EXPORT "#define SUNDIALS_EXPORT")
ENDIF(BUILD_SHARED_LIBS AND WIN32)

# Make sure we build at least one type of libraries
IF(NOT BUILD_STATIC_LIBS AND NOT BUILD_SHARED_LIBS)
  PRINT_WARNING("Both static and shared library generation were disabled"
    "Building static libraries was re-enabled")
  SET(BUILD_STATIC_LIBS ON CACHE BOOL "Build static libraries" FORCE)
ENDIF(NOT BUILD_STATIC_LIBS AND NOT BUILD_SHARED_LIBS)

# Option to specify precision
# ---------------------------

SET(SUNDIALS_PRECISION "double" CACHE STRING "double, single or extended")

# prepare substitution variable PRECISION_LEVEL for sundials_config.h
STRING(TOUPPER ${SUNDIALS_PRECISION} SUNDIALS_PRECISION)
SET(PRECISION_LEVEL "#define SUNDIALS_${SUNDIALS_PRECISION}_PRECISION 1")

# Option to use the generic math libraries (UNIX only)
# ----------------------------------------------------

IF(UNIX)
  OPTION(USE_GENERIC_MATH "Use generic (std-c) math libraries" ON)
  IF(USE_GENERIC_MATH)
    # executables will be linked against -lm 
    SET(EXTRA_LINK_LIBS -lm)
    # prepare substitution variable GENERIC_MATH_LIB for sundials_config.h
    SET(GENERIC_MATH_LIB "#define SUNDIALS_USE_GENERIC_MATH")
  ENDIF(USE_GENERIC_MATH)
ENDIF(UNIX)

# -------------------------------------------------------------
# Enable Fortran support?
# -------------------------------------------------------------

# FCMIX support is an option only if at least one solver that 
# provides such an interface is built.

IF(BUILD_CVODE OR BUILD_IDA OR BUILD_KINSOL)
  SHOW_VARIABLE(FCMIX_ENABLE BOOL "Enable Fortran-C support" OFF)
ELSE(BUILD_CVODE OR BUILD_IDA OR BUILD_KINSOL)
  HIDE_VARIABLE(FCMIX_ENABLE)
ENDIF(BUILD_CVODE OR BUILD_IDA OR BUILD_KINSOL)

# -------------------------------------------------------------
# Enable BLAS/LAPACK support?
# -------------------------------------------------------------

OPTION(LAPACK_ENABLE "Enable Lapack support" OFF)

IF(NOT LAPACK_ENABLE)
  HIDE_VARIABLE(SUNDIALS_F77_FUNC_CASE)
  HIDE_VARIABLE(SUNDIALS_F77_FUNC_UNDERSCORES)
  HIDE_VARIABLE(LAPACK_LIBRARIES)
ENDIF(NOT LAPACK_ENABLE)

# -------------------------------------------------------------
# Enable MPI support?
# -------------------------------------------------------------

OPTION(MPI_ENABLE "Enable MPI support" OFF)

IF(NOT MPI_ENABLE)
  HIDE_VARIABLE(MPI_INCLUDE_PATH)
  HIDE_VARIABLE(MPI_LIBRARIES)
  HIDE_VARIABLE(MPI_EXTRA_LIBRARIES)
  HIDE_VARIABLE(MPI_MPICC)
  HIDE_VARIABLE(MPI_MPIF77)
ENDIF(NOT MPI_ENABLE)

# -------------------------------------------------------------
# Enable examples?
# -------------------------------------------------------------

OPTION(EXAMPLES_ENABLE "Build the SUNDIALS examples" OFF)

IF(EXAMPLES_ENABLE)

  # If examples are enabled, set different options

  # The examples will be linked with the library corresponding to the build type.
  # Whenever building shared libraries, use them to link the examples.
  IF(BUILD_SHARED_LIBS)
    SET(LINK_LIBRARY_TYPE "shared")
  ELSE(BUILD_SHARED_LIBS)
    SET(LINK_LIBRARY_TYPE "static")
  ENDIF(BUILD_SHARED_LIBS)

  # Check if example files are to be exported
  SHOW_VARIABLE(EXAMPLES_INSTALL BOOL "Install example files" ON)

  # If examples are to be exported, check where we should install them.
  IF(EXAMPLES_INSTALL)

    SHOW_VARIABLE(EXAMPLES_INSTALL_PATH STRING
      "Output directory for installing example files" "${CMAKE_INSTALL_PREFIX}/examples")
    
    IF(NOT EXAMPLES_INSTALL_PATH)
      PRINT_WARNING("The example installation path is empty"
        "Example installation path was reset to its default value")
      SET(EXAMPLES_INSTALL_PATH "${CMAKE_INSTALL_PREFIX}/examples" CACHE STRING
        "Output directory for installing example files" FORCE)
    ENDIF(NOT EXAMPLES_INSTALL_PATH)

  ELSE(EXAMPLES_INSTALL)

    HIDE_VARIABLE(EXAMPLES_INSTALL_PATH)

  ENDIF(EXAMPLES_INSTALL)

ELSE(EXAMPLES_ENABLE)

  # If examples are disabled, hide all options related to
  # building and installing the SUNDIALS examples

  HIDE_VARIABLE(EXAMPLES_INSTALL)
  HIDE_VARIABLE(EXAMPLES_INSTALL_PATH)
  
ENDIF(EXAMPLES_ENABLE)

# -------------------------------------------------------------
# Add any other necessary compiler flags & definitions
# -------------------------------------------------------------

# Under Windows, add compiler directive to inhibit warnings
# about use of unsecure functions

IF(WIN32)
  ADD_DEFINITIONS(-D_CRT_SECURE_NO_WARNINGS)
ENDIF(WIN32)

# -------------------------------------------------------------
# A Fortran compiler is needed if:
# (a) FCMIX is enabled
# (b) LAPACK is enabled (for the name-mangling scheme)
# -------------------------------------------------------------

IF(FCMIX_ENABLE OR LAPACK_ENABLE)
  INCLUDE(SundialsFortran)
  IF(NOT F77_FOUND AND FCMIX_ENABLE)
    PRINT_WARNING("Fortran compiler not functional"
      "FCMIX support will not be provided")
  ENDIF(NOT F77_FOUND AND FCMIX_ENABLE)
ENDIF(FCMIX_ENABLE OR LAPACK_ENABLE)

# -------------------------------------------------------------
# Check if we need an alternate way of specifying the Fortran
# name-mangling scheme if we were unable to infer it using a
# compiler. 
# Ask the user to specify the case and number of appended underscores
# corresponding to the Fortran name-mangling scheme of symbol names 
# that do not themselves contain underscores (recall that this is all
# we really need for the interfaces to LAPACK).
# Note: the default scheme is lower case - one underscore
# -------------------------------------------------------------

IF(LAPACK_ENABLE AND NOT F77SCHEME_FOUND)
  # Specify the case for the Fortran name-mangling scheme
  SHOW_VARIABLE(SUNDIALS_F77_FUNC_CASE STRING
    "case of Fortran function names (lower/upper)"
    "lower")
  # Specify the number of appended underscores for the Fortran name-mangling scheme
  SHOW_VARIABLE(SUNDIALS_F77_FUNC_UNDERSCORES STRING 
    "number of underscores appended to Fortran function names"
    "one")  
  # Based on the given case and number of underscores,
  # set the C preprocessor macro definition
  IF(${SUNDIALS_F77_FUNC_CASE} MATCHES "lower")
    IF(${SUNDIALS_F77_FUNC_UNDERSCORES} MATCHES "none")
      SET(CMAKE_Fortran_SCHEME_NO_UNDERSCORES "mysub")
    ENDIF(${SUNDIALS_F77_FUNC_UNDERSCORES} MATCHES "none")
    IF(${SUNDIALS_F77_FUNC_UNDERSCORES} MATCHES "one")
      SET(CMAKE_Fortran_SCHEME_NO_UNDERSCORES "mysub_")
    ENDIF(${SUNDIALS_F77_FUNC_UNDERSCORES} MATCHES "one")
    IF(${SUNDIALS_F77_FUNC_UNDERSCORES} MATCHES "two")
      SET(CMAKE_Fortran_SCHEME_NO_UNDERSCORES "mysub__")
    ENDIF(${SUNDIALS_F77_FUNC_UNDERSCORES} MATCHES "two")
  ELSE(${SUNDIALS_F77_FUNC_CASE} MATCHES "lower")
    IF(${SUNDIALS_F77_FUNC_UNDERSCORES} MATCHES "none")
      SET(CMAKE_Fortran_SCHEME_NO_UNDERSCORES "MYSUB")
    ENDIF(${SUNDIALS_F77_FUNC_UNDERSCORES} MATCHES "none")
    IF(${SUNDIALS_F77_FUNC_UNDERSCORES} MATCHES "one")
      SET(CMAKE_Fortran_SCHEME_NO_UNDERSCORES "MYSUB_")
    ENDIF(${SUNDIALS_F77_FUNC_UNDERSCORES} MATCHES "one")
    IF(${SUNDIALS_F77_FUNC_UNDERSCORES} MATCHES "two")
      SET(CMAKE_Fortran_SCHEME_NO_UNDERSCORES "MYSUB__")
    ENDIF(${SUNDIALS_F77_FUNC_UNDERSCORES} MATCHES "two")
  ENDIF(${SUNDIALS_F77_FUNC_CASE} MATCHES "lower")
  # Since the SUNDIALS codes never use symbol names containing
  # underscores, set a default scheme (probably wrong) for symbols
  # with underscores.
  SET(CMAKE_Fortran_SCHEME_WITH_UNDERSCORES "my_sub_")
  # We now "have" a scheme.
  SET(F77SCHEME_FOUND TRUE)
ENDIF(LAPACK_ENABLE AND NOT F77SCHEME_FOUND)

# -------------------------------------------------------------
# If we have a name-mangling scheme (either automatically
# inferred or provided by the user), set the SUNDIALS 
# compiler preprocessor macro definitions.
# -------------------------------------------------------------

SET(F77_MANGLE_MACRO1 "")
SET(F77_MANGLE_MACRO2 "")

IF(F77SCHEME_FOUND)
  # Symbols WITHOUT underscores
  IF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "mysub")
    SET(F77_MANGLE_MACRO1 "#define SUNDIALS_F77_FUNC(name,NAME) name")
  ENDIF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "mysub")
  IF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "mysub_")
    SET(F77_MANGLE_MACRO1 "#define SUNDIALS_F77_FUNC(name,NAME) name ## _")
  ENDIF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "mysub_")
  IF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "mysub__")
    SET(F77_MANGLE_MACRO1 "#define SUNDIALS_F77_FUNC(name,NAME) name ## __")
  ENDIF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "mysub__")
  IF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "MYSUB")
    SET(F77_MANGLE_MACRO1 "#define SUNDIALS_F77_FUNC(name,NAME) NAME")
  ENDIF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "MYSUB")
  IF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "MYSUB_")
    SET(F77_MANGLE_MACRO1 "#define SUNDIALS_F77_FUNC(name,NAME) NAME ## _")
  ENDIF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "MYSUB_")
  IF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "MYSUB__")
    SET(F77_MANGLE_MACRO1 "#define SUNDIALS_F77_FUNC(name,NAME) NAME ## __")
  ENDIF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "MYSUB__")
  # Symbols with underscores 
  IF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "my_sub")
    SET(F77_MANGLE_MACRO2 "#define SUNDIALS_F77_FUNC_(name,NAME) name")
  ENDIF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "my_sub")
  IF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "my_sub_")
    SET(F77_MANGLE_MACRO2 "#define SUNDIALS_F77_FUNC_(name,NAME) name ## _")
  ENDIF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "my_sub_")
  IF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "my_sub__")
    SET(F77_MANGLE_MACRO2 "#define SUNDIALS_F77_FUNC_(name,NAME) name ## __")
  ENDIF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "my_sub__")
  IF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "MY_SUB")
    SET(F77_MANGLE_MACRO2 "#define SUNDIALS_F77_FUNC_(name,NAME) NAME")
  ENDIF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "MY_SUB")
  IF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "MY_SUB_")
    SET(F77_MANGLE_MACRO2 "#define SUNDIALS_F77_FUNC_(name,NAME) NAME ## _")
  ENDIF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "MY_SUB_")
  IF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "MY_SUB__")
    SET(F77_MANGLE_MACRO2 "#define SUNDIALS_F77_FUNC_(name,NAME) NAME ## __")
  ENDIF(${CMAKE_Fortran_SCHEME_NO_UNDERSCORES} MATCHES "MY_SUB__")
ENDIF(F77SCHEME_FOUND)

# -------------------------------------------------------------
# Find (and test) the Lapack libraries
# -------------------------------------------------------------

# If LAPACK is needed, first try to find the appropriate
# libraries and linker flags needed to link against them.

# Macro to be inserted in sundials_config.h
SET(BLAS_LAPACK_MACRO "#define SUNDIALS_BLAS_LAPACK 0")

IF(LAPACK_ENABLE)

  INCLUDE(SundialsLapack)

  IF(LAPACK_FOUND)
    SET(BLAS_LAPACK_MACRO "#define SUNDIALS_BLAS_LAPACK 1")
  ELSE(LAPACK_FOUND)
    SHOW_VARIABLE(LAPACK_LIBRARIES STRING "Lapack libraries" "${LAPACK_LIBRARIES}")
    SHOW_VARIABLE(LAPACK_LINKER_FLAGS STRING "Lapack required linker flags" "${LAPACK_LINKER_FLAGS}")
  ENDIF(LAPACK_FOUND)

  IF(LAPACK_LIBRARIES AND NOT LAPACK_FOUND)
    PRINT_WARNING("LAPACK not functional"
      "Blas/Lapack support will not be provided")
  ENDIF(LAPACK_LIBRARIES AND NOT LAPACK_FOUND)

ELSE(LAPACK_ENABLE)

  HIDE_VARIABLE(LAPACK_LIBRARIES)
  HIDE_VARIABLE(LAPACK_LINKER_FLAGS)

ENDIF(LAPACK_ENABLE)

# -------------------------------------------------------------
# Decide how to compile MPI codes.
# -------------------------------------------------------------

IF(MPI_ENABLE)
  INCLUDE(SundialsMPIC)
  IF(NOT MPIC_FOUND)
    PRINT_WARNING("MPI not functional"
      "Parallel support will not be provided")
  ENDIF(NOT MPIC_FOUND)
  IF(MPIC_MPI2)
    SET(F77_MPI_COMM_F2C "#define SUNDIALS_MPI_COMM_F2C 1")
  ELSE(MPIC_MPI2)
    SET(F77_MPI_COMM_F2C "#define SUNDIALS_MPI_COMM_F2C 0")
  ENDIF(MPIC_MPI2)
  IF(MPIC_FOUND AND FCMIX_ENABLE)
    INCLUDE(SundialsMPIF)
  ENDIF(MPIC_FOUND AND FCMIX_ENABLE)
ENDIF(MPI_ENABLE)

# -------------------------------------------------------------
# Configure the header file sundials_config.h
# -------------------------------------------------------------

# All required substitution variables should be available at this point.
# Generate the header file and place it in the binary dir.
CONFIGURE_FILE(
  ${PROJECT_SOURCE_DIR}/include/sundials/sundials_config.in
  ${PROJECT_BINARY_DIR}/include/sundials/sundials_config.h
  )

# Add the include directory in the source tree and the one in
# the binary tree (for the header file sundials_config.h)
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/include ${PROJECT_BINARY_DIR}/include)

# -------------------------------------------------------------
# Add selected modules to the build system
# -------------------------------------------------------------

# Shared components

ADD_SUBDIRECTORY(src/sundials)
ADD_SUBDIRECTORY(src/nvec_ser)
IF(MPIC_FOUND)
  ADD_SUBDIRECTORY(src/nvec_par)
ENDIF(MPIC_FOUND)

# CVODE library

IF(BUILD_CVODE)	
  ADD_SUBDIRECTORY(src/cvode)
  IF(FCMIX_ENABLE AND F77_FOUND)
    ADD_SUBDIRECTORY(src/cvode/fcmix)
  ENDIF(FCMIX_ENABLE AND F77_FOUND)
ENDIF(BUILD_CVODE)

# CVODES library

IF(BUILD_CVODES)	
  ADD_SUBDIRECTORY(src/cvodes)
ENDIF(BUILD_CVODES)

# IDA library

IF(BUILD_IDA)	
  ADD_SUBDIRECTORY(src/ida)
  IF(FCMIX_ENABLE AND F77_FOUND)
    ADD_SUBDIRECTORY(src/ida/fcmix)
  ENDIF(FCMIX_ENABLE AND F77_FOUND)
ENDIF(BUILD_IDA)

# IDAS library

IF(BUILD_IDAS)	
  ADD_SUBDIRECTORY(src/idas)
ENDIF(BUILD_IDAS)

# KINSOL library

IF(BUILD_KINSOL)	
  ADD_SUBDIRECTORY(src/kinsol)
  IF(FCMIX_ENABLE AND F77_FOUND)
    ADD_SUBDIRECTORY(src/kinsol/fcmix)
  ENDIF(FCMIX_ENABLE AND F77_FOUND)
ENDIF(BUILD_KINSOL)

# CPODES library

IF(BUILD_CPODES)	
  ADD_SUBDIRECTORY(src/cpodes)
ENDIF(BUILD_CPODES)

# -------------------------------------------------------------
# Include the subdirectories corresponding to various examples
# -------------------------------------------------------------

# If building and installing the examples is enabled, include
# the subdirectories for those examples that will be built.
# Also, if we will generate exported example Makefiles, set 
# variables needed in generating them from templates.

IF(EXAMPLES_ENABLE)

  IF(EXAMPLES_INSTALL)
    SET(SHELL "sh")
    SET(prefix "${CMAKE_INSTALL_PREFIX}")
    SET(exec_prefix "${CMAKE_INSTALL_PREFIX}")
    SET(includedir "${prefix}/include")
    SET(libdir "${exec_prefix}/lib")
    SET(CPP "${CMAKE_C_COMPILER}")
    SET(CC "${CMAKE_C_COMPILER}")
    SET(CPPFLAGS "${CMAKE_C_FLAGS_RELEASE}")
    SET(CFLAGS "${CMAKE_C_FLAGS_RELEASE}")
    SET(LDFLAGS "${CMAKE_EXE_LINKER_FLAGS_RELEASE}")
    LIST2STRING(EXTRA_LINK_LIBS LIBS)
    IF(F77_FOUND)
      SET(F77 "${CMAKE_Fortran_COMPILER}")
      SET(F77_LNKR "${CMAKE_Fortran_COMPILER}")
      SET(FFLAGS "${CMAKE_Fortran_FLAGS_RELEASE}")
      SET(F77_LDFLAGS "${CMAKE_Fortran_FLAGS_RELEASE}")
      LIST2STRING(EXTRA_LINK_LIBS F77_LIBS)
    ENDIF(F77_FOUND)
    IF(LAPACK_FOUND)
      LIST2STRING(LAPACK_LIBRARIES BLAS_LAPACK_LIBS)
      SET(BLAS_LAPACK_LIBS "${LAPACK_LINKER_FLAGS} ${BLAS_LAPACK_LIBS}")
    ENDIF(LAPACK_FOUND)
    IF(MPIC_FOUND)
      IF(MPI_MPICC)
        SET(MPICC "${MPI_MPICC}")
        SET(MPI_INC_DIR ".")
        SET(MPI_LIB_DIR ".")
        SET(MPI_LIBS "")
        SET(MPI_FLAGS "")
      ELSE(MPI_MPICC)
        SET(MPICC "${CMAKE_C_COMPILER}")
        SET(MPI_INC_DIR "${MPI_INCLUDE_PATH}")
        SET(MPI_LIB_DIR ".")
        LIST2STRING(MPI_LIBRARIES MPI_LIBS)
      ENDIF(MPI_MPICC)
    ENDIF(MPIC_FOUND)
    IF(MPIF_FOUND)
      IF(MPI_MPIF77)
        SET(MPIF77 "${MPI_MPIF77}")
        SET(MPIF77_LNKR "${MPI_MPIF77}")
      ELSE(MPI_MPIF77)
        SET(MPIF77 "${CMAKE_Fortran_COMPILER}")
        SET(MPIF77_LNKR "${CMAKE_Fortran_COMPILER}")
        SET(MPI_INC_DIR "${MPI_INCLUDE_PATH}")
        SET(MPI_LIB_DIR ".")
        LIST2STRING(MPI_LIBRARIES MPI_LIBS)
      ENDIF(MPI_MPIF77)
    ENDIF(MPIF_FOUND)
  ENDIF(EXAMPLES_INSTALL)

  IF(BUILD_CVODE)
    ADD_SUBDIRECTORY(examples/cvode/serial)
    IF(FCMIX_ENABLE AND F77_FOUND)
      ADD_SUBDIRECTORY(examples/cvode/fcmix_serial)
    ENDIF(FCMIX_ENABLE AND F77_FOUND)
    IF(MPIC_FOUND)
      ADD_SUBDIRECTORY(examples/cvode/parallel)
    ENDIF(MPIC_FOUND)
    IF(MPIF_FOUND)
      ADD_SUBDIRECTORY(examples/cvode/fcmix_parallel)
    ENDIF(MPIF_FOUND)
  ENDIF(BUILD_CVODE)
  
  IF(BUILD_CVODES)	
    ADD_SUBDIRECTORY(examples/cvodes/serial)
    IF(MPIC_FOUND)
      ADD_SUBDIRECTORY(examples/cvodes/parallel)
    ENDIF(MPIC_FOUND)
  ENDIF(BUILD_CVODES)
  
  IF(BUILD_IDA)
    ADD_SUBDIRECTORY(examples/ida/serial)
    IF(FCMIX_ENABLE AND F77_FOUND)
      ADD_SUBDIRECTORY(examples/ida/fcmix_serial)
    ENDIF(FCMIX_ENABLE AND F77_FOUND)
    IF(MPIC_FOUND)
      ADD_SUBDIRECTORY(examples/ida/parallel)
    ENDIF(MPIC_FOUND)
    IF(MPIF_FOUND)
      ADD_SUBDIRECTORY(examples/ida/fcmix_parallel)
    ENDIF(MPIF_FOUND)
  ENDIF(BUILD_IDA)
  
  IF(BUILD_IDAS)	
    ADD_SUBDIRECTORY(examples/idas/serial)
    IF(MPIC_FOUND)
      ADD_SUBDIRECTORY(examples/idas/parallel)
    ENDIF(MPIC_FOUND)
  ENDIF(BUILD_IDAS)

  IF(BUILD_KINSOL)	
    ADD_SUBDIRECTORY(examples/kinsol/serial)
    IF(FCMIX_ENABLE AND F77_FOUND)
      ADD_SUBDIRECTORY(examples/kinsol/fcmix_serial)
    ENDIF(FCMIX_ENABLE AND F77_FOUND)
    IF(MPIC_FOUND)
      ADD_SUBDIRECTORY(examples/kinsol/parallel)
    ENDIF(MPIC_FOUND)
    IF(MPIF_FOUND)
      ADD_SUBDIRECTORY(examples/kinsol/fcmix_parallel)
    ENDIF(MPIF_FOUND)
  ENDIF(BUILD_KINSOL)

  IF(BUILD_CPODES)	
    ADD_SUBDIRECTORY(examples/cpodes/serial)
    IF(MPIC_FOUND)
      ADD_SUBDIRECTORY(examples/cpodes/parallel)
    ENDIF(MPIC_FOUND)
  ENDIF(BUILD_CPODES)
  
ENDIF(EXAMPLES_ENABLE)

#----------------------------------
# Install configuration header file
#----------------------------------

# install configured header file
INSTALL(
  FILES ${PROJECT_BINARY_DIR}/include/sundials/sundials_config.h
  DESTINATION include/sundials
  )

