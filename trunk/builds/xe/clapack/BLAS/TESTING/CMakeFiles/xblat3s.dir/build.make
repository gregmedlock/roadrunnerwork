# CMAKE generated file: DO NOT EDIT!
# Generated by "Borland Makefiles" Generator, CMake Version 2.8

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canoncical targets will work.
.SUFFIXES:

.SUFFIXES: .hpux_make_needs_suffix_list

# Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force: NUL
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE
NULL=nul
!ENDIF
SHELL = cmd.exe

# The CMake executable.
CMAKE_COMMAND = "C:\Program Files (x86)\CMake 2.8\bin\cmake.exe"

# The command to remove a file.
RM = "C:\Program Files (x86)\CMake 2.8\bin\cmake.exe" -E remove -f

# The program to use to edit the cache.
CMAKE_EDIT_COMMAND = "C:\Program Files (x86)\CMake 2.8\bin\cmake-gui.exe"

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = C:\rrw\ThirdParty\clapack\3.2.1

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = C:\rrw\builds\xe\clapack

# Include any dependencies generated for this target.
!include BLAS\TESTING\CMakeFiles\xblat3s.dir\depend.make

# Include the progress variables for this target.
!include BLAS\TESTING\CMakeFiles\xblat3s.dir\progress.make

# Include the compile flags for this target's objects.
!include BLAS\TESTING\CMakeFiles\xblat3s.dir\flags.make

BLAS\TESTING\CMakeFiles\xblat3s.dir\sblat3.c.obj: BLAS\TESTING\CMakeFiles\xblat3s.dir\flags.make
BLAS\TESTING\CMakeFiles\xblat3s.dir\sblat3.c.obj: C:\rrw\ThirdParty\clapack\3.2.1\BLAS\TESTING\sblat3.c
	$(CMAKE_COMMAND) -E cmake_progress_report C:\rrw\builds\xe\clapack\CMakeFiles $(CMAKE_PROGRESS_1)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building C object BLAS/TESTING/CMakeFiles/xblat3s.dir/sblat3.c.obj"
	cd C:\rrw\builds\xe\clapack\BLAS\TESTING
	C:\PROGRA~2\EMBARC~1\RADSTU~1\8.0\bin\bcc32.exe  -tWR -tW- $(C_DEFINES) @&&|
-DWIN32 -oCMakeFiles\xblat3s.dir\sblat3.c.obj $(C_FLAGS) -c C:\rrw\ThirdParty\clapack\3.2.1\BLAS\TESTING\sblat3.c
|
	cd C:\rrw\builds\xe\clapack

BLAS\TESTING\CMakeFiles\xblat3s.dir\sblat3.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/xblat3s.dir/sblat3.c.i"
	cd C:\rrw\builds\xe\clapack\BLAS\TESTING
	cpp32 $(C_DEFINES) @&&|
-DWIN32 $(C_FLAGS) -oCMakeFiles\xblat3s.dir\sblat3.c.i -c C:\rrw\ThirdParty\clapack\3.2.1\BLAS\TESTING\sblat3.c
|
	cd C:\rrw\builds\xe\clapack

BLAS\TESTING\CMakeFiles\xblat3s.dir\sblat3.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/xblat3s.dir/sblat3.c.s"
	$(CMAKE_COMMAND) -E cmake_unimplemented_variable CMAKE_C_CREATE_ASSEMBLY_SOURCE

BLAS\TESTING\CMakeFiles\xblat3s.dir\sblat3.c.obj.requires:
.PHONY : BLAS\TESTING\CMakeFiles\xblat3s.dir\sblat3.c.obj.requires

BLAS\TESTING\CMakeFiles\xblat3s.dir\sblat3.c.obj.provides: BLAS\TESTING\CMakeFiles\xblat3s.dir\sblat3.c.obj.requires
	$(MAKE) -f BLAS\TESTING\CMakeFiles\xblat3s.dir\build.make -$(MAKEFLAGS) BLAS\TESTING\CMakeFiles\xblat3s.dir\sblat3.c.obj.provides.build
.PHONY : BLAS\TESTING\CMakeFiles\xblat3s.dir\sblat3.c.obj.provides

BLAS\TESTING\CMakeFiles\xblat3s.dir\sblat3.c.obj.provides.build: BLAS\TESTING\CMakeFiles\xblat3s.dir\sblat3.c.obj

# Object files for target xblat3s
xblat3s_OBJECTS = \
"CMakeFiles\xblat3s.dir\sblat3.c.obj"

# External object files for target xblat3s
xblat3s_EXTERNAL_OBJECTS =

BLAS\TESTING\xblat3s.exe: BLAS\TESTING\CMakeFiles\xblat3s.dir\sblat3.c.obj
BLAS\TESTING\xblat3s.exe: BLAS\SRC\blas.lib
BLAS\TESTING\xblat3s.exe: F2CLIBS\libf2c\libf2c.lib
BLAS\TESTING\xblat3s.exe: BLAS\TESTING\CMakeFiles\xblat3s.dir\build.make
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --red --bold "Linking C executable xblat3s.exe"
	cd C:\rrw\builds\xe\clapack\BLAS\TESTING
	C:\PROGRA~2\EMBARC~1\RADSTU~1\8.0\bin\bcc32.exe  -tWR -tW- -exblat3s.exe @&&|
-tM -lS:10000000 -lSc:10000000   -v -tWC  -tM -Od -v  ..\SRC\blas.lib ..\..\F2CLIBS\libf2c\libf2c.lib import32.lib  $(xblat3s_OBJECTS) $(xblat3s_EXTERNAL_OBJECTS)
|
	cd C:\rrw\builds\xe\clapack

# Rule to build all files generated by this target.
BLAS\TESTING\CMakeFiles\xblat3s.dir\build: BLAS\TESTING\xblat3s.exe
.PHONY : BLAS\TESTING\CMakeFiles\xblat3s.dir\build

BLAS\TESTING\CMakeFiles\xblat3s.dir\requires: BLAS\TESTING\CMakeFiles\xblat3s.dir\sblat3.c.obj.requires
.PHONY : BLAS\TESTING\CMakeFiles\xblat3s.dir\requires

BLAS\TESTING\CMakeFiles\xblat3s.dir\clean:
	cd C:\rrw\builds\xe\clapack\BLAS\TESTING
	$(CMAKE_COMMAND) -P CMakeFiles\xblat3s.dir\cmake_clean.cmake
	cd C:\rrw\builds\xe\clapack
.PHONY : BLAS\TESTING\CMakeFiles\xblat3s.dir\clean

BLAS\TESTING\CMakeFiles\xblat3s.dir\depend:
	$(CMAKE_COMMAND) -E cmake_depends "Borland Makefiles" C:\rrw\ThirdParty\clapack\3.2.1 C:\rrw\ThirdParty\clapack\3.2.1\BLAS\TESTING C:\rrw\builds\xe\clapack C:\rrw\builds\xe\clapack\BLAS\TESTING C:\rrw\builds\xe\clapack\BLAS\TESTING\CMakeFiles\xblat3s.dir\DependInfo.cmake --color=$(COLOR)
.PHONY : BLAS\TESTING\CMakeFiles\xblat3s.dir\depend

