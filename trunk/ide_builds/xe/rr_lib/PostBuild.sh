#! /usr/bin/bash
#This script carries out any post build needs
#1: copy road runner dll to the RRW/build/bin folder
#2: copy road runner link lib to RRW/build/link folder

#Argument1 is the applications name (no extension)
arg1=$1

#Argument 2 is the current compilation mode, Debug or Release
arg2=$2
echo "Argument 1 is "$arg1
echo "Argument 2 is "$arg2

APP_EXE=$arg1.exe
OUTPUT_PATH=`cygpath $arg2`

[[ $OUTPUT_PATH =~ 'Debug' ]] && MODE="DEBUG" || MODE="NO_DEBUG"
echo "Compilation mode is: "$MODE

BUILD_INFO_FILE="BuildInfo"
APP_ROOT=`pwd`
BUILD_FOLDER=$APP_ROOT/$arg2/"Win32"
INSTALL_FOLDER="/cygdrive/c/rrw/build"
BIN_FOLDER=$INSTALL_FOLDER"/bin"
LINK_FOLDER=$INSTALL_FOLDER"/link"
THIRD_PARTY="/cygdrive/p/ThirdParty"

#Document SVN revisions
SVN_REVISION=`svn info $APP_ROOT | grep Revision | cut -d ":" -f2`
echo "svn revision: "$SVN_REVISION > $BUILD_INFO_FILE

#Files to move around...
SHARED_BINS=" \
$BUILD_FOLDER/$arg1.dll
"
LINK_LIBS=" \
$BUILD_FOLDER/$arg1.lib
"

DEBUG_FILES=" \
$BUILD_FOLDER/$arg1.tds
"

MISC_FILES=" \
$BUILD_INFO_FILE \
"

#Binaries
for file in $SHARED_BINS; do
	cp -fv $file $BIN_FOLDER
done

#Binaries
for file in $LINK_LIBS; do
	cp -fv $file $LINK_FOLDER
done

#Other
for file in $MISC_FILES; do
	cp -fv $file $INSTALL_FOLDER
done

#Debug Files
if [ $MODE = "DEBUG" ]; then

for file in $DEBUG_FILES; do
	cp -fv $file $BIN_FOLDER
done
fi
echo "DONE"
