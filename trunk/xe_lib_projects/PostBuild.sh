#! /usr/bin/bash
#This script carries out any post build needs
#1: copy road runner dl(s) to the RRW/build/bin folder

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

APP_BUILD_INFO_FILE="AppBuildInfo"
APP_ROOT=`pwd`
RR_BUILD_FOLDER="/cygdrive/c/RRW/build"
RR_BIN_FOLDER=$RR_BUILD_FOLDER"/bin"
RR_LINK_FOLDER=$RR_BUILD_FOLDER"/link"
THIRD_PARTY="/cygdrive/p/ThirdParty"

#Document SVN revisions
RRW_SVN_REVISION=`svn info $APP_ROOT | grep Revision | cut -d ":" -f2`
echo "RR library svn revision: "$RRW_SVN_REVISION > $APP_BUILD_INFO_FILE

#RELEASE_FOLDER=$OUTPUT_PATH
RELEASE_FOLDER=$RR_BIN_FOLDER

#Copy needed "global" binaries needed for releases
SHARED_BINS=" \
$RR_LINK_FOLDER/RoadRunner++_xe.dll
"

DEBUG_FILES=" \
$RR_LINK_FOLDER/RoadRunner++_xe.tds
"

#Copy needed misc files needed for releases
MISC_FILES=" \
$APP_BUILD_INFO_FILE \
"

#Binaries
for file in $SHARED_BINS; do
	cp -fv $file $RELEASE_FOLDER
done

#Other
for file in $MISC_FILES; do
	cp -fv $file $RELEASE_FOLDER
done

#Debug Files
if [ $MODE = "DEBUG" ]; then

for file in $DEBUG_FILES; do
	cp -fv $file $RELEASE_FOLDER
done
fi
echo "DONE"
