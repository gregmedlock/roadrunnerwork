# This file will be configured to contain variables for CPack. These variables
# should be set in the CMake list file of the project before CPack module is
# included. Example variables are:
#   CPACK_GENERATOR                     - Generator used to create package
#   CPACK_INSTALL_CMAKE_PROJECTS        - For each project (path, name, component)
#   CPACK_CMAKE_GENERATOR               - CMake Generator used for the projects
#   CPACK_INSTALL_COMMANDS              - Extra commands to install components
#   CPACK_INSTALLED_DIRECTORIES           - Extra directories to install
#   CPACK_PACKAGE_DESCRIPTION_FILE      - Description file for the package
#   CPACK_PACKAGE_DESCRIPTION_SUMMARY   - Summary of the package
#   CPACK_PACKAGE_EXECUTABLES           - List of pairs of executables and labels
#   CPACK_PACKAGE_FILE_NAME             - Name of the package generated
#   CPACK_PACKAGE_ICON                  - Icon used for the package
#   CPACK_PACKAGE_INSTALL_DIRECTORY     - Name of directory for the installer
#   CPACK_PACKAGE_NAME                  - Package project name
#   CPACK_PACKAGE_VENDOR                - Package project vendor
#   CPACK_PACKAGE_VERSION               - Package project version
#   CPACK_PACKAGE_VERSION_MAJOR         - Package project version (major)
#   CPACK_PACKAGE_VERSION_MINOR         - Package project version (minor)
#   CPACK_PACKAGE_VERSION_PATCH         - Package project version (patch)

# There are certain generator specific ones

# NSIS Generator:
#   CPACK_PACKAGE_INSTALL_REGISTRY_KEY  - Name of the registry key for the installer
#   CPACK_NSIS_EXTRA_UNINSTALL_COMMANDS - Extra commands used during uninstall
#   CPACK_NSIS_EXTRA_INSTALL_COMMANDS   - Extra commands used during install


SET(CPACK_BINARY_BUNDLE "")
SET(CPACK_BINARY_CYGWIN "")
SET(CPACK_BINARY_DEB "")
SET(CPACK_BINARY_DRAGNDROP "")
SET(CPACK_BINARY_NSIS "ON")
SET(CPACK_BINARY_OSXX11 "")
SET(CPACK_BINARY_PACKAGEMAKER "")
SET(CPACK_BINARY_RPM "")
SET(CPACK_BINARY_STGZ "")
SET(CPACK_BINARY_TBZ2 "")
SET(CPACK_BINARY_TGZ "")
SET(CPACK_BINARY_TZ "")
SET(CPACK_BINARY_ZIP "OFF")
SET(CPACK_CMAKE_GENERATOR "Visual Studio 10")
SET(CPACK_COMPONENT_UNSPECIFIED_HIDDEN "TRUE")
SET(CPACK_COMPONENT_UNSPECIFIED_REQUIRED "TRUE")
SET(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "i386")
SET(CPACK_DEBIAN_PACKAGE_SECTION "Libraries")
SET(CPACK_GENERATOR "ZIP")
SET(CPACK_IGNORE_FILES "C:/rrw/rrBuilds/vs-2010/libsbml;/.svn/;/.libs/;/.deps/;/.bzr/;.*.o$;.*.lo$;.*.la$;;/.DS_Store;/.svnignore;blib;libsbml-dist")
SET(CPACK_INSTALLED_DIRECTORIES "C:/rrw/ThirdParty/libsbml;/")
SET(CPACK_INSTALL_CMAKE_PROJECTS "")
SET(CPACK_INSTALL_PREFIX "C;\rrw\Installs\libsbml")
SET(CPACK_MODULE_PATH "")
SET(CPACK_NSIS_DISPLAY_NAME "libSBML 5.4.1")
SET(CPACK_NSIS_INSTALLER_ICON_CODE "")
SET(CPACK_NSIS_INSTALLER_MUI_ICON_CODE "")
SET(CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES")
SET(CPACK_NSIS_PACKAGE_NAME "libSBML 5.4.1")
SET(CPACK_OUTPUT_CONFIG_FILE "C:/rrw/rrBuilds/vs-2010/libsbml/CPackConfig.cmake")
SET(CPACK_PACKAGE_CONTACT "LibSBML Team <libsbml-team@caltech.edu>")
SET(CPACK_PACKAGE_DEFAULT_LOCATION "/")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "C:/rrw/ThirdParty/libsbml/README.txt")
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "An API library for reading/writing/manipulating SBML.")
SET(CPACK_PACKAGE_FILE_NAME "libSBML-5.4.1-Source")
SET(CPACK_PACKAGE_INSTALL_DIRECTORY "libSBML 5.4.1")
SET(CPACK_PACKAGE_INSTALL_REGISTRY_KEY "libSBML 5.4.1")
SET(CPACK_PACKAGE_NAME "libSBML")
SET(CPACK_PACKAGE_RELOCATABLE "true")
SET(CPACK_PACKAGE_VENDOR "The SBML Team")
SET(CPACK_PACKAGE_VERSION "5.4.1")
SET(CPACK_PACKAGE_VERSION_MAJOR "5")
SET(CPACK_PACKAGE_VERSION_MINOR "4")
SET(CPACK_PACKAGE_VERSION_PATCH "1")
SET(CPACK_RESOURCE_FILE_LICENSE "C:/rrw/ThirdParty/libsbml/COPYING.txt")
SET(CPACK_RESOURCE_FILE_README "C:/Program Files/CMake 2.8/share/cmake-2.8/Templates/CPack.GenericDescription.txt")
SET(CPACK_RESOURCE_FILE_WELCOME "C:/Program Files/CMake 2.8/share/cmake-2.8/Templates/CPack.GenericWelcome.txt")
SET(CPACK_RPM_PACKAGE_ARCHITECTURE "i386")
SET(CPACK_RPM_PACKAGE_GROUP "Libraries/Development")
SET(CPACK_RPM_PACKAGE_LICENSE "LGPL")
SET(CPACK_SET_DESTDIR "OFF")
SET(CPACK_SOURCE_CYGWIN "")
SET(CPACK_SOURCE_GENERATOR "ZIP")
SET(CPACK_SOURCE_IGNORE_FILES "C:/rrw/rrBuilds/vs-2010/libsbml;/.svn/;/.libs/;/.deps/;/.bzr/;.*.o$;.*.lo$;.*.la$;;/.DS_Store;/.svnignore;blib;libsbml-dist")
SET(CPACK_SOURCE_INSTALLED_DIRECTORIES "C:/rrw/ThirdParty/libsbml;/")
SET(CPACK_SOURCE_OUTPUT_CONFIG_FILE "C:/rrw/rrBuilds/vs-2010/libsbml/CPackSourceConfig.cmake")
SET(CPACK_SOURCE_PACKAGE_FILE_NAME "libSBML-5.4.1-Source")
SET(CPACK_SOURCE_TBZ2 "")
SET(CPACK_SOURCE_TGZ "")
SET(CPACK_SOURCE_TOPLEVEL_TAG "win32-Source")
SET(CPACK_SOURCE_TZ "")
SET(CPACK_SOURCE_ZIP "ON")
SET(CPACK_STRIP_FILES "")
SET(CPACK_SYSTEM_NAME "win32")
SET(CPACK_TOPLEVEL_TAG "win32-Source")
