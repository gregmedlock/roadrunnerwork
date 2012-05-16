
IF(BUILD_SHARED_LIBS)
set(target rr_shared)
ELSE()
set(target rr_static)
ENDIF()

include_directories(
. 
Common 
ModelGeneration 
Scanner 
Solvers 
LibStructSupport LibStructSupport/libstruct
${SBML_INSTALL_FOLDER}/include
${SBML_INSTALL_FOLDER}/include/sbml
${CVODE_INSTALL_FOLDER}/include/cvode
${THIRD_PARTY_FOLDER}/clapack/3.2.1/INCLUDE
)

add_library(${target} ${LIB_TYPE}
Common/rrDoubleMatrix.cpp
Common/rrException.cpp
Common/rrFileLog.cpp
Common/rrGetOptions.cpp
Common/rrHashTable.cpp
Common/rrIModel.cpp
Common/rrLog.cpp
Common/rrLogFile.cpp
Common/rrLoggerUtils.cpp
Common/rrLogLevel.cpp
Common/rrLogOutput.cpp
Common/rrMisc.cpp
Common/rrModelFromC.cpp
Common/rrModelState.cpp
Common/rrNOMSupport.cpp
Common/rrObject.cpp
Common/rrParameterSet.cpp
Common/rrParameterSets.cpp
Common/rrPendingAssignment.cpp
Common/rrRandom.cpp
Common/rrRoadRunner.cpp
Common/rrRule.cpp
Common/rrSBMLModelSimulation.cpp
Common/rrSBMLSymbol.cpp
Common/rrSBMLSymbolDependencies.cpp
Common/rrSimulationData.cpp
Common/rrSimulationSettings.cpp
Common/rrStreamWriter.cpp
Common/rrStringBuilder.cpp
Common/rrStringList.cpp
Common/rrStringListContainer.cpp
Common/rrStringUtils.cpp
Common/rrSymbol.cpp
Common/rrSymbolList.cpp
Common/rrTComputeEventAssignmentDelegate.cpp
Common/rrTEventAssignmentDelegate.cpp
Common/rrTEventDelayDelegate.cpp
Common/rrTEventDelegate.cpp
Common/rrTextWriter.cpp
Common/rrTParameterType.cpp
Common/rrTPerformEventAssignmentDelegate.cpp
Common/rrTVariableType.cpp
Common/rrUtils.cpp
Common/mtrand/mtrand.cpp
ModelGeneration/rrCGenerator.cpp
ModelGeneration/rrCodeBuilder.cpp
ModelGeneration/rrCompiler.cpp
ModelGeneration/rrCSharpGenerator.cpp
ModelGeneration/rrModelGenerator.cpp
Scanner/rrScanner.cpp
Scanner/rrToken.cpp
Solvers/rrCVodeInterface.cpp
Solvers/rrCvodedll.cpp
Solvers/rrNLEQInterface.cpp
LibStructSupport/rrLibStructSupport.cpp
LibStructSupport/rrInteropUtils.cpp
LibStructSupport/libstruct/complex.cpp
LibStructSupport/libstruct/libla.cpp
LibStructSupport/libstruct/libstructural.cpp
LibStructSupport/libstruct/matrix.cpp
LibStructSupport/libstruct/sbmlmodel.cpp
LibStructSupport/libstruct/util.cpp
)

if(BUILD_SHARED_LIBS)
	add_definitions(-DEXPORT_RR)
	if(${BORLAND})
	set_property(DIRECTORY 		PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${exe_path}/${target}.tds)
	endif(${BORLAND})
else()
add_definitions(-DNO_RR_DLL) 
endif()

#IF(BUILD_SHARED_LIBS) 
#target_link_libraries (${target} libsbml)
#ELSE()
#Link with static libsbml, always..
target_link_libraries (${target} libsbml-static)
#ENDIF()

target_link_libraries (${target} libxml2_xe)
target_link_libraries (${target} sundials_cvode)
target_link_libraries (${target} sundials_nvecserial)
target_link_libraries (${target} blas)
target_link_libraries (${target} lapack)
target_link_libraries (${target} libf2c)

set(subfolders 
Common 
Scanner 
ModelGeneration 
C_src 
Solvers 
LibStructural 
LibStructural/libstruct)

#--- INSTALL
IF(BUILD_SHARED_LIBS) 
install (FILES ${SBML_INSTALL_FOLDER}/bin/libsbml.dll	DESTINATION bin COMPONENT rr_core)
ELSE()
ENDIF()

foreach(subfolder ${subfolders})
FILE (GLOB headers ${subfolder}/*.h)
install (FILES ${headers}			 	DESTINATION include COMPONENT rr_core)
endforeach(subfolder)

install (FILES ${lib_path}/${target}.lib	DESTINATION link COMPONENT rr_core)

if(BUILD_SHARED_LIBS)
install (FILES ${exe_path}/${target}.dll	DESTINATION bin COMPONENT rr_core)
endif()

#=== dependencies
set(SBML_DEPENDENCIES libxml2.dll iconv.dll zlib1.dll)
foreach(system_file ${SBML_DEPENDENCIES})
install (FILES ${THIRD_PARTY_FOLDER}/libsbml/dependencies/bin/${system_file}	DESTINATION bin COMPONENT rr_core)
endforeach()

##== We also need to copy the libxml.lib file to install folder, if we want to link any program with the rr library
#if(${BORLAND})
#install (FILES ${THIRD_PARTY_FOLDER}/libsbml_dependencies/lib/libxml2_xe.lib	DESTINATION link COMPONENT Libs)
#else()
#install (FILES ${THIRD_PARTY_FOLDER}/libsbml_dependencies/lib/libxml2.lib		DESTINATION link COMPONENT Libs)
#endif()


