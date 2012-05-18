
IF(BUILD_SHARED_LIBS)
set(target rr) 
ELSE()
set(target rr_static)
ENDIF()

include_directories(
. 
./libstruct
${SBML_INSTALL_FOLDER}/include
${SBML_INSTALL_FOLDER}/include/sbml
${CVODE_INSTALL_FOLDER}/include/cvode
${THIRD_PARTY_FOLDER}/clapack/3.2.1/INCLUDE
)

set(rrSources 
rrDoubleMatrix.cpp
rrException.cpp
rrFileLog.cpp
rrGetOptions.cpp
rrHashTable.cpp
rrIModel.cpp
rrLog.cpp
rrLogFile.cpp
rrLoggerUtils.cpp
rrLogLevel.cpp
rrLogOutput.cpp
rrMisc.cpp
rrModelFromC.cpp
rrModelState.cpp
rrNOMSupport.cpp
rrObject.cpp
rrParameterSet.cpp
rrParameterSets.cpp
rrPendingAssignment.cpp
rrRandom.cpp
rrRoadRunner.cpp
rrRule.cpp
rrSBMLModelSimulation.cpp
rrSBMLSymbol.cpp
rrSBMLSymbolDependencies.cpp
rrSimulationData.cpp
rrSimulationSettings.cpp
rrStreamWriter.cpp
rrStringBuilder.cpp
rrStringList.cpp
rrStringListContainer.cpp
rrStringUtils.cpp
rrSymbol.cpp
rrSymbolList.cpp
rrTComputeEventAssignmentDelegate.cpp
rrTEventAssignmentDelegate.cpp
rrTEventDelayDelegate.cpp
rrTEventDelegate.cpp
rrTextWriter.cpp
rrTParameterType.cpp
rrTPerformEventAssignmentDelegate.cpp
rrTVariableType.cpp
rrUtils.cpp
mtrand/mtrand.cpp
rrCGenerator.cpp
rrCodeBuilder.cpp
rrCompiler.cpp
rrCSharpGenerator.cpp
rrModelGenerator.cpp
rrScanner.cpp
rrToken.cpp
rrCVodeInterface.cpp
rrCvodedll.cpp
rrNLEQInterface.cpp
rrLibStructSupport.cpp
rrInteropUtils.cpp
libstruct/complex.cpp
libstruct/libla.cpp
libstruct/libstructural.cpp
libstruct/matrix.cpp
libstruct/sbmlmodel.cpp
libstruct/util.cpp
)

add_library(${target} ${LIB_TYPE}
${rrSources}
)

add_library(${target}-static STATIC
${rrSources}
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
.
./C_src 
./libstruct
)

#--- INSTALL
IF(BUILD_SHARED_LIBS) 
install (FILES ${SBML_INSTALL_FOLDER}/bin/libsbml.dll	DESTINATION bin COMPONENT rr_core)
install (FILES ${lib_path}/${target}.lib	DESTINATION lib COMPONENT rr_core)
ELSE()
ENDIF()

foreach(subfolder ${subfolders})
FILE (GLOB headers ${subfolder}/*.h)
install (FILES ${headers}			 		DESTINATION include/rr COMPONENT rr_core)
endforeach(subfolder)


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
#install (FILES ${THIRD_PARTY_FOLDER}/libsbml_dependencies/lib/libxml2_xe.lib	DESTINATION lib COMPONENT Libs)
#else()
#install (FILES ${THIRD_PARTY_FOLDER}/libsbml_dependencies/lib/libxml2.lib		DESTINATION lib COMPONENT Libs)
#endif()


