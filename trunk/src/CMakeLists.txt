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
${THIRD_PARTY_ROOT}/clapack/3.2.1/INCLUDE
)

#add_subdirectory(Common)
##add_subdirectory(C_src)
##add_subdirectory(LibStructSupport)
#add_subdirectory(ModelGeneration)
#add_subdirectory(Scanner)
#add_subdirectory(Solvers)

set(target rr)

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

if(${BUILD_SHARED_LIBS})
	add_definitions(-DDEXPORT_RR)
	if(${BORLAND})
	set_property(DIRECTORY 		PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${exe_path}/${target}.tds)
	endif(${BORLAND})
endif()

set(subfolders Common Scanner ModelGeneration C_src Solvers LibStructural LibStructural/libstruct)

foreach(subfolder ${subfolders})
FILE (GLOB headers ${subfolder}/*.h)
install (FILES ${headers}			 	DESTINATION include COMPONENT cxx_headers)
endforeach(subfolder)

#install (TARGETS ${target} 	DESTINATION libs COMPONENT library_files)

