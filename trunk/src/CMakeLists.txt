CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

set(target roadrunner) 

include_directories(
. 
./libstruct
${LIBSBML_INSTALL_FOLDER}/include
${LIBSBML_INSTALL_FOLDER}/include/sbml
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

#Build both static and shared version
#Link with static libs
add_library(${target} 			SHARED ${rrSources})
add_library(${target}-static 	STATIC ${rrSources})

add_definitions(-DEXPORT_RR)
if(${BORLAND})
	set_property(DIRECTORY 		PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${exe_path}/${target}.tds)
endif(${BORLAND})

target_link_libraries (${target} libsbml-static)


#libraries to link with our shared lib
target_link_libraries (${target} sundials_cvode)
target_link_libraries (${target} sundials_nvecserial)
target_link_libraries (${target} blas)
target_link_libraries (${target} lapack)
target_link_libraries (${target} libf2c)

#libraries to link with our static lib
target_link_libraries (${target}-static libsbml-static)
target_link_libraries (${target}-static sundials_cvode)
target_link_libraries (${target}-static sundials_nvecserial)
target_link_libraries (${target}-static blas)
target_link_libraries (${target}-static lapack)
target_link_libraries (${target}-static libf2c)

#Libxml we can't build right now
if(${BORLAND})
target_link_libraries (${target} libxml2_xe)
target_link_libraries (${target}-static libxml2_xe)
else()
target_link_libraries (${target} libxml2)
target_link_libraries (${target}-static libxml2)
endif()


set(subfolders 
.
./libstruct
)

#--- INSTALL
install (FILES ${lib_path}/${target}.lib	DESTINATION lib COMPONENT rr_core)

if(INSTALL_STATIC_LIB)
install (FILES ${lib_path}/${target}-static.lib	DESTINATION lib COMPONENT rr_core)
endif()

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
install (FILES ${THIRD_PARTY_FOLDER}/runtime_dependencies/${system_file}	DESTINATION bin COMPONENT rr_core)
endforeach()

#=== Install support code for on the fly compilation
FILE(GLOB rr_support rr_support/*.*)
install(FILES ${rr_support} DESTINATION rr_support COMPONENT rr_core) 
##== We also need to copy the libxml.lib file to install folder, if we want to link any program with the rr library
#if(${BORLAND})
#install (FILES ${THIRD_PARTY_FOLDER}/libsbml_dependencies/lib/libxml2_xe.lib	DESTINATION lib COMPONENT Libs)
#else()
#install (FILES ${THIRD_PARTY_FOLDER}/libsbml_dependencies/lib/libxml2.lib		DESTINATION lib COMPONENT Libs)
#endif()


