CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(Apps)
set(RR_INCLUDE_ROOT "../source")

# rr Includes
include_directories(
${RR_INCLUDE_ROOT}
${RR_INCLUDE_ROOT}/libstruct
${RR_INCLUDE_ROOT}/rr_support
${THIRD_PARTY_INSTALL_FOLDER}/include
${THIRD_PARTY_INSTALL_FOLDER}/include/sbml
${THIRD_PARTY_INSTALL_FOLDER}/include/cvode
${THIRD_PARTY_FOLDER}/clapack/3.2.1/INCLUDE
)

set(apps 	
		rr
        rr_test_suite_tester
        rr_performance_tester
)


foreach(app ${apps})
 	add_subdirectory(${app})
	FILE (GLOB hdrs ${app}/*.h)
 	install (FILES ${hdrs} 						DESTINATION Examples/${app}	COMPONENT example_files)
	FILE (GLOB source ${app}/*.cpp)
 	install (FILES ${source} 						DESTINATION Examples/${app}	COMPONENT example_files)
 	install (FILES ${app}/Readme.txt 			DESTINATION Examples/${app}	COMPONENT example_files)
 	install (FILES ${app}/CMakeLists.txt 		DESTINATION Examples/${app}	COMPONENT example_files)
endforeach(app)

install (FILES Readme.txt 			DESTINATION Examples COMPONENT info)
install (FILES CMakeLists.txt 		DESTINATION Examples COMPONENT example_files)
