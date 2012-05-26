CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(Apps)
set(RR_INCLUDE_ROOT "../src")

# rr Includes
include_directories(
${RR_INCLUDE_ROOT}
${RR_INCLUDE_ROOT}/libstruct
${RR_INCLUDE_ROOT}/rr_support
${LIBSBML_INSTALL_FOLDER}/include
${LIBSBML_INSTALL_FOLDER}/include/sbml
${CVODE_INSTALL_FOLDER}/include/cvode
${THIRD_PARTY_FOLDER}/clapack/3.2.1/INCLUDE
)

if(${MSVC})
endif(${MSVC})

set(apps 	
		rr
		rr_ts
)

foreach(app ${apps})
 	add_subdirectory(${app})
# install (FILES ${app}/${app}.cbproj 	DESTINATION Examples/${app} COMPONENT example_files)
	FILE (GLOB hdrs ${app}/*.h)
 	install (FILES ${hdrs} 						DESTINATION Examples/${app}	COMPONENT example_files)
	FILE (GLOB src ${app}/*.cpp)
 	install (FILES ${src} 						DESTINATION Examples/${app}	COMPONENT example_files)
 	install (FILES ${app}/Readme.txt 			DESTINATION Examples/${app}	COMPONENT example_files)
 	install (FILES ${app}/CMakeLists.txt 		DESTINATION Examples/${app}	COMPONENT example_files)
endforeach(app)

install (FILES Readme.txt 			DESTINATION Examples COMPONENT info)
install (FILES CMakeLists.txt 		DESTINATION Examples COMPONENT example_files)
