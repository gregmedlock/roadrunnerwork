CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(Apps)
set(RR_INCLUDE_ROOT "../src")

# rr Includes
include_directories(${RR_INCLUDE_ROOT})
include_directories(${RR_INCLUDE_ROOT}/libstruct)
include_directories(${RR_INCLUDE_ROOT}/rr_support)
include_directories(${SBML_INSTALL_FOLDER}/include)
include_directories(${SBML_INSTALL_FOLDER}/include/sbml)
include_directories(${CVODE_INSTALL_FOLDER}/include/cvode)

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
