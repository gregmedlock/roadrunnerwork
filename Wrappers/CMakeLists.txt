CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(rrWrappers)
#set(RR_INCLUDE_ROOT ${CMAKE_CURRENT_SOURCE_FOLDER"../source")

if(WRAP_TO_C)
set(wrappers "C")
endif()


if(WRAP_TO_DELPHI)
set(wrappers ${wrappers} "Delphi")
endif()

if(WRAP_TO_PYTHON)
set(wrappers ${wrappers} "Python")
endif()

foreach(wrapper ${wrappers})
    add_subdirectory(${wrapper})
 	install(
                FILES ${wrapper}/Readme.txt ${wrapper}/CMakeLists.txt 		
                DESTINATION ${wrapper}	
                COMPONENT ${wrapper}
           )
endforeach()

