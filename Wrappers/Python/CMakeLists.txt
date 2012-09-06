CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(rrPythonWrapper)

#set(C_DLL ${CMAKE_CURRENT_SOURCE_DIR}"/../C/")
#add_subdirectory(${C_DLL} "${CMAKE_CURRENT_BINARY_DIR}")
#=============== INSTALL =====================
add_subdirectory(Tests)
install(   
            FILES 
            rrPython.py 
            rr.py
            SBMLTester.py
            MasterTest.py
            simulationTimer.py
            DESTINATION Python  	
            COMPONENT Python
       )

