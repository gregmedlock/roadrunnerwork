CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(rrWrappers)

#=============== INSTALL =====================

install(   
            FILES rrPython.py SquareWaveTester.py
            DESTINATION Python  	
            COMPONENT Python
       )

