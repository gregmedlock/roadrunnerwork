CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(RR_PYTHON_EXAMPLES)


if(${MSVC})
endif(${MSVC})

#set(examples 	
#)
install(    
            FILES 
            Example1.py 
            DESTINATION Python 	
            COMPONENT Python
       )

