CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(RR_PYTHON_EXAMPLES)

set(examples 	
getStoichMatrix.py
simulateEx.py
simulateExSelectionList.py
simulatePlot.py
)

install(    
            FILES 
            ${examples} 
            DESTINATION Python 	
            COMPONENT Python
       )

