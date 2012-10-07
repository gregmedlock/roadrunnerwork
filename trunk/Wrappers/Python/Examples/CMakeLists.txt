CMAKE_MINIMUM_REQUIRED(VERSION 2.6.3 FATAL_ERROR)
PROJECT(RR_PYTHON_EXAMPLES)

set(examples 	
Example1.py
getConservationMatrix.py
getControlCoefficients.py
getStoichMatrix.py
matplotlibSimulate.py
SimpleSimulation.py
simulateEx.py
simulateExSelectionList.py
simulateSelectionList.py
SteadyState.py
)

install(    
            FILES 
            ${examples} 
            DESTINATION Python 	
            COMPONENT Python
       )

