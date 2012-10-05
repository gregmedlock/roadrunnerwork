import rrPython
import os

modelPath = os.path.abspath(os.path.join(os.path.dirname(__file__), '..','..', '..', '..', 'models\\simple.xml'))

sbml = rrPython.loadSBMLFromFile(modelPath)


print rrPython.steadyState()
