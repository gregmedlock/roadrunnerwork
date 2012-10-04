import rrPython
import os

modelPath = os.path.join(os.path.dirname(__file__), '..','..' 'simple.xml')

sbml = rrPython.loadSBMLFromFile(modelPath)
rrPython.setTimeStart(0.0)
rrPython.setTimeEnd(3.0)
rrPython.setNumPoints(20)
results = rrPython.simulate()

print results
