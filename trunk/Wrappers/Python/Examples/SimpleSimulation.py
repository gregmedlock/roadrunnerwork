import rrPython
import os

modelPath = os.path.abspath(os.path.join(os.path.dirname(__file__), '..','..', '..', '..', 'models\\ss_threeSpecies.xml'))

sbml = rrPython.loadSBMLFromFile(modelPath)
rrPython.setTimeStart(0.0)
rrPython.setTimeEnd(3.0)
rrPython.setNumPoints(20)
rrPython.setSelectionList("time S1 S2 S3 S4")
results = rrPython.simulate()

print results
