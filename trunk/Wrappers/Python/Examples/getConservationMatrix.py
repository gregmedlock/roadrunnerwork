import rrPython

modelPath = ('C:\\RoadRunner\\Models\\test_1.xml')
rrPython.loadSBMLFromFile(modelPath)

print rrPython.getConservationMatrix()