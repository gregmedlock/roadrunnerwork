import rrPython
rrPython.loadSBMLFromFile('C:\\roadRunner\\models\\simple.xml')
results = rrPython.simulateEx(0.0,2.0,20)

print results