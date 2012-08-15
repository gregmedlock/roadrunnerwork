import rrPython
import os
import csv
os.chdir('C:\\RoadRunner\\bin')

function = 'writeSBML'
rrPython.loadSBML('abc123')
sbml = rrPython.writeSBML()
if len(sbml) >= 1:
    result = 'True'
else:
    result = 'False'

PythonTestResults = open('C:\\RoadRunner\\PythonTestResults.csv','a')
writer = csv.writer(PythonTestResults)
writevar = function + '=' + result
writer.writerow([writevar])
PythonTestResults.close()
