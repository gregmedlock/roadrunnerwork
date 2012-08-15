import rrPython
import os
import csv
os.chdir('C:\\RoadRunner\\bin')

function = 'getFloatingSpeciesInitialConditionNames'

names = rrPython.getFloatingSpeciesInitialConditionNames()
if str(names) is not False:
    result = 'True'
else:
    result = 'False'

PythonTestResults = open('C:\\RoadRunner\\PythonTestResults.csv','a')
writer = csv.writer(PythonTestResults)
writevar = function + '=' + result
writer.writerow([writevar])
PythonTestResults.close()