import rrPython
import os
import csv
os.chdir('C:\\RoadRunner\\bin')

function = 'getGlobalParameterByIndex'
rrPython.loadSBMLFromFile('C:\\RoadRunner\\Models\\feedback.xml')

index = 1

try:
    specs = rrPython.getGlobalParameterByIndex(index)
    if str(specs) is not False:
        result = 'True'
    else:
        result = 'False'
except:
    result = 'False'


PythonTestResults = open('C:\\RoadRunner\\PythonTestResults.csv','a')
writer = csv.writer(PythonTestResults)
writevar = function + '=' + result
writer.writerow([writevar])
PythonTestResults.close()