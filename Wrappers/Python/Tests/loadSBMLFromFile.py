import rrPython
import os
rrPython.enableLogging()
rrPython.setTempFolder('C:\\RRTemp')
os.chdir('C:\\RoadRunner\\bin')

function = 'loadSBMLFromFile'

result = rrPython.loadSBMLFromFile('C:\\RoadRunner\\Models\\feedback.xml')
print result

if result == True:
    result = 'True'
else:
    result = 'False'

PythonTestResults = open('C:\\RoadRunner\\PythonTestResults.csv','a')
writer = csv.writer(PythonTestResults)
writevar = function + '=' + result
writer.writerow([writevar])
PythonTestResults.close()