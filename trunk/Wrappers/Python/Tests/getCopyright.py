import rrPython
import os
import csv
os.chdir('C:\\RoadRunner\\bin')

function = 'getCopyright'

copyright = rrPython.getCopyright()
if str(copyright) is not False:
    result = 'True'
else:
    result = 'False'

PythonTestResults = open('C:\\RoadRunner\\PythonTestResults.csv','a')
writer = csv.writer(PythonTestResults)
writevar = function + '=' + result
writer.writerow([writevar])
PythonTestResults.close()