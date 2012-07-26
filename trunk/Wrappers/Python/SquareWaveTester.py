import os
import sys
import csv
#import matplotlib            Only for plotting
#import numpy                 Only for plotting
os.path.dirname(sys.argv[0])
import rrPython

print rrPython.getCopyright()

rrINSTALLfolder = "C:\\RoadRunner"
modelfolder = rrINSTALLfolder + "\\Models"
rrbin = rrINSTALLfolder + "\\bin"
model = "\\squareWaveModel.xml"
rrmodel = modelfolder + model
os.chdir (rrbin)

sbmlstr = open(rrmodel, 'r').read()
print "Load SBML", rrPython.loadSBML(sbmlstr)

rrPython.setSelectionList(['time', 'S1', 'J0'])

print "Set Time Start", rrPython.setTimeStart (0.0)
print "Set Time End" , rrPython.setTimeEnd (40.0)
print "Set Number of Points", rrPython.setNumPoints (200)
result = rrPython.handle.simulate()
print
ResultWrite = rrPython.printResult (result)

p = open('C:\\RoadRunner\\Models\\SquareWaveResults.csv','w')
p.write(ResultWrite)
p.close()

#    ---   Code for plotting using matplotlib and NumPy, not yet working  --- #

#def getColumn (SquareWaveResults, column):
#    results = csv.reader(open('C:\\RoadRunner\\Models\\SquareWaveResults.csv'), delimiter="\t")
#    return [result[column] for result in results]

#time = getColumn("C:\\RoadRunner\\Models\\SquareWaveResults.csv",0)
#S1 = getColumn("C:\\RoadRunner\\Models\\SquareWaveResults.csv",1)

#import matplotlib.pyplot as plt
#plt.plot(time,S1)

