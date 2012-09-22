import os
import rrPython

print 'RoadRunner Build Date: ' + rrPython.getCopyright()

startTime = 0
endTime = 5
numPoints = 50
selList="time,S1,S2"


result = rrPython.loadSBMLFromFile("..\\Models\\test_1.xml")

rrPython.setTimeStart(startTime)
rrPython.setTimeEnd(endTime)
rrPython.setNumPoints(numPoints)
rrPython.setSelectionList(selList)
k = rrPython.simulate()
kk = []
kk = k.split('\n')
print kk

print "done"
