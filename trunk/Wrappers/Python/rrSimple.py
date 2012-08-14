import os
import rrPython
startTime = 0
endTime = 5
numPoints = 50
selList="time,S1,S2"


result = rrPython.loadSBMLFromFile("r:\\rrInstalls\\xe\Models\\test_1.xml")
print result

rrPython.setTimeStart(startTime)
rrPython.setTimeEnd(endTime)
rrPython.setNumPoints(numPoints)
rrPython.setSelectionList(selList)
k = rrPython.simulate()
kk = []
kk = k.split('\n')
print kk

print "done"
