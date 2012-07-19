# Files sbml_model.c sbml_model.def sbml_model.dll sbml_model.h -- delete and simulation
# does not work. located in C:\Users\Greg Medlock\AppData\Local\Temp

print "Load roadRunner via ctypes"
print "=========================="
import sys
import os
from ctypes import *
os.environ['PATH'] =  "c:\\roadRunner\\bin" + ';' + os.environ['PATH']
handle = WinDLL ("c:\\RoadRunner\\bin\\rr_c_api.dll")


#=======================rr_c_api=======================#

#Latest
handle.writeSBML.restype = c_char_p

#Utility and informational methods
handle.getVersion.restype = c_char_p

#Logging
handle.enableLogging.restype = c_bool
handle.setLogLevel.restype = c_bool
handle.getLogLevel.restype = c_bool
handle.getLogFileName.restype = c_char_p
handle.getBuildDate.restype = c_char_p
handle.getCopyright.restype = c_char_p
handle.setTempFolder.restype = c_bool
handle.getTempFolder.restype = c_char_p

#Error Handling
handle.hasError.restype = c_bool
handle.getLastError.restype = c_char_p

#RoadRunner API
#helper functions

#Flags/Options
handle.setComputeAndAssignConservationLaws.restype = c_bool

#Load SBML methods
handle.loadSBML.restype = c_bool
handle.loadSBMLFromFile.restype = c_bool

#SBML utility methods
handle.getParamPromotedSBML.restype = c_char_p
handle.getSBML.restype = c_char_p

#Get and set capability routines
handle.setCapabilities.restype = c_bool
handle.getCapabilities.restype = c_char_p

#Simulation Methods
handle.setTimeStart.restype = c_bool
handle.setTimeEnd.restype = c_bool
handle.setNumPoints.restype = c_bool
handle.setSelectionList.restype = c_bool
#helper function
#helper function
#helper function
handle.oneStep.restype = c_bool
handle.getTimeStart.restype = c_bool
handle.getTimeEnd.restype = c_bool
handle.getNumPoints.restype = c_bool

#Steady state methods
handle.steadyState.restype = c_bool
handle.setSteadyStateSelectionList.restype = c_bool

#Set and get family of methods
handle.getValue.restype = c_bool
handle.setValue.restype = c_bool
handle.setBoundarySpeciesByIndex.restype = c_bool
handle.setFloatingSpeciesByIndex.restype = c_bool
handle.setGlobalParameterByIndex.restype = c_bool
handle.getBoundarySpeciesByIndex.restype = c_bool
handle.getFloatingSpeciesByIndex.restype = c_bool
handle.getGlobalParameterByIndex.restype = c_bool
handle.getCompartmentByIndex.restype = c_bool
handle.setCompartmentByIndex.restype = c_bool

#Jacobian matrix methods
#helper functions

#Stoichiometry methods
#helper functions

#Initial condition methods
handle.reset.restype = c_bool
#handle.setFloatingSpeciesInitialConcentrations.restype = c_bool

#Reaction rates
handle.getNumberOfReactions.restype = c_int
handle.getReactionRate.restype = c_bool

#Rates of change
handle.getRatesOfChange.restype = c_bool
handle.evalModel.restype = c_bool

#Get number family
handle.getNumberOfCompartments.restype = c_int
handle.getNumberOfBoundarySpecies.restype = c_int
handle.getNumberOfFloatingSpecies.restype = c_int
handle.getNumberOfGlobalParameters.restype = c_int
handle.getNumberOfDependentSpecies.restype = c_int
handle.getNumberOfIndependentSpecies.restype = c_int

#Get names family
#helper functions

#Get MCA methods
#helper functions

#MCA methods
handle.getuCC.restype = c_bool
handle.getCC.restype = c_bool
handle.getEE.restype = c_bool
handle.getuEE.restype = c_bool
handle.getScaledFloatingSpeciesElasticity.restype = c_bool

#Print/format functions
handle.printResult.restype = c_char_p
handle.printMatrix.restype = c_char_p
handle.printVector.restype = c_char_p
handle.printList.restype = c_char_p
handle.printStringArrayList.restype = c_char_p
handle.printArrayList.restype = c_char_p

#Free memory functions
handle.freeRRInstance.restype = c_bool
handle.freeResult.restype = c_bool
handle.freeText.restype = c_bool
handle.freeLabelStringList.restype = c_bool
handle.freeStringList.restype = c_bool
handle.freeStringArrayList.restype = c_bool
handle.freeVector.restype = c_bool
handle.freeMatrix.restype = c_bool
handle.freeCCode.restype = c_bool
handle.Pause.restype = None

#Helper routines

handle.getVectorLength.restype = c_int
handle.getVectorElement.restype = c_bool
handle.setVectorElement.restype = c_bool
handle.getStringListLength.restype = c_int
handle.getStringListElement.restype = c_char_p
handle.getMatrixNumRows.restype = c_int
handle.getMatrixNumCols.restype = c_int
handle.getMatrixElement.restype = c_bool
handle.getResultNumRows.restype = c_int
handle.getResultNumCols.restype = c_int
handle.getResultElement.restype = c_bool
handle.getResultColumnLabel.restype = c_char_p
handle.getCCodeHeader.restype = c_char_p
handle.getCCodeSource.restype = c_char_p


rr = handle.getRRInstance()

def getCopyright():
    return handle.getCopyright()

def loadSBML (sbmlstr):
    return handle.loadSBML (sbmlstr)

def printResult (result):
    return handle.printResult (result)

def setTimeStart (timeStart):
    arg = byref (c_double(timeStart))
    return handle.setTimeStart (arg)

def setTimeEnd (timeEnd):
    return handle.setTimeEnd (byref (c_double(timeEnd)))

def setNumPoints (numPoints):
    return handle.setNumPoints (byref (c_int(numPoints)))

def getMatrixElement (m, i, j):
    value = c_double()
    if handle.getMatrixElement (m, i, j, byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getRElementesult (m, i, j):
    value = c_double()
    if handle.getResultElement (m, i, j, byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def setTempFolder(tempfolder):
    return handle.setTempFolder(tempfolder)

def getTempFolder():
    return handle.getTempFolder()

def enableLogging():
    return handle.enableLogging()


#=======================================================#



rr = handle.getRRInstance()
print getCopyright()

rrINSTALLfolder = "C:\\RoadRunner"
modelfolder = rrINSTALLfolder + "\\Models"
rrbin = rrINSTALLfolder + "\\bin"
model = '\\feedback.xml'
rrmodel = modelfolder + model
os.chdir (rrbin)
print os.getcwd()

tempfolder = "C:\\rrTemp"
handle.setTempFolder(tempfolder)
handle.enableLogging()
print handle.getTempFolder()

sbmlstr = open(rrmodel, 'r').read()
#print "Load SBML", loadSBML (sbmlstr)

if not loadSBML(sbmlstr):
    print handle.getLastError()

print os.getcwd()


print "Set Time Start", setTimeStart (0.0)
print "Set Time End" , setTimeEnd (200.0)
print "Set Number of Points", setNumPoints (50)
result = handle.simulate()
print
print printResult (result)

#handle.getMyValue.restype = c_bool
#x = c_double()
#x.value = 0.0
#handle.getMyValue (byref(x))
#print x.value

#v = handle.getRatesOfChange()
#print handle.getVectorLength (v)
#handle.setVectorElement (v, 0, x)
#x.value = x.value + 1
#handle.setVectorElement (v, 1, x)
#x.value = x.value + 1
#handle.setVectorElement (v, 2, x)

#sl = handle.getRatesOfChangeNames()
