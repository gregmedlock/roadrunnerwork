# Place this script in your ...Python27\\Lib  directory so that it can be
# imported with just "import rrPython" in test scripts.

import sys
import os
from ctypes import *
os.environ['PATH'] =  "c:\\RoadRunner\\bin" + ';' + "c:\\Python27" + ';' + os.environ['PATH']
handle = WinDLL ("c:\\RoadRunner\\bin\\rr_c_api.dll")


#=======================rr_c_api=======================#

rr = handle.getRRInstance()

#Latest
handle.writeSBML.restype = c_char_p

def writeSBML():
    return handle.writeSBML()

#Utility and informational methods
handle.getVersion.restype = c_char_p

def getVersion():
    return handle.getVersion()

#Logging
handle.enableLogging.restype = c_bool
handle.setLogLevel.restype = c_bool
handle.getLogLevel.restype = c_bool
handle.getLogFileName.restype = c_char_p
handle.getBuildDate.restype = c_char_p
handle.getCopyright.restype = c_char_p
handle.setTempFolder.restype = c_bool
handle.getTempFolder.restype = c_char_p

def enableLogging():
    return handle.enableLogging()

def setLogLevel(lvl):
    return handle.setTimeEnd (byref (c_double(lvl)))

def getLogLevel():
    return handle.getLogLevel()

def getLogFileName():
    return handle.getLogFileName()

def getBuildDate():
    return handle.getBuildDate()

def getCopyright():
    return handle.getCopyright()

def setTempFolder(folder):
    return handle.setTempFolder(folder)

def getTempFolder():
    return handle.getTempFolder()

#Error Handling
handle.hasError.restype = c_bool
handle.getLastError.restype = c_char_p

def hasError():
    return handle.hasError()

def getLastError():
    return handle.getLastError()

#RoadRunner API
#helper functions

#Flags/Options
handle.setComputeAndAssignConservationLaws.restype = c_bool

def setComputeAndAssignConservationLaws(OnOrOff):
#    value = c_bool
#    if handle.setComputeAndAssignConservationLaws (OnOrOff)
    return handle.setComputeAndAssignConservationLaws(OnOrOff)

#Load SBML methods
handle.loadSBML.restype = c_bool
handle.loadSBMLFromFile.restype = c_bool

def loadSBML(sbml):
    return handle.loadSBML(sbml)

def loadSBMLFromFile(sbml):
    return handle.loadSBMLFromFile(sbml)

#SBML utility methods
handle.getParamPromotedSBML.restype = c_char_p
handle.getSBML.restype = c_char_p

def getParamPromotedSBML(sArg):
    return handle.getParamPromotedSBML(sArg)

def getSBML():
    return handle.getSBML()

#Get and set capability routines
handle.setCapabilities.restype = c_bool
handle.getCapabilities.restype = c_char_p

def setCapabilities(caps):
    return handle.setCapabilities(caps)

def getCapabilities():                      #not yet implemented
    return handle.getCapabilities()

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

def setTimeStart(timeStart):
    return handle.setTimeStart (byref (c_double(timeStart)))

def setTimeEnd(timeEnd):
    return handle.setTimeEnd (byref (c_double(timeEnd)))

def setNumPoints(numPoints):
    return handle.setNumPoints(byref (c_int(numPoints)))

def setSelectionList(list):
    return handle.setSelectionList(list)

def getSelectionList():
    return handle.printList(handle.getSelectionList())

def simulate():
    return handle.printResult(handle.simulate())

def simulateEx(timeStart,timeEnd,numberOfPoints):
    return handle.printResult(handle.simulateEx(timeStart,timeEnd,numberOfPoints))

#def setSelectionList(list):
#    value = c_char_p()
#    if handle.getResultElement (list, byref(value)) == True:
#        return value.value;
#    else:
#        raise RuntimeError('Index out of range')

#def oneStep (currentTime, stepSize):                             #test this
#    value = c_double()
#    if handle.oneStep (currentTime, stepSize) == True:
#        return value.value;
#    else:
#        raise RuntimeError('Index out of range')

def getTimeStart():
    return handle.getTimeStart()

def getTimeEnd(timeEnd):
    return handle.getTimeEnd(timeEnd)

def getNumPoints(numPoints):
    return handle.getNumPoints(numPoints)

#Steady state methods
handle.steadyState.restype = c_bool
handle.setSteadyStateSelectionList.restype = c_bool

def steadyState():
    return handle.steadyState()

def computeSteadyStateValues():
    return handle.printVector(handle.computeSteadyStateValues())

def setSteadyStateSelectionList(list):
    return handle.setSteadyStateSelectionList(list)

def getSteadyStateSelectionList():
    return handle.printList(handle.getSteadyStateSelectionList())


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

def getValue(speciesID):                             #test this
    return handle.getValue(speciesID)

def setValue(speciesID, value):                             #test this
    value = c_double()
    if handle.setValue (speciesID, value, byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getFloatingSpeciesConcentrations():
    return handle.printVector(handle.getFloatingSpeciesConcentrations())

def getGlobalParameterValues():
    return handle.printVector(handle.getGlobalParameterValues())

def setBoundarySpeciesByIndex(index, value):                             #test this
    value = c_double()
    if handle.setBoundarySpeciesByIndex(index, value, byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def setFloatingSpeciesByIndex(index, value):                             #test this
    value = c_double()
    if handle.setFloatingSpeciesByIndex(index, value, byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def setGlobalParameterByIndex(index, value):                             #test this
    value = c_double()
    if handle.setGlobalParameterByIndex(index, value, byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getBoundarySpeciesByIndex(index):                             #test this
    return handle.getBoundarySpeciesByIndex(index)

def getFloatingSpeciesByIndex(index):                             #test this
    return handle.getFloatingSpeciesByIndex(index)

def getGlobalParameterByIndex(index):                             #test this
    return handle.getGlobalParameterByIndex(index)

def getCompartmentByIndex(index):                             #test this
    return handle.getCompartmentByIndex(index)

def setCompartmentByIndex(index, val):                         #test this
    value = c_double()
    if handle.setCompartmentByIndex(index, value, byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')


#Jacobian matrix methods
def getFullJacobian():
    return handle.printMatrix(handle.getFullJacobian())

def getReducedJacobian():
    return handle.printMatrix(handle.getReducedJacobian())

def getEigenvalues():
    return handle.printMatrix(handle.getEigenvalues())

#Stoichiometry methods
def getStoichiometryMatrix():
    return handle.printMatrix(handle.getStoichiometryMatrix())

def getLinkMatrix():
    return handle.printMatrix(handle.getLinkMatrix())

def getNrMatrix():
    return handle.printMatrix(handle.getNrMatrix())

def getL0Matrix():
    return handle.printMatrix(handle.getL0Matrix())

def getConservationMatrix():
    return handle.printMatrix(handle.getConservationMatrix())

#Initial condition methods
handle.reset.restype = c_bool
#handle.setFloatingSpeciesInitialConcentrations.restype = c_bool <----Not working

def reset():
    return handle.reset()

def getFloatingSpeciesInitialConcentrations():
    return handle.printVector(handle.getFloatingSpeciesInitialConcentrations())

def getFloatingSpeciesInitialConditionNames():
    return handle.printList(handle.getFloatingSpeciesInitialConditionNames())

#Reaction rates
handle.getNumberOfReactions.restype = c_int
handle.getReactionRate.restype = c_bool

def getNumberOfReactions():
    return handle.getNumberOfReactions()

def getReactionRate():                                 #test this
    return handle.getReactionRate()

def getReactionRates():
    return handle.printVector(handle.getReactionRates())

def getReactionRatesEx(vec):                            #TEST
    return handle.printVector(handle.getReactionRatesEx(vec))

#Rates of change
handle.getRateOfChange.restype = c_bool
handle.evalModel.restype = c_bool

def getRatesOfChange():
    return handle.printVector(handle.getRatesOfChange())

def getRatesOfChangeNames():
    return handle.printList(handle.getRatesOfChangeNames())

def getRateOfChange():
    return handle.getRateOfChange()

def getRatesOfChangeEx(vec):
    return handle.printVector(handle.getRatesOfChangeEx(vec))

def evalModel():
    return handle.evalModel()

#Get number family
handle.getNumberOfCompartments.restype = c_int
handle.getNumberOfBoundarySpecies.restype = c_int
handle.getNumberOfFloatingSpecies.restype = c_int
handle.getNumberOfGlobalParameters.restype = c_int
handle.getNumberOfDependentSpecies.restype = c_int
handle.getNumberOfIndependentSpecies.restype = c_int

def getNumberOfCompartments():
    return handle.getNumberOfCompartments()

def getNumberOfBoundarySpecies():
    return handle.getNumberOfBoundarySpecies()

def getNumberOfFloatingSpecies():
    return handle.getNumberOfFloatingSpecies()

def getNumberOfGlobalParameters():
    return handle.getNumberOfGlobalParameters()

def getNumberOfDependentSpecies():
    return handle.getNumberOfDependentSpecies()

def getNumberOfIndependentSpecies():
    return handle.getNumberOfIndependentSpecies()

#Get names family
def getReactionNames():
    return handle.printList(handle.getReactionNames())

def getRateOfChangeNames():
    return handle.printList(handle.getRateOfChangeNames())

def getBoundarySpeciesNames():
    return handle.printList(handle.getBoundarySpeciesNames())

def getFloatingSpeciesNames():
    return handle.printList(handle.getFloatingSpeciesNames())

def getGlobalParameterNames():
    return handle.printList(handle.getGlobalParameterNames())

def getCompartmentNames():
    return handle.printList(handle.getCompartmentNames())

def getEigenValueNames():
    return handle.printList(handle.getEigenValueNames())

def getAvailableSymbols():
    return handle.printList(handle.getAvailableSymbols())

#Get MCA methods

def getElasticityCoefficientNames():
    return handle.printStringArrayList(handle.getElasticityCoefficientNames())

def getUnscaledFluxControlCoefficientNames():
    return handle.printStringArrayList(handle.getUnscaledFluxControlCoefficientNames())

def getFluxControlCoefficientNames():
    return handle.printStringArrayList(handle.getFluxControlCoefficientNames())

def getUnscaledConcentrationControlCoefficientNames():
    return handle.printStringArrayList(handle.getUnscaledConcentrationControlCoefficientNames())

def getConcentrationControlCoefficientNames():
    return handle.printStringArrayList(handle.getConcentrationControlCoefficientNames())

def getUnScaledElasticityMatrix():
    return handle.printMatrix(handle.getUnScaledElasticityMatrix())

def getScaledElasticityMatrix():
    return handle.printMatrix(handle.getScaledElasticityMatrix())

def getUnscaledConcentrationControlCoefficientMatrix():
    return handle.printMatrix(handle.getUnscaledConcentrationControlCoefficientMatrix())

def getScaledConcentrationControlCoefficientMatrix():
    return handle.printMatrix(handle.getScaledConcentrationControlCoefficientMatrix())

def getUnscaledFluxControlCoefficientMatrix():
    return handle.printMatrix(handle.getUnscaledFluxControlCoefficientMatrix())

def getScaledFluxControlCoefficientMatrix():
    return handle.printMatrix(handle.getScaledFluxControlCoefficientMatrix())
#MCA methods
handle.getuCC.restype = c_bool
handle.getCC.restype = c_bool
handle.getEE.restype = c_bool
handle.getuEE.restype = c_bool
handle.getScaledFloatingSpeciesElasticity.restype = c_bool

def getuCC(variable, parameter, value):                         #test this
    value = c_double()
    if handle.getuCC(variable, parameter, value, byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getCC(variable, parameter, value):                         #test this
    value = c_double()
    if handle.getCC(variable, parameter, value,  byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getEE(name, species, value):                         #test this
    value = c_double()
    if handle.getEE(name, species, value,  byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getuEE(name, species, value):                         #test this
    value = c_double()
    if handle.getuEE(name, species, value,  byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getgetScaledFloatingSpeciesElasticity(reactionName, speciesName, value):                         #test this
    value = c_double()
    if handle.getScaledFloatingSpeciesElasticity(reactionName, speciesName, value,  byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')
#Print/format functions
handle.printResult.restype = c_char_p
handle.printMatrix.restype = c_char_p
handle.printVector.restype = c_char_p
handle.printList.restype = c_char_p
handle.printStringArrayList.restype = c_char_p
#handle.printArrayList.restype = c_char_p

def printResult(result):
    return handle.printResult(result)

def printMatrix(mat):
    return handle.printMatrix(mat)

def printVector(vec):
    return handle.printVector(vec)

def printList(list):
    return handle.printList(list)

def printStringArrayList(list):
    return handle.printStringArrayList(list)

#def printArrayList(list):
#    return handle.printArrayList(list)

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

def freeRRInstance(handle):
    return handle.freeRRInstance(handle)

def freeResult(handle):
    return handle.freeResult(handle)

def freeText(text):
    return handle.freeText(text)

def freeLabelStringList(sl):
    return handle.freeLabelStringList(sl)

def freeStringList(sl):
    return handle.freeStringList(sl)

def freeStringArrayList(sl):
    return handle.freeStringArrayList(sl)

def freeVector(vector):
    return handle.freeVector(vector)

def freeMatrix(matrix):
    return handle.freeMatrix(matrix)

def freeCCode(code):
    return handle.freeCCode(code)

def Pause():
    return handle.Pause()

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

def getVectorLength(vector):
    return handle.getVectorLength(vector)

def getVectorElement(vector):
    return handle.getVectorElement(vector)

def setVectorElement(vector, index, value):
    value = c_double()
    if handle.setVectorElement(vector, index, value,  byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getStringListLength(stringList):
    return handle.getStringListLength(stringList)

def getStringListElement(stringList, index):
    value = c_int()
    if handle.getStringListElement(stringList, index, byref(value)) == True:
        return value.value
    else:
        raise RuntimeError("Index out of range")

def getMatrixNumRows():
    return handle.getMatrixNumRows()

def getMatrixNumCols():
    return handle.getMatrixNumCols()

def getMatrixElement (m, i, j):
    value = c_double()
    if handle.getMatrixElement (m, i, j, byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getResultNumRows():
    return handle.getResultNumRows()

def getResultNumCols():
    return handle.getResultNumCols()

def getResultElement():
    return handle.getResultElement()

def getResultColumnLabel():
    return handle.getResultColumnLabel()

def getCCodeHeader():
    return handle.getCCodeHeader()

def getCCodeSource():
    return handle.getCCodeSource()


#def setVectorElement(stringList, index):
#    value = c_int()
#    if handle.setVectorElement(stringList, index, value,  byref(value)) == True:
#        return value.value;
#    else:
#        raise RuntimeError('Index out of range')







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

#def getRElementesult (m, i, j):
#    value = c_double()
#    if handle.getResultElement (m, i, j, byref(value)) == True:
#        return value.value;
#    else:
#        raise RuntimeError('Index out of range')

def setTempFolder(tempfolder):
    return handle.setTempFolder(tempfolder)

def getTempFolder():
    return handle.getTempFolder()

def enableLogging():
    return handle.enableLogging()

#handle.getResultNumRows.restype = c_int
#handle.getResultNumCols.restype = c_int

def getResultElement (m, i, j):
    value = c_double()
    if handle.getResultElement (m, i, j, byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')


#handle.getResultNumRows.restype = c_int


#=======================================================#
