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
    value = c_int
    if handle.getLogLevel(byref(value)) == True:
        return value.value
    else:
        raise RuntimeError('Index out of range')

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

#########################################################################################################################

#Flags/Options
handle.setComputeAndAssignConservationLaws.restype = c_bool

def setComputeAndAssignConservationLaws(OnOrOff):
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
    value = handle.getSelectionList()
    result = handle.printList(value)
    handle.freeStringList(value)
    return result

def simulate():
    value = handle.simulate()
    result = handle.printResult(value)
    handle.freeResult(value)
    return result

#psimulate = POINTER(simulate())
#simulate, send results to array, free memory

def simulateEx(timeStart,timeEnd,numberOfPoints):
    startValue = c_double(timeStart)
    endValue = c_double(timeEnd)
    pointsValue = c_int(numberOfPoints)
    simulation = handle.simulateEx(byref(startValue),byref(endValue),byref(pointsValue))
    result = handle.printResult(simulation)
    handle.freeResult(simulation)
    return result;


#def oneStep (currentTime, stepSize):                             #test this
#    curtime = c_double(currentTime)
#    stepValue = c_double(stepSize)
#    value = c_double()
#    if handle.oneStep(byref(curtime), byref(stepSize), byref(value)) == True:
#        return value.value;
#    else:
#        raise RuntimeError('Index out of range')

def getTimeStart():
    value = c_double()
    if handle.getTimeStart(byref(value)) == True:
        return value.value
    else:
        return ('Index out of Range')

def getTimeEnd():
    value = c_double()
    if handle.getTimeEnd(byref(value)) == True:
        return value.value
    else:
        return ('Index out of Range')

def getNumPoints():
    value = c_int()
    if handle.getNumPoints(byref(value)) == True:
        return value.value
    else:
        return ('Index out of Range')

#Steady state methods
handle.steadyState.restype = c_bool
handle.setSteadyStateSelectionList.restype = c_bool

def steadyState():
    value = c_int()
    if handle.steadyState(byref(value)) == True:
        return value.value
    else:
        return ('Index out of Range')

def computeSteadyStateValues():
    values = handle.computeSteadyStateValues()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

def setSteadyStateSelectionList(list):
    return handle.setSteadyStateSelectionList(list)

def getSteadyStateSelectionList():
    values = handle.getSteadyStateSelectionList()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result


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

def getValue(speciesID):
    value = c_double()
    if handle.getValue(speciesID, byref(value)) == True:
        return value.value
    else:
        raise RuntimeError('Index out of Range')                             #test this

def setValue(speciesID, val):                             #test this
    value = c_double(val)
    if handle.setValue(speciesID, byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getFloatingSpeciesConcentrations():
    values = handle.getFloatingSpeciesConcentrations()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

def getGlobalParameterValues():
    values = handle.getGlobalParameterValues()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

def setBoundarySpeciesByIndex(index, val):                             #test this
    value = c_double(val)
    ivalue = c_int(index)
    if handle.setBoundarySpeciesByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def setFloatingSpeciesByIndex(index, val):                             #test this
    value = c_double(val)
    ivalue = c_int(index)
    if handle.setFloatingSpeciesByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def setGlobalParameterByIndex(index, val):                             #test this
    value = c_double(val)
    ivalue = c_int(index)
    if handle.setGlobalParameterByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getBoundarySpeciesByIndex(index):                             #test this
    value = c_double()
    ivalue = c_int(index)
    if handle.getBoundarySpeciesByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getFloatingSpeciesByIndex(index):                             #test this
    value = c_double()
    ivalue = c_int(index)
    if handle.getFloatingSpeciesByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')


def getGlobalParameterByIndex(index):
    value = c_double()
    ivalue = c_int(index)
    if handle.getGlobalParameterByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of Range')

def getCompartmentByIndex(index):
    value = c_double()
    ivalue = c_int(index)
    if handle.getCompartmentByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of Range')

def setCompartmentByIndex(index, val):                         #test this
    value = c_double(val)
    ivalue = c_int(index)
    if handle.setCompartmentByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')


#Jacobian matrix methods
def getFullJacobian():
    values = handle.getFullJacobian()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

def getReducedJacobian():
    values = handle.getReducedJacobian()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

def getEigenvalues():
    values = handle.getEigenvalues()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

#Stoichiometry methods
def getStoichiometryMatrix():
    values = handle.getStoichimetryMatrix()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

def getLinkMatrix():
    values = handle.getLinkMatrix()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

def getNrMatrix():
    values = handle.getNrMatrix()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

def getL0Matrix():
    values = handle.getL0Matrix()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

def getConservationMatrix():
    values = handle.getConservationMatrix()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

#Initial condition methods
handle.reset.restype = c_bool
#handle.setFloatingSpeciesInitialConcentrations.restype = c_bool

def reset():
    return handle.reset()

#def setFloatingSpeciesInitialConcentration(vec):
#    return handle.setFloatingSpeciesInitialConcentration(vec)

def getFloatingSpeciesInitialConcentrations():
    values = handle.getFloatingSpeciesInitialConcentrations()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

def getFloatingSpeciesInitialConditionNames():
    values = handle.getFloatingSpeciesInitialConditionNames()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

#Reaction rates
handle.getNumberOfReactions.restype = c_int
handle.getReactionRate.restype = c_bool

def getNumberOfReactions():
    return handle.getNumberOfReactions()

def getReactionRate():
    value = c_double()
    if handle.getReactionRate(byref(value)) == True:
        return value.value
    else:
        raise RuntimeError('Index out of Range')                                 #test this

def getReactionRates():
    values = handle.getReactionRates()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

def getReactionRatesEx(vec):                            #TEST
    return handle.printVector(handle.getReactionRatesEx(vec))

#Rates of change
handle.getRateOfChange.restype = c_bool
handle.evalModel.restype = c_bool

def getRatesOfChange():
    values = handle.getRatesOfChange()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

def getRatesOfChangeNames():
    values = handle.getRatesOfChangeNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result

def getRateOfChange():
    return handle.getRateOfChange()

def getRatesOfChangeEx(vec):                     #TEST
    values = handle.getRatesOfChangEx()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

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
    values = handle.getReactionNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result

def getRateOfChangeNames():
    values = handle.getRateOfChangeNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result

def getBoundarySpeciesNames():
    values = handle.getBoundarySpeciesNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result

def getFloatingSpeciesNames():
    values = handle.getFloatingSpeciesNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result

def getGlobalParameterNames():
    values = handle.getGlobalParameterNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result

def getCompartmentNames():
    values = handle.getCompartmentNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result

def getEigenValueNames():
    values = handle.getEigenValueNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result

def getAvailableSymbols():                              #FIX
    value = c_char_p()
    if handle.printArrayList(handle.getAvailableSymbols(byref(value))) == True:
        return value.value
    else:
        raise RuntimeError('Index out of range')
#    return handle.printArrayList(handle.getAvailableSymbols())

#Get MCA methods

def getElasticityCoefficientNames():
    value = handle.getElasticityCoefficientNames()
    result = handle.printStringArrayList(value)
    handle.freeStringArrayList(value)
    return result

def getUnscaledFluxControlCoefficientNames():
    value = handle.getUnscaledFluxControlCoefficientNames()
    result = handle.printStringArrayList(value)
    handle.freeStringArrayList(value)
    return result

def getFluxControlCoefficientNames():
    value = handle.getFluxControlCoefficientNames()
    result = handle.printStringArrayList(value)
    handle.freeStringArrayList(value)
    return result

def getUnscaledConcentrationControlCoefficientNames():
    value = handle.getUnscaledConcentrationCoefficientNames()
    result = handle.printStringArrayList(value)
    handle.freeStringArrayList(value)
    return result

def getConcentrationControlCoefficientNames():
    value = handle.getConcentrationControlCoefficientNames()
    result = handle.printStringArrayList(value)
    handle.freeStringArrayList(value)
    return result

def getUnScaledElasticityMatrix():
    value = handle.getUnscaledElasticityMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

def getScaledElasticityMatrix():
    value = handle.getScaledElasticityMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

def getUnscaledConcentrationControlCoefficientMatrix():
    value = handle.getUnscaledConcentrationControlCoefficientMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

def getScaledConcentrationControlCoefficientMatrix():
    value = handle.getScaledConcentrationControlCoefficientMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

def getUnscaledFluxControlCoefficientMatrix():
    value = handle.getUnscaledFluxControlCoefficientMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

def getScaledFluxControlCoefficientMatrix():
    value = handle.getScaledFluxControlCoefficientMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

#MCA methods
handle.getuCC.restype = c_bool
handle.getCC.restype = c_bool
handle.getEE.restype = c_bool
handle.getuEE.restype = c_bool
handle.getScaledFloatingSpeciesElasticity.restype = c_bool

def getuCC(variable, parameter):                         #test this
    variable = c_char_p()
    parameter = c_char_p()
    value = c_double()
    if handle.getuCC(byref(variable), byref(parameter), byref(value)) == True:
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

def getScaledFloatingSpeciesElasticity(reactionName, speciesName, value):                         #test this
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

handle.Pause.restype = None

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
    value = c_int
    if handle.getMatrixNumRows(byref(value)) == True:
        return value.value
    else:
        raise RuntimeError('Index out of range')


def getMatrixNumCols():
    return handle.getMatrixNumCols()

def getMatrixElement (m, i, j):
    value = c_double()
    m = c_int
    i = c_int
    j = c_int
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
