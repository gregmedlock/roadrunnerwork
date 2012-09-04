# Place this script in your ...Python27\\Lib  directory so that it can be
# imported with just "import rrPython" in test scripts.

"""@package rrPython
This module allows access to the rr_c_api.dll from python

"""

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
    """Returns the version of the C API"""
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
    """Allows a log file to be written"""
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
    """Returns the name of the log file"""
    return handle.getLogFileName()

def getBuildDate():
    """Returns the date of the installed C API"""
    return handle.getBuildDate()

def getCopyright():
    """Returns the copyright date"""
    return handle.getCopyright()

def setTempFolder(folder):
    """Sets the write location for the temporary file

    Takes a string as an argument"""
    return handle.setTempFolder(folder)

def getTempFolder():
    """Returns the full path of the temporary folder"""
    return handle.getTempFolder()

#Error Handling
handle.hasError.restype = c_bool
handle.getLastError.restype = c_char_p

def hasError():
    return handle.hasError()

def getLastError():
    """Returns the last error"""
    return handle.getLastError()

#RoadRunner API

#########################################################################################################################

#Flags/Options
handle.setComputeAndAssignConservationLaws.restype = c_bool

def setComputeAndAssignConservationLaws(OnOrOff):
    """Turns on/off conservation laws

    Takes a 1 (on) or 0 (off) as an argument"""
    return handle.setComputeAndAssignConservationLaws(OnOrOff)

#Load SBML methods
handle.loadSBML.restype = c_bool
handle.loadSBMLFromFile.restype = c_bool

def loadSBML(sbml):
    """Loads SBML model from a string

    Takes a string as an argument"""
    return handle.loadSBML(sbml)

def loadSBMLFromFile(sbml):
    """Loads SBML model from a file

    Takes a string as an argument"""
    return handle.loadSBMLFromFile(sbml)

#SBML utility methods
handle.getParamPromotedSBML.restype = c_char_p
handle.getSBML.restype = c_char_p

def getParamPromotedSBML(sArg):
    """Returns the SBML with the current parameterset"""
    return handle.getParamPromotedSBML(sArg)

def getSBML():
    """Returns the SBML model that is currently loaded"""
    return handle.getSBML()

#Get and set capability routines
handle.setCapabilities.restype = c_bool
handle.getCapabilities.restype = c_char_p

def setCapabilities(caps):
    """Sets simulator capabilities"""
    return handle.setCapabilities(caps)

def getCapabilities():
    """Returns simulator capabilities"""                      #not yet implemented
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
    """Sets the start time for the simulation

    Takes a double as an argument"""
    return handle.setTimeStart (byref (c_double(timeStart)))

def setTimeEnd(timeEnd):
    """Sets the end time for the simulation

    Takes a double as an argument"""
    return handle.setTimeEnd (byref (c_double(timeEnd)))

def setNumPoints(numPoints):
    """Sets the number of points for the simulation

    Takes an int as an argument"""
    return handle.setNumPoints(byref (c_int(numPoints)))

def setSelectionList(list):
    """Sets the list of variables returned by simulate()

    Available symbols for setSelectionList can be found by using getAvailableSymbols() after loading a model

    Takes a string with variable names separated by a space or a comma as an argument"""
    return handle.setSelectionList(list)

def getSelectionList():
    """Returns the list of variables returned by simulate()"""
    value = handle.getSelectionList()
    result = handle.printList(value)
    handle.freeStringList(value)
    return result

def simulate():
    """Simulates the model that is currently loaded and returns values for variables as chosen with setSelectionList()"""
    value = handle.simulate()
    result = handle.printResult(value)
    handle.freeResult(value)
    return result

#psimulate = POINTER(simulate())
#simulate, send results to array, free memory

def simulateEx(timeStart,timeEnd,numberOfPoints):
    """Simulates a reaction in a given period and with a given number of points

    Takes (double,double,int) as an argument for timeStart, timeEnd, and numberOfPoints, respectively"""
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
    """Returns the simulation start time"""
    value = c_double()
    if handle.getTimeStart(byref(value)) == True:
        return value.value
    else:
        return ('Index out of Range')

def getTimeEnd():
    """Returns the simulation end time"""
    value = c_double()
    if handle.getTimeEnd(byref(value)) == True:
        return value.value
    else:
        return ('Index out of Range')

def getNumPoints():
    """Returns the number of points in the simulation"""
    value = c_int()
    if handle.getNumPoints(byref(value)) == True:
        return value.value
    else:
        return ('Index out of Range')

#Steady state methods
handle.steadyState.restype = c_bool
handle.setSteadyStateSelectionList.restype = c_bool

def steadyState():
    """Computes the steady state of the loaded model and returns the sum of squares of the solution"""
    value = c_int()
    if handle.steadyState(byref(value)) == True:
        return value.value
    else:
        return ('Index out of Range')

def computeSteadyStateValues():
    """Computes and returns the steady state solution of the loaded model"""
    values = handle.computeSteadyStateValues()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

def setSteadyStateSelectionList(list):
    """Sets the variables returned by steadyState() and computeSteadyStateValues()

    Takes a string with variables separated by a space or a comma as an argument"""
    return handle.setSteadyStateSelectionList(list)

def getSteadyStateSelectionList():
    """Returned the variables returned by steadyState() and computeSteadyStateValues()"""
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
    """Returns the current value for a single species in loaded model

    Takes a string as an argument"""
    value = c_double()
    if handle.getValue(speciesID, byref(value)) == True:
        return value.value
    else:
        raise RuntimeError('Index out of Range')

def setValue(speciesID, val):
    """Sets the value of a single species

    Takes (string, double) as an argument for speciesID and value, respectively"""
    value = c_double(val)
    if handle.setValue(speciesID, byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getFloatingSpeciesConcentrations():
    """Returns the concentration of all floating species"""
    values = handle.getFloatingSpeciesConcentrations()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

def getGlobalParameterValues():
    """Returns the value of all global parameters"""
    values = handle.getGlobalParameterValues()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

def setBoundarySpeciesByIndex(index, val):
    """Sets the value of a boundary species by its index. species are indexed starting at 0.

    Takes (int, double) as an argument for index and value, respectively"""                             #test this
    value = c_double(val)
    ivalue = c_int(index)
    if handle.setBoundarySpeciesByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def setFloatingSpeciesByIndex(index, val):
    """Sets the value of a floating species by its index. species are indexed starting at 0.

    Takes (int, double) as an argument for index and value, respectively"""                             #test this
    value = c_double(val)
    ivalue = c_int(index)
    if handle.setFloatingSpeciesByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def setGlobalParameterByIndex(index, val):
    """Sets the value of a global parameter by its index. global parameters are indexed starting at 0.

    Takes (int, double) as an argument for index and value, respectively"""                             #test this
    value = c_double(val)
    ivalue = c_int(index)
    if handle.setGlobalParameterByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getBoundarySpeciesByIndex(index):
    """Returns the value of a boundary species by its index. Boundary species are indexed starting at 0.

    Takes an integer as an argument"""                             #test this
    value = c_double()
    ivalue = c_int(index)
    if handle.getBoundarySpeciesByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getFloatingSpeciesByIndex(index):
    """Returns the value of a floating species by its index. Floating species are indexed starting at 0.

    Takes an integer as an argument"""                             #test this
    value = c_double()
    ivalue = c_int(index)
    if handle.getFloatingSpeciesByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')


def getGlobalParameterByIndex(index):
    """Returns the value of a global parameter by its index. Global parameters are indexed starting at 0.

    Takes an integer as an argument"""
    value = c_double()
    ivalue = c_int(index)
    if handle.getGlobalParameterByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of Range')

def getCompartmentByIndex(index):
    """Returns the volume of a compartment by its index. Compartments are indexed starting at 0.

    Takes an integer as an argument"""
    value = c_double()
    ivalue = c_int(index)
    if handle.getCompartmentByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of Range')

def setCompartmentByIndex(index, val):
    """Sets the volume of a compartment by its index. Compartments are indexed starting at 0.

    Takes (int, double) as an argument for index and value, respectively"""                         #test this
    value = c_double(val)
    ivalue = c_int(index)
    if handle.setCompartmentByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')


#Jacobian matrix methods
def getFullJacobian():
    """Compute the full Jacobian at the current operating point"""
    values = handle.getFullJacobian()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

def getReducedJacobian():
    """Compute the reduced Jacobian at the current operating point"""
    values = handle.getReducedJacobian()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

def getEigenvalues():
    """Compute the eigenvalues at the current operating point"""
    values = handle.getEigenvalues()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

#Stoichiometry methods
def getStoichiometryMatrix():
    """Returns the stoichiometry matrix for the currently loaded model"""
    values = handle.getStoichimetryMatrix()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

def getLinkMatrix():
    """Returns the link matrix for the currently loaded model"""
    values = handle.getLinkMatrix()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

def getNrMatrix():
    """Returns the reduced stoichiometry matrix for the currently loaded model"""
    values = handle.getNrMatrix()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

def getL0Matrix():
    """Returns the L0 matrix for the currently loaded model"""
    values = handle.getL0Matrix()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

def getConservationMatrix():
    """Returns the conservation matrix (gamma) for the currently loaded model"""
    values = handle.getConservationMatrix()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

#Initial condition methods
handle.reset.restype = c_bool
#handle.setFloatingSpeciesInitialConcentrations.restype = c_bool

def reset():
    """resets the simulator to the initial conditions specified in the loaded SBML model"""
    return handle.reset()

#def setFloatingSpeciesInitialConcentration(vec):
#    return handle.setFloatingSpeciesInitialConcentration(vec)

def getFloatingSpeciesInitialConcentrations():
    """Returns a string with initial floating species concentrations"""
    values = handle.getFloatingSpeciesInitialConcentrations()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

def getFloatingSpeciesInitialConditionNames():
    """Returns a string with floating species initial condition names"""
    values = handle.getFloatingSpeciesInitialConditionNames()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

#Reaction rates
handle.getNumberOfReactions.restype = c_int
handle.getReactionRate.restype = c_bool

def getNumberOfReactions():
    """Get the number of reactions"""
    return handle.getNumberOfReactions()

def getReactionRate(index):
    """Returns the reaction rate by index"""
    value = c_int(index)
    if handle.getReactionRate(byref(value)) == True:
        return value.value
    else:
        raise RuntimeError('Index out of Range')                                 #test this

def getReactionRates():
    """Returns a vector with the current reaction rates"""
    values = handle.getReactionRates()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

def getReactionRatesEx(vec):                                                        #FIX THIS
    """Returns the rates of change given an array of new floating species concentrations"""
    return handle.printVector(handle.getReactionRatesEx(vec))

#Rates of change
handle.getRateOfChange.restype = c_bool
handle.evalModel.restype = c_bool

def getRatesOfChange():
    """Returns the current vector of rates of change"""
    values = handle.getRatesOfChange()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

def getRateOfChangeNames():
    """Returns the names given to the rate of change of the floating species"""
    values = handle.getRatesOfChangeNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result

def getRateOfChange():                                                 #FIX THIS
    """Returns the rate of change of a species by its index"""
    return handle.getRateOfChange()

def getRatesOfChangeEx(vec):
    """Returns the rates of changes given an array of new floating species concentrations"""                     #TEST
    values = handle.getRatesOfChangEx()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

def evalModel():
    """Updates the model based on all recent changes"""
    return handle.evalModel()

#Get number family
handle.getNumberOfCompartments.restype = c_int
handle.getNumberOfBoundarySpecies.restype = c_int
handle.getNumberOfFloatingSpecies.restype = c_int
handle.getNumberOfGlobalParameters.restype = c_int
handle.getNumberOfDependentSpecies.restype = c_int
handle.getNumberOfIndependentSpecies.restype = c_int

def getNumberOfCompartments():
    """Get the number of compartments"""
    return handle.getNumberOfCompartments()

def getNumberOfBoundarySpecies():
    """Get the number of boundary species"""
    return handle.getNumberOfBoundarySpecies()

def getNumberOfFloatingSpecies():
    """Get the number of floating species"""
    return handle.getNumberOfFloatingSpecies()

def getNumberOfGlobalParameters():
    """Get the number of global parameters"""
    return handle.getNumberOfGlobalParameters()

def getNumberOfDependentSpecies():
    """Get the number of dependent species"""
    return handle.getNumberOfDependentSpecies()

def getNumberOfIndependentSpecies():
    """Get the number of independent species"""
    return handle.getNumberOfIndependentSpecies()

#Get names family
def getReactionNames():
    """Returns a list of reaction names"""
    values = handle.getReactionNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result

def getRateOfChangeNames():
    """Returns the names given to the rate of change of the floating species"""
    values = handle.getRateOfChangeNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result

def getBoundarySpeciesNames():
    """Gets the list of boundary species names"""
    values = handle.getBoundarySpeciesNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result

def getFloatingSpeciesNames():
    """Gets the list of floating species names"""
    values = handle.getFloatingSpeciesNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result

def getGlobalParameterNames():
    """Gets the list of global parameter names"""
    values = handle.getGlobalParameterNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result


def getCompartmentNames():
    """Gets the list of compartment names"""
    values = handle.getCompartmentNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result

def getEigenValueNames():
    """Returns the symbols of all floating species eigenvalues"""
    values = handle.getEigenValueNames()
    result = handle.printList(values)
    handle.freeStringList(values)
    return result

def getAvailableSymbols():
    """Returns symbols of the currently loaded model that can be used for the selectionlist format array of arrays"""                              #FIX
    value = handle.getAvailableSymbols()
    result = handle.printArrayList(value)
    return result

#Get MCA methods

def getElasticityCoefficientNames():
    """Returns the symbols of all elasticity coefficients"""
    value = handle.getElasticityCoefficientNames()
    result = handle.printStringArrayList(value)
    handle.freeStringArrayList(value)
    return result

def getUnscaledFluxControlCoefficientNames():
    """Returns the symbols of all unscaled flux control coefficients"""
    value = handle.getUnscaledFluxControlCoefficientNames()
    result = handle.printStringArrayList(value)
    handle.freeStringArrayList(value)
    return result

def getFluxControlCoefficientNames():
    """Returns the symbols of all flux control coefficients"""
    value = handle.getFluxControlCoefficientNames()
    result = handle.printStringArrayList(value)
    handle.freeStringArrayList(value)
    return result

def getUnscaledConcentrationControlCoefficientNames():
    """Returns the symbols of all unscaled concentration control coefficients"""
    value = handle.getUnscaledConcentrationCoefficientNames()
    result = handle.printStringArrayList(value)
    handle.freeStringArrayList(value)
    return result

def getConcentrationControlCoefficientNames():
    """Returns the symbols of all concentration control coefficients"""
    value = handle.getConcentrationControlCoefficientNames()
    result = handle.printStringArrayList(value)
    handle.freeStringArrayList(value)
    return result

def getUnScaledElasticityMatrix():
    """Compute the unscaled species elasticity matrix at the current operating point"""
    value = handle.getUnscaledElasticityMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

def getScaledElasticityMatrix():
    """Compute the scaled elasticity matrix at the current operating point"""
    value = handle.getScaledElasticityMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

def getUnscaledConcentrationControlCoefficientMatrix():
    """Compute the matrix of unscaled concentration control coefficients"""
    value = handle.getUnscaledConcentrationControlCoefficientMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

def getScaledConcentrationControlCoefficientMatrix():
    """Compute the matrix of unscaled concentration control coefficients"""
    value = handle.getScaledConcentrationControlCoefficientMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

def getUnscaledFluxControlCoefficientMatrix():
    """Compute the matrix of unscaled flux control coefficients"""
    value = handle.getUnscaledFluxControlCoefficientMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

def getScaledFluxControlCoefficientMatrix():
    """Compute the matrix of scaled flux control coefficients"""
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

def getuCC(variable, parameter):
    """Get unscaled control coefficient with respect to a global parameter

    Takes (variableName, parameterName) as an argument, where both arguments are strings"""
    variable = c_char_p()
    parameter = c_char_p()
    value = c_double()
    if handle.getuCC(byref(variable), byref(parameter), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getCC(variable, parameter, value):
    """Get scaled control coefficient with respect to a global parameter

    Takes (variableName, parameterName) as an argument, where both arguments are strings"""
    value = c_double()
    if handle.getCC(variable, parameter, value,  byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getEE(name, species, value):
    """Get scaled elasticity coefficient with respect to a global parameter or species

    Takes (reactionName, parameterName) as an argument, where both arguments are strings"""
    value = c_double()
    if handle.getEE(name, species, value,  byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getuEE(name, species, value):
    """Get unscaled elasticity coefficient with respect to a global parameter or species

    Takes (reactionName, parameterName) as an argument, where both arguments are strings"""
    value = c_double()
    if handle.getuEE(name, species, value,  byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

def getScaledFloatingSpeciesElasticity(reactionName, speciesName, value):
    """Compute the scaled elasticity for a given reaction and given species

    Takes (reactionName, parameterName) as an argument, where both arguments are strings"""
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
    """Sets the path for temporary files"""
    return handle.setTempFolder(tempfolder)

def getTempFolder():
    """Returns the path for temporary files"""
    return handle.getTempFolder()

def enableLogging():
    """Enables logging"""
    return handle.enableLogging()

def getResultElement (m, i, j):
    value = c_double()
    if handle.getResultElement (m, i, j, byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

#=======================================================#
