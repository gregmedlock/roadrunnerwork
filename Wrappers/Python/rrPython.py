##@Module rrPython
#
#This module allows access to the rr_c_api.dll from python"""

import sys
import os
from ctypes import *
os.chdir(os.path.dirname(__file__))
rrInstallFolder = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'bin'))
os.environ['PATH'] = rrInstallFolder + ';' + "c:\\Python27" + ';' + os.environ['PATH']
handle = WinDLL (rrInstallFolder + "\\rr_c_api.dll")

##\mainpage notitle
#\section Introduction
#RoadRunner is a high performance and portable simulation engine for systems and synthetic biology. To run a simple SBML model and generate time series data we would call:
#
#@code
#
#import rrPython
#
#rrPython.loadSBMLFromFile('C:\\Models\\mymodel.xml')
#
#rrPython.simulate()
#@endcode


#=======================rr_c_api=======================#
rr = handle.getRRInstance()

#Latest
handle.writeSBML.restype = c_char_p

##Returns the SBML with the current parameterset
def writeSBML():
    return handle.writeSBML()

#Utility and informational methods
handle.getVersion.restype = c_char_p
#\addtogroup group Utility and Informational Methods
#@{

##Returns the version of the C API
def getVersion():
    return handle.getVersion()

##@}

#Logging
handle.enableLogging.restype = c_bool
handle.setLogLevel.restype = c_bool
handle.getLogLevel.restype = c_char_p
handle.getLogFileName.restype = c_char_p
handle.getBuildDate.restype = c_char_p
handle.getCopyright.restype = c_char_p
handle.setTempFolder.restype = c_bool
handle.getTempFolder.restype = c_char_p

##\addtogroup Logging
#@{

##Allows a log file to be written
def enableLogging():
    return handle.enableLogging()

##Sets the log level. Available levels are  "ANY", "DEBUG5", "DEBUG4", "DEBUG3", "DEBUG2", "DEBUG1", "DEBUG", "INFO", "WARNING", and "ERROR"
def setLogLevel(lvl):
    return handle.setLogLevel(lvl)

##Returns the log level
def getLogLevel():
    return handle.getLogLevel()

##Returns the name of the log file
def getLogFileName():
    return handle.getLogFileName()

##Returns the date of the installed C API
def getBuildDate():
    return handle.getBuildDate()

##Returns the copyright date
def getCopyright():
    return handle.getCopyright()

##Sets the write location for the temporary file
#
#Takes a string as an argument
def setTempFolder(folder):
    return handle.setTempFolder(folder)

##Returns the full path of the temporary folder
def getTempFolder():
    return handle.getTempFolder()

##@}

#Error Handling
handle.hasError.restype = c_bool
handle.getLastError.restype = c_char_p

##\addtogroup Error Handling
#@{

##
#
def hasError():
    return handle.hasError()

##Returns the last error
def getLastError():
    return handle.getLastError()

##@}

#RoadRunner API

#########################################################################################################################

#Flags/Options
handle.setComputeAndAssignConservationLaws.restype = c_bool

##\addtogroup Flags and Options
#@{

##Turns on/off conservation laws
#
#Takes a 1 (on) or 0 (off) as an argument
def setComputeAndAssignConservationLaws(OnOrOff):
    return handle.setComputeAndAssignConservationLaws(OnOrOff)

##@}

#Load SBML methods
handle.loadSBML.restype = c_bool
handle.loadSBMLFromFile.restype = c_bool

##\addtogroup Load SBML Methods
#@{

##Loads SBML model from a string
#
#Takes a string as an argument
def loadSBML(sbml):
    return handle.loadSBML(sbml)

##Loads SBML model from a file
#
#Takes a string as an argument
def loadSBMLFromFile(fileName):
    return handle.loadSBMLFromFile(fileName)

##@}

#SBML utility methods
handle.getParamPromotedSBML.restype = c_char_p
handle.getSBML.restype = c_char_p

##\addtogroup SBML Utility Methods
#@{

##Returns the SBML with the current parameterset
def getParamPromotedSBML(sArg):
    return handle.getParamPromotedSBML(sArg)

##Returns the SBML model that is currently loaded
def getSBML():
    return handle.getSBML()

##@}

#Get and set capability routines
handle.setCapabilities.restype = c_bool
handle.getCapabilities.restype = c_char_p

##\addtogroup Get and Set Capability Routines
#@{

##Sets simulator capabilities
def setCapabilities(caps):
    return handle.setCapabilities(caps)

##Returns simulator capabilities
def getCapabilities():
    return handle.getCapabilities()

##@}

#Simulation Methods
handle.setTimeStart.restype = c_bool
handle.setTimeEnd.restype = c_bool
handle.setNumPoints.restype = c_bool
handle.setSelectionList.restype = c_bool
handle.oneStep.restype = c_bool
handle.getTimeStart.restype = c_bool
handle.getTimeEnd.restype = c_bool
handle.getNumPoints.restype = c_bool

##\addtogroup Simulation Methods
#@{

##Sets the start time for the simulation
#
#Takes a double as an argument
def setTimeStart(timeStart):
    return handle.setTimeStart (byref (c_double(timeStart)))

##Sets the end time for the simulation
#
#Takes a double as an argument
def setTimeEnd(timeEnd):
    return handle.setTimeEnd (byref (c_double(timeEnd)))

##Sets the number of points for the simulation
#
#Takes an int as an argument
def setNumPoints(numPoints):
    return handle.setNumPoints(byref (c_int(numPoints)))

##Sets the list of variables returned by simulate()
#
#Available symbols for setSelectionList can be found by using getAvailableSymbols() after loading a model
#
#Takes a string with variable Ids separated by a space or a comma as an argument
def setSelectionList(list):
    return handle.setSelectionList(list)

##Returns the list of variables returned by simulate()
def getSelectionList():
    value = handle.getSelectionList()
    result = handle.printStringList(value)
    handle.freeStringList(value)
    return result

##Simulates the model that is currently loaded and returns values for variables as chosen with setSelectionList()
def simulate():
    value = handle.simulate()
    result = handle.printResult(value)
    handle.freeResult(value)
    return result

##Simulates a reaction in a given period and with a given number of points
#
#Takes (double,double,int) as an argument for timeStart, timeEnd, and numberOfPoints, respectively
def simulateEx(timeStart,timeEnd,numberOfPoints):
    startValue = c_double(timeStart)
    endValue = c_double(timeEnd)
    pointsValue = c_int(numberOfPoints)
    simulation = handle.simulateEx(byref(startValue),byref(endValue),byref(pointsValue))
    result = handle.printResult(simulation)
    handle.freeResult(simulation)
    return result;

##Carry out a single integration step using a stepsize as indicated in the method call (the intergrator is reset to take into account all variable changes). Arguments: double CurrentTime, double StepSize, Return Value: new CurrentTime.
#
#Takes (double, double) as an argument
def oneStep (currentTime, stepSize):                             #test this
    curtime = c_double(currentTime)
    stepValue = c_double(stepSize)
    value = c_double()
    if handle.oneStep(byref(curtime), byref(stepSize), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

##Returns the simulation start time
def getTimeStart():
    value = c_double()
    if handle.getTimeStart(byref(value)) == True:
        return value.value
    else:
        return ('Index out of Range')

##Returns the simulation end time
def getTimeEnd():
    value = c_double()
    if handle.getTimeEnd(byref(value)) == True:
        return value.value
    else:
        return ('Index out of Range')

##Returns the number of points in the simulation
def getNumPoints():
    value = c_int()
    if handle.getNumPoints(byref(value)) == True:
        return value.value
    else:
        return ('Index out of Range')

##@}

#Steady state methods
handle.steadyState.restype = c_bool
handle.setSteadyStateSelectionList.restype = c_bool

##\addtogroup Steady State Methods
#@{

##Computes the steady state of the loaded model and returns the sum of squares of the solution
def steadyState():
    value = c_int()
    if handle.steadyState(byref(value)) == True:
        return value.value
    else:
        return ('Index out of Range')

##Computes and returns the steady state solution of the loaded model
def computeSteadyStateValues():
    values = handle.computeSteadyStateValues()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

##Sets the variables returned by steadyState() and computeSteadyStateValues()
#
#Takes a string with variables separated by a space or a comma as an argument
def setSteadyStateSelectionList(list):
    return handle.setSteadyStateSelectionList(list)

##Returned the variables returned by steadyState() and computeSteadyStateValues()
def getSteadyStateSelectionList():
    values = handle.getSteadyStateSelectionList()
    result = handle.printStringList(values)
    handle.freeStringList(values)
    return result

##@}

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

##\addtogroup Set and Get Family of Methods
#@{

##Returns the current value for a single species in loaded model
#
#Takes a string as an argument
def getValue(symbolName):
    value = c_double()
    if handle.getValue(symbolName, byref(value)) == True:
        return value.value
    else:
        raise RuntimeError('Index out of Range')

##Sets the value of a single species
#
#Takes (string, double) as an argument for symbolName and value, respectively
def setValue(symbolName, value):
    value = c_double(value)
    if handle.setValue(symbolName, byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

##Returns the concentration of all floating species
def getFloatingSpeciesConcentrations():
    values = handle.getFloatingSpeciesConcentrations()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

##Returns the value of all global parameters
def getGlobalParameterValues():
    values = handle.getGlobalParameterValues()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

##Sets the value of a boundary species by its index. species are indexed starting at 0.
#
#Takes (int, double) as an argument for index and value, respectively
def setBoundarySpeciesByIndex(index, value):
    value = c_double(value)
    ivalue = c_int(index)
    if handle.setBoundarySpeciesByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

##Sets the value of a floating species by its index. species are indexed starting at 0.
#
#Takes (int, double) as an argument for index and value, respectively
def setFloatingSpeciesByIndex(index, value):
    value = c_double(value)
    ivalue = c_int(index)
    if handle.setFloatingSpeciesByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

##Sets the value of a global parameter by its index. global parameters are indexed starting at 0.
#
#Takes (int, double) as an argument for index and value, respectively
def setGlobalParameterByIndex(index, value):
    value = c_double(value)
    ivalue = c_int(index)
    if handle.setGlobalParameterByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

##Returns the value of a boundary species by its index. Boundary species are indexed starting at 0.
#
#Takes an integer as an argument
def getBoundarySpeciesByIndex(index):
    value = c_double()
    ivalue = c_int(index)
    if handle.getBoundarySpeciesByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

##Returns the value of a floating species by its index. Floating species are indexed starting at 0.
#
#Takes an integer as an argument
def getFloatingSpeciesByIndex(index):
    value = c_double()
    ivalue = c_int(index)
    if handle.getFloatingSpeciesByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

##Returns the value of a global parameter by its index. Global parameters are indexed starting at 0.
#
#Takes an integer as an argument
def getGlobalParameterByIndex(index):
    value = c_double()
    ivalue = c_int(index)
    if handle.getGlobalParameterByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of Range')

##Returns the volume of a compartment by its index. Compartments are indexed starting at 0.
#
#Takes an integer as an argument
def getCompartmentByIndex(index):
    value = c_double()
    ivalue = c_int(index)
    if handle.getCompartmentByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of Range')

##Sets the volume of a compartment by its index. Compartments are indexed starting at 0.
#
#Takes (int, double) as an argument for index and value, respectively
def setCompartmentByIndex(index, value):
    value = c_double(value)
    ivalue = c_int(index)
    if handle.setCompartmentByIndex(byref(ivalue), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

##@}

#Jacobian matrix methods

##\addtogroup Jacobian Matrix Methods
#@{

##Compute the full Jacobian at the current operating point
def getFullJacobian():
    values = handle.getFullJacobian()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

##Compute the reduced Jacobian at the current operating point
def getReducedJacobian():
    values = handle.getReducedJacobian()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

##Compute the eigenvalues at the current operating point
def getEigenvalues():
    values = handle.getEigenvalues()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

##@}

#Stoichiometry methods

##\addtogroup Stoichiometry Methods
#@{

##Returns the stoichiometry matrix for the currently loaded model
def getStoichiometryMatrix():
    values = handle.getStoichimetryMatrix()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

##Returns the link matrix for the currently loaded model
def getLinkMatrix():
    values = handle.getLinkMatrix()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

##Returns the reduced stoichiometry matrix for the currently loaded model
def getNrMatrix():
    values = handle.getNrMatrix()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

##Returns the L0 matrix for the currently loaded model
def getL0Matrix():
    values = handle.getL0Matrix()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

##Returns the conservation matrix (gamma) for the currently loaded model
def getConservationMatrix():
    values = handle.getConservationMatrix()
    result = handle.printMatrix(values)
    handle.freeMatrix(values)
    return result

##@}

#Initial condition methods
handle.reset.restype = c_bool
#handle.setFloatingSpeciesInitialConcentrations.restype = c_bool

##\addtogroup Initial Condition Methods
#@{

##resets the simulator to the initial conditions specified in the loaded SBML model
def reset():
    return handle.reset()

#def setFloatingSpeciesInitialConcentration(vec):                                        #FIX THIS
#    return handle.setFloatingSpeciesInitialConcentration(vec)

##Returns a string with initial floating species concentrations
def getFloatingSpeciesInitialConcentrations():
    values = handle.getFloatingSpeciesInitialConcentrations()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

##Returns a string with floating species initial condition Ids
def getFloatingSpeciesInitialConditionIds():
    values = handle.getFloatingSpeciesInitialConditionIds()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

##@}

#Reaction rates
handle.getNumberOfReactions.restype = c_int
handle.getReactionRate.restype = c_bool

##\addtogroup Reaction Rates
#@{

##Get the number of reactions
def getNumberOfReactions():
    return handle.getNumberOfReactions()

##Returns the reaction rate by index
def getReactionRate(index):
    value = c_int(index)
    if handle.getReactionRate(byref(value)) == True:
        return value.value
    else:
        raise RuntimeError('Index out of Range')                                 #test this

##Returns a vector with the current reaction rates
def getReactionRates():
    values = handle.getReactionRates()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

##Returns the rates of change given an array of new floating species concentrations
def getReactionRatesEx(vec):                                                        #FIX THIS
    return handle.printVector(handle.getReactionRatesEx(vec))

##@}

#Rates of change
handle.getRateOfChange.restype = c_bool
handle.evalModel.restype = c_bool

##\addtogroup Rates of Change
#@{

##Returns the current vector of rates of change
def getRatesOfChange():
    values = handle.getRatesOfChange()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

##Returns the Ids given to the rate of change of the floating species
def getRateOfChangeIds():
    values = handle.getRatesOfChangeIds()
    result = handle.printStringList(values)
    handle.freeStringList(values)
    return result

##Returns the rate of change of a species by its index
def getRateOfChange():                                                 #FIX THIS
    return handle.getRateOfChange()

##Returns the rates of changes given an array of new floating species concentrations
def getRatesOfChangeEx(vec):                                          #TEST
    values = handle.getRatesOfChangEx()
    result = handle.printVector(values)
    handle.freeVector(values)
    return result

##Updates the model based on all recent changes
def evalModel():
    return handle.evalModel()

##@}

#Get number family
handle.getNumberOfCompartments.restype = c_int
handle.getNumberOfBoundarySpecies.restype = c_int
handle.getNumberOfFloatingSpecies.restype = c_int
handle.getNumberOfGlobalParameters.restype = c_int
handle.getNumberOfDependentSpecies.restype = c_int
handle.getNumberOfIndependentSpecies.restype = c_int

##\addtogroup Get Number Family
#@{

##Get the number of compartments
def getNumberOfCompartments():
    return handle.getNumberOfCompartments()

##Get the number of boundary species
def getNumberOfBoundarySpecies():
    return handle.getNumberOfBoundarySpecies()

##Get the number of floating species
def getNumberOfFloatingSpecies():
    return handle.getNumberOfFloatingSpecies()

##Get the number of global parameters
def getNumberOfGlobalParameters():
    return handle.getNumberOfGlobalParameters()

##Get the number of dependent species
def getNumberOfDependentSpecies():
    return handle.getNumberOfDependentSpecies()

##Get the number of independent species
def getNumberOfIndependentSpecies():
    return handle.getNumberOfIndependentSpecies()

##@}

#Get Ids family

##\addtogroup Get Ids Family
#@{

##Returns a list of reaction Ids
def getReactionIds():
    values = handle.getReactionIds()
    result = handle.printStringList(values)
    handle.freeStringList(values)
    return result

##Returns the Ids given to the rate of change of the floating species
def getRateOfChangeIds():
    values = handle.getRateOfChangeIds()
    result = handle.printStringList(values)
    handle.freeStringList(values)
    return result

##Gets the list of boundary species Ids
def getBoundarySpeciesIds():
    values = handle.getBoundarySpeciesIds()
    result = handle.printStringList(values)
    handle.freeStringList(values)
    return result

##Gets the list of floating species Ids
def getFloatingSpeciesIds():
    values = handle.getFloatingSpeciesIds()
    result = handle.printStringList(values)
    handle.freeStringList(values)
    return result

##Gets the list of global parameter Ids
def getGlobalParameterIds():
    values = handle.getGlobalParameterIds()
    result = handle.printStringList(values)
    handle.freeStringList(values)
    return result

##Gets the list of compartment Ids
def getCompartmentIds():
    values = handle.getCompartmentIds()
    result = handle.printStringList(values)
    handle.freeStringList(values)
    return result

##Returns the symbols of all floating species eigenvalues
def getEigenValueIds():
    values = handle.getEigenValueIds()
    result = handle.printStringList(values)
    handle.freeStringList(values)
    return result

##Returns symbols of the currently loaded model that can be used for the selectionlist format array of arrays
def getAvailableSymbols():                              #FIX
    value = handle.getAvailableSymbols()
    result = handle.printArrayList(value)
    return result

##@}

#Get MCA methods

##\addtogroup Get MCA Methods
#@{

##Returns the symbols of all elasticity coefficients
def getElasticityCoefficientIds():
    value = handle.getElasticityCoefficientIds()
    result = handle.printStringArrayList(value)
    handle.freeStringArrayList(value)
    return result

##Returns the symbols of all unscaled flux control coefficients
def getUnscaledFluxControlCoefficientIds():
    value = handle.getUnscaledFluxControlCoefficientIds()
    result = handle.printStringArrayList(value)
    handle.freeStringArrayList(value)
    return result

##Returns the symbols of all flux control coefficients
def getFluxControlCoefficientIds():
    value = handle.getFluxControlCoefficientIds()
    result = handle.printStringArrayList(value)
    handle.freeStringArrayList(value)
    return result

##Returns the symbols of all unscaled concentration control coefficients
def getUnscaledConcentrationControlCoefficientIds():
    value = handle.getUnscaledConcentrationCoefficientIds()
    result = handle.printStringArrayList(value)
    handle.freeStringArrayList(value)
    return result

##Returns the symbols of all concentration control coefficients
def getConcentrationControlCoefficientIds():
    value = handle.getConcentrationControlCoefficientIds()
    result = handle.printStringArrayList(value)
    handle.freeStringArrayList(value)
    return result

##Compute the unscaled species elasticity matrix at the current operating point
def getUnScaledElasticityMatrix():
    value = handle.getUnscaledElasticityMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

##Compute the scaled elasticity matrix at the current operating point
def getScaledElasticityMatrix():
    value = handle.getScaledElasticityMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

##Compute the matrix of unscaled concentration control coefficients
def getUnscaledConcentrationControlCoefficientMatrix():
    value = handle.getUnscaledConcentrationControlCoefficientMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

##Compute the matrix of unscaled concentration control coefficients
def getScaledConcentrationControlCoefficientMatrix():
    value = handle.getScaledConcentrationControlCoefficientMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

##Compute the matrix of unscaled flux control coefficients
def getUnscaledFluxControlCoefficientMatrix():
    value = handle.getUnscaledFluxControlCoefficientMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

##Compute the matrix of scaled flux control coefficients
def getScaledFluxControlCoefficientMatrix():
    value = handle.getScaledFluxControlCoefficientMatrix()
    result = handle.printMatrix(value)
    handle.freeMatrix(value)
    return result

##@}

#MCA methods
handle.getuCC.restype = c_bool
handle.getCC.restype = c_bool
handle.getEE.restype = c_bool
handle.getuEE.restype = c_bool
handle.getScaledFloatingSpeciesElasticity.restype = c_bool

##\addtogroup MCA Methods
#@{

##Get unscaled control coefficient with respect to a global parameter
#
#Takes (variableName, parameterName) as an argument, where both arguments are strings
def getuCC(variable, parameter):
    variable = c_char_p()
    parameter = c_char_p()
    value = c_double()
    if handle.getuCC(byref(variable), byref(parameter), byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

##Get scaled control coefficient with respect to a global parameter
#
#Takes (variableName, parameterName) as an argument, where both arguments are strings
def getCC(variable, parameter, value):
    value = c_double()
    if handle.getCC(variable, parameter, value,  byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

##Get scaled elasticity coefficient with respect to a global parameter or species
#
#Takes (reactionName, parameterName) as an argument, where both arguments are strings
def getEE(name, species, value):
    value = c_double()
    if handle.getEE(name, species, value,  byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

##Get unscaled elasticity coefficient with respect to a global parameter or species
#
#Takes (reactionName, parameterName) as an argument, where both arguments are strings
def getuEE(name, species, value):
    value = c_double()
    if handle.getuEE(name, species, value,  byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

##Compute the scaled elasticity for a given reaction and given species
#
#Takes (reactionName, parameterName) as an argument, where both arguments are strings
def getScaledFloatingSpeciesElasticity(reactionName, speciesName, value):
    value = c_double()
    if handle.getScaledFloatingSpeciesElasticity(reactionName, speciesName, value,  byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

##@}

#Print/format functions
handle.printResult.restype = c_char_p
handle.printMatrix.restype = c_char_p
handle.printVector.restype = c_char_p
handle.printStringList.restype = c_char_p
handle.printStringArrayList.restype = c_char_p
#handle.printArrayList.restype = c_char_p

def printResult(result):
    return handle.printResult(result)

def printMatrix(mat):
    return handle.printMatrix(mat)

def printVector(vec):
    return handle.printVector(vec)

def printStringList(list):
    return handle.printStringList(list)

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

def getResultElement (m, i, j):
    value = c_double()
    if handle.getResultElement (m, i, j, byref(value)) == True:
        return value.value;
    else:
        raise RuntimeError('Index out of range')

#=======================================================#
