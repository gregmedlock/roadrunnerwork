/**
 * @file rr_c_api.h
 * @brief roadRunner C API 2012
 * @author Totte Karlsson & Herbert M Sauro
 *
 * <--------------------------------------------------------------
 * This file is part of cRoadRunner.
 * See http://code.google.com/p/roadrunnerwork/ for more details.
 *
 * Copyright (C) 2012
 *   University of Washington, Seattle, WA, USA
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * In plain english this means:
 *
 * You CAN freely download and use this software, in whole or in part, for personal,
 * company internal, or commercial purposes;
 *
 * You CAN use the software in packages or distributions that you create.
 *
 * You SHOULD include a copy of the license in any redistribution you may make;
 *
 * You are NOT required include the source of software, or of any modifications you may
 * have made to it, in any redistribution you may assemble that includes it.
 *
 * YOU CANNOT:
 *
 * redistribute any piece of this software without proper attribution;
*/

/*! \mainpage cRoadRunner Library
 *
 * \section intro_sec Introduction
 *
 * RoadRunner is a SBML compliant high performance and portable simulation engine 
 * for systems and synthetic biology. To run a simple SBML model 
 * and generate time series data we would call:
 *
 \code
 RRResultHandle result;

 if (!loadSBMLFromFile ("mymodel.xml"))
    exit;

 result = simulate (0, 10, 100);
 printf (resultToString (output)
 \endcode

 More complex example:

 \code
 #include <stdlib.h>
 #include <stdio.h>
 #include "rr_c_api.h"

 int main(int nargs, char** argv)
 {
        RRHandle rrInstance = getRRInstance();

        printf("loading model file %s\n", argv[1]);
 
        if (!loadSBMLFromFile(argv[1])) {
           printf ("Error while loading SBML file\n");
           printf ("Error message: %s\n", getLastError());
           exit();
        }
		   
        RRResultHandle output = simulate (0, 100, 1000);  // start time, end time, and number of points
        
        printf("Output table has %i rows and %i columns\n", output->RSize, output->RCols);
        printResult (output);
        
        freeResult (output);
        freeRRInstance (rrInstance)

        return 0;
 }
 \endcode
 * \section install_sec Installation
 *
 * Installation documentation is provided in the main google code page.

 \defgroup initialization Library initialization and termination methods
 \brief Initailize library and terminate linbrary instance

 \defgroup loadsave Read and Write models
 \brief Read and write models to files or strings. Support for SBML formats.

 \defgroup utility Utility functions
 \brief Various miscellaneous routines that return useful inforamtion about the library

 \defgroup errorfunctions Error handling functions
 \brief Error handlining routines

 \defgroup state Current state of system
 \brief Compute derivatives, fluxes, and other values of the system at the current state

 \defgroup steadystate Steady State Routines
 \brief Compute and obtain basic information about the steady state

 \defgroup reaction Reaction group
 \brief Get information about reaction rates

 \defgroup rateOfChange Rates of change group
 \brief Get information about rates of change

 \defgroup boundary Boundary species group
 \brief Get information about boundary species

 \defgroup floating Floating species group
 \brief Get information about floating species

 \defgroup initialConditions Initial conditions group
 \brief Set or get initial conditions
 
 \defgroup parameters Parameter group
 \brief Set and get global and local parameters

 \defgroup compartment Compartment group
 \brief Set and Get information on compartments

 \defgroup simulation Time-course simulation
 \brief Deterministic, stochastic, and hybrid simulation algorithms

 \defgroup mca Metabolic Control Analysis
 \brief Calculate control coefficients and sensitivities

 \defgroup Stoich Stoichiometry analysis
 \brief Linear algebra based methods for analyzing a reaction network

 \defgroup helperRoutines Helper Routines
 \brief Helper routines for acessing the various C API types, eg lists and arrays

 \defgroup toString ToString Routines
 \brief Render various result data types as strings

 \defgroup freeRoutines Free memory routines
 \brief Routines that should be used to free various data structures generated during the course of using the library

*/

#ifndef rrC_APIH
#define rrC_APIH

#if defined(__cplusplus)
extern "C"
{
#else
//#include <stdio.h>
//#include "rr_support/stdbool.h"
#endif

#include "rr_c_api_exporter.h"
#include "rr_c_types.h"

#if defined(_MSC_VER)
	#include <direct.h>
	#define getcwd _getcwd
	#define chdir  _chrdir
	#define MAXPATH _MAX_PATH
#elif defined(__BORLANDC__)
  	#include <dir.h>
#else
#include <unistd.h>
#endif

// -----------------------------------------------------------------------
/** \} */
/**
  * @name Utility routines
  */
/** \{ */
// -----------------------------------------------------------------------

/*!
 \brief Retrieve the current version number of the library
 \return char* verison - Returns null if it fails, otherwise it returns the version number of the library
 \ingroup utility
*/
C_DECL_SPEC char* rrCallConv getVersion();

/*!
 \brief Retrieve the current build date of the library
 \return Returns null if it fails, otherwise it returns the build date
 \ingroup utility
*/
C_DECL_SPEC char*  rrCallConv getBuildDate();

/*!
 \brief Retrieve the current copyright notice for the library
 \return Returns null if it fails, otherwise it returns the copyright string
 \ingroup utility
*/
C_DECL_SPEC char*  rrCallConv getCopyright();

/*!
 \brief SEt the temporary folder

 When cRoadRunner is run in C generation mode its uses a temporary folder to store the 
 generate C source code. This method can be used to set the temporary folder path if necessary.

 \return Returns true if succesful
 \ingroup utility
*/
C_DECL_SPEC bool rrCallConv setTempFolder(const char* folder);

/*!
 \brief Retrieve the current temporary folder path

 When cRoadRunner is run in C generation mode its uses a temporary folder to store the 
 generate C source code. This method can be used to get the current value
 for the the temporary folder path.

 \return Returns null if it fails, otherwise it returns the path
 \ingroup utility
*/
C_DECL_SPEC char* rrCallConv getTempFolder();

/*!
 \brief Retrieve a pointer to the C code structure, RRCCode

 When cRoadRunner is run in C generation mode its uses a temporary folder to store the 
 generate C source code. This method can be used to obtain the header and main source
 code after a model has been loaded. 

 \return Returns null if it fails, otherwise it returns a pointer to the RRCode structure
 \ingroup utility
*/
C_DECL_SPEC RRCCode* rrCallConv getCCode();


// Logging

/*!
 \brief Enable logging

 \return Returns true if succesful
 \ingroup errorfunctions
*/
C_DECL_SPEC bool rrCallConv enableLogging();

/*!
 \brief Set the logging status level

 The logging level is determined by the following strings
 
 "ANY", "DEBUG5", "DEBUG4", "DEBUG3", "DEBUG2", "DEBUG1",
 "DEBUG", "INFO", "WARNING", "ERROR"

 Example: setLogLevel ("DEBUG4")

 \param lvl Pointer to the logging level string. 
 \return Ruturns true if succesful
 \ingroup errorfunctions
*/
C_DECL_SPEC bool rrCallConv setLogLevel(const char* lvl);

/*!
 \brief Get the logging status level as a pointer to a string

 The logging level can be one of the following strings
 
 "ANY", "DEBUG5", "DEBUG4", "DEBUG3", "DEBUG2", "DEBUG1",
 "DEBUG", "INFO", "WARNING", "ERROR"

 Example: str = getLogLevel ()

 \return Returns null is it fails else returns a pointer to the logging string
 \ingroup errorfunctions
*/
C_DECL_SPEC char* rrCallConv getLogLevel();

/*!
 \brief Get the logging status level as a pointer to a string

 The logging level can be one of the following strings
 
 "ANY", "DEBUG5", "DEBUG4", "DEBUG3", "DEBUG2", "DEBUG1",
 "DEBUG", "INFO", "WARNING", "ERROR"

 Example: str = getLogFileName ()

 \return Returns null is it fails else returns the full path to the logging file name
 \ingroup errorfunctions
*/
C_DECL_SPEC char* rrCallConv getLogFileName();

/*!
 \brief Check if there is an error string to retrieve

 Example: status = hasError ()

 \return status - Returns true if there is an error waiting to be retrieved
 \ingroup errorfunctions
*/
C_DECL_SPEC bool rrCallConv hasError();

/*!
 \brief Retrieve the currnt error string

 Example: str = getLastError ()

 \return Return null if fails, otherwise returns a pointer to the error string
 \ingroup errorfunctions
*/
C_DECL_SPEC char* rrCallConv getLastError();


/*!
 \brief Initialize the roadRunner library and return an instance
 \return Returns an instance of the library, returns null if it fails
 \ingroup initialization
*/
C_DECL_SPEC RRHandle rrCallConv getRRInstance();

/*!
 \brief Free the roadRunner instance 
 \param handle Free the roadRunner instance given in the argument
 \ingroup initialization
*/
C_DECL_SPEC bool rrCallConv freeRRInstance(RRHandle handle);


// Flags/Options
/*!
 \brief Enable or disable conservation analysis
 \param OnOrOff Set true to switch on conservation analysis
 \return Returns true if successful
 \ingroup initialization
*/
C_DECL_SPEC bool rrCallConv setComputeAndAssignConservationLaws(const bool& OnOrOff);

// -----------------------------------------------------------------------
// Read and Write models
// -----------------------------------------------------------------------

/*!
 \brief Create a model from an SBML string
 \param[in] sbml string
 \return Returns true if sucessful
 \ingroup loadsave
*/
C_DECL_SPEC bool rrCallConv loadSBML(const char* sbml);

/*!
 \brief Create a model from a SBML file
 \param fileName file name 
 \return Returns true if sucessful
 \ingroup loadsave
*/
C_DECL_SPEC bool rrCallConv loadSBMLFromFile(const char* fileName);

/*!
 \brief Retrieve the <b>current state</b> of the model in the form of an SBML string
  \return Returns null is the call fails, otherwise returns a pointer to the SBML string
 \ingroup loadsave
*/
C_DECL_SPEC char* rrCallConv getCurrentSBML();      

/*!
 \brief Retrieve the last SBML model that was loaded
 \return Returns null is the call fails, otherwise returns a pointer to the SBML string
 \ingroup loadsave
*/
C_DECL_SPEC char* rrCallConv getSBML();


// -------------------------------------------------------------------------
// SBML utility methods
/*!
 \brief Promote any local parameters to global status
 \param sArg points to the SBML model to promote
 \return Returns null if it fails otherwise it returns the promoted SBML string
 \ingroup sbml
*/
C_DECL_SPEC char* rrCallConv getParamPromotedSBML(const char* sArg);

/*!
 \brief Set the simulator's capabilities
 \param[out] caps An XML string that specifies the simulators capabilities
 \return Returns true if sucessful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv setCapabilities (const char* caps);


/*!
 \brief Get the simulator's capabilities
 \return Returns null if it fails, otherwise it returns the simulator's capabilities in the form of an XML string
 \ingroup simulation
*/C_DECL_SPEC char* rrCallConv getCapabilities();

/*!
 \brief Set the time start for a simulation
 \param  timeStart
 \return Returns True if sucessful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv setTimeStart(const double& timeStart);

/*!
 \brief Set the time end for a simulation
 \param timeEnd
 \return Returns true if sucessful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv setTimeEnd(const double& timeEnd);

/*!
 \brief Set the number of points to generate in a simulation
 \param nrPoints Number of points to generate
 \return Returns true if sucessful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv setNumPoints(const int& nrPoints);

/*!
 \brief Set the selection list for output from simulate() or simulateEx()

 Example: setSelectionList ("Time, S1, J1, J2")

 \param list A string of Ids separated by spaces or comma characters
 \return Returns true if sucessful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv setSelectionList(const char* list);

/*!
 \brief Get the current selection list for simulate() or simulateEx()

 \return A list of symbol Ids indicating the current selection list
 \ingroup simulation
*/
C_DECL_SPEC RRStringArrayHandle rrCallConv getSelectionList();

/*!
 \brief Carry out a time-course simulation, use setTimeStart etc to set
 characteristics

 \return Returns an array containing the results of the simulation
 \ingroup simulation
*/
C_DECL_SPEC RRResultHandle rrCallConv simulate();

/*!
 \brief Carry out a time-course simulation based on the given arguments

 Example: m = simulateEx (0, 25, 200);

 \return Returns an array containing the results of the simulation
 \ingroup simulation
*/
C_DECL_SPEC RRResultHandle rrCallConv simulateEx(const double& timeStart, const double& timeEnd, const int& numberOfPoints);

/*!
 \brief Carry out a one step integration of the model

 Example: status = OneStep (&currentTime, &timeStep, &newTimeStep);

 \param[in] currentTime The current time in the simulation
 \param[in] stepSize The step size to use in the integration
 \param[in] value The new time (currentTime + stepSize)
 \return Returns true if successful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv oneStep(const double& currentTime, const double& stepSize, double& value);

/*!
 \brief Get the value of the current time start

 Example: status = getTimeStart (&timeStart);

 \param[out] timeStart The current value for the time start
 \return Returns true if successful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv getTimeStart(double& timeStart);

/*!
 \brief Get the value of the current time end

 Example: status = getTimeEnd (&timeEnd);

 \param timeEnd The current value for the time end
 \return Returns true if successful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv getTimeEnd(double& timeEnd);

/*!
 \brief Get the value of the current number of points

 Example: status = getNumPoints (&numberOfPoints);

 \param numPoints The current value for the number of points
 \return Returns true if successful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv getNumPoints (int& numPoints);


/*!
 \brief Compute the steady state of the current model

 Example: status = steadyState (&closenessToSteadyState);

 \param value This value is set during the call and indicates how close the solution is to the steady state. The smaller the value the better.
 \return Returns true if successful
 \ingroup steadystate
*/
C_DECL_SPEC bool rrCallConv steadyState(double& value);

/*!
 \brief A convenient method for returning a vector of the steady state species concentrations

 Example: RRVectorHandle values = computeSteadyStateValues ();

 \return Returns the vector of steady state values or null if an error occured
 \ingroup steadystate
*/
C_DECL_SPEC RRVectorHandle rrCallConv computeSteadyStateValues();

/*!
 \brief Set the selection list of the steady state analysis

 \param[in] list The string argument should be a space separated list of symbols in the selection list
 \return Returns true if successful
 \ingroup steadystate
*/
C_DECL_SPEC bool rrCallConv setSteadyStateSelectionList(const char* list);


/*!
 \brief Get the selection list for the steady state analysis

 \return Returns null if it fails, otherwise it returns a list of strings representing symbols in the selection list
 \ingroup steadystate
*/
C_DECL_SPEC cRRListHandle rrCallConv getSteadyStateSelectionList();


/*!
 \brief Get the value for a given symbol, use getAvailableSymbols() for a list of symbols

 Example: status = getValue ("S1", &value)

 \param symbolId The symbol that we wish to obtain the value for
 \param value The value that will be retrievd
 \return Returns true if succesful
 \ingroup state
*/
C_DECL_SPEC bool rrCallConv getValue(const char* symbolId, double& value);


/*!
 \brief Set the value for a given symbol, use getAvailableSymbols() for a list of symbols

 Example: status = setValue ("S1", 0.5)

 \param symbolId The symbol that we wish to set the value
 \param value The value that will be set to the symbol
 \return Returns true if succesful
 \ingroup state
*/
C_DECL_SPEC bool rrCallConv setValue(const char* symbolId, const double& value);


/*!
 \brief Retrieve in a vector the concentrations for all the floating species

 Example: RRVectorHandle values = getFloatingSpeciesConcentrations ();

 \return Returns the vector of flaoting species concentrations or null if an error occured
 \ingroup floating
*/
C_DECL_SPEC RRVectorHandle rrCallConv getFloatingSpeciesConcentrations();


/*!
 \brief Retrieve in a vector the concentrations for all the boundary species

 Example: RRVectorHandle values = getBoundarySpeciesConcentrations ();

 \return Returns the vector of boundary species concentrations or null if an error occured
 \ingroup boundary
*/
C_DECL_SPEC RRVectorHandle rrCallConv getBoundarySpeciesConcentrations();

// --------------------------------------------------------------------------------
// Get and Set Routines
// --------------------------------------------------------------------------------

/*!
 \brief Retrieve in a vector the values for all the lgobal parameter values

 Example: RRVectorHandle values = getGlobalParameterValues ();

 \return Returns the vector of global parameter values or null if an error occured
 \ingroup parameters
*/
C_DECL_SPEC RRVectorHandle rrCallConv getGlobalParameterValues();

/*!
 \brief Set the concentration for a particular boundary species. 

 \param index The index to the boundary species (corresponds to position in getBoundarySpeciesIds())
 \param value The concentration of the species to set
 \return Returns true if successful
 \ingroup boundary
*/
C_DECL_SPEC bool rrCallConv setBoundarySpeciesByIndex(const int& index, const double& value);

/*!
 \brief Set the concentration for a particular floating species. 

 \param index The index to the floating species (corresponds to position in getFloatingSpeciesIds())
 \param value The concentration of the species to set
 \return Returns true if successful
 \ingroup floating
*/
C_DECL_SPEC bool rrCallConv setFloatingSpeciesByIndex(const int& index, const double& value);

/*!
 \brief Set the value for a particular global parameter

 \param index The index to the global parameter
 \param value The value of the parameter to set
 \return Returns true if successful
 \ingroup parameters
*/
C_DECL_SPEC bool rrCallConv setGlobalParameterByIndex(const int& index, const double& value);


/*!
 \brief Retrieve the concentration for a particular floating species. 

 \param index The index to the boundary species (corresponds to position in getBoundarySpeciesIds())
 \param value The value returned by the method
 \return Returns true if successful
 \ingroup boundary
*/
C_DECL_SPEC bool rrCallConv getBoundarySpeciesByIndex(const int& index, double& value);

/*!
 \brief Retrieve the concentration for a particular floating species. 

 \param index The index to the floating species (corresponds to position in getFloatingSpeciesIds())
 \param value The value returned by the method
 \return Returns true if successful
 \ingroup floating
*/
C_DECL_SPEC bool rrCallConv getFloatingSpeciesByIndex(const int& index, double& value);

/*!
 \brief Retrieve the global parameter value 
 \param index The index to the global parameter (corresponds to position in getGlboalParametersIds())
 \param value The value returned by the method
 \return Returns true if successful
 \ingroup floating
*/
C_DECL_SPEC bool rrCallConv getGlobalParameterByIndex(const int& index, double& value);

/*!
 \brief Retrieve the compartment volume for a particular compartment. 

 \param index The index to the compartment (corresponds to position in getCompartmentIds())
 \param value The value returned by the method
 \return Returns true if successful
 \ingroup compartment
*/
C_DECL_SPEC bool rrCallConv getCompartmentByIndex (const int& index, double& value);


/*!
 \brief Set the volume for a particular compartment

 \param index The index to the compartment (corresponds to position in getCompartmentIds())
 \param value The volume of the compartment to set
 \return Returns true if successful
 \ingroup compartment

*/
C_DECL_SPEC bool rrCallConv setCompartmentByIndex (const int& index, const double& value);

/*!
 \brief Retrieve the full Jacobian for the current model

 \return Returns null if it fails, otherwise returns the full Jacobian matrix 
 \ingroup Stoich
*/
C_DECL_SPEC RRMatrixHandle rrCallConv getFullJacobian();

/*!
 \brief Retreive the reduced Jacobian for the current model

 \return Returns null if it fails, otherwise returns the reduced Jacobian matrix 
 \ingroup Stoich
*/
C_DECL_SPEC RRMatrixHandle rrCallConv getReducedJacobian();

/*!
 \brief Retreive the eigenvalue matrix for the current model

 \return Returns null if it fails, otherwise returns a matrix of eigenvalues.
 The first column will contain the real values and the second column the imaginary values
 \ingroup Stoich
*/
C_DECL_SPEC RRMatrixHandle rrCallConv getEigenValues();

// Stoichiometry methods
/*!
 \brief Retreive the stoichiometry matrix for the current model

 \return Returns null if it fails, otherwise returns the stoichiometry matrix.
 \ingroup Stoich
*/
C_DECL_SPEC RRMatrixHandle rrCallConv getStoichiometryMatrix();

/*!
 \brief Retreive the Link matrix for the current model

 \return Returns null if it fails, otherwise returns the Link matrix.
 \ingroup Stoich
*/
C_DECL_SPEC RRMatrixHandle rrCallConv getLinkMatrix();

/*!
 \brief Retrieve the reduced stoichiometry matrix for the current model

 \return Returns null if it fails, otherwise returns reduced stoichiometry matrix
 \ingroup Stoich
*/
C_DECL_SPEC RRMatrixHandle rrCallConv getNrMatrix();

/*!
 \brief Retrieve the L9 matrix for the current model

 \return Returns null if it fails, otherwise returns the L0 matrix.
 \ingroup Stoich
*/
C_DECL_SPEC RRMatrixHandle rrCallConv getL0Matrix();

/*!
 \brief Retrieve the conservation matrix for the current model

 \return Returns null if it fails, otherwise returns the conservation matrix.
 \ingroup Stoich
*/
C_DECL_SPEC RRMatrixHandle rrCallConv getConservationMatrix();


// Initial condition Methods
/*!
 \brief Reset all floating species concentrations to their intial conditions

 Example: status = reset ();

 \return Returns true if successful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv reset();

/*!
 \brief Set the initial floating species concentrations

 Example: status = setFloatingSpeciesInitialConcentrations (vec);

 \param vec A vector of species concentrations: order given by getFloatingSpeciesIds()
 \return Returns true if successful
 \ingroup initialConditions
*/
C_DECL_SPEC bool rrCallConv setFloatingSpeciesInitialConcentrations (const RRVector* vec);

/*!
 \brief Get the initial floating species concentrations

 Example: vec = getFloatingSpeciesInitialConcentrations ();

 \return Returns null if it fails otherwise returns a vector containing the initial conditions
 \ingroup initialConditions
*/
C_DECL_SPEC RRVectorHandle rrCallConv getFloatingSpeciesInitialConcentrations ();

/*!
 \brief Get the initial floating species Ids

 Example: vec = getFloatingSpeciesInitialConditionIds ();

 \return Returns null if it fails otherwise returns a vector containing names of the floating species
 \ingroup initialConditions
*/
C_DECL_SPEC RRStringArrayHandle rrCallConv getFloatingSpeciesInitialConditionIds();


// Reaction rates
// Initial condition Methods
/*!
 \brief Obtain the number of reactions in the loaded model

 Example: number = getNumberOfReactions ();

 \return Returns -1 if it fails, if succesful it return 0 or more, indicating the number of reactions
 \ingroup state
*/
C_DECL_SPEC int rrCallConv getNumberOfReactions();


/*!
 \brief Retrieve a give reaction as indicated by the index parameter

 \param index - The index is used to specif which reaction rate to retrieve
 \param rate - The reaction rate is returned in the rate argument
 \return Returns false if it fails
 \ingroup reaction
*/
C_DECL_SPEC bool rrCallConv getReactionRate(const int& index, double& rate);


/*!
 \brief Retrieve a vector of current reaction rates

 \return Returns null if it fails, otherwise it returns a vector of reaction rates
 \ingroup reaction
*/
C_DECL_SPEC RRVectorHandle rrCallConv getReactionRates();


/*!
 \brief Retrieve a vector of reactionb rates given a vector of species concentrations

 \param vec The vector of floating species concentrations
 \return Returns null if it fails otherwise it returns a vector of reaction rates
 \ingroup reaction
*/
C_DECL_SPEC RRVectorHandle rrCallConv getReactionRatesEx (const RRVectorHandle vec);


/*!
 \brief Retrieve the vector of rates of change

 Example: values = getRatesOfChange ();

 \return Returns null if it fails, otherwise returns a vector of rates of change values
 \ingroup rateOfChange
*/
C_DECL_SPEC RRVectorHandle rrCallConv getRatesOfChange();

/*!
 \brief Retrieve the string list of rates of change Ids

 Example: Ids = getRatesOfChangeIds ();

 \return Returns null if it fails, otherwise returns a list of rates of change Ids
 \ingroup rateOfChange
*/
C_DECL_SPEC RRStringArrayHandle rrCallConv getRatesOfChangeIds();


/*!
 \brief Retrieve the rate of change for a given floating species

 Example: status = getRateOfChange (&index, &value);

 \return Returns false if it fails, otherwise value contains the rate of change.
 \ingroup rateOfChange
*/
C_DECL_SPEC bool rrCallConv getRateOfChange(const int&, double& value);


/*!
 \brief Retrieve the vector of rates of change given a vector of floating species concentrations

 Example: values = getRatesOfChangeEx (vector);

 \return Returns null if it fails
 \ingroup rateOfChange
*/
C_DECL_SPEC RRVectorHandle rrCallConv getRatesOfChangeEx (const RRVectorHandle vec);

/*!
 \brief Evaluate the current models, that it update all assignments and rates of change

 \return Returns false if it fails
 \ingroup state
*/
C_DECL_SPEC bool rrCallConv evalModel();


// Get number family
/*!
 \brief Returns the number of compartments in the model
 \ingroup compartment
*/
C_DECL_SPEC int rrCallConv getNumberOfCompartments ();


/*!
 \brief Returns the number of boundary species in the model
 \ingroup boundary
*/
C_DECL_SPEC int rrCallConv getNumberOfBoundarySpecies();


/*!
 \brief Returns the number of floating species in the model
 \ingroup floating
*/
C_DECL_SPEC int rrCallConv getNumberOfFloatingSpecies();


/*!
 \brief Returns the number of global parameters in the model
 \ingroup parameters
*/
C_DECL_SPEC int rrCallConv getNumberOfGlobalParameters();


/*!
 \brief Returns the number of dependent species in the model
 \ingroup floating
*/
C_DECL_SPEC int rrCallConv getNumberOfDependentSpecies();


// Get number family
/*!
 \brief Returns the number of independent species in the model
 \ingroup floating
*/
C_DECL_SPEC int rrCallConv getNumberOfIndependentSpecies();

// Get Ids family

/*!
 \brief Obtain the list of reaction Ids

 \return Returns null if it fails, if succesful it returns a pointer to a RRStingListHandle struct
 \ingroup compartment
*/
C_DECL_SPEC RRStringArrayHandle rrCallConv getReactionIds();


/*!
 \brief Obtain the list of rate of change Ids

 \return Returns null if it fails, if succesful it returns a pointer to a RRStingListHandle struct
 \ingroup compartment
*/
C_DECL_SPEC RRStringArrayHandle rrCallConv getRateOfChangeIds();

/*!
 \brief Obtain the list of boundary species Ids

 \return Returns null if it fails, if succesful it returns a pointer to a RRStingListHandle struct
 \ingroup boundary
*/
C_DECL_SPEC RRStringArrayHandle rrCallConv getBoundarySpeciesIds();

/*!
 \brief Obtain the list of floating species Id

 \return Returns null if it fails, if succesful it returns a pointer to a RRStingListHandle struct
 \ingroup floating
*/
C_DECL_SPEC RRStringArrayHandle rrCallConv getFloatingSpeciesIds();

/*!
 \brief Obtain the list of global parameter Ids

 \return Returns null if it fails, if succesful it returns a pointer to a RRStingListHandle struct
 \ingroup parameters
*/
C_DECL_SPEC RRStringArrayHandle rrCallConv getGlobalParameterIds();

/*!
 \brief Obtain the list of compartment Ids

 Example: str = getCompartmentIds ();

 \return Returns -1 if it fails, if succesful it returns a pointer to a RRStingListHandle struct
 \ingroup compartment
*/
C_DECL_SPEC RRStringArrayHandle rrCallConv getCompartmentIds();

/*!
 \brief Obtain the list of eigenvalue Ids

 \return Returns -1 if it fails, if succesful it returns a pointer to a RRStingListHandle struct
 \ingroup state
*/
C_DECL_SPEC RRStringArrayHandle rrCallConv getEigenValueIds();

/*!
 \brief Obtain the list of al avialable symbols

 \return Returns -1 if it fails, if succesful it returns a pointer to a RRArrayListHandle struct
 \ingroup state
*/
C_DECL_SPEC cRRListHandle rrCallConv getAvailableSymbols();

// MCA methods
/*!
 \brief Obtain the list of elasticity coefficient Ids

 \return Returns null if it fails, if succesful it returns a pointer to a RRStingListHandle struct
 \ingroup mca 
*/
C_DECL_SPEC RRStringArrayHandle rrCallConv getElasticityCoefficientIds();

/*!
 \brief Obtain the list of unscaled flux control coefficient Ids

 \return Returns null if it fails, if succesful it returns a pointer to a RRStingListHandle struct
 \ingroup mca
*/
C_DECL_SPEC RRStringArrayHandle rrCallConv getUnscaledFluxControlCoefficientIds();

/*!
 \brief Obtain the list of flux control coefficient Ids

 \return Returns null if it fails, if succesful it returns a pointer to a RRStingListHandle struct
 \ingroup mca 
*/
C_DECL_SPEC RRStringArrayHandle rrCallConv getFluxControlCoefficientIds();

/*!
 \brief Obtain the list of unscaled concentration control coefficient Ids

 \return Returns null if it fails, if succesful it returns a pointer to a RRStingListHandle struct
 \ingroup mca 
*/
C_DECL_SPEC RRStringArrayHandle rrCallConv getUnscaledConcentrationControlCoefficientIds();

/*!
 \brief Obtain the list of concentration coefficient Ids

 \return Returns null if it fails, if succesful it returns a pointer to a RRStingListHandle struct
 \ingroup mca 
*/
C_DECL_SPEC RRStringArrayHandle rrCallConv getConcentrationControlCoefficientIds();

/*!
 \brief Retrieve the unscaled elasticity matrix for the current model

 \return Returns nil if it fails, otherwise returns a matrix of unscaled elasticities
 The first column will contain the real values and the second column the imaginary values
 \ingroup mca
*/
C_DECL_SPEC RRMatrixHandle rrCallConv getUnScaledElasticityMatrix();

/*!
 \brief Retrieve the scaled elasticity matrix for the current model

 \return Returns null if it fails, otherwise returns a matrix of scaled elasticities.
 TO DO: The first column will contain the real values and the second column the imaginary values
 \ingroup mca
*/
C_DECL_SPEC RRMatrixHandle rrCallConv getScaledElasticityMatrix();


/*!
 \brief Retrieve the scaled elasticity matrix for the current model

 \param reactionId The reaction Id for computing the elasticity
 \param speciesId The floating species to compute the elasticity for
 \param value The return value for the elasticity
 \return Returns null if it fails, otherwise returns a particular scaled elasticity
 \ingroup mca
*/
C_DECL_SPEC bool rrCallConv getScaledFloatingSpeciesElasticity(const char* reactionId, const char* speciesId, double& value);

/*!
 \brief Retrieve the matrix of unscaled concentration control coefficients for the current model

 \return Returns null if it fails, otherwise returns a matrix of unscaled concentration control coefficients 
 The first column will contain the real values and the second column the imaginary values
 \ingroup mca
*/
C_DECL_SPEC RRMatrixHandle rrCallConv getUnscaledConcentrationControlCoefficientMatrix();


/*!
 \brief Retrieve the matrix of scaled concentration control coefficients for the current model

 \return Returns null if it fails, otherwise returns a matrix of scaled concentration control coefficients 
 The first column will contain the real values and the second column the imaginary values
 \ingroup mca
*/
C_DECL_SPEC RRMatrixHandle rrCallConv getScaledConcentrationControlCoefficientMatrix();

/*!
 \brief Retrieve the matrix of unscaled flux control coefficients for the current model

 \return Returns null if it fails, otherwise returns a matrix of unscaled flux control coefficients 
 The first column will contain the real values and the second column the imaginary values
 \ingroup mca
*/
C_DECL_SPEC RRMatrixHandle rrCallConv getUnscaledFluxControlCoefficientMatrix();

/*!
 \brief Retrieve the matrix of scaled flux control coefficients for the current model

 \return Returns null if it fails, otherwise returns a matrix of scaled flux control coefficients 
 The first column will contain the real values and the second column the imaginary values
 \ingroup mca
*/
C_DECL_SPEC RRMatrixHandle rrCallConv getScaledFluxControlCoefficientMatrix();

/*!
 \brief Retireve a single unscaled control coefficient

 \param[in] variable This is the dependent variable of the coefficient, for example a flux or species concentration
 \param[in] parameter This is the independent parameter, for example a kinetic constant or boundary species
 \param[out] value This is the value of the unscaled control coefficeint returns to the caller
 \return Returns true if successful
 \ingroup mca
*/
C_DECL_SPEC bool rrCallConv getuCC (const char* variable, const char* parameter, double& value);

/*!
 \brief Retireve a single control coefficient

 \param[in] variable This is the dependent variable of the coefficient, for example a flux or species concentration
 \param[in] parameter This is the independent parameter, for example a kinetic constant or boundary species
 \param[out] value This is the value of the control coefficeint returns to the caller
 \return Returns true if successful
 \ingroup mca
*/
C_DECL_SPEC bool  rrCallConv getCC (const char* variable, const char* parameter, double& value);

/*!
 \brief Retireve a single elasticity coefficient

 \param[in] name This is the reaction variable for the elasticity
 \param[in] species This is the independent parameter, for example a floating of boundary species
 \param[out] value This is the value of the elasticity coefficient returns to the caller
 \return Returns true if successful
 \ingroup mca
*/
C_DECL_SPEC bool rrCallConv getEE(const char* name, const char* species, double& value);

/*!
 \brief Retireve a single unscaled elasticity coefficient

 \param[in] name This is the reaction variable for the unscaled elasticity
 \param[in] species This is the independent parameter, for example a floating of boundary species
 \param[out] value This is the value of the unscaled elasticity coefficient returns to the caller
 \return Returns true if successful
 \ingroup mca
*/
C_DECL_SPEC bool rrCallConv getuEE(const char* name, const char* species, double& value);

// What's this, not sure if we need it?
C_DECL_SPEC bool rrCallConv getScaledFloatingSpeciesElasticity(const char* reactionName, const char* speciesName, double& value);



// NOM lib forwarded functions
/*!
 \brief Returns the number of rules, in the current model
 \return Returns an integer larger or equal to 0 if succesful, or -1 on failure
 \ingroup NOM functions
*/
C_DECL_SPEC int rrCallConv getNumberOfRules();

// Print/format functions
/*!
 \brief Returns a result struct in string form.
 \return Returns result struct as a character string
 \ingroup print
*/
C_DECL_SPEC char* rrCallConv resultToString(const RRResultHandle result);

/*!
 \brief Returns a matrix in string form.
 \return Returns matrix as a character string
 \ingroup toString
*/
C_DECL_SPEC char* rrCallConv matrixToString (const RRMatrixHandle mat);

/*!
 \brief Returns a vector in string form.
 \return Returns vector as a character string
 \ingroup toString
*/
C_DECL_SPEC char* rrCallConv vectorToString(const RRVectorHandle vec);

/*!
 \brief Returns a string list in string form.
 \return Returns string list as a character string
 \ingroup toString
*/
C_DECL_SPEC char* rrCallConv stringArrayToString(const RRStringArrayHandle list);

/*!
 \brief Returns a string array in string form.
 \return Returns string array as a character string
 \ingroup toString
*/
//C_DECL_SPEC char* rrCallConv printStringArrayList(const RRStringArrayList* list);

/*!
 \brief Returns a list in string form.
 \return Returns list as a character string
 \ingroup toString
*/
C_DECL_SPEC char* rrCallConv listToString(const cRRListHandle list);

// Free memory functions
/*!
 \brief Free the result struct returned by simulate() and simulateEx()
 \ingroup freeRoutines
*/
C_DECL_SPEC bool rrCallConv freeResult(RRResultHandle handle);

/*!
 \brief Free char* generated by library routines
 \ingroup freeRoutines
*/
C_DECL_SPEC bool rrCallConv freeText(char* text);


/*!
 \brief Free RRStringListHandle structures
 \ingroup freeRoutines
*/
//C_DECL_SPEC bool rrCallConv freeStringList(RRStringListHandle sl);

/*!
 \brief Free RRStringArrayHandle structures
 \ingroup freeRoutines
*/
C_DECL_SPEC bool rrCallConv freeStringArray(RRStringArrayHandle sl);

/*!
 \brief Free RRListHandle structures
 \ingroup freeRoutines
*/
//C_DECL_SPEC bool rrCallConv freeList(cRRListHandle theList);

/*!
 \brief Free RRVectorHandle structures
 \ingroup freeRoutines
*/
C_DECL_SPEC bool rrCallConv freeVector(RRVectorHandle vector);

/*!
 \brief Free RRMatrixHandle structures
 \ingroup freeRoutines
*/
C_DECL_SPEC bool rrCallConv freeMatrix(RRMatrixHandle matrix);

/*!
 \brief Free RRCCodeHandle structures
 \ingroup freeRoutines
*/
C_DECL_SPEC bool rrCallConv freeCCode(RRCCodeHandle code);

/*!
 \brief Pause
 \return void
*/
C_DECL_SPEC void rrCallConv Pause();


// Helper Methods
/*!
 \brief Get the number of elements in a vector type

 Example: count = getVectorLength (myVector);

 \param vector A pointer to the vector variable type
 \return Returns -1 if it fails, otherwise returns the number of elements in the vector
 \ingroup helperRoutines
*/
C_DECL_SPEC int rrCallConv getVectorLength (RRVectorHandle vector);

/*!
 \brief Create a new vector with a given size

 Example: myVector = createVector (10);

 \param size The number of element in the new vector
 \return Returns null if it fails, otherwise returns a pointer to the new vector
 \ingroup helperRoutines
*/
C_DECL_SPEC RRVectorHandle rrCallConv createVector (int size);

/*!
 \brief Get a particular element from a vector

 Example: status = getVectorElement (myVector, 10, &value);

 \param vector A pointer to the vector variable type
 \param index An integer indicating the ith element to retrieve (indexing is from zero)
 \param value A pointer to the retrieved double value
 \return Returns true if succesful
 \ingroup helperRoutines
*/
C_DECL_SPEC bool rrCallConv getVectorElement (RRVectorHandle vector, int index, double& value);


/*!
 \brief Get a particular element from a vector

 Example: status = setVectorElement (myVector, 10, 3.1415);

 \param vector A pointer to the vector variable type
 \param index An integer indicating the ith element to set (indexing is from zero)
 \param value The value to store in the vector at the indexth position
 \return Returns true if succesful
 \ingroup helperRoutines
*/
C_DECL_SPEC bool rrCallConv setVectorElement (RRVectorHandle vector, int index, double value);

/*!
 \brief To be deprecated

 \param stringList
 \return Deprecated
*/
//C_DECL_SPEC int                     rrCallConv  getStringListLength (RRStringListHandle stringList);

/*!
 \brief To be deprecated

 \param stringList
 \param index
 \return Deprecated
*/
//C_DECL_SPEC char*                   rrCallConv  getStringListElement (RRStringListHandle stringList, int index);

/*!
 \brief Retrieve the number of rows in the given matrix

 Example: nRows = getMatrixNumRows (m);

 \param m A pointer to a matrix type variable
 \return Returns -1 if fails, otherwise returns the number of rows
 \ingroup helperRoutines
*/
C_DECL_SPEC int rrCallConv getMatrixNumRows (RRMatrixHandle m);

/*!
 \brief Retrieve the number of columns in the given matrix

 Example: nRows = getMatrixNumCols (m);

 \param m A pointer to a matrix type variable
 \return Returns -1 if fails, otherwise returns the number of columns
 \ingroup helperRoutines
*/
C_DECL_SPEC int rrCallConv getMatrixNumCols (RRMatrixHandle m);

/*!
 \brief Retrieves an element at a given row and column from a matrix type variable

 Example: status = getMatrixElement (m, 2, 4, &value);

 \param m A pointer to a matrix type variable
 \param r - The row index to the matrix
 \param c - The column index to the matrix
 \param value - The retrieved value from the matrix
 \return Returns True if succesful
 \ingroup helperRoutines
*/
C_DECL_SPEC bool rrCallConv getMatrixElement (RRMatrixHandle m, int r, int c, double& value);

/*!
 \brief Retrieve the number of rows in the given result data

 Example: nRows = getResultNumRows (result);

 \param result A pointer to a result type variable
 \return Returns -1 if fails, otherwise returns the number of rows
 \ingroup helperRoutines
*/
C_DECL_SPEC int rrCallConv getResultNumRows (RRResultHandle result);

/*!
 \brief Retrieve the number of columns in the given result data

 Example: nRows = getResultNumCols (result);

 \param result A pointer to a result type variable
 \return Returns -1 if fails, otherwise returns the number of columns
 \ingroup helperRoutines
*/
C_DECL_SPEC int rrCallConv getResultNumCols (RRResultHandle result);

/*!
 \brief Retrieves an element at a given row and column from a result type variable

 Example: status = getResultElement (result, 2, 4, &value);

 \param result A pointer to a result type variable
 \param r -The row index to the result data
 \param c - The column index to the result data
 \param value - The retrieved value from the result data
 \return Returns true if succesful
 \ingroup helperRoutines
*/
C_DECL_SPEC bool rrCallConv getResultElement (RRResultHandle result, int r, int c, double& value);

/*!
 \brief Retrieves an element at a given row and column from a result type variable

 Example: str = getResultColumnLabel (result, 2, 4);

 \param result A pointer to a result type variable
 \param column - The column index for the result data (indexing from zero)
 \return Returns null if fails, otherwise returns a pointer to the string column label
 \ingroup helperRoutines
*/
C_DECL_SPEC char* rrCallConv getResultColumnLabel (RRResultHandle result, int column);

/*!
 \brief Retrieve the header file for the current model (if applicable)

 Example: header = getCCodeHeader (code);

  \param code A pointer to a string that stores the header code
  \return Returns true if succesful
 \ingroup helperRoutines
*/
C_DECL_SPEC char* rrCallConv getCCodeHeader (RRCCodeHandle code);

/*!
 \brief Retrieve the main source file for the current model (if applicable)

 Example: source = getCCodeSource (code);

 \param code A pointer to a string that stores the main source code
 \return Returns true if succesful
 \ingroup helperRoutines
*/
C_DECL_SPEC char* rrCallConv getCCodeSource (RRCCodeHandle code);


/*!
 \brief Retrieve the name of model source file for the current model (if applicable)

 Example: fileName = getCSourceFileName();

 \return Returns null if fails, otherwise returns a pointer to a string containing the file name
 \ingroup helperRoutines
*/
C_DECL_SPEC char*             rrCallConv getCSourceFileName();

// List support routines
C_DECL_SPEC cRRListHandle 	  rrCallConv createRRList ();

C_DECL_SPEC void 			  rrCallConv freeRRList (cRRListHandle list);
C_DECL_SPEC cRRListItemHandle rrCallConv createIntegerItem (int value);
C_DECL_SPEC cRRListItemHandle rrCallConv createDoubleItem  (double value);
C_DECL_SPEC cRRListItemHandle rrCallConv createStringItem  (char* value);
C_DECL_SPEC cRRListItemHandle rrCallConv createListItem    (cRRList* value);

// Add item and return index of item
C_DECL_SPEC int rrCallConv addItem (cRRListHandle list, cRRListItemHandle *item);

// Returns the index^th item from the list
C_DECL_SPEC cRRListItemHandle rrCallConv getListItem (cRRListHandle list, int index);

C_DECL_SPEC bool rrCallConv isListItemInteger (cRRListItemHandle item);
C_DECL_SPEC bool rrCallConv isListItemDouble  (cRRListItemHandle item);
C_DECL_SPEC bool rrCallConv isListItemString  (cRRListItemHandle item);
C_DECL_SPEC bool rrCallConv isListItemList    (cRRListItemHandle item);

C_DECL_SPEC bool rrCallConv isListItem (cRRListItemHandle item, ListItemType itemType);

// Returns the length of the list
C_DECL_SPEC int rrCallConv getListLength (cRRListHandle myList);

// Returns NULL if item isn't a list, otherwise it returns a list from the item
C_DECL_SPEC cRRListHandle rrCallConv getList (cRRListItemHandle item);

#if defined( __cplusplus)
}
#endif

#endif
