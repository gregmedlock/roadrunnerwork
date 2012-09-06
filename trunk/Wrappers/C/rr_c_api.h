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
 * RoadRunner is a high performance and portable simulation engine 
 * for systems and synthetic biology. To run a simple SBML model 
 * and generate time series data we would call:
 *
 \code
 RRResultHandle output;

 loadSBMLFromFile ("mymodel.xml");

 output = simulate (0, 10, 100);
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
        freeRrInstance (rrInstance)

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

 \derfgroup steadystate
 \brief Compute and obtain basic information about the steady state

 \defgroup reaction Reaction group
 \brief Get information about reaction rates

 \defgroup rateOfChange Rates of change group
 \brief Get information about rates of change

 \defgroup boundary Boundary species group
 \brief Get information about boundary species

 \defgroup floating Floating species group
 \brief Get information about floating species

 \defgroup parameters Parameter group
 \brief set and get global and local parameters

 \defgroup compartment Compartment group
 \brief set and get information on compartments

 \defgroup simulation Time-course simulation
 \brief Deterministic, stochastic, and hybrid simulation algorithms

 \defgroup mca Metabolic Control Analysis
 \brief Calculate control coefficients and sensitivities

 \defgroup matrix Stoichiometry analysis
 \brief Linear algebra based methods for analyzing a reaction network

 \defgroup helperRoutines Helper Routines
 \brief Helper routines for acessing the various C API types, eg lists and arrays
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
 \return char* buildDate - Returns null if it fails, otherwise it returns the build date
 \ingroup utility
*/
C_DECL_SPEC char*  rrCallConv getBuildDate();

/*!
 \brief Retrieve the current copyright notice for the library
 \return char* copyRight - Returns null if it fails, otherwise it returns the copyright string
 \ingroup utility
*/
C_DECL_SPEC char*  rrCallConv getCopyright();

/*!
 \brief SEt the temporary folder

 When cRoadRunner is run in C generation mode its uses a temporary folder to store the 
 generate C source code. This method can be used to set the temporary folder path if necessary.

 \return bool status - Returns true if succesful
 \ingroup utility
*/
C_DECL_SPEC bool rrCallConv setTempFolder(const char* folder);

/*!
 \brief Retrieve the current temporary folder path

 When cRoadRunner is run in C generation mode its uses a temporary folder to store the 
 generate C source code. This method can be used to get the current value
 for the the temporary folder path.

 \return char* path - Returns null if it fails, otherwise it returns the path
 \ingroup utility
*/
C_DECL_SPEC char* rrCallConv getTempFolder();

/*!
 \brief Retrieve a pointer to the C code structure, RRCCode

 When cRoadRunner is run in C generation mode its uses a temporary folder to store the 
 generate C source code. This method can be used to obtain the header and main source
 code after a model has been loaded. 

 \return char* path - Returns null if it fails, otherwise it returns a pointer to the RRCode structure
 \ingroup utility
*/
C_DECL_SPEC RRCCode* rrCallConv getCCode();


// -----------------------------------------------------------------------
/** \} */
/**
  * @name Error handling and informational methods
  */
/** \{ */
// -----------------------------------------------------------------------

// Logging

/*!
 \brief Enable logging

 \return bool status - Ruturns true if succesful
 \ingroup errorfunctions
*/
C_DECL_SPEC bool rrCallConv enableLogging();

/*!
 \brief Set the logging status level

 The logging level is determined by the following strings
 
 "ANY", "DEBUG5", "DEBUG4", "DEBUG3", "DEBUG2", "DEBUG1",
 "DEBUG", "INFO", "WARNING", "ERROR"

 Example: setLogLevel ("DEBUG4")

 \param char* level - Pointer to the logging level string. 
 \return bool status - Ruturns true if succesful
 \ingroup errorfunctions
*/
C_DECL_SPEC bool rrCallConv setLogLevel(const char* lvl);

/*!
 \brief Get the logging status level as a pointer to a string

 The logging level can be one of the following strings
 
 "ANY", "DEBUG5", "DEBUG4", "DEBUG3", "DEBUG2", "DEBUG1",
 "DEBUG", "INFO", "WARNING", "ERROR"

 Example: str = getLogLevel ()

 \return char* level - Returns null is it fails else returns a pointer to the logging string
 \ingroup errorfunctions
*/
C_DECL_SPEC char* rrCallConv getLogLevel();

/*!
 \brief Get the logging status level as a pointer to a string

 The logging level can be one of the following strings
 
 "ANY", "DEBUG5", "DEBUG4", "DEBUG3", "DEBUG2", "DEBUG1",
 "DEBUG", "INFO", "WARNING", "ERROR"

 Example: str = getLogFileName ()

 \return char* level - Returns null is it fails else returns the full path to the logging file name
 \ingroup errorfunctions
*/
C_DECL_SPEC char* rrCallConv getLogFileName();

/*!
 \brief Check if there is an error string to retrieve

 Example: status = hasError ()

 \return bool status - Returns true if there is an error waiting to be retrieved
 \ingroup errorfunctions
*/
C_DECL_SPEC bool rrCallConv hasError();

/*!
 \brief Retrieve the currnt error string

 Example: str = getLastError ()

 \return char* errStrings - Return nukll if fails, otherwise returns a pointer to the error string
 \ingroup errorfunctions
*/
C_DECL_SPEC char* rrCallConv getLastError();


// -----------------------------------------------------------------------
/** \} */
/**
  * @name Library initialization and termination routines
  */
/** \{ */
// -----------------------------------------------------------------------

/*!
 \brief Initialize the roadRunner library and return an instance
 \return RRHandle instance Returns an instance of the library, returns null if it fails
 \ingroup initialization
*/
C_DECL_SPEC RRHandle rrCallConv getRRInstance();

/*!
 \brief Free the roadRunner instance 
 \param RRHandle instance Free the roadRunner instance given in the argument
 \ingroup initialization
*/
C_DECL_SPEC bool rrCallConv freeRRInstance(RRHandle handle);

// -----------------------------------------------------------------------
/** \} */
/**
  * @name Flags and options
  */
/** \{ */
// -----------------------------------------------------------------------

// Flags/Options
C_DECL_SPEC bool rrCallConv setComputeAndAssignConservationLaws(const bool& OnOrOff);

// -----------------------------------------------------------------------
/** \} */
/**
  * @name Read and Write models
  */
/** \{ */
// -----------------------------------------------------------------------

/*!
 \brief Create a model from an SBML string
 \param char* sbml string
 \return bool Returns true if sucessful
 \ingroup loadsave
*/
C_DECL_SPEC bool rrCallConv loadSBML(const char* sbml);

/*!
 \brief Create a model from a SBML file
 \param char* file name 
 \return bool Returns true if sucessful
 \ingroup loadsave
*/
C_DECL_SPEC bool rrCallConv loadSBMLFromFile(const char* sbml);

/*!
 \brief Retrive the current state of the model in the form of an SBML string
 \param char* file name 
 \return bool Returns null is the call fails, otherwise returns a pointer to the SBML string
 \ingroup loadsave
*/
C_DECL_SPEC char* rrCallConv writeSBML();      

/*!
 \brief Retrieve the last SBML model that was loaded
 \return char* Returns null is the call fails, otherwise returns a pointer to the SBML string
 \ingroup loadsave
*/
C_DECL_SPEC char* rrCallConv getSBML();


// -------------------------------------------------------------------------
// SBML utility methods
C_DECL_SPEC char* 				    rrCallConv   getParamPromotedSBML(const char* sArg);

// Get and set capability routines
C_DECL_SPEC bool                    rrCallConv   setCapabilities (const char* caps);
C_DECL_SPEC char*                   rrCallConv   getCapabilities();

/*!
 \brief Set the time start for a simulation
 \param const double& time start
 \return bool Returns True if sucessful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv setTimeStart(const double& timeStart);

/*!
 \brief Set the time end for a simulation
 \param const double& time start
 \return bool Returns True if sucessful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv setTimeEnd(const double& timeEnd);

/*!
 \brief Set the number of points to generate in a simulation
 \param const int& Number of points to generate
 \return bool Returns True if sucessful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv setNumPoints(const int& nrPoints);

/*!
 \brief Set the selection list for output from simulate() or simulateEx()

 Example: setSelectionList ("Time, S1, J1, J2")

 \param const char* A string of names separated by spaces or comma characters
 \return bool Returns True if sucessful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv setSelectionList(const char* list);

/*!
 \brief Get the current selection list for simulate() or simulateEx()

 \return char* A list of symbol names indicating the current selection list
 \ingroup simulation
*/
C_DECL_SPEC RRStringListHandle rrCallConv getSelectionList();

/*!
 \brief Carry out a time-course simulation, use setTimeStart etc to set
 characteristics

 \return RRResultHandle Returns an array containing the results of the simulation
 \ingroup simulation
*/
C_DECL_SPEC RRResultHandle rrCallConv simulate();

/*!
 \brief Carry out a time-course simulation based on the given arguments

 Example: m = simulateEx (0, 25, 200);

 \return RRResultHandle Returns an array containing the results of the simulation
 \ingroup simulation
*/
C_DECL_SPEC RRResultHandle rrCallConv simulateEx(const double& timeStart, const double& timeEnd, const int& numberOfPoints);

/*!
 \brief Carry out a one step integration of the model

 Example: status = OneStep (&currentTime, &timeStep, &newTimeStep);

 \param double* currentTime The current time in the simulation
 \param double* stepSize The step size to use in the integration
 \param double* The new time (currentTime + stepSize)
 \return Returns True if successful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv oneStep(const double& currentTime, const double& stepSize, double& value);

/*!
 \brief Get the value of the current time start

 Example: status = getTimeStart (&timeStart);

 \param double* timeStart The current value for the time start
 \return Returns True if successful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv getTimeStart(double& timeStart);

/*!
 \brief Get the value of the current time end

 Example: status = getTimeStart (&timeEnd);

 \param double* timeStart The current value for the time end
 \return Returns True if successful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv getTimeEnd(double& timeEnd);

/*!
 \brief Get the value of the current number of points

 Example: status = getNumPoints (&numberOfPoints);

 \param int* numPoints The current value for the number of points
 \return Returns True if successful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv getNumPoints (int& numPoints);


// -----------------------------------------------------------------------
/** \} */
/**
  * @name Steady State Methods
  */
/** \{ */
// -----------------------------------------------------------------------

/*!
 \brief Compute the steady state of the current model

 Example: status = steadyState (&closenessToSteadyState);

 \param double* value This value is set during the call and indicates how close the solution is to the steady state. The smaller the value the better.
 \return Returns True if successful
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

C_DECL_SPEC bool                    rrCallConv   setSteadyStateSelectionList(const char* list);

C_DECL_SPEC RRStringListHandle      rrCallConv   getSteadyStateSelectionList();

// -----------------------------------------------------------------------
/** \} */
/**
  * @name Get and set values from the model
  */
/** \{ */
// -----------------------------------------------------------------------

// Set and get family of methods
C_DECL_SPEC bool        			rrCallConv   getValue(const char* speciesID, double& value);
C_DECL_SPEC bool                    rrCallConv   setValue(const char* speciesId, const double& val);


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

 \param int* index - The index to the boundary species (corresponds to position in getBoundarySpeciesNames())
 \param double* value - The concentration of the species to set
 \return bool status - Returns true if successful
 \ingroup boundary
*/
C_DECL_SPEC bool rrCallConv setBoundarySpeciesByIndex(const int& index, const double& value);

/*!
 \brief Set the concentration for a particular floating species. 

 \param int* index - The index to the floating species (corresponds to position in getFloatingSpeciesNames())
 \param double* value - The concentration of the species to set
 \return bool status - Returns true if successful
 \ingroup floating
*/
C_DECL_SPEC bool rrCallConv setFloatingSpeciesByIndex(const int& index, const double& value);

/*!
 \brief Set the value for a particular global parameter

 \param int* index - The index to the global parameter
 \param double* value - The value of the parameter to set
 \return bool status - Returns true if successful
 \ingroup parameters
*/
C_DECL_SPEC bool rrCallConv setGlobalParameterByIndex(const int& index, const double& value);


/*!
 \brief Retrieve the concentration for a particular floating species. 

 \param int* index - The index to the boundary species (corresponds to position in getBoundarySpeciesNames())
 \param double* value - The value returned by the method
 \return bool status - Returns true if successful
 \ingroup boundary
*/
C_DECL_SPEC bool rrCallConv getBoundarySpeciesByIndex(const int& index, double& val);

/*!
 \brief Retrieve the concentration for a particular floating species. 

 \param int* index - The index to the floating species (corresponds to position in getFloatingSpeciesNames())
 \param double* value - The value returned by the method
 \return bool status - Returns true if successful
 \ingroup floating
*/
C_DECL_SPEC bool rrCallConv getFloatingSpeciesByIndex(const int& index, double& val);

/*!
 \brief Retrieve the global parameter value 
 \param int* index - The index to the global parameter (corresponds to position in getGlboalParametersNames())
 \param double* value - The value returned by the method
 \return bool status - Returns true if successful
 \ingroup floating
*/
C_DECL_SPEC bool rrCallConv getGlobalParameterByIndex(const int& index, double& val);

/*!
 \brief Retrieve the compartment volume for a particular compartment. 

 \param int* index - The index to the compartment (corresponds to position in getCompartmentNames())
 \param double* value - The value returned by the method
 \return bool status - Returns true if successful
 \ingroup compartment
*/
C_DECL_SPEC bool rrCallConv getCompartmentByIndex (const int& index, double& value);

/*!
 \brief Set the volume for a particular compartment

 \param int* index - The index to the compartment (corresponds to position in getCompartmentNames())
 \param double* value - The volume of the compartment to set
 \return bool status - Returns true if successful
 \ingroup parameters
*/
C_DECL_SPEC bool rrCallConv setCompartmentByIndex (const int& index, const double& value);

// -----------------------------------------------------------------------
/** \} */
/**
  * @name Retrieve matrix properties from the model
  */
/** \{ */
// -----------------------------------------------------------------------

// Jacobian matrix methods
C_DECL_SPEC RRMatrixHandle          rrCallConv   getFullJacobian();
C_DECL_SPEC RRMatrixHandle          rrCallConv   getReducedJacobian();
C_DECL_SPEC RRMatrixHandle          rrCallConv   getEigenValues();

// Stoichiometry methods
C_DECL_SPEC RRMatrixHandle          rrCallConv   getStoichiometryMatrix();
C_DECL_SPEC RRMatrixHandle          rrCallConv   getLinkMatrix();
C_DECL_SPEC RRMatrixHandle          rrCallConv   getNrMatrix();
C_DECL_SPEC RRMatrixHandle          rrCallConv   getL0Matrix();
C_DECL_SPEC RRMatrixHandle          rrCallConv   getConservationMatrix();


// -----------------------------------------------------------------------
/** \} */
/**
  * @name Initial condition methods
  */
/** \{ */
// -----------------------------------------------------------------------

// Initial condition Methods
/*!
 \brief Reset all floating species concentrations to their intial conditions

 Example: status = reset ();

 \return Returns True if successful
 \ingroup simulation
*/
C_DECL_SPEC bool rrCallConv   reset();
C_DECL_SPEC bool                    rrCallConv   setFloatingSpeciesInitialConcentrations (const RRVector* vec);
C_DECL_SPEC RRVectorHandle          rrCallConv   getFloatingSpeciesInitialConcentrations ();
C_DECL_SPEC RRStringListHandle      rrCallConv   getFloatingSpeciesInitialConditionNames();

// -----------------------------------------------------------------------
/** \} */
/**
  * @name Retrieve reaction rate information
  */
/** \{ */
// -----------------------------------------------------------------------

// Reaction rates
// Initial condition Methods
/*!
 \brief Obtain the number of reactions in the loaded model

 Example: number = getNumberOfReactions ();

 \return Returns -1 if it fails, if succesful it return 0 or more, indicating the number of reactions
 \ingroup state
*/
C_DECL_SPEC int  rrCallConv   getNumberOfReactions();


C_DECL_SPEC bool                  	rrCallConv   getReactionRate(const int&, double& rate);
C_DECL_SPEC RRVectorHandle          rrCallConv   getReactionRates();
C_DECL_SPEC RRVectorHandle          rrCallConv   getReactionRatesEx (const RRVectorHandle vec);

// -----------------------------------------------------------------------
/** \} */
/**
  * @name Retrieve rates of change information
  */
/** \{ */
// -----------------------------------------------------------------------

// Rates of change
C_DECL_SPEC RRVectorHandle          rrCallConv   getRatesOfChange();
C_DECL_SPEC RRStringListHandle      rrCallConv   getRatesOfChangeNames();
C_DECL_SPEC bool                    rrCallConv   getRateOfChange(const int&, double& value);
C_DECL_SPEC RRVectorHandle          rrCallConv   getRatesOfChangeEx (const RRVectorHandle vec);

C_DECL_SPEC bool                    rrCallConv   evalModel();

// -----------------------------------------------------------------------
/** \} */
/**
  * @name Retrieve various dimensions form the model
  */
/** \{ */
// -----------------------------------------------------------------------

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
C_C_DECL_SPEC int rrCallConv getNumberOfBoundarySpecies();


/*!
 \brief Returns the number of floating species in the model
 \ingroup floating
*/
C_C_DECL_SPEC int rrCallConv getNumberOfFloatingSpecies();


/*!
 \brief Returns the number of global parameters in the model
 \ingroup parameters
*/
C_C_DECL_SPEC int rrCallConv getNumberOfGlobalParameters();


/*!
 \brief Returns the number of dependent species in the model
 \ingroup floating
*/
C_C_DECL_SPEC int rrCallConv getNumberOfDependentSpecies();


// Get number family
/*!
 \brief Returns the number of independent species in the model
 \ingroup floating
*/
C_C_DECL_SPEC int rrCallConv getNumberOfIndependentSpecies();

// -----------------------------------------------------------------------
/** \} */
/**
  * @name Retrieve various names from the model
  */
/** \{ */
// -----------------------------------------------------------------------

// Get names family
C_DECL_SPEC RRStringListHandle      rrCallConv   getReactionNames();
C_DECL_SPEC RRStringListHandle      rrCallConv   getRateOfChangeNames();
C_DECL_SPEC RRStringListHandle      rrCallConv   getBoundarySpeciesNames();
C_DECL_SPEC RRStringListHandle      rrCallConv   getFloatingSpeciesNames();
C_DECL_SPEC RRStringListHandle      rrCallConv   getGlobalParameterNames();
C_DECL_SPEC RRStringListHandle      rrCallConv   getCompartmentNames();
C_DECL_SPEC RRStringListHandle      rrCallConv   getEigenValueNames();
C_DECL_SPEC RRArrayList2Handle      rrCallConv   getAvailableSymbols();

// -----------------------------------------------------------------------
/** \} */
/**
  * @name Retrieve metabolic control analysis values
  */
/** \{ */
// -----------------------------------------------------------------------

// MCA methods
C_DECL_SPEC RRStringArrayListHandle rrCallConv   getElasticityCoefficientNames();
C_DECL_SPEC RRStringArrayListHandle rrCallConv   getUnscaledFluxControlCoefficientNames();
C_DECL_SPEC RRStringArrayListHandle rrCallConv   getFluxControlCoefficientNames();
C_DECL_SPEC RRStringArrayListHandle rrCallConv   getUnscaledConcentrationControlCoefficientNames();
C_DECL_SPEC RRStringArrayListHandle rrCallConv   getConcentrationControlCoefficientNames();

C_DECL_SPEC RRMatrixHandle          rrCallConv   getUnScaledElasticityMatrix();
C_DECL_SPEC RRMatrixHandle          rrCallConv   getScaledElasticityMatrix();
C_DECL_SPEC RRMatrixHandle          rrCallConv   getUnscaledConcentrationControlCoefficientMatrix();
C_DECL_SPEC RRMatrixHandle          rrCallConv   getScaledConcentrationControlCoefficientMatrix();
C_DECL_SPEC RRMatrixHandle          rrCallConv   getUnscaledFluxControlCoefficientMatrix();
C_DECL_SPEC RRMatrixHandle          rrCallConv   getScaledFluxControlCoefficientMatrix();

C_DECL_SPEC bool                    rrCallConv   getuCC (const char* variable, const char* parameter, double& value);
C_DECL_SPEC bool                    rrCallConv   getCC (const char* variable, const char* parameter, double& value);
C_DECL_SPEC bool                    rrCallConv   getEE(const char* name, const char* species, double& value);
C_DECL_SPEC bool                    rrCallConv   getuEE(const char* name, const char* species, double& value);
C_DECL_SPEC bool                    rrCallConv   getScaledFloatingSpeciesElasticity(const char* reactionName, const char* speciesName, double& value);

// Print/format functions
C_DECL_SPEC char*                   rrCallConv   printResult(const RRResultHandle result);
C_DECL_SPEC char*                   rrCallConv   printMatrix(const RRMatrixHandle mat);
C_DECL_SPEC char*                   rrCallConv   printVector(const RRVectorHandle vec);
C_DECL_SPEC char*                   rrCallConv   printStringList(const RRStringListHandle list);
C_DECL_SPEC char*                   rrCallConv   printStringArrayList(const RRStringArrayList* list);
C_DECL_SPEC char*                   rrCallConv   printArrayList(const RRArrayList2Handle list);

// -----------------------------------------------------------------------
/** \} */
/**
  * @name Free resources methods
  */
/** \{ */
// -----------------------------------------------------------------------

// Free memory functions
C_DECL_SPEC bool                    rrCallConv   freeResult(RRResultHandle handle);
C_DECL_SPEC bool                    rrCallConv   freeText(char* text);
C_DECL_SPEC bool                    rrCallConv   freeLabelStringList(RRLabelStringListHandle sl);
C_DECL_SPEC bool                    rrCallConv   freeStringList(RRStringListHandle sl);
C_DECL_SPEC bool                    rrCallConv   freeStringArrayList(RRStringArrayListHandle sl);
C_DECL_SPEC bool 					rrCallConv 	 freeArrayList(RRArrayList2Handle theList);
C_DECL_SPEC bool                    rrCallConv   freeVector(RRVectorHandle vector);
C_DECL_SPEC bool                    rrCallConv   freeMatrix(RRMatrixHandle matrix);
C_DECL_SPEC bool                    rrCallConv   freeCCode(RRCCodeHandle code);
C_DECL_SPEC void                    rrCallConv   Pause();


// Helper Methods
/*!
 \brief Get the number of elements in a vector type

 Example: count = getVectorLength (myVector);

 \param RRVectorHandle vector A pointer to the vector variable type
 \return Returns -1 if it fails, otherwise returns the number of elements in the vector
 \ingroup helperRoutines
*/
C_DECL_SPEC int rrCallConv  getVectorLength (RRVectorHandle vector);

/*!
 \brief Create a new vector with a given size

 Example: myVector = createVectorAPI (10);

 \param int size The number of element in the new vector
 \return Returns null if it fails, otherwise returns a pointer to the new vector
 \ingroup helperRoutines
*/
C_DECL_SPEC RRVectorHandle rrCallConv  createVectorAPI (int size);

/*!
 \brief Get a particular element from a vector

 Example: status = getVectorElement (myVector, 10, &value);

 \param RRVectorHandle vector A pointer to the vector variable type
 \param int index An integer indicating the ith element to retrieve (indexing is from zero)
 \param double* value A pointer to the retrieved double value
 \return Returns True if succesful
 \ingroup helperRoutines
*/
C_DECL_SPEC bool rrCallConv  getVectorElement (RRVectorHandle vector, int index, double& value);


/*!
 \brief Get a particular element from a vector

 Example: status = setVectorElement (myVector, 10, 3.1415);

 \param RRVectorHandle vector - A pointer to the vector variable type
 \param int index - An integer indicating the ith element to set (indexing is from zero)
 \param double value - The value to store in the vector at the indexth position
 \return Returns True if succesful
 \ingroup helperRoutines
*/
C_DECL_SPEC bool rrCallConv  setVectorElement (RRVectorHandle vector, int index, double value);


C_DECL_SPEC int                     rrCallConv  getStringListLength (RRStringListHandle stringList);
C_DECL_SPEC char*                   rrCallConv  getStringListElement (RRStringListHandle stringList, int index);

/*!
 \brief Retrieve the number of rows in the given matrix

 Example: nRows = getMatrixNumRows (m);

 \param RRMatrixHandle m - A pointer to a matrix type variable
 \return Returns -1 if fails, otherwise returns the number of rows
 \ingroup helperRoutines
*/
C_DECL_SPEC int rrCallConv  getMatrixNumRows (RRMatrixHandle m);

/*!
 \brief Retrieve the number of columns in the given matrix

 Example: nRows = getMatrixNumCols (m);

 \param RRMatrixHandle m - A pointer to a matrix type variable
 \return Returns -1 if fails, otherwise returns the number of columns
 \ingroup helperRoutines
*/
C_DECL_SPEC int rrCallConv  getMatrixNumCols (RRMatrixHandle m);

/*!
 \brief Retrieves an element at a given row and column from a matrix type variable

 Example: status = getMatrixElement (m, 2, 4, &value);

 \param RRMatrixHandle m A pointer to a matrix type variable
 \param int r - The row index to the matrix
 \param int c - The column index to the matrix
 \param double* value - The retrieved value from the matrix
 \return Returns True if succesful
 \ingroup helperRoutines
*/
C_DECL_SPEC bool rrCallConv  getMatrixElement (RRMatrixHandle m, int r, int c, double& value);

/*!
 \brief Retrieve the number of rows in the given result data

 Example: nRows = getResultNumRows (result);

 \param RRResultHandle result - A pointer to a result type variable
 \return Returns -1 if fails, otherwise returns the number of rows
 \ingroup helperRoutines
*/
C_DECL_SPEC int rrCallConv  getResultNumRows (RRResultHandle result);

/*!
 \brief Retrieve the number of columns in the given result data

 Example: nRows = getResultNumCols (result);

 \param RRResultHandle result - A pointer to a result type variable
 \return Returns -1 if fails, otherwise returns the number of columns
 \ingroup helperRoutines
*/
C_DECL_SPEC int rrCallConv  getResultNumCols (RRResultHandle result);

/*!
 \brief Retrieves an element at a given row and column from a result type variable

 Example: status = getResultElement (result, 2, 4, &value);

 \param RRResultHandle result - A pointer to a result type variable
 \param int r -The row index to the result data
 \param int c - The column index to the result data
 \paramt double* value - The retrieved value from the result data
 \return - Returns True if succesful
 \ingroup helperRoutines
*/
C_DECL_SPEC bool rrCallConv  getResultElement (RRResultHandle result, int r, int c, double& value);

/*!
 \brief Retrieves an element at a given row and column from a result type variable

 Example: str = getResultColumnLabel (result, 2, 4);

 \param RRResultHandle result -  A pointer to a result type variable
 \param int r - The row index for the result data (indexing from zero)
 \param int c - The column index for the result data (indexing from zero)
 \return Returns null if fails, otherwise returns a pointer to the string column label
 \ingroup helperRoutines
*/
C_DECL_SPEC char* rrCallConv  getResultColumnLabel (RRResultHandle result, int column);

/*!
 \brief Retrieve the header file for the current model (if applicable)

 Example: header = getCCodeHeader (code);

  \param RRCCodeHandle code - A pointer to a string that stores the header code
  \return Returns True if succesful
 \ingroup helperRoutines
*/
C_DECL_SPEC char* rrCallConv  getCCodeHeader (RRCCodeHandle code);

/*!
 \brief Retrieve the main source file for the current model (if applicable)

 Example: source = getCCodeSource (code);

 \param RRCCodeHandle code - A pointer to a string that stores the main source code
 \return Returns True if succesful
 \ingroup helperRoutines
*/
C_DECL_SPEC char* rrCallConv  getCCodeSource (RRCCodeHandle code);

#if defined( __cplusplus)
}
#endif

#endif
