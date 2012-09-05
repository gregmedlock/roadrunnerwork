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
        RRResultHandle efm, output, params;

        if (nargs < 2)
        {
            m1 = model1();
        }
        else
        {
            printf("loading model file %s\n", argv[1]);
            loadSBMLFromFile(argv[1]);
        }


        output = simualte (0, 100, 1000);  // start, end, num. points
        printf("output.tab has %i rows and %i columns\n", output->RSize, output->RCols);
        printResult (output);
        freeREsult (output);

        return 0;
 }
 \endcode
 * \section install_sec Installation
 *
 * Installation documentation is provided in the main google code page.

 \defgroup loadsave Read and Write models
 \brief Read and write models to files or strings. Support for SBML formats.

 \defgroup state Current state of system
 \brief Compute derivatives, fluxes, and other values of the system at the current state

 \defgroup reaction Reaction group
 \brief Get information about reaction rates

 \defgroup rateOfChange Rates of change group
 \brief Get information about rates of change

 \defgroup boundary Boundary species group
 \brief Get information about reaction rates

 \defgroup floating Floating species group
 \brief Get information about reaction rates

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


//  The latest
C_DECL_SPEC char*                   rrCallConv  writeSBML();       //Current SBML

// Utility and informational methods
C_DECL_SPEC char*                   rrCallConv  getVersion();

// Logging
C_DECL_SPEC bool                    rrCallConv  enableLogging();
C_DECL_SPEC bool                    rrCallConv  setLogLevel(const int& lvl);
C_DECL_SPEC bool                    rrCallConv  setLogLevelFromString(const char* lvl);
C_DECL_SPEC bool                    rrCallConv  getLogLevel(int& lvl);
C_DECL_SPEC char*                   rrCallConv  getLogFileName();

C_DECL_SPEC char*                   rrCallConv  getBuildDate();
C_DECL_SPEC char*                   rrCallConv  getCopyright();
C_DECL_SPEC bool                    rrCallConv  setTempFolder(const char* folder);
C_DECL_SPEC char*                   rrCallConv  getTempFolder();

// Error handling
C_DECL_SPEC bool                    rrCallConv  hasError();
C_DECL_SPEC char*                   rrCallConv  getLastError();

// RoadRunner API
C_DECL_SPEC RRHandle                rrCallConv   getRRInstance();
C_DECL_SPEC RRCCode*               	rrCallConv   getCCode();

// Flags/Options
C_DECL_SPEC bool                    rrCallConv   setComputeAndAssignConservationLaws(const bool& OnOrOff);

// -----------------------------------------------------------------------
/** \} */
/**
  * @name Read models
  */
/** \{ */

/*!
 \brief Create a model from an SBML string
 \param char* sbml string
 \return bool Returns true if sucessful
 \ingroup loadsave
*/
C_DECL_SPEC bool  rrCallConv   loadSBML(const char* sbml);

/*!
 \brief Create a model from a SBML file
 \param char* file name 
 \return bool Returns true if sucessful
 \ingroup loadsave
*/
C_DECL_SPEC bool rrCallConv   loadSBMLFromFile(const char* sbml);

// SBML utility methods
C_DECL_SPEC char* 				    rrCallConv   getParamPromotedSBML(const char* sArg);
C_DECL_SPEC char* 				    rrCallConv   getSBML();

// Get and set capability routines
C_DECL_SPEC bool                    rrCallConv   setCapabilities (const char* caps);
C_DECL_SPEC char*                   rrCallConv   getCapabilities();

/*!
 \brief Set the time start for a simulation
 \param const double& time start
 \return bool Returns True if sucessful
 \ingroup simulation
*/
C_DECL_SPEC bool                    rrCallConv   setTimeStart(const double& timeStart);

/*!
 \brief Set the time end for a simulation
 \param const double& time start
 \return bool Returns True if sucessful
 \ingroup simulation
*/
C_DECL_SPEC bool                    rrCallConv   setTimeEnd(const double& timeEnd);

/*!
 \brief Set the number of points to generate in a simulation
 \param const int& Number of points to generate
 \return bool Returns True if sucessful
 \ingroup simulation
*/
C_DECL_SPEC bool                    rrCallConv   setNumPoints(const int& nrPoints);

/*!
 \brief Set the selection list for output from simulate() or simulateEx()

 Example: setSelectionList ("Time, S1, J1, J2")

 \param const char* A string of names separated by spaces or comma characters
 \return bool Returns True if sucessful
 \ingroup simulation
*/
C_DECL_SPEC bool                    rrCallConv   setSelectionList(const char* list);

/*!
 \brief Get the current selection list for simulate() or simulateEx()

 \return char* A list of symbol names indicating the current selection list
 \ingroup simulation
*/
C_DECL_SPEC RRStringListHandle      rrCallConv   getSelectionList();

/*!
 \brief Carry out a time-course simulation, use setTimeStart etc to set
 characteristics

 \return RRResultHandle Returns an array containing the results of the simulation
 \ingroup simulation
*/
C_DECL_SPEC RRResultHandle          rrCallConv   simulate();
C_DECL_SPEC RRResultHandle          rrCallConv   simulateEx(const double& timeStart, const double& timeEnd, const int& numberOfPoints);
C_DECL_SPEC bool                  	rrCallConv   oneStep(const double& currentTime, const double& stepSize, double& value);
C_DECL_SPEC bool                    rrCallConv   getTimeStart(double& timeStart);
C_DECL_SPEC bool                    rrCallConv   getTimeEnd(double& timeEnd);
C_DECL_SPEC bool                    rrCallConv   getNumPoints (int& numPoints);

// Steady state methods
C_DECL_SPEC bool                    rrCallConv   steadyState(double& value);
C_DECL_SPEC RRVectorHandle          rrCallConv   computeSteadyStateValues();
C_DECL_SPEC bool                    rrCallConv   setSteadyStateSelectionList(const char* list);
C_DECL_SPEC RRStringListHandle      rrCallConv   getSteadyStateSelectionList();

// Set and get family of methods
C_DECL_SPEC bool        			rrCallConv   getValue(const char* speciesID, double& value);
C_DECL_SPEC bool                    rrCallConv   setValue(const char* speciesId, const double& val);
C_DECL_SPEC RRVectorHandle          rrCallConv   getFloatingSpeciesConcentrations();
C_DECL_SPEC RRVectorHandle          rrCallConv   getBoundarySpeciesConcentrations();
C_DECL_SPEC RRVectorHandle          rrCallConv   getGlobalParameterValues();

C_DECL_SPEC bool                    rrCallConv   setBoundarySpeciesByIndex(const int& index, const double& value);
C_DECL_SPEC bool                    rrCallConv   setFloatingSpeciesByIndex(const int& index, const double& value);
C_DECL_SPEC bool                    rrCallConv   setGlobalParameterByIndex(const int& index, const double& value);
C_DECL_SPEC bool                  	rrCallConv   getBoundarySpeciesByIndex(const int& index, double& val);
C_DECL_SPEC bool                    rrCallConv   getFloatingSpeciesByIndex(const int& index, double& val);
C_DECL_SPEC bool                  	rrCallConv   getGlobalParameterByIndex(const int& index, double& val);
C_DECL_SPEC bool                    rrCallConv   getCompartmentByIndex (const int& index, double& value);
C_DECL_SPEC bool                    rrCallConv   setCompartmentByIndex (const int& index, const double& value);

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

// Initial condition Methods
C_DECL_SPEC bool                    rrCallConv   reset();
C_DECL_SPEC bool                    rrCallConv   setFloatingSpeciesInitialConcentrations (const RRVector* vec);
C_DECL_SPEC RRVectorHandle          rrCallConv   getFloatingSpeciesInitialConcentrations ();
C_DECL_SPEC RRStringListHandle      rrCallConv   getFloatingSpeciesInitialConditionNames();

// Reaction rates
C_DECL_SPEC int                     rrCallConv   getNumberOfReactions();
C_DECL_SPEC bool                  	rrCallConv   getReactionRate(const int&, double& rate);
C_DECL_SPEC RRVectorHandle          rrCallConv   getReactionRates();
C_DECL_SPEC RRVectorHandle          rrCallConv   getReactionRatesEx (const RRVectorHandle vec);

// Rates of change
C_DECL_SPEC RRVectorHandle          rrCallConv   getRatesOfChange();
C_DECL_SPEC RRStringListHandle      rrCallConv   getRatesOfChangeNames();
C_DECL_SPEC bool                    rrCallConv   getRateOfChange(const int&, double& value);
C_DECL_SPEC RRVectorHandle          rrCallConv   getRatesOfChangeEx (const RRVectorHandle vec);

C_DECL_SPEC bool                    rrCallConv   evalModel();

// Get number family
C_DECL_SPEC int                     rrCallConv   getNumberOfCompartments ();
C_DECL_SPEC int                     rrCallConv   getNumberOfBoundarySpecies();
C_DECL_SPEC int                     rrCallConv   getNumberOfFloatingSpecies();
C_DECL_SPEC int                     rrCallConv   getNumberOfGlobalParameters();
C_DECL_SPEC int                     rrCallConv   getNumberOfDependentSpecies();
C_DECL_SPEC int                     rrCallConv   getNumberOfIndependentSpecies();

// Get names family
C_DECL_SPEC RRStringListHandle      rrCallConv   getReactionNames();
C_DECL_SPEC RRStringListHandle      rrCallConv   getRateOfChangeNames();
C_DECL_SPEC RRStringListHandle      rrCallConv   getBoundarySpeciesNames();
C_DECL_SPEC RRStringListHandle      rrCallConv   getFloatingSpeciesNames();
C_DECL_SPEC RRStringListHandle      rrCallConv   getGlobalParameterNames();
C_DECL_SPEC RRStringListHandle      rrCallConv   getCompartmentNames();
C_DECL_SPEC RRStringListHandle      rrCallConv   getEigenValueNames();
C_DECL_SPEC RRArrayList2Handle      rrCallConv   getAvailableSymbols();

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
C_DECL_SPEC char*                   rrCallConv   printList(const RRStringListHandle list);
C_DECL_SPEC char*                   rrCallConv   printStringArrayList(const RRStringArrayList* list);
C_DECL_SPEC char*                   rrCallConv   printArrayList(const RRArrayList2Handle list);

// Free memory functions
C_DECL_SPEC bool                    rrCallConv   freeRRInstance(RRHandle handle);
C_DECL_SPEC bool                    rrCallConv   freeResult(RRResultHandle handle);
C_DECL_SPEC bool                    rrCallConv   freeText(char* text);
C_DECL_SPEC bool                    rrCallConv   freeLabelStringList(RRLabelStringListHandle sl);
C_DECL_SPEC bool                    rrCallConv   freeStringList(RRStringListHandle sl);
C_DECL_SPEC bool                    rrCallConv   freeStringArrayList(RRStringArrayListHandle sl);
C_DECL_SPEC bool                    rrCallConv   freeVector(RRVectorHandle vector);
C_DECL_SPEC bool                    rrCallConv   freeMatrix(RRMatrixHandle matrix);
C_DECL_SPEC bool                    rrCallConv   freeCCode(RRCCodeHandle code);
C_DECL_SPEC void                    rrCallConv   Pause();

// Helper Routines
C_DECL_SPEC RRVectorHandle          rrCallConv  createVectorAPI (int size);

C_DECL_SPEC int                     rrCallConv  getVectorLength (RRVectorHandle vector);
C_DECL_SPEC bool                    rrCallConv  getVectorElement (RRVectorHandle vector, int index, double& value);
C_DECL_SPEC bool                    rrCallConv  setVectorElement (RRVectorHandle vector, int index, double value);

C_DECL_SPEC int                     rrCallConv  getStringListLength (RRStringListHandle stringList);
C_DECL_SPEC char*                   rrCallConv  getStringListElement (RRStringListHandle stringList, int index);

C_DECL_SPEC int                     rrCallConv  getMatrixNumRows (RRMatrixHandle m);
C_DECL_SPEC int                     rrCallConv  getMatrixNumCols (RRMatrixHandle m);
C_DECL_SPEC bool                    rrCallConv  getMatrixElement (RRMatrixHandle m, int r, int c, double& value);

C_DECL_SPEC int                     rrCallConv  getResultNumRows (RRResultHandle result);
C_DECL_SPEC int                     rrCallConv  getResultNumCols (RRResultHandle result);
C_DECL_SPEC bool                    rrCallConv  getResultElement (RRResultHandle result, int r, int c, double& value);
C_DECL_SPEC char*                   rrCallConv  getResultColumnLabel (RRResultHandle result, int column);

C_DECL_SPEC char*                   rrCallConv  getCCodeHeader (RRCCodeHandle code);
C_DECL_SPEC char*                   rrCallConv  getCCodeSource (RRCCodeHandle code);

#if defined( __cplusplus)
}
#endif

#endif
