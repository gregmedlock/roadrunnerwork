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

#ifndef rrC_APIH
#define rrC_APIH

#if defined(__cplusplus)
extern "C"
{
#else
#include <stdio.h>
#include "rr_support/stdbool.h"
#endif

#include "rr_c_api_exporter.h"
#include "rr_c_types.h"

//The latest
C_DECL_SPEC bool                    __stdcall   setCapabilities (const char* caps);
C_DECL_SPEC char*                   __stdcall   getCapabilities();

// Utility and informational methods
C_DECL_SPEC char*                   __stdcall   getRevision();
//C_DECL_SPEC char*                   __stdcall   getLatestLog();
//C_DECL_SPEC char*                   __stdcall   getLatestCommitAuthor();
C_DECL_SPEC char*                   __stdcall   getBuildDate();
C_DECL_SPEC char*                   __stdcall   getCopyright();
C_DECL_SPEC bool                    __stdcall   setTempFolder(const char* folder);
C_DECL_SPEC char*                   __stdcall   getTempFolder();

// Error handling
C_DECL_SPEC bool                    __stdcall   hasError();
C_DECL_SPEC char*                   __stdcall   getLastError();

// RoadRunner API
C_DECL_SPEC RRHandle                __stdcall   getRRInstance();
C_DECL_SPEC RRCCode*               	__stdcall   getCCode();

// Flags/Options
C_DECL_SPEC bool                    __stdcall   setComputeAndAssignConservationLaws(const bool& OnOrOff);

// Load SBML methods
C_DECL_SPEC bool                    __stdcall   loadSBML(const char* sbml);
C_DECL_SPEC bool                    __stdcall   loadSBMLFromFile(const char* sbml);

// SBML utility methods
C_DECL_SPEC char* 				          __stdcall   getParamPromotedSBML(const char* sArg);
C_DECL_SPEC char* 				          __stdcall   getSBML();

// Simulation methods
C_DECL_SPEC bool                    __stdcall   setTimeStart(const double& timeStart);
C_DECL_SPEC bool                    __stdcall   setTimeEnd(const double& timeEnd);
C_DECL_SPEC bool                    __stdcall   setNumPoints(const int& nrPoints);
C_DECL_SPEC bool                    __stdcall   setSelectionList(const char* list);
C_DECL_SPEC RRStringListHandle      __stdcall   getSelectionList();
C_DECL_SPEC RRResultHandle          __stdcall   simulate();
C_DECL_SPEC RRResultHandle          __stdcall   simulateEx(const double& timeStart, const double& timeEnd, const int& numberOfPoints);
C_DECL_SPEC bool                  	__stdcall   oneStep(const double& currentTime, const double& stepSize, double& value);

// Steady state methods
C_DECL_SPEC bool                    __stdcall   steadyState(double& value);
C_DECL_SPEC RRVectorHandle          __stdcall   computeSteadyStateValues();
C_DECL_SPEC bool                    __stdcall   setSteadyStateSelectionList(const char* list);
C_DECL_SPEC RRStringListHandle      __stdcall   getSteadyStateSelectionList();

// Set and get family of methods
C_DECL_SPEC bool                  	__stdcall   getValue(const char* speciesID, double& value);
C_DECL_SPEC bool                    __stdcall   setValue(const char* speciesId, const double& val);
C_DECL_SPEC bool                    __stdcall   setBoundarySpeciesByIndex(const int& index, const double& value);
C_DECL_SPEC bool                    __stdcall   setFloatingSpeciesByIndex(const int& index, const double& value);
C_DECL_SPEC bool                    __stdcall   setGlobalParameterByIndex(const int& index, const double& value);
C_DECL_SPEC bool                  	__stdcall   getBoundarySpeciesByIndex(const int& index, double& val);
C_DECL_SPEC bool                    __stdcall   getFloatingSpeciesByIndex(const int& index, double& val);
C_DECL_SPEC bool                  	__stdcall   getGlobalParameterByIndex(const int& index, double& val);
C_DECL_SPEC bool                    __stdcall   getCompartmentByIndex (const int& index, double& value);
C_DECL_SPEC bool                    __stdcall   setCompartmentByIndex (const int& index, const double& value);

// Jacobian matrix methods
C_DECL_SPEC RRMatrixHandle          __stdcall   getFullJacobian();
C_DECL_SPEC RRMatrixHandle          __stdcall   getReducedJacobian();

// Stoichiometry methods
C_DECL_SPEC RRMatrixHandle          __stdcall   getStoichiometryMatrix();

// Initial condition Methods
C_DECL_SPEC bool                    __stdcall   reset();
C_DECL_SPEC bool                    __stdcall   setFloatingSpeciesInitialConcentrations (const RRVector* vec);
C_DECL_SPEC RRVectorHandle          __stdcall   getFloatingSpeciesInitialConcentrations ();

// Reaction rates
C_DECL_SPEC int                     __stdcall   getNumberOfReactions();
C_DECL_SPEC bool                  	__stdcall   getReactionRate(const int&, double& rate);
C_DECL_SPEC RRVectorHandle          __stdcall   getReactionRates();
C_DECL_SPEC RRVectorHandle          __stdcall   getReactionRatesEx (const RRVectorHandle vec);

// Rates of change
C_DECL_SPEC RRVectorHandle          __stdcall   getRatesOfChange();
C_DECL_SPEC RRStringListHandle      __stdcall   getRatesOfChangeNames();
C_DECL_SPEC bool                    __stdcall   getRateOfChange(const int&, double& value);
C_DECL_SPEC RRVectorHandle          __stdcall   getRatesOfChangeEx (const RRVectorHandle vec);

C_DECL_SPEC bool                    __stdcall   evalModel();

// Get number family
C_DECL_SPEC int                     __stdcall   getNumberOfCompartments ();
C_DECL_SPEC int                     __stdcall   getNumberOfBoundarySpecies();
C_DECL_SPEC int                     __stdcall   getNumberOfFloatingSpecies();
C_DECL_SPEC int                     __stdcall   getNumberOfGlobalParameters();
C_DECL_SPEC int                     __stdcall   getNumberOfGlobalParameterNames();
C_DECL_SPEC int                     __stdcall   getNumberOfDependentSpecies();
C_DECL_SPEC int                     __stdcall   getNumberOfIndependentSpecies();

// Get names family
C_DECL_SPEC RRStringListHandle      __stdcall   getReactionNames();
C_DECL_SPEC RRStringListHandle      __stdcall   getRateOfChangeNames();
C_DECL_SPEC RRStringListHandle      __stdcall   getBoundarySpeciesNames();
C_DECL_SPEC RRStringListHandle      __stdcall   getFloatingSpeciesNames();
C_DECL_SPEC RRStringListHandle      __stdcall   getGlobalParameterNames();
C_DECL_SPEC RRStringListHandle      __stdcall   getCompartmentNames();
C_DECL_SPEC RRStringListHandle      __stdcall   getEigenValueNames();
C_DECL_SPEC RRStringListHandle      __stdcall   getElasticityNames();
C_DECL_SPEC RRStringListHandle      __stdcall   getElasticityCoefficientNames();
C_DECL_SPEC RRSymbolListsHandle     __stdcall   getAvailableSymbols();

// MCA methods
C_DECL_SPEC RRStringListHandle      __stdcall   getFluxControlCoefficientNames();
C_DECL_SPEC RRStringListHandle      __stdcall   getConcentrationControlCoefficientNames();
C_DECL_SPEC RRMatrixHandle          __stdcall   getScaledElasticityMatrix();
C_DECL_SPEC bool                    __stdcall   getCC(const char* variable, const char* parameter, double& value);
C_DECL_SPEC bool                    __stdcall   getEE(const char* name, const char* species, double& value);
C_DECL_SPEC bool                    __stdcall   getScaledFloatingSpeciesElasticity(const char* reactionName, const char* speciesName, double& value);
C_DECL_SPEC RRStringListHandle      __stdcall   getFloatingSpeciesInitialConditionNames();

// Print/format functions
C_DECL_SPEC char*                   __stdcall   printResult(const RRResultHandle result);
C_DECL_SPEC char*                   __stdcall   printMatrix(const RRMatrixHandle mat);
C_DECL_SPEC char*                   __stdcall   printVector(const RRVectorHandle vec);
C_DECL_SPEC char*                   __stdcall   printList(const RRStringListHandle list);

// Free memory functions
C_DECL_SPEC bool                    __stdcall   freeRRInstance(RRHandle handle);
C_DECL_SPEC bool                    __stdcall   freeResult(RRResultHandle handle);
C_DECL_SPEC bool                    __stdcall   freeText(char* text);
C_DECL_SPEC bool                    __stdcall   freeLabelStringList(RRLabelStringListHandle sl);
C_DECL_SPEC bool                    __stdcall   freeStringList(RRStringListHandle sl);
C_DECL_SPEC bool                    __stdcall   freeVector(RRVectorHandle vector);
C_DECL_SPEC bool                    __stdcall   freeMatrix(RRMatrixHandle matrix);
C_DECL_SPEC bool                    __stdcall   freeCCode(RRCCodeHandle code);

#if defined( __cplusplus)
}
#endif

#endif
