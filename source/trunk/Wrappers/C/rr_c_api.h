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
//#include <stdio.h>
//#include "rr_support/stdbool.h"
#endif

#include "rr_c_api_exporter.h"
#include "rr_c_types.h"

//The latest
C_DECL_SPEC bool                    rrCallConv   setCapabilities (const char* caps);
C_DECL_SPEC char*                   rrCallConv   getCapabilities();

// Utility and informational methods
C_DECL_SPEC char*                   rrCallConv  getVersion();
//C_DECL_SPEC char*                   rrCallConv   getLatestLog();
//C_DECL_SPEC char*                   rrCallConv   getLatestCommitAuthor();
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

// Load SBML methods
C_DECL_SPEC bool                    rrCallConv   loadSBML(const char* sbml);
C_DECL_SPEC bool                    rrCallConv   loadSBMLFromFile(const char* sbml);

// SBML utility methods
C_DECL_SPEC char* 				    rrCallConv   getParamPromotedSBML(const char* sArg);
C_DECL_SPEC char* 				    rrCallConv   getSBML();

// Simulation methods
C_DECL_SPEC bool                    rrCallConv   setTimeStart(const double& timeStart);
C_DECL_SPEC bool                    rrCallConv   setTimeEnd(const double& timeEnd);
C_DECL_SPEC bool                    rrCallConv   setNumPoints(const int& nrPoints);
C_DECL_SPEC bool                    rrCallConv   setSelectionList(const char* list);
C_DECL_SPEC RRStringListHandle      rrCallConv   getSelectionList();
C_DECL_SPEC RRResultHandle          rrCallConv   simulate();
C_DECL_SPEC RRResultHandle          rrCallConv   simulateEx(const double& timeStart, const double& timeEnd, const int& numberOfPoints);
C_DECL_SPEC bool                  	rrCallConv   oneStep(const double& currentTime, const double& stepSize, double& value);

// Steady state methods
C_DECL_SPEC bool                    rrCallConv   steadyState(double& value);
C_DECL_SPEC RRVectorHandle          rrCallConv   computeSteadyStateValues();
C_DECL_SPEC bool                    rrCallConv   setSteadyStateSelectionList(const char* list);
C_DECL_SPEC RRStringListHandle      rrCallConv   getSteadyStateSelectionList();

// Set and get family of methods
C_DECL_SPEC bool        			rrCallConv   getValue(const char* speciesID, double& value);
C_DECL_SPEC bool                    rrCallConv   setValue(const char* speciesId, const double& val);
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
C_DECL_SPEC RRMatrixHandle          rrCallConv   getEigenvalues();

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
C_DECL_SPEC int                     rrCallConv   getNumberOfGlobalParameterNames();
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
C_DECL_SPEC RRStringListHandle      rrCallConv   getElasticityCoefficientNames();
C_DECL_SPEC RRSymbolListsHandle     rrCallConv   getAvailableSymbols();

// MCA methods
C_DECL_SPEC RRStringListHandle      rrCallConv   getUnscaledFluxControlCoefficientNames();
C_DECL_SPEC RRStringListHandle      rrCallConv   getFluxControlCoefficientNames();
C_DECL_SPEC RRStringListHandle      rrCallConv   getUnscaledConcentrationControlCoefficientNames();
C_DECL_SPEC RRStringListHandle      rrCallConv   getConcentrationControlCoefficientNames();

C_DECL_SPEC RRMatrixHandle          rrCallConv   getUnScaledElasticityMatrix();
C_DECL_SPEC RRMatrixHandle          rrCallConv   getScaledElasticityMatrix();
C_DECL_SPEC RRMatrixHandle          rrCallConv   getUnscaledConcentrationControlCoefficientMatrix();
C_DECL_SPEC RRMatrixHandle          rrCallConv   getScaledConcentrationControlCoefficientMatrix();

C_DECL_SPEC bool                    rrCallConv   getCC(const char* variable, const char* parameter, double& value);
C_DECL_SPEC bool                    rrCallConv   getEE(const char* name, const char* species, double& value);
C_DECL_SPEC bool                    rrCallConv   getScaledFloatingSpeciesElasticity(const char* reactionName, const char* speciesName, double& value);
C_DECL_SPEC RRStringListHandle      rrCallConv   getFloatingSpeciesInitialConditionNames();

// Print/format functions
C_DECL_SPEC char*                   rrCallConv   printResult(const RRResultHandle result);
C_DECL_SPEC char*                   rrCallConv   printMatrix(const RRMatrixHandle mat);
C_DECL_SPEC char*                   rrCallConv   printVector(const RRVectorHandle vec);
C_DECL_SPEC char*                   rrCallConv   printList(const RRStringListHandle list);

// Free memory functions
C_DECL_SPEC bool                    rrCallConv   freeRRInstance(RRHandle handle);
C_DECL_SPEC bool                    rrCallConv   freeResult(RRResultHandle handle);
C_DECL_SPEC bool                    rrCallConv   freeText(char* text);
C_DECL_SPEC bool                    rrCallConv   freeLabelStringList(RRLabelStringListHandle sl);
C_DECL_SPEC bool                    rrCallConv   freeStringList(RRStringListHandle sl);
C_DECL_SPEC bool                    rrCallConv   freeVector(RRVectorHandle vector);
C_DECL_SPEC bool                    rrCallConv   freeMatrix(RRMatrixHandle matrix);
C_DECL_SPEC bool                    rrCallConv   freeCCode(RRCCodeHandle code);

#if defined( __cplusplus)
}
#endif

#endif
