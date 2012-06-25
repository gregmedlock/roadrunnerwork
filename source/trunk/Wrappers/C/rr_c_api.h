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

//  RoadRunner API
C_DECL_SPEC RRHandle                __stdcall   getRRInstance(void);
C_DECL_SPEC char*                   __stdcall   getBuildDate(void);
C_DECL_SPEC char*                   __stdcall   getCopyright(void);
C_DECL_SPEC bool                    __stdcall   setTempFolder(const char* folder);
C_DECL_SPEC char*                   __stdcall   getTempFolder(void);

C_DECL_SPEC RRCCode*               	__stdcall   getCCode(void);


//Flags/Options
C_DECL_SPEC bool                    __stdcall   setComputeAndAssignConservationLaws(const bool& OnOrOff);

// Error handling
C_DECL_SPEC bool                    __stdcall   hasError();
C_DECL_SPEC char*                   __stdcall   getLastError();

// SBML Methods
C_DECL_SPEC bool                    __stdcall   loadSBML(const char* sbml);
C_DECL_SPEC bool                    __stdcall   loadSBMLFromFile(const char* sbml);

// Simulation Methods
C_DECL_SPEC bool                    __stdcall   setTimeStart(double timeStart);
C_DECL_SPEC bool                    __stdcall   setTimeEnd(double timeEnd);
C_DECL_SPEC bool                    __stdcall   setNumPoints(int nrPoints);
C_DECL_SPEC bool                    __stdcall   setSelectionList(const char* list);
C_DECL_SPEC RRStringListHandle      __stdcall   getSelectionList();
C_DECL_SPEC RRResultHandle          __stdcall   simulate(void);
C_DECL_SPEC RRResultHandle          __stdcall   simulateEx(const double& timeStart, const double& timeEnd, const int& numberOfPoints);
C_DECL_SPEC double                  __stdcall   oneStep(const double& currentTime, const double& stepSize);

// Steady State Methods
C_DECL_SPEC double                  __stdcall   steadyState();
C_DECL_SPEC RRDoubleVectorHandle    __stdcall   computeSteadyStateValues();
C_DECL_SPEC bool                    __stdcall   setSteadyStateSelectionList(char *);
C_DECL_SPEC RRStringListHandle      __stdcall   getSteadyStateSelectionList();

C_DECL_SPEC double                  __stdcall   getValue(const char* speciesID);
C_DECL_SPEC bool                    __stdcall   setValue(const char* speciesId, const double& val);
//C_DECL_SPEC bool                    __stdcall   setBoundarySpeciesByIndex (int index, double value); <- to be added
//C_DECL_SPEC bool                    __stdcall   setFloatingSpeciesByIndex (int index, double value);
//C_DECL_SPEC bool                    __stdcall   setGlobalParameterByIndex  (int index, double value);
//C_DECL_SPEC double                  __stdcall   getBoundarySpeciesByIndex (int index);
//C_DECL_SPEC double                  __stdcall   getFloatingSpeciesByIndex (int index);
//C_DECL_SPEC double                  __stdcall   getGlobalParameterByIndex (int index);

// Jacobian Matrix Methods
//C_DECL_SPEC RRDataMatrixHandle      __stdcall   getFullJacobian(void);  <- to be added
//C_DECL_SPEC RRDataMatrixHandle      __stdcall   getReducedJacobian(void);  <- to be added

// Stoichiometry Methods
C_DECL_SPEC RRDataMatrixHandle      __stdcall   getStoichiometryMatrix(void);

C_DECL_SPEC bool                    __stdcall   reset();
C_DECL_SPEC bool                    __stdcall   setFloatingSpeciesInitialConcentrations (RRDoubleVector* vec);
C_DECL_SPEC RRDoubleVectorHandle    __stdcall   getFloatingSpeciesInitialConcentrations (void);

// Reaction Rates
C_DECL_SPEC int                     __stdcall   getNumberOfReactions();
C_DECL_SPEC double                  __stdcall   getReactionRate(int);
C_DECL_SPEC RRDoubleVectorHandle    __stdcall   getReactionRates();

// Rates of Change
//C_DECL_SPEC RRStringListHandle      __stdcall   getRatesOfChangeNames(); <- to be added

// get Number Family
C_DECL_SPEC int                     __stdcall   getNumberOfBoundarySpecies();
C_DECL_SPEC int                     __stdcall   getNumberOfFloatingSpecies();
C_DECL_SPEC int                     __stdcall   getNumberOfGlobalParameters();
C_DECL_SPEC int                     __stdcall   getNumberOfGlobalParameterNames();

// get Names Family
C_DECL_SPEC RRStringListHandle      __stdcall   getReactionNames(void);
C_DECL_SPEC RRStringListHandle      __stdcall   getBoundarySpeciesNames();
C_DECL_SPEC RRStringListHandle      __stdcall   getFloatingSpeciesNames();
C_DECL_SPEC RRStringListHandle      __stdcall   getGlobalParameterNames();
C_DECL_SPEC RRSymbolListsHandle     __stdcall   getAvailableSymbols();              // <- You'll have to decide what type to return

// Print/format functions
C_DECL_SPEC char*                   __stdcall   getResultAsString(RRResultHandle result);
C_DECL_SPEC void                    __stdcall   printMatrix(RRDataMatrixHandle mat);
C_DECL_SPEC void                    __stdcall   printVector(RRDoubleVectorHandle vec);

// Free memory functions
C_DECL_SPEC void                    __stdcall   freeRRInstance(RRHandle handle);
C_DECL_SPEC bool                    __stdcall   freeRRResult(RRResultHandle handle);
C_DECL_SPEC bool                    __stdcall   freeText(char* text);
C_DECL_SPEC bool                    __stdcall   freeLabelStringList(RRLabelStringListHandle sl);
C_DECL_SPEC bool                    __stdcall   freeStringList(RRStringListHandle sl);
C_DECL_SPEC bool                    __stdcall   freeRRDoubleVector(RRDoubleVectorHandle vector);
C_DECL_SPEC bool                    __stdcall   freeRRDataMatrix(RRDataMatrixHandle matrix);
C_DECL_SPEC bool                    __stdcall   freeCCode(RRCCodeHandle code);

#if defined( __cplusplus)
}
#endif

#endif
