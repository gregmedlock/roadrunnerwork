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
//C_DECL_SPEC char*                   __stdcall   getTempFolder(void);
//C_DECL_SPEC char*                   __stdcall   getCCode(void);

// SBML Methods
C_DECL_SPEC bool                    __stdcall   loadSBML(const char* sbml);
C_DECL_SPEC bool                    __stdcall   loadSBMLFromFile(const char* sbml);

// Simulation Methods
C_DECL_SPEC bool                    __stdcall   setTimeStart(double timeStart);
C_DECL_SPEC bool                    __stdcall   setTimeEnd(double timeEnd);
C_DECL_SPEC bool                    __stdcall   setNumPoints(int nrPoints);
C_DECL_SPEC bool                    __stdcall   setSelectionList(const char* list);
// C_DECL_SPEC RRStringListHandle      __stdcall   getSelectionList(); <- to be added
C_DECL_SPEC RRResultHandle          __stdcall   simulate(void);
C_DECL_SPEC RRResultHandle          __stdcall   simulateEx(const double& timeStart, const double& timeEnd, const int& numberOfPoints);
C_DECL_SPEC double                  __stdcall   oneStep(const double& currentTime, const double& stepSize);

// Steady State Methods
C_DECL_SPEC double                  __stdcall   steadyState();
C_DECL_SPEC RRDoubleVectorHandle    __stdcall   computeSteadyStateValues();
C_DECL_SPEC bool                    __stdcall   setSteadyStateSelectionList(char *);
//C_DECL_SPEC RRStringListHandle      __stdcall   getSteadyStateSelectionList(); <- to be added

C_DECL_SPEC double                  __stdcall   getValue(const char* speciesID);
C_DECL_SPEC bool                    __stdcall   setValue(const char* speciesId, const double& val);

// Jacobian Matrix Methods
//C_DECL_SPEC RRDataMatrixHandle      __stdcall   getFullJacobian(void);  <- to be added
//C_DECL_SPEC RRDataMatrixHandle      __stdcall   getReducedJacobian(void);  <- to be added

// Stoichiometry Methods
C_DECL_SPEC RRDataMatrixHandle      __stdcall   getStoichiometryMatrix(void);
C_DECL_SPEC void                    __stdcall   printMatrix(RRDataMatrixHandle mat);

// Error handling
C_DECL_SPEC bool                    __stdcall   hasError();
C_DECL_SPEC char*                   __stdcall   getLastError();

C_DECL_SPEC bool                    __stdcall   reset();
C_DECL_SPEC bool                    __stdcall   setInitialConditions(RRDoubleVector* vec); <- rename to setFloatingSpeciesInitialConcentrations
// C_DECL_SPEC RRDoubleVectorHandle __stccall   getFloatingSpeciesInitialConcentrations (void);  <- to be added  

// Reaction Rates
C_DECL_SPEC int                     __stdcall   getNumberOfReactions();
C_DECL_SPEC double                  __stdcall   getReactionRate(int);
//C_DECL_SPEC RRDoubleVectorHandle    __stdcall   getReactionRates(); <- to be added

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

// Free memory functions
C_DECL_SPEC void                    __stdcall   freeRRInstance(RRHandle handle);
C_DECL_SPEC bool                    __stdcall   freeRRResult(RRResultHandle handle);
C_DECL_SPEC bool                    __stdcall   freeText(char* text);
C_DECL_SPEC bool                    __stdcall   freeLabelStringList(RRLabelStringListHandle sl);
C_DECL_SPEC bool                    __stdcall   freeStringList(RRStringListHandle sl);
C_DECL_SPEC bool                    __stdcall   freeRRDoubleVector(RRDoubleVectorHandle vector);
C_DECL_SPEC bool                    __stdcall   freeRRDataMatrix(RRDataMatrixHandle matrix);

#if defined( __cplusplus)
}
#endif

#endif
