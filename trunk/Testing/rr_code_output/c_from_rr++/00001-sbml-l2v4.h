#ifndef modelH
#define modelH
#include <stdio.h>
#include <stdbool.h>

#if defined(BUILD_MODEL_DLL)
#define D_S __declspec(dllexport)
#else
#define D_S __declspec(dllimport)
#endif
//************************************************************************** 
	// Model Symbol Mappings

	// y[0] = S1
	// y[1] = S2
//************************************************************************** 


	//End of base class members

	char*                              mModelName;                                       
	char**                             mWarnings;                                       
	double                             _gp[1];                                 //Vector containing all the global parameters in the System  
	double*                            _lp[1];                                 //Vector containing all the local parameters in the System  
	double                             _y[2];                                  //Vector containing the concentrations of all floating species
	double                             _init_y[2];                             //Vector containing the initial concentrations of all floating species
	double                             _amounts[2];                            //Vector containing the amounts of all floating species 
	double                             _bc[0];                                 //Vector containing all the boundary species concentration values
	double                             _c[1];                                  //Vector containing all the compartment values   
	double                             _dydt[2];                               //Vector containing rates of changes of all species   
	double                             _rates[1];                              //Vector containing the rate laws of all reactions    
	double                             _ct[1];                                 //Vector containing values of all conserved sums      
	double                             _eventTests[0];                         //Vector containing results of any event tests        
	//TEventDelayDelegate              _eventDelay[0];                         //Array of trigger function pointers
	bool                               _eventType[0];                          //Array holding the status whether events are useValuesFromTriggerTime or not
	bool                               _eventPersistentType[0];                //Array holding the status whether events are persitstent or not
	double                             _time;                                       
	int                                numIndependentVariables;                                       
	int                                numDependentVariables;                                       
	int                                numTotalVariables;                                       
	int                                numBoundaryVariables;                                       
	int                                numGlobalParameters;                                       
	int                                numCompartments;                                       
	int                                numReactions;                                       
	int                                numRules;                                       
	int                                numEvents;                                       
	char*                              variableTable[2];                       
	char*                              boundaryTable[0];                       
	char*                              globalParameterTable[1];                
	int                                localParameterDimensions[1];            
	//TEventAssignmentDelegate         _eventAssignments;                                       
	double                             _eventPriorities;                                       
	//TComputeEventAssignmentDelegate  _computeEventAssignments;                                       
	//TPerformEventAssignmentDelegate  _performEventAssignments;                                       
	bool                               _eventStatusArray[0];                   
	bool                               _previousEventStatusArray[0];           


//NON - EXPORTS ========================================
    void                               InitializeDelays();                     


//EXPORTS ========================================
D_S int                                InitModel();                            
D_S char*                              GetModelName();                         
D_S void                               resetEvents();                          
D_S void                               setConcentration(int index, double value);
D_S double                             getConcentration(int index);            
D_S void                               convertToAmounts();                     
D_S void                               convertToConcentrations();              
D_S int                                getNumLocalParameters(int reactionId);  
D_S void                               initializeInitialConditions();          
D_S void                               setInitialConditions();                 
D_S void                               setBoundaryConditions();                
D_S void                               setCompartmentVolumes();                
D_S void                               setParameterValues();                   
D_S void                               computeConservedTotals();               
D_S void                               updateDependentSpeciesValues(double* y);
D_S void                               computeRules(double* y);                
D_S void                               InitializeRates();                      
D_S void                               AssignRates();                          
D_S void                               InitializeRateRuleSymbols();            
D_S void                               AssignRates(double oRates[]);           
D_S double*                            GetCurrentValues();                     
D_S void                               computeAllRatesOfChange();              
D_S void                               computeReactionRates(double time, double y[]);
D_S void                               evalModel(double, double*);             
D_S void                               evalEvents(double timeIn, double oAmounts[]);
D_S void                               evalInitialAssignments();               
D_S void                               testConstraints();                      


#endif //modelH
