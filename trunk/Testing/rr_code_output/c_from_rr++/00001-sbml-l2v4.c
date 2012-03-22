#include "00001-sbml-l2v4.h"

#include <stdio.h>
void loadSymbolTables()
{
	variableTable[0] = "S1";
	boundaryTable[0] = "Xo";
	boundaryTable[1] = "X1";
	globalParameterTable[0] = "k1";
	globalParameterTable[1] = "k2";
}

void resetEvents()
{}


void setConcentration(int index, double value)
{
	double volume = 0.0;
	_y[index] = value;
	switch (index)
	{
		case 0:
			volume = _c[0];
		break;
	}
	_amounts[index] = _y[index]*volume;
}

double getConcentration(int index)
{
	return _y[index];
}

void convertToAmounts()
{
	_amounts[0] = _y[0]*_c[0];
}

void convertToConcentrations()
{
	_y[0] = _amounts[0] / _c[0];
}

int getNumLocalParameters(int reactionId)
{
	return localParameterDimensions[reactionId];
}
void initializeInitialConditions()
{
	_init_y[0] = (double)0;
}

void setInitialConditions()
{
	_y[0] =  _init_y[0];
	_amounts[0] = _y[0]*_c[0];
}

void setBoundaryConditions()
{	_bc[0] = (double)10;
	_bc[1] = (double)0;
}

void setCompartmentVolumes()
{
	_c[0] = (double)1;
	_c[0] = 
	(double)1;
}

void setParameterValues()
{
	_gp[0] = (double)1;

	_gp[1] = (double)2;
}

// Uses the equation: C = Sd - L0*Si
void computeConservedTotals()
{}

// Compute values of dependent species 
// Uses the equation: Sd = C + L0*Si
void updateDependentSpeciesValues(double* y)
{}

void computeRules(double* y)
{}

	 double _rateRules[0];           // Vector containing values of additional rate rules      
void InitializeRates()
{}

void AssignRates()
{
}

void InitializeRateRuleSymbols() 
{}

void AssignRates(double oRates[])
{}

double* GetCurrentValues()
{
	double* dResult = (double*) malloc(sizeof(double)*0);
	return dResult;
}

//Uses the equation: dSd/dt = L0 dSi/dt
void computeAllRatesOfChange()
{
	//double* dTemp = (double*) malloc( sizeof(double)* (amounts.Length + rateRules.Length) );
	//amounts.CopyTo(dTemp, rateRules.Length); Todo: fix this..
	evalModel(_time, _amounts);
}

// Compute the reaction rates
void computeReactionRates(double time, double y[])
{
	_rates[0] = _gp[0]*
	_bc[0];

	_rates[1] = _gp[1]*
	y[0];
}

//Model Function
void evalModel (double timein, double* oAmounts)
{
	_y[0] = oAmounts[0]/_c[0];

		convertToAmounts();
		_time = timein;  // Don't remove
		updateDependentSpeciesValues (_y);
		computeReactionRates (_time, _y);
		_dydt[0] = + _rates[0] - _rates[1];
	convertToAmounts();
}

// Event handling function
void evalEvents(double timeIn, double oAmounts[])
{
	_time = timeIn;  // Don't remove
	updateDependentSpeciesValues(_y);
	computeRules(_y);
}

void InitializeDelays()
{
	printf("At line %d in function %s \n",__LINE__, __FUNCTION__);
}

void computeEventPriorites()
{}

void evalInitialAssignments()
{}

void testConstraints()
{}


//Function to initialize the model data structure. Returns an integer indicating result
int InitModel()
{
	numIndependentVariables = 1;
	numDependentVariables = 0;
	numTotalVariables = 1;
	numBoundaryVariables = 2;
	numGlobalParameters = 2;
	numCompartments = 1;
	numReactions = 2;
	numEvents = 0;
	mModelName = (char*) malloc(sizeof(char)*5);
	strcpy(mModelName,"cell");
	_gp[0] = 1234;
	InitializeDelays();
	localParameterDimensions[0] = 0;
	_lp[0] = (double*) malloc(sizeof(double)*0);
	localParameterDimensions[1] = 0;
	_lp[1] = (double*) malloc(sizeof(double)*0);
	return 0;
}


char* GetModelName()
{
	return mModelName;
}
