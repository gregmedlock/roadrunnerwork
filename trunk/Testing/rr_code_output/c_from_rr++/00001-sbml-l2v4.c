#include "00001-sbml-l2v4.h"

#include <stdio.h>
void loadSymbolTables()
{
	variableTable[0] = "S1";
	variableTable[1] = "S2";
	globalParameterTable[0] = "k1";
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
		case 1:
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
	_amounts[1] = _y[1]*_c[0];
}

void convertToConcentrations()
{
	_y[0] = _amounts[0] / _c[0];
	_y[1] = _amounts[1] / _c[0];
}

int getNumLocalParameters(int reactionId)
{
	return localParameterDimensions[reactionId];
}
void initializeInitialConditions()
{
	_init_y[0] = (double) 0.00015/ _c[0];
	_init_y[1] = (double) 0/ _c[0];
}

void setInitialConditions()
{
	_y[0] =  _init_y[0];
	_amounts[0] = _y[0]*_c[0];

	_y[1] =  _init_y[1];
	_amounts[1] = _y[1]*_c[0];
}

void setBoundaryConditions()
{}

void setCompartmentVolumes()
{
	_c[0] = (double)1;
	_c[0] = 
	(double)1;
}

void setParameterValues()
{
	_gp[0] = (double)1;
}

// Uses the equation: C = Sd - L0*Si
void computeConservedTotals()
{
	_ct[0] =  + _y[0]*_c[0] + _y[1]*_c[0];
}

// Compute values of dependent species 
// Uses the equation: Sd = C + L0*Si
void updateDependentSpeciesValues(double* y)
{
	_y[1] = (_ct[0] - y[0]*_c[0]) / _c[0];
}

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
	_dydt[1] =  - _dydt[0];

}

// Compute the reaction rates
void computeReactionRates(double time, double y[])
{
	_rates[0] = _c[0]*
	_gp[0]*
	y[0];
}

//Model Function
void evalModel (double timein, double* oAmounts)
{
	_y[0] = oAmounts[0]/_c[0];

	_y[1] = oAmounts[1]/_c[0];

		convertToAmounts();
		_time = timein;  // Don't remove
		updateDependentSpeciesValues (_y);
		computeReactionRates (_time, _y);
		_dydt[0] = - _rates[0];
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
	numDependentVariables = 1;
	numTotalVariables = 2;
	numBoundaryVariables = 0;
	numGlobalParameters = 1;
	numCompartments = 1;
	numReactions = 1;
	numEvents = 0;
	mModelName = (char*) malloc(sizeof(char)*10);
	strcpy(mModelName,"case00001");
	_gp[0] = 1234;
	InitializeDelays();
	localParameterDimensions[0] = 0;
	_lp[0] = (double*) malloc(sizeof(double)*0);
	return 0;
}


char* GetModelName()
{
	return mModelName;
}
