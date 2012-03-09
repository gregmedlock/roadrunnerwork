#include "00001-sbml-l2v4.h"

#include <stdio.h>
void loadSymbolTables()
{
	g.variableTable[0] = "S1";
	g.variableTable[1] = "S2";
		g.globalParameterTable[0] = "k1";
	}


	 void resetEvents() {
	}

void InitializeDelays()
{	
	printf("In File %s ",__FILE__);
	printf("In Function %s ",__FUNCTION__);
	printf("At Line %d ",__LINE__);
}

void computeEventPriorites()
{
}


//Function to initialize the model data structure. Returns an integer indicating result
int InitModel()
{
	g.numIndependentVariables = 1;
	g.numDependentVariables = 1;
	g.numTotalVariables = 2;
	g.numBoundaryVariables = 0;
	g.numGlobalParameters = 1;
	g.numCompartments = 1;
	g.numReactions = 1;
	g.numEvents = 0;
	g.mModelName = (char*) malloc(sizeof(char)*10);
	strcpy(g.mModelName,"case00001");
	g._gp[0] = 1234;
	InitializeDelays();
	g.localParameterDimensions[0] = 0;
	g._lp[0] = (double*) malloc(sizeof(double)*0);
	return 0;
}


char* GetModelName()
{
	return g.mModelName;
}
