#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrIModel.h"
//---------------------------------------------------------------------------
#if defined(__CODEGEARC__)
#pragma package(smart_init)
#endif
//---------------------------------------------------------------------------


namespace rr
{

IModel::IModel()
:
mDummyInt(0),
numIndependentVariables(&mDummyInt),
numDependentVariables(&mDummyInt),
numTotalVariables(&mDummyInt),
numBoundaryVariables(&mDummyInt),
numGlobalParameters(&mDummyInt),
numCompartments(&mDummyInt),
numReactions(&mDummyInt),
numRules(&mDummyInt),
numEvents(&mDummyInt),
time(0),
mModelName("NoNameSet")
{}

IModel::~IModel(){}


int IModel::getNumIndependentVariables()
{
	return *numIndependentVariables;
}

int IModel::getNumDependentVariables()
{
	return *numDependentVariables;
}

int IModel::getNumTotalVariables()
{
	return *numTotalVariables;
}

int IModel::getNumBoundarySpecies()
{
	return *numBoundaryVariables;	//Todos: bad naming - is Variables/Species, choose one..
}

int IModel::getNumGlobalParameters()
{
	return *numGlobalParameters;
}

int IModel::getNumCompartments()
{
	return *numCompartments;
}

int IModel::getNumReactions()
{
	return *numReactions;
}

int IModel::getNumRules()
{
	return *numRules;
}

int IModel::getNumEvents()
{
	return *numEvents;
}

//////Virtual functions that should be implemented in decendant..
void  IModel::initializeInitialConditions(){}
void  IModel::setInitialConditions(){}
void  IModel::setParameterValues(){}
void  IModel::setBoundaryConditions(){}
void  IModel::InitializeRates(){}
void  IModel::AssignRates(){}
void  IModel::AssignRates(vector<double>& rates){}
void  IModel::computeConservedTotals(){}
void  IModel::computeEventPriorites(){}
void  IModel::setConcentration(int index, double value){}
void  IModel::convertToAmounts(){}
void  IModel::convertToConcentrations(){}
void  IModel::updateDependentSpeciesValues(vector<double>& _y){}
void  IModel::computeRules(vector<double>& _y){}
void  IModel::computeReactionRates(double time, vector<double>& y){}
void  IModel::computeAllRatesOfChange(){}
void  IModel::evalModel(double time, vector<double>& y){}
void  IModel::evalEvents(double time, vector<double>& y){}
void  IModel::resetEvents(){}
void  IModel::evalInitialAssignments(){}
void  IModel::testConstraints(){}
void  IModel::InitializeRateRuleSymbols(){}

} //namespace rr
