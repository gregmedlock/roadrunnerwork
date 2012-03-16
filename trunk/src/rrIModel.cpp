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
numIndependentVariables(-1),
numDependentVariables(-1),
numTotalVariables(-1),
numBoundaryVariables(-1),
numGlobalParameters(-1),
numCompartments(-1),
numReactions(-1),
numRules(-1),
numEvents(-1)
{}

IModel::~IModel(){}

//get,set
vector<double>&	IModel::Get_y()
{
	return y;
}

vector<double>& IModel::Get_bc()
{
	return bc;
}

vector<double>&	IModel::Get_c()
{
	return  c;
}

vector<double>&	IModel::Get_gp()
{
	return  gp;
}

vector<double>&	IModel::Get_ct()
{
	return  ct;
}

vector<double>&	IModel::Get_dydt()
{
	return  dydt;
}

vector<double>&	IModel::Get_rates()
{
	return  rates;
}

vector<double>&	IModel::Get_rateRules()
{
	return  rateRules;
}

vector<double>&	IModel::Get_sr()
{
	return  sr;
}

double IModel::Get_time()
{
	return  time;
}

vector<bool>& IModel::Get_eventStatusArray()
{
	return eventStatusArray;
}

vector<double>&	IModel::Get_eventTests()
{
	return eventTests;
}

vector<bool>& IModel::Get_previousEventStatusArray()
{
	return previousEventStatusArray;
}

int IModel::getNumIndependentVariables()
{
	return 0;
}

int IModel::getNumDependentVariables()
{
	return 0;
}

int IModel::getNumTotalVariables()
{
	return 0;
}

int IModel::getNumBoundarySpecies()
{
	return 0;
}

int IModel::getNumGlobalParameters()
{
	return 0;
}

int IModel::getNumCompartments()
{
	return 0;
}

int IModel::getNumReactions()
{
	return 0;
}

int IModel::getNumRules()
{
	return 0;
}

int IModel::getNumEvents()
{
	return 0;
}



} //namespace rr
