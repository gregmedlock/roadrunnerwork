#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <iostream>
#include "rrLogger.h"
#include "rrModelFromC.h"
#include "rrCGenerator.h"
//---------------------------------------------------------------------------
using namespace std;
namespace rr
{
bool CopyDblArray(double* src, vector<double>& dest, int size);

ModelFromC::ModelFromC(CGenerator* generator, HINSTANCE dllHandle)
:
mIsInitialized(false),
mDLLHandle(dllHandle),
mCodeGenerator(generator)
{
	if(mDLLHandle)
	{
		SetupDLLFunctions();
		SetupDLLData();
	}
}

ModelFromC::~ModelFromC()
{}

void ModelFromC::LoadData()
{
	CopyDblArray(mGP, 			gp, 			mCodeGenerator->GetNumberOfFloatingSpecies());
	CopyDblArray(mInitY, 		init_y, 		mCodeGenerator->GetNumberOfFloatingSpecies());
	CopyDblArray(mY, 			y, 				mCodeGenerator->GetNumberOfFloatingSpecies());
	CopyDblArray(m_dydt, 		dydt, 			mCodeGenerator->GetNumberOfFloatingSpecies());
//	CopyDblArray(mAmounts, 		amounts, 		mCodeGenerator->GetNumberOfFloatingSpecies());
	CopyDblArray(mRates, 		rates, 			mCodeGenerator->GetNumberOfReactions());
}

bool CopyDblArray(double* src, vector<double>& dest, int size)
{
	if(!src)
	{
		Log(lError)<<"Tried to copy from NULL vector";
		return false;
	}

	dest.resize(size);
	for(int i = 0; i < size; i++)
	{
		dest[i] = src[i];
	}
	return true;
}

double ModelFromC::GetAmounts(const int& i)
{
	return (mAmounts ) ? mAmounts[i] : -1;
}

bool ModelFromC::SetupDLLFunctions()
{
	//Exported functions in the dll need to be assigned to a function pointer here..
	if(!mDLLHandle)
	{
		Log(lError)<<"DLL handle not valid in SetupModel function";
		return false;
	}

	//Load functions..
	cInitModel                          = (c_int)						GetFunctionPtr("InitModel");
	cGetModelName                       = (c_charStar)		 		    GetFunctionPtr("GetModelName");
	cinitializeInitialConditions        = (c_void)                	    GetFunctionPtr("initializeInitialConditions");
	csetParameterValues                 = (c_void)                	    GetFunctionPtr("setParameterValues");
	csetCompartmentVolumes              = (c_void)  				    GetFunctionPtr("setCompartmentVolumes");
	cgetNumLocalParameters              = (c_int_int)             	    GetFunctionPtr("getNumLocalParameters");
	csetBoundaryConditions              = (c_void)                	    GetFunctionPtr("setBoundaryConditions");
	csetInitialConditions               = (c_void)                	    GetFunctionPtr("setInitialConditions");
	cevalInitialAssignments             = (c_void)                      GetFunctionPtr("evalInitialAssignments");
	ccomputeRules                       = (c_void_doubleStar)           GetFunctionPtr("computeRules");
	cconvertToAmounts                   = (c_void)                      GetFunctionPtr("convertToAmounts");
	ccomputeConservedTotals             = (c_void)                      GetFunctionPtr("computeConservedTotals");
	cgetConcentration                   = (c_double_int)                GetFunctionPtr("getConcentration");
	cGetCurrentValues                   = (c_doubleStar)    	        GetFunctionPtr("GetCurrentValues");
	cevalModel                			= (c_void_double_doubleStar)    GetFunctionPtr("evalModel");
	cconvertToConcentrations  			= (c_void)     				    GetFunctionPtr("convertToConcentrations");
	cevalEvents							= (c_void_double_doubleStar)    GetFunctionPtr("evalEvents");
	cupdateDependentSpeciesValues	    = (c_void_doubleStar)			GetFunctionPtr("updateDependentSpeciesValues");
	ccomputeAllRatesOfChange   			= (c_void)						GetFunctionPtr("computeAllRatesOfChange");
	cAssignRates_a 						= (c_void)						GetFunctionPtr("AssignRates");
	cAssignRates_b 						= (c_void_doubleStar)			GetFunctionPtr("AssignRates");
	ctestConstraints 	   				= (c_void)						GetFunctionPtr("testConstraints");
	cresetEvents	 	   				= (c_void)						GetFunctionPtr("resetEvents");
	cInitializeRateRuleSymbols			= (c_void)						GetFunctionPtr("InitializeRateRuleSymbols");
	cInitializeRates					= (c_void)						GetFunctionPtr("InitializeRates");
	return true;
}

bool ModelFromC::SetupDLLData()
{
	if(!cInitModel)
	{
		Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
		return false;
	}

	//This setup up data in the DLL...
	if(cInitModel() != 0)
	{
		Log(lError)<<"Failed to InitModel in "<<__FUNCTION__;
		return false;
	}

	char* modelName = cGetModelName();
	if(modelName)
	{
		mModelName = modelName;
	}

	//Simple variables...
	int *test = (int*) GetProcAddress((HMODULE) mDLLHandle, "numIndependentVariables");
	numIndependentVariables = test;

	test = (int*) GetProcAddress((HMODULE) mDLLHandle, "numDependentVariables");
	numDependentVariables = test;

	test = (int*) GetProcAddress((HMODULE) mDLLHandle, "numTotalVariables");
	numTotalVariables = test;

	test = (int*) GetProcAddress((HMODULE) mDLLHandle, "numBoundaryVariables");
	numBoundaryVariables = test;

	test = (int*) GetProcAddress((HMODULE) mDLLHandle, "numGlobalParameters");
	numGlobalParameters = test;

	test = (int*) GetProcAddress((HMODULE) mDLLHandle, "numCompartments");
	numCompartments = test;

	test = (int*) GetProcAddress((HMODULE) mDLLHandle, "numReactions");
	numReactions = test;

	test = (int*) GetProcAddress((HMODULE) mDLLHandle, "numEvents");
	numEvents = test;

	mAmounts  = (double*) GetProcAddress((HMODULE) mDLLHandle, "_amounts");
	if(!mAmounts)
	{
		Log(lError)<<"Failed to assign to mAmounts";
		return false;
	}

	m_dydt  = (double*) GetProcAddress((HMODULE) mDLLHandle, "_dydt");
	if(!m_dydt)
	{
		Log(lError)<<"Failed to assign to m_dydt";
        return false;
    }

    mY  = (double*) GetProcAddress((HMODULE) mDLLHandle, "_y");
    if(!mY)
    {
		Log(lError)<<"Failed to assign to mY";
        return false;
    }

    mRates  = (double*) GetProcAddress((HMODULE) mDLLHandle, "_rates");
    if(!mY)
    {
		Log(lError)<<"Failed to assign to mRateLaws";
        return false;
    }

    time	   = (double*) GetProcAddress((HMODULE) mDLLHandle, "_time");
    if(!time)
    {
		Log(lError)<<"Failed to assign to time";
        return false;
	}

    mInitY	   = (double*) GetProcAddress((HMODULE) mDLLHandle, "_init_y");
    if(!mInitY)
    {
		Log(lError)<<"Failed to assign to mInitY";
        return false;
    }

    mGP	   = (double*) GetProcAddress((HMODULE) mDLLHandle, "_gp");
    if(!mGP)
    {
		Log(lError)<<"Failed to assign to mGP";
        return false;
    }

    return true;
}

HANDLE ModelFromC::GetFunctionPtr(const string& funcName)
{
	HANDLE handle = GetProcAddress((HMODULE) mDLLHandle, funcName.c_str());
    if(handle == NULL)
    {
        Log(lError) << "Unable to load the function: " << funcName;
        return NULL;
    }
    Log(lInfo)<<"Loaded function " << funcName;
    return handle;
}

void ModelFromC::setCompartmentVolumes()
{
    if(!csetCompartmentVolumes)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
		return;
	}

	csetCompartmentVolumes();
}

vector<double> ModelFromC::GetCurrentValues()
{
    vector<double> vals;
    if(!cGetCurrentValues)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return vals;
	}

	double* values = cGetCurrentValues();     //The size of this double* is mMapRateRule.size(); ??

    int count = mCodeGenerator->NumAdditionalRates();
    if(values)
    {
    	for(int i = 0; i < count; i++)
        {
			vals.push_back(values[i]);
        }
    }

	return vals;
}

vector<double> ModelFromC::GetdYdT()
{
	//Copy values from dll to vector
    int nrSpecies = mCodeGenerator->getFloatingSpeciesConcentrationList().size();
    dydt.resize(nrSpecies);
    for(int i = 0; i < nrSpecies; i++)
    {
		dydt[i] = m_dydt[i];
    }
    return dydt;

}

double ModelFromC::getConcentration(int index)
{
	if(!cgetConcentration)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
		return 0;
    }

	return cgetConcentration(index);
}

int ModelFromC::getNumLocalParameters(int reactionId)
{
	if(!cgetNumLocalParameters)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return 0;
	}

	return cgetNumLocalParameters(reactionId);
}

//////Virtual functions that should be implemented in decendant..
void ModelFromC::initializeInitialConditions()
{
	if(!cinitializeInitialConditions)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
		return;
    }
	cinitializeInitialConditions();
}

//void ModelFromC::setInitialConditions(){}
void ModelFromC::setParameterValues()
{
	if(!csetParameterValues)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
		return;
    }
    csetParameterValues();
}

void ModelFromC::setBoundaryConditions()
{
    if(!csetBoundaryConditions)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}
    csetBoundaryConditions();
}

void ModelFromC::InitializeRates()
{
    if(!cInitializeRates)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}
    cInitializeRates();
}

void ModelFromC::AssignRates()
{
    if(!cAssignRates_a)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}
    cAssignRates_a();
}

void ModelFromC::AssignRates(vector<double>& _rates)
{
    if(!cAssignRates_b)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}

	auto_ptr<double> rates(new double(_rates.size()));
	for(int i = 0; i < _rates.size(); i++)
	{
		(rates).get()[i] = _rates[i];
	}

    cAssignRates_b(rates.get());
}

void ModelFromC::computeConservedTotals()
{
    if(!ccomputeConservedTotals)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}
	ccomputeConservedTotals();
}

//void ModelFromC::computeEventPriorites(){}
//void ModelFromC::setConcentration(int index, double value){}
void ModelFromC::convertToAmounts()
{
    if(!cconvertToAmounts)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}
	cconvertToAmounts();
}

void ModelFromC::convertToConcentrations()
{
	if(!cconvertToConcentrations)
	{
		Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
		return;
	}
	cconvertToConcentrations();
}

void ModelFromC::updateDependentSpeciesValues(vector<double>& y)
{
	if(!cupdateDependentSpeciesValues)
	{
		Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
		return;
	}

	int size = y.size();
	double* y_vec = new double[y.size()];
	for(int i = 0; i < y.size(); i++)
	{
		y_vec[i] = y[i];
	}

	cupdateDependentSpeciesValues(y_vec);
	delete [] y_vec;
}

void ModelFromC::computeRules(vector<double>& y)
{

	if(!ccomputeRules)
	{
		Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
		return;
	}

	auto_ptr<double> y_vec (new double(y.size()));
	ccomputeRules(y_vec.get());

}

void ModelFromC::setInitialConditions()
{
    if(!csetInitialConditions)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}

    csetInitialConditions();
}

//void ModelFromC::computeReactionRates(double time, vector<double>& y){}
void ModelFromC::computeAllRatesOfChange()
{
	if(!ccomputeAllRatesOfChange)
	{
		Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
		return;
	}
	ccomputeAllRatesOfChange();
}

void ModelFromC::evalModel(double timein, vector<double>& y)
{
	if(!cevalModel)
	{
		Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
		return;
	}

	//copy y values to mAmounts
	for(u_int i = 0; i < y.size(); i++)
    {
    	mAmounts[i] = y[i];
    	//y[i] = mAmounts[i];
    }

    cevalModel(timein, mAmounts);

}
void ModelFromC::evalEvents(double timeIn, vector<double>& y)
{
    if(!cevalEvents)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}

    cevalEvents(timeIn, mAmounts);

}

void ModelFromC::resetEvents()
{
    if(!cresetEvents)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}

    cresetEvents();
}

void ModelFromC::evalInitialAssignments()
{
    if(!cevalInitialAssignments)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}

    cevalInitialAssignments();
}

void ModelFromC::testConstraints()
{
    if(!ctestConstraints)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}

    ctestConstraints();
}

void ModelFromC::InitializeRateRuleSymbols()
{
    if(!cInitializeRateRuleSymbols)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}

    cInitializeRateRuleSymbols();
}

}//Namespace rr
