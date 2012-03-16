#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <iostream>
#include "rrLogger.h"
#include "rrModelFromC.h"
#include "rrCGenerator.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

using namespace std;
namespace rr
{

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

bool ModelFromC::SetupDLLFunctions()
{
	//Exported functions in the dll need to be assigned to a function pointer here..
    if(!mDLLHandle)
    {
    	Log(lError)<<"DLL handle not valid in SetupModel function";
        return false;
    }

    //Load functions..
    cInitModel                        = (c_int)					GetFunctionPtr("InitModel");
    cGetModelName                     = (c_charStar)			GetFunctionPtr("GetModelName");
    cinitializeInitialConditions      = (c_void)                GetFunctionPtr("initializeInitialConditions");
    csetParameterValues               = (c_void)                GetFunctionPtr("setParameterValues");
    csetCompartmentVolumes            = (c_void)  				GetFunctionPtr("setCompartmentVolumes");
    cgetNumLocalParameters            = (c_int_int)             GetFunctionPtr("getNumLocalParameters");
    csetBoundaryConditions            = (c_void)                GetFunctionPtr("setBoundaryConditions");
    csetInitialConditions             = (c_void)                GetFunctionPtr("setInitialConditions");
    cevalInitialAssignments           = (c_void)                GetFunctionPtr("evalInitialAssignments");
    ccomputeRules                     = (c_void_doubleStar)     GetFunctionPtr("computeRules");
    cconvertToAmounts                 = (c_void)                GetFunctionPtr("convertToAmounts");
    ccomputeConservedTotals           = (c_void)                GetFunctionPtr("computeConservedTotals");
    cgetConcentration                 = (c_double_int)          GetFunctionPtr("getConcentration");
    cGetCurrentValues                 = (c_doubleStar_void)     GetFunctionPtr("GetCurrentValues");

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

bool ModelFromC::SetupDLLData()
{
    if(!cInitModel)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return false;
	}

    int res = cInitModel();
	char* modelName = cGetModelName();
    if(modelName)
    {
    	mModelName = modelName;
    }

    return true;
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
void  ModelFromC::initializeInitialConditions()
{
	if(!cinitializeInitialConditions)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
		return;
    }
    cinitializeInitialConditions();
}

//void  ModelFromC::setInitialConditions(){}
void  ModelFromC::setParameterValues()
{
	if(!csetParameterValues)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
		return;
    }
    csetParameterValues();
}

void  ModelFromC::setBoundaryConditions()
{
    if(!csetBoundaryConditions)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}
    csetBoundaryConditions();
}

//void  ModelFromC::InitializeRates(){}
//void  ModelFromC::AssignRates(){}
//void  ModelFromC::AssignRates(vector<double>& rates){}
void  ModelFromC::computeConservedTotals()
{
    if(!ccomputeConservedTotals)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}
	ccomputeConservedTotals();
}

//void  ModelFromC::computeEventPriorites(){}
//void  ModelFromC::setConcentration(int index, double value){}
void  ModelFromC::convertToAmounts()
{
    if(!cconvertToAmounts)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}
	cconvertToAmounts();
}

//void  ModelFromC::convertToConcentrations(){}
//void  ModelFromC::updateDependentSpeciesValues(vector<double>& _y){}

void  ModelFromC::computeRules(vector<double>& y)
{

    if(!ccomputeRules)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}

	double *y_vec = new double(y.size());
    ccomputeRules(y_vec);
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

//void  ModelFromC::computeReactionRates(double time, vector<double>& y){}
//void  ModelFromC::computeAllRatesOfChange(){}
//void  ModelFromC::evalModel(double time, vector<double>& y){}
//void  ModelFromC::evalEvents(double time, vector<double>& y){}
//void  ModelFromC::resetEvents(){}

void  ModelFromC::evalInitialAssignments()
{
    if(!cevalInitialAssignments)
    {
    	Log(lError)<<"Tried to call NULL function in "<<__FUNCTION__;
        return;
	}

    cevalInitialAssignments();
}

//void  ModelFromC::testConstraints(){}
//void  ModelFromC::InitializeRateRuleSymbols(){}

}//Namespace rr
