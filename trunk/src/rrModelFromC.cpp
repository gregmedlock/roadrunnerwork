#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <iostream>
#include "rrLogger.h"
#include "rrModelFromC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

using namespace std;
namespace rr
{

ModelFromC::ModelFromC()
:
mIsInitialized(false),
mDLLHandle(NULL)
{

}

ModelFromC::~ModelFromC()
{}

bool ModelFromC::SetupFunctions(HINSTANCE dllHandle)
{
	//Exported functions in the dll need to be assigned to a function pointer here..
    //First check if the DLL handle is valid..
    mDLLHandle = dllHandle;
    if(!mDLLHandle)
    {
    	Log(lError)<<"DLL handle not valid in SetupModel function";
        return false;
    }

    //Load functions..
    CInitModel = (cfunc_int_none) GetFunctionPtr("InitModel");

	CsetCompartmentVolumes 	= (cfunc_void_none) 		GetFunctionPtr(	"setCompartmentVolumes");
   	CGetModelName 			= (cfunc_charStar_none) 	GetFunctionPtr( "GetModelName");
//    if(GetModelName == NULL)
//    {
//        Log(lError) << "Unable to load function." << endl;
//        FreeLibrary((HMODULE) dllHandle);
//        PauseBeforeExit();
//    }


	return true;
}

HANDLE ModelFromC::GetFunctionPtr(const string& funcName)
{
	HANDLE handle = GetProcAddress((HMODULE) mDLLHandle, funcName.c_str());
    if(handle == NULL)
    {
        Log(lError) << "Unable to load the function: " << funcName<< endl;
        return NULL;
    }
    return handle;
}

void ModelFromC::setCompartmentVolumes()
{
	CsetCompartmentVolumes();
}

vector<double> ModelFromC::GetCurrentValues()
{
	return vector<double>(0);
}

double ModelFromC::getConcentration(int index)
{
	return 0;
}

int ModelFromC::getNumLocalParameters(int reactionId)
{
	return 0;
}

}//Namespace rr