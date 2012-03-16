#ifndef rrModelFromCH
#define rrModelFromCH
#include <windows.h>
#include "rrIModel.h"
//---------------------------------------------------------------------------

namespace rr
{

typedef void 	(WINAPI*cfunc_void_none)();
typedef int 	(WINAPI*cfunc_int_none)();
typedef char* 	(WINAPI*cfunc_charStar_none)();


class RR_DECLSPEC ModelFromC : public IModel	//This model sets up nnecessary handles to C DLL functions
{
	protected:
        bool						mIsInitialized;	//If all functions are found properly in the dll, this one is true
		HINSTANCE					mDLLHandle;

        cfunc_void_none 			CsetCompartmentVolumes;	//Prefix c function pointers with "C"
        cfunc_int_none 				CInitModel;
        cfunc_charStar_none 		CGetModelName;
		HANDLE 						GetFunctionPtr(const string& function);


    public:
						    		ModelFromC();
                                   ~ModelFromC();
		bool						SetupFunctions(HINSTANCE dllHandle);
    	void 						setCompartmentVolumes();
        vector<double> 				GetCurrentValues();
        double 						getConcentration(int index);
        int 						getNumLocalParameters(int reactionId);

};

}


#endif
