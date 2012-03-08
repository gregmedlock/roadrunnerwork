#ifndef modelH
#define modelH
#include <stdio.h>

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


D_S struct TModel
{
	char*				mModelName;
	char**				mWarnings;
	double _gp[1];		// Vector containing all the global parameters in the System  
} gTheModel;	//This is global data in the DLL

//EXPORTS ========================================
D_S int InitModel();
D_S char* GetModelName();
#endif //modelH
