#include "00001-sbml-l2v4.h"


//Initialize DLL data, i.e. the TModel struct, and return integer indicating result
D_S int InitModel()
{
	strcpy(gTheModel.mModelName,"00001-sbml-l2v4.xml");
	gTheModel._gp[0] = 1234;
	return 0;
}

D_S char* GetModelName()
{
	return gTheModel.mModelName;
}
