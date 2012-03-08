#include "00001-sbml-l2v4.h"


//Function to initialize the model data structure. Returns an integer indicating result
int InitModel()
{
	gTheModel.mModelName = (char*) malloc(sizeof(char)*10);
	strcpy(gTheModel.mModelName,"case00001");
	gTheModel._gp[0] = 1234;
	return 0;
}


char* GetModelName()
{
	return gTheModel.mModelName;
}
