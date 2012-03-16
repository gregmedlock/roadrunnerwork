#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrModelFromC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

namespace rr
{

ModelFromC::ModelFromC()
{}

ModelFromC::~ModelFromC()
{}

void ModelFromC::setCompartmentVolumes()
{}

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