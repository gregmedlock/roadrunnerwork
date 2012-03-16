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
{}

double ModelFromC::getConcentration(int index)
{}

int ModelFromC::getNumLocalParameters(int reactionId)
{}

}//Namespace rr