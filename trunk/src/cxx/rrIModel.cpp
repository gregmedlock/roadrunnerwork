#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrIModel.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

namespace rr
{

//get,set
vector<double>&	IModel::Get_y(){return y;}
vector<double>& IModel::Get_bc(){return bc;}
vector<double>&	IModel::Get_c(){return  c;}
vector<double>&	IModel::Get_gp(){return  gp;}
vector<double>&	IModel::Get_ct(){return  ct;}
vector<double>&	IModel::Get_dydt(){return  dydt;}
vector<double>&	IModel::Get_rates(){return  rates;}
vector<double>&	IModel::Get_rateRules(){return  rateRules;}
vector<double>&	IModel::Get_sr(){return  sr;}
double			IModel::Get_time(){return  time;}
vector<bool>&	IModel::Get_eventStatusArray(){return eventStatusArray;}
vector<double>&	IModel::Get_eventTests(){return eventTests;}
vector<bool>&	IModel::Get_previousEventStatusArray(){return previousEventStatusArray;}

IModel::~IModel(){}

} //namespace rr
