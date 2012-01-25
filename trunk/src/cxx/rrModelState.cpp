#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrModelState.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

namespace rr
{
ModelState::ModelState(IModel& model)
{
	InitializeFromModel(model);
}

void ModelState::InitializeFromModel(IModel& model)
{
    model.convertToConcentrations();
    _FloatingSpeciesConcentrations = GetCopy(model.Get_y());
    _BoundarySpeciesConcentrations = GetCopy(model.Get_bc());
    _CompartmentVolumes = GetCopy(model.Get_c());
    _GlobalParameters = GetCopy(model.Get_gp());
    _ConservedTotals = GetCopy(model.Get_ct());
    _DyDt = GetCopy(model.Get_dydt());
    _Rates = GetCopy(model.Get_rates());
    _RateRules = GetCopy(model.Get_rateRules());
    _ModifiableSpeciesReferences = GetCopy(model.Get_sr());
    _Time = model.Get_time();

    _EventStatusArray 		   = GetCopy(model.Get_eventStatusArray());
    _EventTests 			   = GetCopy(model.Get_eventTests());
    _PreviousEventStatusArray  = GetCopy(model.Get_previousEventStatusArray());
}


vector<double> ModelState::GetCopy(const vector<double>& oVector)
{
    return vector<double>(oVector);
}

vector<bool> ModelState::GetCopy(const vector<bool>& oVector)
{
//    vector<bool> oResult(oVector);
    return vector<bool>(oVector);
}

}


