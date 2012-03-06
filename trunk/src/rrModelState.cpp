#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrModelState.h"
//---------------------------------------------------------------------------
#if defined(__CODEGEARC__)
#pragma package(smart_init)
#endif
//---------------------------------------------------------------------------


namespace rr
{
ModelState::ModelState(IModel& model)
{
	InitializeFromModel(model);
}

void ModelState::InitializeFromModel(IModel& model)
{
    model.convertToConcentrations();
    _FloatingSpeciesConcentrations 	= GetCopy(model.Get_y());
    _BoundarySpeciesConcentrations 	= GetCopy(model.Get_bc());
    _CompartmentVolumes 			= GetCopy(model.Get_c());
    _GlobalParameters 				= GetCopy(model.Get_gp());
    _ConservedTotals 				= GetCopy(model.Get_ct());
    _DyDt 							= GetCopy(model.Get_dydt());
    _Rates 							= GetCopy(model.Get_rates());
    _RateRules 						= GetCopy(model.Get_rateRules());
    _ModifiableSpeciesReferences 	= GetCopy(model.Get_sr());
    _Time 							= model.Get_time();
    _EventStatusArray 		   		= GetCopy(model.Get_eventStatusArray());
    _EventTests 			   		= GetCopy(model.Get_eventTests());
    _PreviousEventStatusArray  		= GetCopy(model.Get_previousEventStatusArray());
}

vector<double> ModelState::GetCopy(const vector<double>& oVector)
{
    return vector<double>(oVector);
}

vector<bool> ModelState::GetCopy(const vector<bool>& oVector)
{
    return vector<bool>(oVector);
}


//        public void WriteTo(string fileName)
//        {
//            var stream = new FileStream(fileName, FileMode.Create);
//            WriteTo(stream);
//        }
//
//        public static ModelState ReadFrom(Stream stream)
//        {
//            var formatter = new BinaryFormatter();
//            var state = (ModelState) formatter.Deserialize(stream);
//            stream.Close();
//            return state;
//        }
//
//        public static ModelState ReadFrom(string fileName)
//        {
//            var stream = new FileStream(fileName, FileMode.Open);
//            return ReadFrom(stream);
//        }
//
//        public void WriteTo(Stream stream)
//        {
//            var formatter = new BinaryFormatter();
//            formatter.Serialize(stream, this);
//            stream.Flush();
//            stream.Close();
//        }
//
void ModelState::AssignToModel(IModel& model)
{
   model.y = _FloatingSpeciesConcentrations;
   model.bc = _BoundarySpeciesConcentrations;
   model.c = _CompartmentVolumes;
   model.gp = _GlobalParameters;
   model.ct = _ConservedTotals;

   model.dydt = _DyDt;
   model.rates = _Rates;
   model.rateRules = _RateRules;

   model.eventTests = _EventTests;
   model.eventStatusArray = _EventStatusArray;
   model.previousEventStatusArray = _PreviousEventStatusArray;
   model.time = _Time;
   model.convertToAmounts();

   model.sr = _ModifiableSpeciesReferences;
}



}


