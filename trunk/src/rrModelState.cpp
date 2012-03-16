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
    mFloatingSpeciesConcentrations 	= GetCopy(model.y);
    mBoundarySpeciesConcentrations 	= GetCopy(model.bc);
    mCompartmentVolumes 			= GetCopy(model.c);
    mGlobalParameters 				= GetCopy(model.gp);
    mConservedTotals 				= GetCopy(model.ct);
    mDyDt 							= GetCopy(model.dydt);
    mRates 							= GetCopy(model.rates);
    mRateRules 						= GetCopy(model.rateRules);
    mModifiableSpeciesReferences 	= GetCopy(model.sr);
    mTime 							= model.time;
    mEventStatusArray 		   		= GetCopy(model.eventStatusArray);
    mEventTests 			   		= GetCopy(model.eventTests);
    mPreviousEventStatusArray  		= GetCopy(model.previousEventStatusArray);
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
   model.y = mFloatingSpeciesConcentrations;
   model.bc = mBoundarySpeciesConcentrations;
   model.c = mCompartmentVolumes;
   model.gp = mGlobalParameters;
   model.ct = mConservedTotals;

   model.dydt = mDyDt;
   model.rates = mRates;
   model.rateRules = mRateRules;

   model.eventTests = mEventTests;
   model.eventStatusArray = mEventStatusArray;
   model.previousEventStatusArray = mPreviousEventStatusArray;
   model.time = mTime;
   model.convertToAmounts();

   model.sr = mModifiableSpeciesReferences;
}



}


