#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrModelState.h"
//---------------------------------------------------------------------------

namespace rr
{
ModelState::ModelState(IModel& model)
{
	InitializeFromModel(model);
}

void ModelState::InitializeFromModel(IModel& model)
{
	model.LoadData();

    model.convertToConcentrations();
    mFloatingSpeciesConcentrations 	= model.y;
    mBoundarySpeciesConcentrations 	= model.bc;
    mCompartmentVolumes 			= model.c;
    mGlobalParameters 				= model.gp;
    mConservedTotals 				= model.ct;
    mDyDt 							= model.dydt;
    mRates 							= model.rates;
    mRateRules 						= model.rateRules;
    mModifiableSpeciesReferences 	= model.sr;
    mTime 							= model.GetTime();
    mEventStatusArray 		   		= model.eventStatusArray;
    mEventTests 			   		= model.eventTests;
    mPreviousEventStatusArray  		= model.previousEventStatusArray;
}

void ModelState::AssignToModel(IModel& model)
{
   model.y 		                    = mFloatingSpeciesConcentrations;
   model.bc 	                    = mBoundarySpeciesConcentrations;
   model.c 		                    = mCompartmentVolumes;
   model.gp 	                    = mGlobalParameters;
   model.ct 	                    = mConservedTotals;
   model.dydt 	                    = mDyDt;
   model.rates 			            = mRates;
   model.rateRules 		            = mRateRules;
   model.eventTests 	            = mEventTests;
   model.eventStatusArray 	        = mEventStatusArray;
   model.previousEventStatusArray 	= mPreviousEventStatusArray;
   model.SetTime(mTime);
   model.convertToAmounts();
   model.sr = mModifiableSpeciesReferences;
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


}


