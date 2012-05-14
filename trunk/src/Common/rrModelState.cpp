#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrUtils.h"
#include "rrModelState.h"
//---------------------------------------------------------------------------

namespace rr
{
ModelState::ModelState(ModelFromC& model)
{
	InitializeFromModel(model);
}

void ModelState::InitializeFromModel(ModelFromC& model)
{
    model.convertToConcentrations();
    CopyCArrayToStdVector(model.y,		  				  mFloatingSpeciesConcentrations, 	*model.ySize);
    CopyCArrayToStdVector(model.bc,                       mBoundarySpeciesConcentrations, 	*model.bcSize);
    CopyCArrayToStdVector(model.c,                        mCompartmentVolumes, 				*model.cSize);
    CopyCArrayToStdVector(model.gp,                       mGlobalParameters, 				*model.gpSize);
    CopyCArrayToStdVector(model.ct,                       mConservedTotals,	 				*model.ctSize);
    CopyCArrayToStdVector(model.dydt,                     mDyDt, 							*model.dydtSize);
    CopyCArrayToStdVector(model.rates,                    mRates, 							*model.ratesSize);
    CopyCArrayToStdVector(model.rateRules,                mRateRules, 						model.rateRulesSize);
    CopyCArrayToStdVector(model.sr,                       mModifiableSpeciesReferences, 	*model.srSize);
    CopyCArrayToStdVector(model.eventStatusArray,         mEventStatusArray, 				*model.eventStatusArraySize);
    CopyCArrayToStdVector(model.eventTests,               mEventTests, 						*model.eventTestsSize);
    CopyCArrayToStdVector(model.previousEventStatusArray, mPreviousEventStatusArray, 		*model.previousEventStatusArraySize);
	mTime = *model.time;
}

void ModelState::AssignToModel(ModelFromC& model)
{
	CopyStdVectorToCArray(mFloatingSpeciesConcentrations,	model.y,					   *model.ySize							);
	CopyStdVectorToCArray(mBoundarySpeciesConcentrations,	model.bc,					   *model.bcSize						);
	CopyStdVectorToCArray(mCompartmentVolumes,				model.c,					   *model.cSize							);
	CopyStdVectorToCArray(mGlobalParameters,				model.gp,					   *model.gpSize						);
	CopyStdVectorToCArray(mConservedTotals,					model.ct,					   *model.ctSize						);
	CopyStdVectorToCArray(mDyDt,							model.dydt,					   *model.dydtSize						);
	CopyStdVectorToCArray(mRates,							model.rates,				   *model.ratesSize						);
	CopyStdVectorToCArray(mRateRules,						model.rateRules,			   model.rateRulesSize					);
	CopyStdVectorToCArray(mEventTests,						model.eventTests,			   *model.eventTestsSize				);
	CopyStdVectorToCArray(mEventStatusArray,				model.eventStatusArray,		   *model.eventStatusArraySize			);
	CopyStdVectorToCArray(mPreviousEventStatusArray,		model.previousEventStatusArray,*model.previousEventStatusArraySize	);
	CopyStdVectorToCArray(mModifiableSpeciesReferences,		model.sr, 					   *model.srSize							);
	model.convertToAmounts();
	model.SetTime(mTime);
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


