#ifndef rrSimulationSettingsH
#define rrSimulationSettingsH
#include "rrObject.h"

//---------------------------------------------------------------------------
namespace rr
{

class RR_DECLSPEC SimulationSettings : rrObject
{
	public:
    				SimulationSettings();

    	int			mSteps;
        double		mStartTime;
     	double		mDuration;
		double 		mEndTime;
        double		mRelative;		//what is this for??
};



} //End of namespace
#endif
