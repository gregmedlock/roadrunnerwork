#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrSimulationSettings.h"
//---------------------------------------------------------------------------
#if defined(__CODEGEARC__)
	#pragma package(smart_init)
#endif

namespace rr
{

SimulationSettings::SimulationSettings()
:
mSteps(50),
mStartTime(0),
mDuration(5),
mRelative(0.0001),
mEndTime(mStartTime + mDuration)
{}


}