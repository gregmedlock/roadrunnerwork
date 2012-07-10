#ifdef USE_PCH
#include "rr_pch.h"
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
mEndTime(mStartTime + mDuration),
mAbsolute(1.e-7),
mRelative(1.e-4)
{}


}
