#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include "rrLogger.h"
#include "rrStopWatch.h"
//---------------------------------------------------------------------------

namespace rr
{
StopWatch::StopWatch()
:
mStartTime(std::clock()), //mStartTime counting time
mIsRunning(true)
{
}

StopWatch::~StopWatch()
{
//    mTotalTime = clock() - mStartTime; //get elapsed time
//    Log(lDebug4)<<"total of ticks for this activity: "<<mTotalTime<<endl;
//    Log(lDebug4)<<"in seconds: "<<double(mTotalTime)/CLOCKS_PER_SEC<<endl;
}

void StopWatch::Start()
{
    mStartTime = std::clock();
    mIsRunning = true;
}
void StopWatch::Stop()
{
     mTotalTime = clock() - mStartTime; //get elapsed time
     mIsRunning = false;
     mStartTime = 0;
}

double StopWatch::GetTime()
{
    if(mIsRunning)
    {
        return (clock() - mStartTime) /CLOCKS_PER_SEC;; //get elapsed time
    }
    else
    {
        return double(mTotalTime)/CLOCKS_PER_SEC;
    }
}

}
