#ifndef rrStopWatchH
#define rrStopWatchH
//---------------------------------------------------------------------------
#include <ctime>
#include "rrObject.h"

namespace rr
{

class RR_DECLSPEC StopWatch : public rrObject
{
    private:
        clock_t     		mStartTime;
        clock_t 	    	mTotalTime;
        bool 				mIsRunning;

    public:
        					StopWatch();
        				   ~StopWatch();
        void 				Start();
        void 				Stop();
        double 				GetTime();
};
}

#endif
