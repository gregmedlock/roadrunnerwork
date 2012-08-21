#ifndef rrStopWatchH
#define rrStopWatchH
//---------------------------------------------------------------------------
#include <time.h>
#include "rrObject.h"

namespace rr
{

class RR_DECLSPEC StopWatch : public rrObject
{
    private:
        std::clock_t 		mStartTime;
        std::clock_t 		mTotalTime;
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
