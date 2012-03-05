#ifndef rrTPerformEventAssignmentDelegateH
#define rrTPerformEventAssignmentDelegateH
#include <vector>

using std::vector;

namespace rr
{
	typedef void (*TPerformEventAssignmentDelegate)(vector<double>& values);
}

#endif



//C#
//namespace LibRoadRunner
//{
//    public delegate void TPerformEventAssignmentDelegate(double[] values);
//}