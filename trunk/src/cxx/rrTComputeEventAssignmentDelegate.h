#ifndef rrTComputeEventAssignmentDelegateH
#define rrTComputeEventAssignmentDelegateH
#include <vector>
#include "rrExporter.h"

using std::vector;

namespace rr
{
	//Function pointer returning a double taking no arg
	typedef vector<double> (*TComputeEventAssignmentDelegate)();
}

#endif

//C#
//namespace LibRoadRunner
//{
//    public delegate double[] TComputeEventAssignmentDelegate();
//}

