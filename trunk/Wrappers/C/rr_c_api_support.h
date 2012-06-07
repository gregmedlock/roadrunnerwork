//---------------------------------------------------------------------------
#ifndef rr_c_api_supportH
#define rr_c_api_supportH
#include <string>
#include <vector>
#include "rrRoadRunner.h"
#include "rr_c_types.h"
//---------------------------------------------------------------------------

using std::vector;
using std::string;
namespace rr_c_api
{
//Error/Warning Messages
const char ALLOCATE_API_ERROR_MSG[] = {"Please allocate a handle to the roadrunner API before calling any API function"};

//Internal prototypes
void                SetAPIError(const string& err);
bool                CopyRRVector(const RRDoubleVector* vec, vector<double>& aVec);
RRDoubleVector*     CreateRRDoubleVecFrom(const vector<double>& vec);
}

#endif
