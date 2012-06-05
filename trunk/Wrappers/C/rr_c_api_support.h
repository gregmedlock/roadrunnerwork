//---------------------------------------------------------------------------
#ifndef rr_c_api_supportH
#define rr_c_api_supportH
#include <string>
#include "rrRoadRunner.h"
//---------------------------------------------------------------------------

using std::string;

//Error/Warning Messages
const char ALLOCATE_API_ERROR_MSG[] = {"Please allocate a handle to the roadrunner API before calling any API function"};

//Internal prototypes
void SetAPIError(const string& err);



#endif
