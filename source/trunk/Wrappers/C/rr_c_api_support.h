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
void                setError(const string& err);
bool                copyVector(const RRVector* source, vector<double>& dest);

RRVector*           createVector(const vector<double>& vec);
char*               createText(const string& str);      //To be deleted by client using freeText
RRMatrix*           createMatrix(const LIB_LA::DoubleMatrix& mat);
RRStringList*       createList(const StringList& aList);
RRStringList*       createList(const ArrayList& aList);

}

#endif
