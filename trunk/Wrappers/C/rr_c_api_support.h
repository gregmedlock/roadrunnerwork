//---------------------------------------------------------------------------
#ifndef rr_c_api_supportH
#define rr_c_api_supportH
#include <string>
#include <vector>
#include "libstruct/lsMatrix.h"
#include "rrStringList.h"
#include "rrStringListContainer.h"
#include "rrArrayList.h"
#include "rrArrayList2.h"
#include "rrUtils.h"
#include "rr_c_types.h"
//---------------------------------------------------------------------------

using std::vector;
using std::string;

namespace rr_c_api
{
using rr::StringList;
using rr::ArrayList;
using rr::RRArrayList;

//Error/Warning Messages
extern const char* ALLOCATE_API_ERROR_MSG;

//Internal prototypes (not exported)
void                setError(const string& err);
bool                copyVector(const RRVector* source, vector<double>& dest);

RRVector*           createVectorFromVector_double(const vector<double>& vec);
vector<double>      createVectorFromRRVector(const RRVector* vec);
char*               createText(const char* str);        //To be deleted by client using freeText
char*               createText(const string& str);      //To be deleted by client using freeText
RRMatrix*           createMatrix(const LIB_LA::DoubleMatrix& mat);
RRStringArrayHandle createList(const rr::StringList& aList);
//RRStringList*       createList(const rr::ArrayList& aList);
cRRList*            createList(const ArrayList& arrList);
//RRStringArrayList*  createList(const rr::RRArrayList<string>& aList);
RRStringArrayHandle createList(const RRArrayList<string>& aList);
//cRRList*            createList(const rr::ArrayList2& aList);

// List support routines
cRRList* createList ();
cRRListItemHandle addItem (cRRListHandle list, int value);
cRRListItemHandle addItem (cRRListHandle list, double value);
cRRListItemHandle addItem (cRRListHandle list, char* value);
cRRListItemHandle addItem (cRRListHandle list, cRRList* value);

}

#endif
