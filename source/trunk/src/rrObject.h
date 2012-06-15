#ifndef rrObjectH
#define rrObjectH
#include <string>
#include <limits>
#include "rrExporter.h"

namespace rr
{
using namespace std;

// Enums...
enum SBMLType {stCompartment = 0, stSpecies, stParameter};    //Species clashes with class Species, prefix enums with st, for SbmlType

// Typedefs
typedef unsigned int u_int;
typedef long*   IntPtr;

// Constants
const char tab = '\t';
const double    DoubleNaN   = std::numeric_limits<double>::quiet_NaN() ;
const float     FloatNaN    = std::numeric_limits<float>::quiet_NaN() ;
const int       MAX_MODULE  = 512;

//Have all RoadRunner classes descending from a rrObject
class RR_DECLSPEC rrObject
{
    protected:

    public:
                        rrObject();
        virtual        ~rrObject();
};

}
#endif
