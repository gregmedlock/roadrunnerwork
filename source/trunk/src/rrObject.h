#ifndef rrObjectH
#define rrObjectH
#include <string>
#include <limits>
#include "rrExporter.h"

namespace rr
{

using namespace std;

const char tab = '\t';
enum SBMLType {stCompartment = 0, stSpecies, stParameter};    //Species clashes with class Species, prefix enums with st, for SbmlType
const int MAX_MODULE = 512;
typedef unsigned int u_int;

const double DoubleNaN = std::numeric_limits<double>::quiet_NaN() ;
const float  FloatNaN  = std::numeric_limits<float>::quiet_NaN() ;

//Have all RoadRunner classes descending from rrObject
class RR_DECLSPEC rrObject
{
    protected:

    public:
                        rrObject();
        virtual        ~rrObject();
};

}
#endif
