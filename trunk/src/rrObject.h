#ifndef rrObjectH
#define rrObjectH
#include <string>
#include "rrExporter.h"

namespace rr
{
using std::string;

enum SBMLType {stCompartment = 0, stSpecies, stParameter};	//Species clashes with class Species, prefix enums with st, for SbmlType
const int MAX_MODULE = 512;
typedef unsigned int u_int;

//Have all RoadRunner classes descending from rrObject
class RR_DECLSPEC rrObject
{
	protected:

    public:
    					rrObject();
        virtual 	   ~rrObject();


};

}
#endif
