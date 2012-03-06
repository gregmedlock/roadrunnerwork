#ifndef rrObjectH
#define rrObjectH
#include <string>
#include "rrExporter.h"

namespace rr
{
using std::string;

enum SBMLType {stCompartment = 0, stSpecies, stParameter};	//Species clashes with class Species, prefix enums with t, for SbmlType



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
