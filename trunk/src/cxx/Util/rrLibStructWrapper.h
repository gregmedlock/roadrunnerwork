#ifndef rrLibStructWrapperH
#define rrLibStructWrapperH
#include <vector>
#include <string>
#include "rrObject.h"
#include "libstructural.h"

using std::vector;
using std::string;

//---------------------------------------------------------------------------
namespace rr
{
using namespace LIB_STRUCTURAL;
class RR_DECLSPEC LibStructWrapper : public rrObject
{
    protected:
       	LibStructural		   *mInstance;

   	public:
						    	LibStructWrapper();
    	virtual 			   ~LibStructWrapper();
//        bool					LoadSBML(const string& sbml);
		LibStructural&			GetInstance(){return *mInstance;}
//        int						GetNumberOfIndependentSpecies();
//		vector<string>			GetIndependentSpecies();
//		vector<string> 			GetDependentSpecies();
//
//		int        				GetNumSpecies();
//		vector<string>			GetSpecies();

		vector<string>			GetReorderedSpeciesIds();

};

}
#endif
