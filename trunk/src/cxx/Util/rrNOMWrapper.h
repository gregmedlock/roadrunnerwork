#ifndef rrNOMWrapperH
#define rrNOMWrapperH
//---------------------------------------------------------------------------
#include <vector>
#include <string>
#include "NOMLib.h"
#include "rrObject.h"

using std::vector;
using std::string;

//---------------------------------------------------------------------------
namespace rr
{

class RR_DECLSPEC NOMWrapper : public rrObject
{
    protected:
		//The C# SBMLSupport.cs have two statuc objects, SBMLDocument and Model, created when NOM reads SBML..

   	public:
						    	NOMWrapper();
    	virtual 			   ~NOMWrapper();

		string					getNthCompartmentId(const int& i);
        double					getValue(const string& id);

};

}


#endif
