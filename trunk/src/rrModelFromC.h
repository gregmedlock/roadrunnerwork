#ifndef rrModelFromCH
#define rrModelFromCH
#include "rrIModel.h"
//---------------------------------------------------------------------------

namespace rr
{
class RR_DECLSPEC ModelFromC : public IModel	//This model sets up nnecessary handles to C DLL functions
{
	protected:

    public:
						    		ModelFromC();
                                   ~ModelFromC();
    	void 						setCompartmentVolumes();
        vector<double> 				GetCurrentValues();
        double 						getConcentration(int index);
        int 						getNumLocalParameters(int reactionId);

};

}


#endif
