#ifndef rrRandomH
#define rrRandomH
//---------------------------------------------------------------------------
#include "rrObject.h"
#include "mtrand/mtrand.h"

namespace rr
{

class RR_DECLSPEC Random : public rrObject, public MTRand_open
{

    private:
    public:
        Random();
};

}
#endif
