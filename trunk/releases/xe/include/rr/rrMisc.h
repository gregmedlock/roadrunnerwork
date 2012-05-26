#ifndef rrMiscH
#define rrMiscH
#include <string>
#include <iomanip>
#include <ostream>
#include "rrExporter.h"
using std::ostream;

using std::string;
using std::endl;
//---------------------------------------------------------------------------
namespace rr
{

enum TSelectionType
{
        clTime = 0,
        clBoundarySpecies,
        clFloatingSpecies,
        clFlux,
        clRateOfChange,
        clVolume,
        clParameter,
/*7*/   clFloatingAmount,
/*8*/   clBoundaryAmount,
        clElasticity,
        clUnscaledElasticity,
        clEigenValue,
        clUnknown,
        clStoichiometry
};

struct RR_DECLSPEC TSelectionRecord
{
    unsigned int        index;
    string              p1;
    string              p2;
    TSelectionType      selectionType;
                        TSelectionRecord();

};

ostream& operator<< (ostream& stream, const TSelectionRecord& rec);
}


#endif
