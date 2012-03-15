#ifndef rrMiscH
#define rrMiscH
#include <string>
#include "rrExporter.h"
using std::string;
//---------------------------------------------------------------------------
namespace rr
{

enum TSelectionType
{
    clTime,
    clBoundarySpecies,
    clFloatingSpecies,
    clFlux,
    clRateOfChange,
    clVolume,
    clParameter,
    clFloatingAmount,
    clBoundaryAmount,
    clElasticity,
    clUnscaledElasticity,
    clEigenValue,
    clUnknown,
    clStoichiometry
};

struct TSelectionRecord
{
    int 			index;
    string 			p1;
    string 			p2;
    TSelectionType selectionType;
};


}


#endif
