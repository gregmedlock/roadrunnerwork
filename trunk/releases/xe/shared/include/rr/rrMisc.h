#ifndef rrMiscH
#define rrMiscH
#include <string>
#include <iomanip>
#include "rrExporter.h"
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

struct TSelectionRecord
{
    unsigned int    index;
    string             p1;
    string             p2;
    TSelectionType selectionType;

};

ostream& operator<< (ostream& stream, const TSelectionRecord& rec);

ostream& operator<< (ostream& stream, const TSelectionRecord& rec)
{
    stream<<"Index: "<<rec.index<<endl;
    stream<<"p1: "<<rec.p1<<endl;
       stream<<"p2: "<<rec.p1<<endl;
       stream<<"SelectionType: "<<rec.selectionType<<endl;
    return stream;
}



}


#endif
