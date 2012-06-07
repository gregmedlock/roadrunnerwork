#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include "rrMisc.h"

//---------------------------------------------------------------------------
namespace rr
{

TSelectionRecord::TSelectionRecord()
:
index(0),
p1(""),
p2(""),
selectionType(clUnknown)
{}

ostream& operator<< (ostream& stream, const TSelectionRecord& rec)
{
    stream<<"A Selection Record --"<<endl;
    stream<<"Index: "<<rec.index<<endl;
    stream<<"p1: "<<rec.p1<<endl;
    stream<<"p2: "<<rec.p1<<endl;
    stream<<"SelectionType: "<<rec.selectionType<<endl;
    return stream;
}

}
