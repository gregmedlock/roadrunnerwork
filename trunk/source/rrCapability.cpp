#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <sstream>
#include "rrCapability.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)


namespace rr
{

Capability::Capability(const string& name, const string& hint)
:
mName(name),
mHint(hint)
{

}

string Capability::AsString() const
{
    stringstream val;

    val<<"Name: "<<mName<<endl;
    val<<"Value: "<<ValueAsString()<<endl;
    val<<"HInt: "<<mHint<<endl;
    val<<"Type: "<<GetType()<<endl;
    return val.str();
}

string Capability::GetType() const
{
    string val("no info");

    //Downcasts
    Capability* ptr = const_cast<Capability*>(this);

    if(dynamic_cast< CapabilityType<int>* >(ptr))
    {
        return "integer";
    }

    if(dynamic_cast< CapabilityType<double>* >(ptr))
    {
        return "double";
    }

    if(dynamic_cast< CapabilityType<bool>* >(ptr))
    {
        return "boolean";
    }

    return val;
}


}
