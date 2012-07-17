#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include "rrCapability.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

namespace rr
{


Capability(const string& name, const string& value, const string& hint, const string& type)
{
    Hint = hint;
    Name = name;
    Type = type;
    Value = value;
}

}
