#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <sstream>
#include "rrArrayList.h"
//---------------------------------------------------------------------------

namespace rr
{


template <>
RRArrayListItem<string>::operator string()
{
    if(mValue)
    {
        return *mValue;
    }

    if(mLinkedList)
    {
        return mLinkedList->AsString();
    }
    return "";
}

template <>
RRArrayListItem<int>::operator int()
{
    if(mValue)
    {
        return *mValue;
    }

    if(mLinkedList)
    {
        return mLinkedList->operator [](0);
    }
    return -1;
}



}

