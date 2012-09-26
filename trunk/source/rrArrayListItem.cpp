#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include "rrArrayListItem.h"
#include "rrArrayList2.h"
//---------------------------------------------------------------------------

namespace rr
{

ArrayList2Item::ArrayList2Item()
: mValue(NULL)
{
    mValue = new ArrayList2;
}

ArrayList2Item::~ArrayList2Item()
{
    delete mValue;
}

ArrayList2Item::ArrayList2Item(const ArrayList2Item& copyMe)
{
    this->mValue = new ArrayList2;
    (*this->mValue) = (*copyMe.mValue);
}

ArrayList2Item& ArrayList2Item::operator=(const ArrayList2Item& list)
{
    if(this != &list)
    {
        (*this->mValue) = (*list.mValue);
    }

    return *this;
}

ArrayList2Item::ArrayList2Item(const ArrayList2& list)
{
    mValue = new ArrayList2(list);
}

unsigned int ArrayList2Item::Count() const
{
    return (mValue) ? mValue->Count() : 0;
}

const ArrayListItemObject& ArrayList2Item::operator[](int pos) const
{
    return (*mValue)[pos];
}

ArrayListItemObject& ArrayList2Item::operator[](int pos)
{
    return (*mValue)[pos];
}

ostream& operator<<(ostream& stream, const ArrayListItemObject& item)
{
    //Have to figure out subtype of item
    ArrayListItemObject* ptr = const_cast<ArrayListItemObject*>(&item);
    if(dynamic_cast<ArrayListItem<int>*>(ptr))
    {
        stream << (int) *(dynamic_cast<ArrayListItem<int>*>(ptr));
    }
    else if(dynamic_cast<ArrayListItem<double>*>(ptr))
    {
        stream << (double) *(dynamic_cast<ArrayListItem<double>*>(ptr));
    }
    else if(dynamic_cast<ArrayListItem<string>*>(ptr))
    {
        stream << "\""<<(string) *(dynamic_cast<ArrayListItem<string>*>(ptr))<<"\"";
    }
    else if(dynamic_cast<ArrayListItem<StringList>*>(ptr))
    {
        stream << "\""<<(StringList) *(dynamic_cast<ArrayListItem<StringList>*>(ptr))<<"\"";
    }

    else if(dynamic_cast<ArrayListItem<ArrayList2Item>*>(ptr))
    {
        stream << (ArrayList2Item) *(dynamic_cast<ArrayListItem<ArrayList2Item>*>(ptr));
    }
    else
    {
        stream<<"Stream operator not implemented for this type";
    }
    return stream;
}

template<>
const char ArrayListItem<string>::operator[](const int& pos) const
{
    return (char) mItemValue[pos];
}

ostream& operator<<(ostream& stream, const ArrayList2Item& item)
{
    stream<<"{";
    for(int i = 0; i < item.Count(); i++)
    {
        stream<<item[i];
        if(i < item.Count() -1)
        {
        	stream<<",";
        }
    }

    stream<<"}";
    return stream;
}

}





