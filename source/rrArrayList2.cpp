#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <sstream>
#include "rrArrayList2.h"
//---------------------------------------------------------------------------

namespace rr
{

ListItemBase::~ListItemBase()
{}

ArrayList2Type::ArrayList2Type()
: mValue(NULL)
{
    mValue = new ArrayList2;
}

ArrayList2Type::~ArrayList2Type()
{
    delete mValue;
}

ArrayList2Type::ArrayList2Type(const ArrayList2Type& copyMe)
{
    this->mValue = new ArrayList2;
    (*this->mValue) = (*copyMe.mValue);
}

ArrayList2Type& ArrayList2Type::operator=(const ArrayList2Type& list)
{
    (*this->mValue) = (*list.mValue);

    return *this;
}

ArrayList2Type::ArrayList2Type(const ArrayList2& list)
{
    mValue = new ArrayList2(list);
}

unsigned int ArrayList2Type::Count() const
{
    return (mValue) ? mValue->Count() : 0;
}

void ArrayList2::Clear()
{
    if(Count())
    {
        for(u_int i = 0; i < Count(); i++)
        {
            delete mList[i];
        }
        mList.clear();
    }
}

const ListItemBase& ArrayList2Type::operator[](int pos) const
{
    return (*mValue)[pos];
}

// ============= ArrayList2 ==========================
ArrayList2::ArrayList2()
{}


ArrayList2::~ArrayList2()
{
    if(mList.size())
    {
        for(u_int i = 0; i < Count(); i++)
        {
            delete mList[i];
        }
        mList.clear();
    }
}

unsigned int ArrayList2::Count() const
{
    return mList.size();
}

ArrayList2::ArrayList2(const ArrayList2& copyMe)
{
    //Deep copy
    Clear();
    mList.resize(copyMe.Count());
    for(u_int i = 0; i < copyMe.Count(); i++)
    {
        //const ArrayList2Item<T>& item = copyMe[i];
        ListItemBase* ptr = const_cast<ListItemBase*>(&copyMe[i]);
        if(dynamic_cast<ListItem<int>*>(ptr))
        {
            mList[i] = new ListItem<int>(*(dynamic_cast<ListItem<int>*>(ptr)));
        }
        else if(dynamic_cast<ListItem<double>*>(ptr))
        {
            mList[i] = new ListItem<double>(*(dynamic_cast<ListItem<double>*>(ptr)));
        }
        else if(dynamic_cast<ListItem<string>*>(ptr))
        {
            mList[i] = new ListItem<string>(*(dynamic_cast<ListItem<string>*>(ptr)));
        }
        else if(dynamic_cast<ListItem<ArrayList2Type>*>(ptr))
        {
            mList[i] = new ListItem<ArrayList2Type>(*(dynamic_cast<ListItem<ArrayList2Type>*>(ptr)));
        }
        else
        {
            mList[i] = NULL;
        }
    }
}

void ArrayList2::operator=(const ArrayList2& rhs)
{
    //Deep copy
    Clear();
    mList.resize(rhs.Count());
    for(u_int i = 0; i < rhs.Count(); i++)
    {
        ListItemBase* ptr = const_cast<ListItemBase*>(&rhs[i]);
        if(dynamic_cast<ListItem<int>*>(ptr))
        {
            mList[i] = new ListItem<int>(*(dynamic_cast<ListItem<int>*>(ptr)));
        }
        else if(dynamic_cast<ListItem<double>*>(ptr))
        {
            mList[i] = new ListItem<double>(*(dynamic_cast<ListItem<double>*>(ptr)));
        }
        else if(dynamic_cast<ListItem<string>*>(ptr))
        {
            mList[i] = new ListItem<string>(*(dynamic_cast<ListItem<string>*>(ptr)));
        }
        else if(dynamic_cast<ListItem<ArrayList2Type>*>(ptr))
        {
            mList[i] = new ListItem<ArrayList2Type>(*(dynamic_cast<ListItem<ArrayList2Type>*>(ptr)));
        }
        else
        {
            mList[i] = NULL;
        }
    }
}

void ArrayList2::Add(const int& item)
{
    ListItem<int>* ptr =  new ListItem<int>(item);
    mList.push_back(ptr);
}

void ArrayList2::Add(const double& item)
{
    ListItem<double>* ptr = new ListItem<double>(item);
    mList.push_back(ptr);
}

void ArrayList2::Add(const string& item)
{
    ListItem<string> *ptr = new ListItem<string>(item);
    mList.push_back(ptr);
}

void ArrayList2::Add(const ListItem<ArrayList2Type>& item)
{
    ListItem<ArrayList2Type> *ptr = new ListItem<ArrayList2Type>(item);
    mList.push_back(ptr);
}

void ArrayList2::Add(const ArrayList2& item)
{
    ListItem<ArrayList2Type> *aList = new ListItem<ArrayList2Type>(item);
    mList.push_back(aList);
}

const ListItemBase& ArrayList2::operator[](int pos) const
{
    return *mList[pos];
}

//================== ostreams =============
ostream& operator<<(ostream& stream, const ListItemBase& item)
{
    //Have to figure out subtype of item
    ListItemBase* ptr = const_cast<ListItemBase*>(&item);
    if(dynamic_cast<ListItem<int>*>(ptr))
    {
        stream << (int) *(dynamic_cast<ListItem<int>*>(ptr));
    }
    else if(dynamic_cast<ListItem<double>*>(ptr))
    {
        stream << (double) *(dynamic_cast<ListItem<double>*>(ptr));
    }
    else if(dynamic_cast<ListItem<string>*>(ptr))
    {
        stream << "\""<<(string) *(dynamic_cast<ListItem<string>*>(ptr))<<"\"";
    }
    else if(dynamic_cast<ListItem<ArrayList2Type>*>(ptr))
    {
        stream << (ArrayList2Type) *(dynamic_cast<ListItem<ArrayList2Type>*>(ptr));
    }
    else
    {
        stream<<"Stream operator not implemented for this type";
    }
    return stream;
}

ostream& operator<<(ostream& stream, const ArrayList2Type& item)
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

ostream& operator<<(ostream& stream, const ArrayList2& list)
{
   	stream<<"{";

    for(u_int i = 0; i < list.Count(); i++)
    {
        stream<<list[i];
        if(i < list.Count() -1)
        {
        	stream<<",";
        }
    }
    stream<<"}";
    return stream;
}

}

