#ifndef rrArrayListH
#define rrArrayListH
#include <vector>
#include <string>
#include <list>
#include "rrObject.h"
#include "rrStringList.h"
using std::list;
using std::vector;
using std::string;

namespace rr
{

template <class T>
class ArrayList;

template <class T>
class ArrayListItem : public rrObject
{
    public:
        T                                          *mValue;
        ArrayList< T >                             *mLinkedList;
        								           	ArrayListItem(const T& primitive);
                                                    ArrayListItem(const ArrayListItem<T>& item);
        								           	ArrayListItem(ArrayList<T>* item);

                                                   ~ArrayListItem();
        T                                           GetValue() const;
                                                    operator T();
        bool                                        HasValue() const;
        bool                                        HasList() const;
};

template <class T>
class ArrayList : public rrObject
{
    protected:
    public:
        vector< ArrayListItem<T>* >		   			mList;	//Contains list items..

    public:
                                        			ArrayList();
                                        			ArrayList(const ArrayList& cpyMe);
                                        		   ~ArrayList();
		mutable
        vector< ArrayListItem<T>* >::const_iterator mIter;

        int                                         Count() const;
        void                                        Clear();
		void                                        Add(const T& item);
        void							            Add(ArrayList<T>& subList);

        ArrayListItem<T>&                           operator[](const int& index);
        const ArrayListItem<T>&                     operator[](const int& index) const;
        void                                        operator=(const ArrayList& rhs);
        string                                      AsString();
};

template<class T>
ostream& operator<<(ostream& stream, ArrayList<T>& list);

template<class T>
ostream& operator<<(ostream& stream, ArrayListItem<T>& listItem);

template<class T>
ArrayList<T>::ArrayList(){}

template<class T>
ArrayList<T>::ArrayList(const ArrayList& copyMe)
{
    //Copy each item in copyMe
    mList.resize(copyMe.Count());
    for(int i = 0; i < copyMe.Count(); i++)
    {
        const ArrayListItem<T>& item = copyMe[i];
        mList[i] = new ArrayListItem<T>(item);
    }
}

template<class T>
ArrayList<T>::~ArrayList()
{
    for(int i = 0; i < Count(); i++)
    {
        delete mList[i];
    }
    mList.clear();
}

template<class T>
void ArrayList<T>::operator=(const ArrayList& rhs)
{
    Clear();

    //Deep copy..
    mList.resize(rhs.Count());
    for(int i = 0; i < rhs.Count(); i++)
    {
        const ArrayListItem<T>& copyItem = rhs[i];
        ArrayListItem<T> *item = new ArrayListItem<T>(copyItem);
        mList[i] = item;
    }
}

template<class T>
void ArrayList<T>::Clear()
{
    for(int i = 0; i < Count(); i++)
    {
        delete mList[i];
    }
    mList.clear();
}

template<class T>
void ArrayList<T>::Add(const T& _item)
{
	ArrayListItem<T> *item = new ArrayListItem<T>(_item);
    if(item)
    {
        mList.push_back(item);
    }
}

template<class T>
void ArrayList<T>::Add(ArrayList<T>& subList)
{
    ArrayListItem<T>* newSubList = new ArrayListItem<T>(&subList);

    //Don't use push back
    mList.resize(mList.size() + 1);
    mList[mList.size() - 1] = newSubList;
}

template<class T>
int ArrayList<T>::Count() const
{
    return mList.size();
}

template<class T>
string ArrayList<T>::AsString()
{
    string theList;
    for(int i = 0; i < Count(); i++)
    {
        string item = (*this)[i];

        theList += item;
        if(i < Count() -1)
        {
            theList += ",";
        }
    }
    return theList;
}


////////////////////////////////////////////////////////////////////////////
template< class T >
ArrayListItem<T>::ArrayListItem(const ArrayListItem<T>& item)
{
    if(item.HasValue())
    {
        mValue = new T(item.GetValue());
        mLinkedList = NULL;
    }
    else if (item.mLinkedList)
    {
        mLinkedList = new ArrayList<T>(*item.mLinkedList);
        mValue = NULL;
    }
    else
    {
        mValue = NULL;
        mLinkedList = NULL;
    }
}

template< class T >
ArrayListItem<T>::ArrayListItem(const T& item)
:
mLinkedList(NULL)
{
    mValue = new T(item);
}

template< class T >
ArrayListItem<T>::~ArrayListItem()
{
    delete mValue;
    delete mLinkedList;
}

template< class T >
ArrayListItem<T>::ArrayListItem(ArrayList<T>* item)
:
mValue(NULL)
{
    if(item)
    {
        mLinkedList = new ArrayList<T>(*item);
    }
    else
    {
        mLinkedList = NULL;
    }
}

template<class T>
ArrayListItem<T>& ArrayList<T>::operator[](const int& index)
{
    ArrayListItem<T> *item = mList[index];
    return *item;
}

template<class T>
const ArrayListItem<T>&  ArrayList<T>::operator[](const int& index) const
{
    ArrayListItem<T> *item = mList[index];
    return *item;
}

template<class T>
ostream& operator<<(ostream& stream, ArrayList<T>& list)
{
    int i = 0;
   	stream<<"{";
    for(list.mIter = list.mList.begin(); list.mIter != list.mList.end(); list.mIter++)
    {
        ArrayListItem<T>* item = (*list.mIter);
        if(item->mLinkedList != NULL)
        {
            stream<<*item->mLinkedList;
        }

        if(item->mValue)
        {
            stream<< *item->mValue;
        }

        if(i < list.Count() -1)
        {
        	stream<<",";
        }
        i++;
    }
    stream<<"}";
    return stream;
}

template<class T>
ostream& operator<<(ostream& stream, ArrayListItem<T>& listItem)
{
    if(listItem.mValue)
    {
        stream<<listItem.mValue;
    }
    return stream;
}

template <class T>
T ArrayListItem<T>::GetValue() const
{
    return *mValue;
}

template <>
ArrayListItem<string>::operator string()
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
ArrayListItem<int>::operator int()
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

template <class T>
bool ArrayListItem<T>::HasValue() const
{
    return mValue == NULL ? false : true;
}

template <class T>
bool ArrayListItem<T>::HasList() const
{
    return mLinkedList == NULL ? false : true;
}

}
#endif
