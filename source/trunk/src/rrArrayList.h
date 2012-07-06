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
class ArrayListItem
{
    public:
        ArrayListItem          			   *mPrevious;
        T		     						value;
        ArrayListItem          			   *mNext;
        									ArrayListItem(const T& item)
                                            {
                                            	value = (item);
                                            }
};

template <class T>
class ArrayList : public rrObject
{
    //This list can only hold items of type T
    protected:
        ArrayListItem<T>               	   			*mPrevious;
        ArrayListItem<T>		      	   			*mNext;
        //ArrayListItem<T>               	   		*mCurrent;
    public:
        list< ArrayListItem<T>* >		   			mList;	//Contains current list items..

    public:
                                        			ArrayList();
		mutable list< ArrayListItem<T>* >::const_iterator  		mIter;
        ArrayListItem<T>*	 						GetFirst() {return mList.begin();}
        ArrayListItem<T>*				   			GetLast()  {return mList.end();}
//                                        ArrayList(const ArrayList& cp);
//                                        ArrayList(const StringList& cp);
//        void                            operator=(const ArrayList& rhs);
//        void                            Add(const ArrayList<T>& list);
//        void                            Clear();

		void                            Add(const T& item);
        void							Add(const ArrayList<T>& subList);
//        void                            Add(const string& listName, const StringList& coll);
//        void                            Add(const StringList& coll);


        int                             Count() const;
//        StringList&                     operator[](const int& index);
//        const StringList&               operator[](const int& index) const;
//        vector<StringList>::iterator    begin();
//        vector<StringList>::iterator    end();
};

template<class T>
ostream& operator<<(ostream& stream, ArrayList<T>& list);

template<class T>
ArrayList<T>::ArrayList(){}

template<class T>
void ArrayList<T>::Add(const T& _item)
{
	ArrayListItem<T> *item = new ArrayListItem<T>(_item);
    if(Count())
    {
    	//Get last item, not part of a sub list

    	item->mPrevious = mList.back();
        item->mNext = NULL;
        item->mPrevious->mNext = item;
    }
    mList.push_back(item);
}

template<class T>
void ArrayList<T>::Add(const ArrayList<T>& subList)
{
	subList.mIter = subList.mList.begin();
    while(subList.mIter != subList.mList.end())
    {
    	mList.push_back(*(subList.mIter));
        subList.mIter++;
    }
}

//template<class T>
//ArrayList<T>::ArrayList::ArrayList(const ArrayList& cp)
//:
//mContainer(cp.mContainer)
//{}
//
//template<class T>
//ArrayList<T>::ArrayList::ArrayList(const string& lbl, const ArrayList& cp)
//:
//mLabel(lbl),
//mContainer(cp.mContainer)
//{}
//
//template<class T>
//void ArrayList<T>::ArrayList::Clear()
//{
//    mContainer.empty();
//}

//template<class T>
//ArrayList<T>::ArrayList(const StringList& cp)
//{
//    Add(cp.mLabel, cp);
//}

//void ArrayList::operator=(const ArrayList& rhs)
//{
//
//}

template<class T>
int ArrayList<T>::Count() const
{
    return mList.size();
}

//template<class T>
//StringList& ArrayList::operator[](const int& index)
//{
//    return mContainer[index];
//}

//template<class T>
//const StringList& ArrayList::operator[](const int& index) const
//{
//    return mContainer[index];
//}
//
//template<class T>
//vector<StringList>::iterator ArrayList::begin()
//{
//    return mContainer.begin();
//}
//
//template<class T>
//vector<StringList>::iterator ArrayList<T>::end()
//{
//    return mContainer.end();
//}

//template<class T>
//void ArrayList<T>::Add(const ArrayList<T>& lists)
//{
//    Add(lists);
//}
//
//template<class T>
//void ArrayList<T>::Add(const ArrayList<T>& lists)
//{
//    for(int i = 0; i < lists.Count(); i++)
//    {
//        StringList aList;
//        aList = lists.mContainer[i];    //Todo: lists[i] should work...
//        Add(aList);
//    }
//}
//
//template<class T>
//void ArrayList<T>::Add(const StringList& list)
//{
//    mContainer.push_back(list);
//}

//
//template<class T>
//void ArrayList<T>::Add(const string& item)
//{
//    StringList list;
//    list.push_back(item);
//    Add(list);
//
//}
//
//template<class T>
//void ArrayList<T>::Add(const int& atPos)
//{
//
//}

template<class T>
ostream& operator<<(ostream& stream, ArrayList<T>& list)
{
    int i = 0;
    for(list.mIter = list.mList.begin(); list.mIter != list.mList.end(); list.mIter++)
    {
        ArrayListItem<T>* item = (*list.mIter);
        if(item->mPrevious == NULL)
        {
        	stream<<"{";
        }

        stream<<item->value;
        if(item->mNext)
        {
        	stream<<",";
        }
        else
        {
        	stream<<"}";
        }
        i++;
    }
    stream<<endl;
    return stream;
}

}
#endif
