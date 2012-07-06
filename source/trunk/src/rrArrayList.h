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
        ArrayListItem          *mPrevious;
        list<ArrayListItem<T>* >     value;
        ArrayListItem          *mNext;
};

template <class T>
class ArrayList : public rrObject
{
    //This list can only hold items of type T
    protected:
        ArrayListItem<T>               *mPrevious;
        list< ArrayListItem<T>* >      *mNext;
        ArrayListItem<T>               *mCurrent;

    public:
                                        ArrayList();
//                                        ArrayList(const ArrayList& cp);
//                                        ArrayList(const StringList& cp);
//        void                            operator=(const ArrayList& rhs);
//        void                            Add(const ArrayList<T>& list);
//        void                            Clear();

//        void                            Add(const T& item);
//        void                            Add(const string& listName, const StringList& coll);
//        void                            Add(const StringList& coll);


//        int                             Count() const;
//        int                             ListCount() const;
//        int                             TotalCount() const;
//        StringList&                     operator[](const int& index);
//        const StringList&               operator[](const int& index) const;
//        vector<StringList>::iterator    begin();
//        vector<StringList>::iterator    end();
};

//ostream& operator<<(ostream& stream, const ArrayList& list);

template<class T>
ArrayList<T>::ArrayList()
{
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

//template<class T>
//int ArrayList::TotalCount() const
//{
//    //Returns the total count of all list items..
//    int cnt = 0;
//    for(int i = 0; i < Count(); i++)
//    {
//        cnt += mContainer[i].Count();
//    }
//    return cnt;
//}

//template<class T>
//int ArrayList::ListCount() const
//{
//    return mContainer.size();
//}
//
//template<class T>
//int ArrayList::Count() const
//{
//    return mContainer.size();
//}

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

//template<class T>
//void ArrayList<T>::Add(const string& listName, const StringList& aList)
//{
//    StringList list(aList);
//    list.Label(listName);
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

//ostream& operator<<(ostream& stream, const ArrayList& list)
//{
//    vector<StringList>::iterator iter;
//    for(int  i = 0; i < list.Count(); i++)
//    {
//        string item = list[i].AsString();
//        stream<<"List Item "<<i+1<<" : "<<item<<endl;
//    }
//    return stream;
//}
//
}
#endif
