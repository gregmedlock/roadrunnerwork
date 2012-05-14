#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <sstream>
#include "rrStringListContainer.h"
//---------------------------------------------------------------------------



namespace rr
{

StringListContainer::StringListContainer()
{
}

StringListContainer::StringListContainer(const StringListContainer& cp)
{
	mContainer = cp.mContainer;
}

//void StringListContainer::operator=(const StringListContainer& rhs)
//{
//
//}

void StringListContainer::Add(const StringList& list)
{
	mContainer.push_back(list);
}

void StringListContainer::Add(const string& listName, const StringList& aList)
{
	StringList list(aList);
	list.Label(listName);
	mContainer.push_back(list);
}

void StringListContainer::Add(const string& item)
{
	StringList list;
    list.push_back(item);
	Add(list);

}

void StringListContainer::Add(const int& atPos)
{

}

//ostream& operator<<(ostream& stream, StringListContainer& list)
//{
//	vector<string>::iterator iter;
//    int count = 0;
//    for(iter = list.begin(); iter != list.end(); iter++)
//    {
//		stream<<"List Item "<<++count<<" : "<<(*iter)<<std::endl;
//    }
//	return stream;
//}

ostringstream& operator<<(ostringstream& stream, StringListContainer& list)
{
	vector<StringList>::iterator iter;
    int count = 0;
    for(iter = list.begin(); iter != list.end(); iter++)
    {
		stream<<"Label: " <<(*iter).mLabel<<std::endl;
    }
	return stream;
}


}

