#ifndef rrArrayList2H
#define rrArrayList2H
#include <vector>
#include <string>
#include <list>
#include <iostream>
#include "rrArrayListItem.h"
#include "rrArrayList.h"
using namespace std;
namespace rr
{
class StringList;

class RR_DECLSPEC ArrayList2 : public rrObject
{
    protected:
    public:
        vector< ArrayListItemObject* >		mList; //List of ArrayListItemObject items

    public:
                                            ArrayList2();
                                            ArrayList2(const ArrayList2& cpyMe);
                                           ~ArrayList2();
        unsigned int                        Count() const;
        void                                Clear();
        void                                Add(const int& item);
        void                                Add(const double& item);
        void                                Add(const string& item);
        void                                Add(const ArrayList2& item);
        void                                Add(const ArrayListItem<ArrayList2Item>& item);

        void                                Add(const StringList& list);
        void                                Add(const string& lbl, const StringList& lists);
        void                                Add(const string& lbl, const ArrayList2& lists);
//        void                                Add(const string& lbl, const StringArrayList& lists);

        const ArrayListItemObject&            operator[](int pos) const;
        ArrayListItemObject&                  operator[](int pos);
        void                                operator = (const ArrayList2& rhs);
        StringList                          GetSubList(const string& lName);
        StringList                          GetStringList(const int& index);
};


RR_DECLSPEC ostream& operator<<(ostream& stream, const ArrayList2& list);

typedef ArrayList2 ArrayList;
typedef ArrayList2 StringArrayList;
}
#endif
