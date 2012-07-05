#ifndef rrArrayListH
#define rrArrayListH
#include <vector>
#include <string>
#include "rrObject.h"
#include "rrStringList.h"
using std::list;
using std::vector;
using std::string;

namespace rr
{

class RR_DECLSPEC ArrayStringList : public rrObject
{
    protected:
        string                          mLabel;
        vector< list<string> >          mContainer;

    public:
                                        ArrayList();
                                        ArrayList(const string& lbl, const ArrayList& cp);
                                        ArrayList(const ArrayList& cp);
                                        ArrayList(const StringList& cp);
        void                            operator=(const ArrayList& rhs);
        void                            Add(const ArrayList& lists);
        void                            Add(const string& lbl, const ArrayList& lists);
        void                            Clear();

        void                            Add(const string& listName, const StringList& coll);
        void                            Add(const StringList& coll);
        void                            Add(const string& coll);
        void                            Add(const int& coll);
        int                             Count() const;
        int                             ListCount() const;
        int                             TotalCount() const;
        StringList&                     operator[](const int& index);
        const StringList&               operator[](const int& index) const;
        vector<StringList>::iterator    begin();
        vector<StringList>::iterator    end();
};

ostream& operator<<(ostream& stream, const ArrayList& list);

//typedef ArrayList ArrayList; //ArrayList is C# name..
//typedef ArrayList StringCollection;
}
#endif
