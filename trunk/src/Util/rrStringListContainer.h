#ifndef rrStringListContainerH
#define rrStringListContainerH
#include <vector>
#include <string>
#include "rrObject.h"
#include "rrStringList.h"
using std::vector;
using std::string;

namespace rr
{

class RR_DECLSPEC StringListContainer : public rrObject
{
    protected:
        vector<StringList>                 mContainer;

    public:
                                           StringListContainer();
                                           StringListContainer(const StringListContainer& cp);
        void                            operator=(const StringListContainer& rhs);
           void                             Add(const string& listName, const StringList& coll);
        void                             Add(const StringList& coll);
        void                             Add(const string& coll);
        void                             Add(const int& coll);
        int                              size(){return mContainer.size();}
        int                              Count(){return mContainer.size();}
        StringList&                     operator[](const int& index){return mContainer[index];}
        vector<StringList>::iterator     begin(){return mContainer.begin();}
        vector<StringList>::iterator     end(){return mContainer.end();}


};

ostream& operator<<(ostream& stream, StringListContainer& list);
ostringstream& operator<<(ostringstream& stream, StringListContainer& list);

typedef StringListContainer ArrayList; //ArrayList is C# name..
typedef StringListContainer StringCollection;
}
#endif
