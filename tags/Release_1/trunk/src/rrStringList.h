#ifndef rrStringListH
#define rrStringListH
#include <vector>
#include <string>
#include "rrObject.h"

using std::vector;
using std::string;
using std::ostream;

namespace rr
{

class RR_DECLSPEC StringList : public rrObject
{
    protected:

        vector<string>              mStrings;
        vector<string>::iterator    mLI;    //ListITerator

    public:
           string                   mLabel;
                                    StringList();
                                    StringList(const string& str, const string& delimiter);
                                    StringList(const vector<string>& strings);
                                    StringList(const StringList& cp);

                                   ~StringList();

        void                        Label(const string& lbl){mLabel = lbl;}
        void                        Add(const string& str);
        string                      AsString(const string& delimiter = ",");
        int                         size() const {return mStrings.size();}
        int                         Count() const {return mStrings.size();}
        void                        operator=(const StringList& rhs);
        string&                     operator[](const int& index){return mStrings[index];}
        string                      operator[](const int& index) const {return mStrings[index];}

        StringList                  operator-(const StringList& rhs);

        int                         find(const string& item);
        int                         IndexOf(const string& item);
        void                        empty(){mStrings.clear();}
        bool                        Contains(const string& item);
        void                        push_back(const string& item);
        vector<string>::iterator    begin(){return mStrings.begin();}
        vector<string>::iterator    end(){return mStrings.end();}
        void                        PreFix(const string& fix);
        void                        PostFix(const string& fix);
//        friend ostream&             operator<<(ostream& stream, StringList& list);
};

RR_DECLSPEC ostream&             operator<<(ostream& stream, StringList& list);
//ostringstream&  operator<<(ostringstream& stream, StringList& list);
}
#endif
