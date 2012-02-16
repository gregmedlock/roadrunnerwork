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
		vector<StringList> 			mContainer;

	public:
		void 						Add(const StringList& coll);
		void 						Add(const string& coll);
		void 						Add(const int& coll);
        int  						size(){return mContainer.size();}
        int  						Count(){return mContainer.size();}
        StringList& 				operator[](const int& index){return mContainer[index];}
};

typedef StringListContainer ArrayList; //ArrayList is C# name..
typedef StringListContainer StringCollection;
}
#endif
