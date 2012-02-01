#ifndef rrStringCollectionH
#define rrStringCollectionH
#include <vector>
#include <string>
#include "rrObject.h"

using std::vector;
using std::string;

namespace rr
{

class RR_DECLSPEC StringCollection : public rrObject
{
	protected:

		vector<string> mCollection;

	private:

};
}
#endif
