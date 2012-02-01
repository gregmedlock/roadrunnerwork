#ifndef rrStringCollectionH
#define rrStringCollectionH
#include <vector>
#include <string>
#include "rrExporter.h"
using std::vector;
using std::string;

class RR_DECLSPEC StringCollection : public rrObject
{
	protected:
		vector<string> mCollection;

	private:

};

#endif
