#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrTextWriter.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

using namespace std;
namespace rr
{


TextWriter::TextWriter(ostream& aStream)
:
mStream(aStream)
{

}

void TextWriter::Write(const string& chars)
{
	mStream<<chars;
}

void TextWriter::WriteLine()
{
	mStream<<endl;
}

}

