#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrStreamWriter.h"
//---------------------------------------------------------------------------
#if defined(__BORLANDC__)
#pragma package(smart_init)
#endif

//---------------------------------------------------------------------------


namespace rr
{

StreamWriter::StreamWriter(const string& filePath)
:
mFilePath(filePath)
{
	mFileStream.open(filePath.c_str(), ios::trunc);
}

bool StreamWriter::WriteLine(const string& line)
{
	if(mFileStream.is_open())
   	{
   		mFileStream << line <<std::endl;
       	return true;
   	}
	return false;
}

bool StreamWriter::Write(const string& text)
{
	if(mFileStream.is_open())
   	{
   		mFileStream << text;
       	return true;
   	}
	return false;
}

bool StreamWriter::Close()
{
	mFileStream.close();
    return true;
}

}//namespace rr

