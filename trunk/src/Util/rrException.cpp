#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrException.h"
//---------------------------------------------------------------------------
#if defined(__CODEGEARC__)
#pragma package(smart_init)
#endif

namespace rr
{


Exception::Exception(const string& desc)
:
mMessage(desc), Message(mMessage)
{
}

Exception::~Exception() throw() {}

const char* Exception::what() const throw()
{
    return mMessage.c_str();
}

RRException::RRException(const string& msg)
:
Exception(msg)
{}

SBWApplicationException::SBWApplicationException(const string& msg)
:
RRException(msg)
{}

SBWException::SBWException(const string& msg)
:
RRException(msg)
{}

ScannerException::ScannerException(const string& msg)
:
RRException(msg)
{}

NLEQException::NLEQException(const string& msg)
:
RRException(msg)
{}

CvodeException::CvodeException(const string& msg)
:
RRException(msg)
{}

}
