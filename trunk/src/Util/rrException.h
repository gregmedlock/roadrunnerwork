#ifndef rrExceptionH
#define rrExceptionH
#include <exception>
#include <string>
#include "rrObject.h"

using std::string;
using std::exception;

namespace rr
{

class RR_DECLSPEC Exception : public std::exception
{
	protected:
    	string mMessage;   //Exception message

	public:
        string& Message;

		Exception(const string& desc);
       	virtual ~Exception() throw();
		virtual const char* what() const throw();
};

class RR_DECLSPEC RRException : public Exception
{
	public:
	    RRException(const string& msg);
};

class RR_DECLSPEC SBWApplicationException : public RRException
{
	public:
	    SBWApplicationException(const string& msg);
};

class RR_DECLSPEC ScannerException : public RRException
{
	public:
	    ScannerException(const string& msg);

};


}//namepsace rr
#endif
