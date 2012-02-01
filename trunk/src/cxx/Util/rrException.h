#ifndef rrExceptionH
#define rrExceptionH
#include <exception>
#include <string>
#include "rrObject.h"

using std::string;
using std::exception;

namespace rr
{

class RR_DECLSPEC RRException : public rrObject , public std::exception
{
	protected:
    	string mMessage;

	public:
		RRException(const string& desc) : mMessage(desc){}
       	virtual ~RRException() throw() {}

		virtual const char* what() const throw()
  		{
    		return mMessage.c_str();
  		}
};

class RR_DECLSPEC SBWApplicationException : public RRException
{
	public:
	    SBWApplicationException(const string& msg)
        : RRException(msg){}
};



}//namepsace rr
#endif
