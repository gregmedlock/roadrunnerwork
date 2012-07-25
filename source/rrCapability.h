//---------------------------------------------------------------------------
#ifndef rrCapabilityH
#define rrCapabilityH
#include "rrObject.h"
//---------------------------------------------------------------------------
namespace rr
{
class AbstractCapability : public rrObject
{
    public:
        string          mSection;


};

template<class T>
class Capability : public AbstractCapability
{
    protected:
        string      Hint;
        string      Name;
//        string      Type;
        T           Value;

    public:
        /// <summary>
        /// Initializes a new instance of the Capability class.
        /// </summary>
        Capability(const string& name, const T& value, const string& hint);//, const string& type);
};

template<class T>
Capability<T>::Capability(const string& name, const T& value, const string& hint)
:
Name(name),
Hint(hint),
Value(value)
{

}

}
#endif
