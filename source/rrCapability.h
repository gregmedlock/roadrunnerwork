//---------------------------------------------------------------------------
#ifndef rrCapabilityH
#define rrCapabilityH
#include "rrObject.h"
#include "rrStringUtils.h"
//---------------------------------------------------------------------------
namespace rr
{

class RR_DECLSPEC Capability : public rrObject
{
    protected:
        virtual string                      GetValueAsString() const = 0;


    public:
        string                              mName;
        string                              mHint;
                                            Capability(const string& name, const string& hint);
        virtual                            ~Capability(){}
        friend ostream&                     operator<<(ostream& stream, const Capability& outMe);

        string                              AsString() const;
        string                              GetType() const;
        string                              GetName() const;
        string                              GetHint() const;
        string                              GetValue() const;
};


template<class T>
class CapabilityType: public Capability
{
    protected:
        T                                   mValue;
        virtual string                      GetValueAsString() const;


    public:
                                            /// <summary>
                                            /// Initializes a new instance of the Capability class.
                                            /// </summary>
                                            CapabilityType(const string& name, const T& value, const string& hint);//, const string& type);
};

ostream&  operator<<(ostream& stream, const Capability& outMe)
{
    stream<<outMe.AsString();   //virtual friend idiom
    return stream;
}

template<class T>
CapabilityType<T>::CapabilityType(const string& name, const T& value, const string& hint)
:
Capability(name, hint),
mValue(value)
{}

template<class T>
string CapabilityType<T>::GetValueAsString() const
{
    return ToString(mValue);
}

}
#endif
