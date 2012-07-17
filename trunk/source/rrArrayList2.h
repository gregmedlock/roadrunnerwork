#ifndef rrArrayList2H
#define rrArrayList2H
#include <vector>
#include <string>
#include <list>
#include "rrObject.h"
#include "rrStringList.h"
#include <iostream>

using namespace std;
namespace rr
{

class RR_DECLSPEC ListItemBase : public rrObject
{
    public:
        virtual ~ListItemBase();
};

ostream& operator<<(ostream& stream, const ListItemBase& item);

template <class T>
class ListItem : public ListItemBase
{
    private:
        T                           value;

    public:
                                    ListItem(const T& val);

        virtual                    ~ListItem(){}
                                    operator T(){return value;}
        virtual const char          operator[](const int& pos) const {}     //Make sense for string types
        ListItem<T>&                operator=(const ListItem<T>& rhs);
};

template<class T>
ListItem<T>::ListItem(const T& val)
:
value(val)
{}

template<>
const char ListItem<string>::operator[](const int& pos) const 
{
    return (char) value[pos];
}

template<class T>
ListItem<T>& ListItem<T>::operator=(const ListItem<T>& rhs)
{
    if(this != &rhs)
    {
        value = rhs.value;
    }

    return *this;
}

class ArrayList2;
class RR_DECLSPEC ArrayList2Type  : public rrObject
{
    public:
        ArrayList2                         *mValue;
                                            ArrayList2Type();
                                            ArrayList2Type(const ArrayList2Type& copyMe);                                            
        ArrayList2Type&                     operator=(const ArrayList2Type& list);
        
                                            ArrayList2Type(const ArrayList2& list); //Copy constructor from ArrayList2
                                           ~ArrayList2Type();
        unsigned int                        Count() const;
        const ListItemBase&                 operator[](int pos) const;
};


ostream& operator<<(ostream& stream, const ArrayList2Type& item);

class RR_DECLSPEC ArrayList2 : public rrObject
{
    protected:
    public:
        vector< ListItemBase* >		   	    mList; //List of ListItemBase items

    public:
                                        	ArrayList2();
                                        	ArrayList2(const ArrayList2& cpyMe);
                                           ~ArrayList2();
        unsigned int                        Count() const;
        void                                Clear();
		void                                Add(const int& item);
		void                                Add(const double& item);
		void                                Add(const string& item);
        void							    Add(const ListItem<ArrayList2Type>& item);
        void							    Add(const ArrayList2& item);
        const ListItemBase&                 operator[](int pos) const;
        void                                operator = (const ArrayList2& rhs);
};


ostream& operator<<(ostream& stream, const ArrayList2& list);

template<>
ListItem<ArrayList2Type>::ListItem(const ArrayList2Type& item)
{
    value = item;
}

}
#endif
