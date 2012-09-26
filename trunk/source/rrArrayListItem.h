#ifndef rrArrayListItemH
#define rrArrayListItemH
//---------------------------------------------------------------------------
#include "rrObject.h"
#include "rrStringList.h"
#include "rrArrayListItemObject.h"
//This unit contains
// 1) a base class for ArrayListItems
// 2) Template for basic type ArrayListItems, such as int, char double etc.
// 3) An ArrayList2Item class, that represent an ArrayList2, within an ArrayList2 object


namespace rr
{


template <class T>
class ArrayListItem : public ArrayListItemObject
{
    private:
        T                           mItemValue;

    public:
                                    ArrayListItem(const T& val);

        virtual                    ~ArrayListItem(){}
                                    operator T(){return mItemValue;}
        virtual const char          operator[](const int& pos) const {return '\0';}     //Make sense for string types
        ArrayListItem<T>&           operator=(const ArrayListItem<T>& rhs);
};

template<class T>
ArrayListItem<T>::ArrayListItem(const T& val)
:
mItemValue(val)
{}

template<class T>
ArrayListItem<T>& ArrayListItem<T>::operator=(const ArrayListItem<T>& rhs)
{
    if(this != &rhs)
    {
        mItemValue = rhs.mItemValue;
    }

    return *this;
}

class ArrayList2;
class RR_DECLSPEC ArrayList2Item  : public rrObject
{
    public:
        ArrayList2                         *mValue;
                                            ArrayList2Item();
                                            ArrayList2Item(const ArrayList2Item& copyMe);
        ArrayList2Item&                     operator=(const ArrayList2Item& list);

                                            ArrayList2Item(const ArrayList2& list); //Copy constructor from ArrayList2
                                           ~ArrayList2Item();
        unsigned int                        Count() const;
        const ArrayListItemObject&            operator[](int pos) const;
        ArrayListItemObject&                  operator[](int pos);
};

ostream& operator<<(ostream& stream, const ArrayList2Item& item);
ostream& operator<<(ostream& stream, const ArrayListItemObject& item);

}
#endif
