#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include "rr_c_api.h"
#include "rr_c_api_support.h"
#include "rrUtils.h"
//---------------------------------------------------------------------------
#if defined (__CODEGEAR__)
#pragma package(smart_init)
#endif

namespace rr_c_api
{
extern char* gLastError;

//static const char* ALLOCATE_API_ERROR_MSG = {"Please allocate a handle to the roadrunner API before calling any API function"};
void setError(const string& err)
{
    if(gLastError)
    {
        delete gLastError;
    }
    gLastError = new char[err.size() + 1];
    strcpy(gLastError, err.c_str());
}

char* createText(const char* str)
{
    char* newstr = new char[strlen(str) + 1];
    strcpy(newstr, str);
    return newstr;
}

char* createText(const string& str)
{
    char* newstr = new char[str.size() + 1];
    strcpy(newstr, str.c_str());
    return newstr;
}

RRMatrix* createMatrix(const LIB_LA::DoubleMatrix& mat)
{
    RRMatrixHandle matrix = new RRMatrix;

    matrix->RSize = mat.RSize();
    matrix->CSize = mat.CSize();
    matrix->Data =  new double[mat.RSize()*mat.CSize()];

    int index = 0;
    for(rr::u_int row = 0; row < mat.RSize(); row++)
    {
        for(rr::u_int col = 0; col < mat.CSize(); col++)
        {
            matrix->Data[index++] = mat(row,col);
        }
    }
    return matrix;
}

vector<double> createVector(const RRVector* vec)
{
    vector<double> aVec;

    if(!vec)
    {
        return aVec;
    }

    aVec.resize(vec->Size);
    for(int i = 0; i < aVec.size(); i++)
    {
        aVec[i] =  vec->Data[i];
    }

    return aVec;
}

RRVector* createVector(const vector<double>& vec)
{
    RRVector* aVec = new RRVector;
    aVec->Size = vec.size();

    if(aVec->Size)
    {
        aVec->Data = new double[aVec->Size];
    }

    for(int i = 0; i < aVec->Size; i++)
    {
        aVec->Data[i] =  vec[i];
    }

    return aVec;
}

bool copyVector(const RRVector* src, vector<double>& dest)
{
    if(!src)
    {
        return false;
    }

    dest.resize(src->Size);

    for(int i = 0; i < src->Size; i++)
    {
        dest[i] = src->Data[i];
    }

    return true;
}

RRStringList* createList(const StringList& sList)
{
    if(!sList.Count())
    {
        return NULL;
    }

    RRStringList* list = new RRStringList;
    list->Count = sList.Count();

    list->String = new char*[list->Count];

    for(int i = 0; i < list->Count; i++)
    {
        list->String[i] = new char[sList[i].size()];
        strcpy(list->String[i], sList[i].c_str());
    }
    return list;
}

RRStringList* createList(const ArrayList& arrList)
{

    if(!arrList.Count())
    {
        return NULL;
    }

    RRStringListHandle list = new RRStringList;

    list->Count = arrList.TotalCount();
    list->String = new char*[arrList.TotalCount()];

    int itemCount = 0;
    for(int i = 0; i < arrList.ListCount(); i++)
    {
        StringList subList = arrList[i];
        for(int j = 0; j < subList.Count(); j++)
        {
            string item = subList[j].c_str();
            list->String[itemCount] = new char[item.size()+1];
            strcpy(list->String[itemCount++], item.c_str());
        }
    }
    return list;
}

RRStringArrayList* createList(const RRArrayList<string>& aList)
{
    if(!aList.Count())
    {
        return NULL;
    }

    //Setup a RRStringArrayList structure from aList
    RRStringArrayList* theList = new RRStringArrayList;

    theList->ItemCount = aList.Count();
    theList->Items = new RRStringArrayListItem[aList.Count()];
    int itemCount = aList.Count();
    for(int i = 0; i < itemCount; i++)
    {
        if(aList[i].HasValue())
        {
            theList->Items[i].SubList = NULL;
            string item = aList[i].GetValue();
            theList->Items[i].Item = new char[item.size() + 1];
            strcpy(theList->Items[i].Item, item.c_str());
        }
        else
        {
            //Item is a sublist
            theList->Items[i].Item = NULL;
            RRStringArrayList* list = createList((*aList[i].mLinkedList));
            theList->Items[i].SubList = list;
        }
    }
    return theList;
}

}
