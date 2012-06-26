/**
 * @file rr_c_api.cpp
 * @brief roadRunner C API 2012
 * @author Totte Karlsson & Herbert M Sauro
 *
 * <--------------------------------------------------------------
 * This file is part of cRoadRunner.
 * See http://code.google.com/p/roadrunnerwork/ for more details.
 *
 * Copyright (C) 2012
 *   University of Washington, Seattle, WA, USA
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * In plain english this means:
 *
 * You CAN freely download and use this software, in whole or in part, for personal,
 * company internal, or commercial purposes;
 *
 * You CAN use the software in packages or distributions that you create.
 *
 * You SHOULD include a copy of the license in any redistribution you may make;
 *
 * You are NOT required include the source of software, or of any modifications you may
 * have made to it, in any redistribution you may assemble that includes it.
 *
 * YOU CANNOT:
 *
 * redistribute any piece of this software without proper attribution;
*/

#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
//---------------------------------------------------------------------------
#include <windows.h>
#include <sstream>
#include "rrRoadRunner.h"
#include "rrCGenerator.h"
#include "rrLogger.h"           //Might be useful for debugging later on
#include "rr_c_api.h"
#include "rr_c_api_support.h"   //Support functions, not exposed as api functions and or data
#include "rrException.h"
//---------------------------------------------------------------------------

using namespace std;
using namespace rr;
using namespace rr_c_api;
namespace rr_c_api
{
static  rr::RoadRunner*     gRRHandle       = NULL;
char*                       gLastError      = NULL;
}

char* __stdcall getBuildDate(void)
{
    char* date = new char[strlen(__DATE__) + 1];
    strcpy(date, __DATE__);
    return date;
}

RRHandle __stdcall getRRInstance()
{
	try
    {
        if(!gRRHandle)
        {
            gRRHandle = new rr::RoadRunner();
        }
    	return gRRHandle;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return NULL;
}

char* __stdcall getCopyright()
{
	try
    {
        char* text = NULL;
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
        }
        else
        {
            text = new char[gRRHandle->getCopyright().size() + 1];
            strcpy(text, gRRHandle->getCopyright().c_str());
        }
        return text;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return NULL;
}

//Flags and Options
bool __stdcall setComputeAndAssignConservationLaws(const bool& OnOrOff)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        gRRHandle->ComputeAndAssignConservationLaws(OnOrOff);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
     }

    return false;
}

bool __stdcall setTempFolder(const char* folder)
{
	try
    {
    	if(!gRRHandle)
    	{
        	SetAPIError(ALLOCATE_API_ERROR_MSG);
        	return false;
    	}
	    return gRRHandle->SetTempFileFolder(folder);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return false;
}

char* __stdcall getTempFolder()
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

	    char* text = new char[gRRHandle->GetTempFileFolder().size() + 1];
    	strcpy(text, gRRHandle->GetTempFileFolder().c_str());
	    return text;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return NULL;
}

bool __stdcall loadSBMLFromFile(const char* sbmlFileName)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        if(!gRRHandle->loadSBMLFromFile(sbmlFileName))
        {
            SetAPIError("Failed to load SBML semantics");
            return false;
        }
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
    return false;
}

bool __stdcall loadSBML(const char* sbml)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        if(!gRRHandle->loadSBML(sbml))
        {
            SetAPIError("Failed to load SBML semantics");
            return false;
        }
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return false;
}

bool __stdcall setTimeStart(double timeStart)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
        gRRHandle->setTimeStart(timeStart);
    	return false;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());

    }

    return false;
}

bool __stdcall setTimeEnd(double timeEnd)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        gRRHandle->setTimeEnd(timeEnd);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
    return false;
}

bool __stdcall setNumPoints(int nrPoints)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        gRRHandle->setNumPoints(nrPoints);
	    return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return false;
}

bool  __stdcall setSelectionList(const char* list)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        gRRHandle->setSelectionList(list);
        return true;

    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
    return false;
}

RRStringListHandle __stdcall getSelectionList()
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        StringList sNames = gRRHandle->getSelectionList();

        if(!sNames.Count())
        {
            return NULL;
        }

        RRStringListHandle list = new RRStringList;
        list->Count = sNames.size();
        list->String = new char*[list->Count];

        for(int i = 0; i < list->Count; i++)
        {
            list->String[i] = new char[sNames[i].size()];
            strcpy(list->String[i], sNames[i].c_str());
        }
        return list;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
    return NULL;
}

RRResultHandle __stdcall simulate(void)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        if(!gRRHandle->Simulate())
        {
            return NULL;
        }

        SimulationData result = gRRHandle->GetSimulationResult();

        //Extract the data and return struct..
        RRResult* aResult  = new RRResult;
        aResult->ColumnHeaders = new char*[result.GetNrOfCols()];
        for(int i = 0; i < result.GetNrOfCols(); i++)
        {
            aResult->ColumnHeaders[i] = new char(32);
            strcpy(aResult->ColumnHeaders[i], result.GetColumnNames()[i].c_str());
        }

        aResult->RSize = result.GetNrOfRows();
        aResult->CSize = result.GetNrOfCols();
        int size = aResult->RSize*aResult->CSize;
        aResult->Data = new double[size];

        int index = 0;
        //The data layout is simple row after row, in one single long row...
        for(int row = 0; row < aResult->RSize; row++)
        {
            for(int col = 0; col < aResult->CSize; col++)
            {
                aResult->Data[index++] = result(row, col);
            }
        }
	    return aResult;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return NULL;
}

RRResultHandle __stdcall simulateEx (const double& timeStart, const double& timeEnd, const int& numberOfPoints)
{
	try
    {
        setTimeStart(timeStart);
        setTimeEnd (timeEnd);
        setNumPoints(numberOfPoints);
	  	return simulate();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return NULL;
}

RRStringListHandle __stdcall getReactionNames(void)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        StringList rNames = gRRHandle->getReactionNames();

        if(!rNames.Count())
        {
            return NULL;
        }

        RRStringListHandle list = new RRStringList;
        list->Count = rNames.size();
        list->String = new char*[list->Count];

        for(int i = 0; i < list->Count; i++)
        {
            list->String[i] = new char[rNames[i].size()];
            strcpy(list->String[i], rNames[i].c_str());
        }
        return list;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return NULL;
}

RRStringListHandle __stdcall getRatesOfChangeNames()
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        StringList rNames = gRRHandle->getRateOfChangeNames();

        if(!rNames.Count())
        {
            return NULL;
        }

        RRStringListHandle list = new RRStringList;
        list->Count = rNames.size();
        list->String = new char*[list->Count];

        for(int i = 0; i < list->Count; i++)
        {
            list->String[i] = new char[rNames[i].size()];
            strcpy(list->String[i], rNames[i].c_str());
        }
        return list;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return NULL;
}

double __stdcall getValue(const char* speciesID)
{
	try
    {
    	if(!gRRHandle)
    	{
        	SetAPIError(ALLOCATE_API_ERROR_MSG);
        	return 0;
    	}
	    return gRRHandle->getValue(speciesID);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return 0;	//todo: how to indicate error???
}

bool __stdcall setValue(const char* speciesID, const double& val)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
	    return gRRHandle->setValue(speciesID, val);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return false;
}

RRMatrixHandle __stdcall getStoichiometryMatrix(void)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        LIB_LA::DoubleMatrix tempMat = gRRHandle->getStoichiometryMatrix();

        RRMatrixHandle matrix = new RRMatrix;
        matrix->RSize = tempMat.RSize();
        matrix->CSize = tempMat.CSize();
        matrix->Data =  new double[tempMat.RSize()*tempMat.CSize()];

        int index = 0;
        for(rr::u_int row = 0; row < tempMat.RSize(); row++)
        {
            for(rr::u_int col = 0; col < tempMat.CSize(); col++)
            {
                matrix->Data[index++] = tempMat(row,col);
            }
        }
	    return matrix;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return false;
}

C_DECL_SPEC bool __stdcall hasError()
{
    return (gLastError != NULL) ? true : false;
}

char* __stdcall getLastError()
{
    return gLastError;
}

bool __stdcall reset()
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
        gRRHandle->reset();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }

    return false;
}

int __stdcall getNumberOfReactions()
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return -1;
        }

        return gRRHandle->getNumberOfReactions();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return false;
}

double __stdcall getReactionRate(int rateNr)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return -1;
        }
        return gRRHandle->getReactionRate(rateNr);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return 0;
}

RRVectorHandle __stdcall getReactionRates()
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
        vector<double> vec =  gRRHandle->getReactionRates();

        RRVector* aVec = createVectorFrom(vec);
        return aVec;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return NULL;
}


int __stdcall getNumberOfBoundarySpecies()
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return -1;
        }
        return gRRHandle->getNumberOfBoundarySpecies();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return -1;
}

RRStringListHandle __stdcall getBoundarySpeciesNames()          // <- treat char* as you treat it in setSelectionList (char *)
{
	try
    {

        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        StringList bNames = gRRHandle->getBoundarySpeciesNames();

        if(!bNames.Count())
        {
            return NULL;
        }

        RRStringListHandle list = new RRStringList;
        list->Count = bNames.size();
        list->String = new char*[list->Count];

        for(int i = 0; i < list->Count; i++)
        {
            list->String[i] = new char[bNames[i].size()];
            strcpy(list->String[i], bNames[i].c_str());
        }
        return list;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
   	return NULL;
}

int __stdcall getNumberOfFloatingSpecies()
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return -1;
        }
        return gRRHandle->getNumberOfFloatingSpecies();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
   	return -1;
}

RRStringListHandle __stdcall getFloatingSpeciesNames()
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        StringList fNames = gRRHandle->getFloatingSpeciesNames();

        if(!fNames.Count())
        {
            return NULL;
        }

        RRStringListHandle list = new RRStringList;
        list->Count = fNames.size();
        list->String = new char*[list->Count];

        for(int i = 0; i < list->Count; i++)
        {
            list->String[i] = new char[fNames[i].size()];
            strcpy(list->String[i], fNames[i].c_str());
        }

        return list;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
    return false;
}

int __stdcall getNumberOfGlobalParameters()
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
        return gRRHandle->getNumberOfGlobalParameters();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
   	return -1;
}

RRStringListHandle __stdcall getGlobalParameterNames()
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
        StringList pNames = gRRHandle->getGlobalParameterNames();

        if(!pNames.Count())
        {
            return NULL;
        }

        RRStringListHandle list = new RRStringList;
        list->Count = pNames.size();
        list->String = new char*[list->Count];

        for(int i = 0; i < list->Count; i++)
        {
            list->String[i] = new char[pNames[i].size()];
            strcpy(list->String[i], pNames[i].c_str());
        }
        return list;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
   	return NULL;
}

RRVectorHandle __stdcall getFloatingSpeciesInitialConcentrations()
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        vector<double> vec =  gRRHandle->getFloatingSpeciesInitialConcentrations();

        RRVector* aVec = createVectorFrom(vec);
        return aVec;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return NULL;
}

bool __stdcall setFloatingSpeciesByIndex (int index, double value)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        gRRHandle->setFloatingSpeciesByIndex(index, value);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return false;
}

bool __stdcall setBoundarySpeciesByIndex (int index, double value)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        gRRHandle->setBoundarySpeciesByIndex(index, value);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return false;

}


bool __stdcall setGlobalParameterByIndex(int index, double value)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        gRRHandle->setGlobalParameterByIndex(index, value);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return false;
}

bool __stdcall setFloatingSpeciesInitialConcentrations(RRVector* vec)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
        vector<double> aVec;
        CopyRRVector(vec, aVec);
        gRRHandle->changeInitialConditions(aVec);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return false;
}

double __stdcall oneStep(const double& currentTime, const double& stepSize)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        return gRRHandle->oneStep(currentTime, stepSize);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return 0;
}

RRSymbolListsHandle __stdcall getAvailableSymbols()              // <- You'll have to decide what type to return
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        StringListContainer slSymbols = gRRHandle->getAvailableSymbols();

        RRSymbolListsHandle symbols = new RRSymbolLists;
        symbols->NumberOfLists  = slSymbols.Count();
        symbols->List           = new RRLabelStringList[slSymbols.Count()];

        //Allocate and fill out lists
        for(int listNr = 0; listNr < slSymbols.Count(); listNr++)
        {
            StringList aList = slSymbols[listNr];
            symbols->List[listNr].Count = aList.Count();
            symbols->List[listNr].Label = new char[aList.mLabel.size() + 1];
            strcpy(symbols->List[listNr].Label, aList.mLabel.c_str());

            if(aList.Count())
            {
                symbols->List[listNr].String = new char*[aList.Count()];
                for(int itemNr = 0; itemNr < aList.Count(); itemNr++)
                {
                    symbols->List[listNr].String[itemNr] = new char[aList[itemNr].size() + 1];
                    strcpy(symbols->List[listNr].String[itemNr], aList[itemNr].c_str());
                }
            }
        }
        return symbols;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return NULL;
}

double __stdcall getBoundarySpeciesByIndex (int index)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        return gRRHandle->getBoundarySpeciesByIndex(index);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return -1;  //Todo: return NaN??
}

double __stdcall getFloatingSpeciesByIndex (int index)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        return gRRHandle->getFloatingSpeciesByIndex(index);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return -1;  //Todo: return NaN??

}

double __stdcall getGlobalParameterByIndex (int index)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        return gRRHandle->getGlobalParameterByIndex(index);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return -1;  //Todo: return NaN?? throw??

}

double __stdcall getCC(char* variable, char* parameter)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        return gRRHandle->getCC(variable, parameter);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return -1;  //Todo: return NaN?? throw??
}

double __stdcall getEE(char* name, char* species)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        return gRRHandle->getEE(name, species);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return -1;  //Todo: return NaN?? throw??
}

double __stdcall steadyState()
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return -1;
        }
	   	return gRRHandle->steadyState();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
  	return -1;
}

RRVectorHandle __stdcall computeSteadyStateValues()
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
        vector<double> vec =  gRRHandle->computeSteadyStateValues();

        RRVector* aVec = createVectorFrom(vec);
        return aVec;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return NULL;
}

bool __stdcall setSteadyStateSelectionList(char* list)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        StringList aList(list, " ,");
        gRRHandle->setSteadyStateSelectionList(aList);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return false;
}

RRStringListHandle __stdcall getSteadyStateSelectionList()
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        ArrayList sNames = gRRHandle->getSteadyStateSelectionList();

        if(!sNames.Count())
        {
            return NULL;
        }

        RRStringListHandle list = new RRStringList;
        list->Count = sNames.size();
        list->String = new char*[list->Count];

        for(int i = 0; i < list->Count; i++)
        {
            list->String[i] = new char[sNames[i].size()];
            strcpy(list->String[i], sNames[i].AsString().c_str());
        }

        return list;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return NULL;
}


RRMatrixHandle __stdcall getFullJacobian(void)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        LIB_LA::DoubleMatrix tempMat = gRRHandle->getFullJacobian();
        return createMatrixFrom(tempMat);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return false;
}

RRMatrixHandle __stdcall getReducedJacobian(void)
{
	try
    {
        if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        LIB_LA::DoubleMatrix tempMat = gRRHandle->getReducedJacobian();
        return createMatrixFrom(tempMat);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return false;
}

RRCCode* __stdcall getCCode(void)
{
	try
    {
    	if(!gRRHandle)
        {
            SetAPIError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        CGenerator* generator = gRRHandle->GetCGenerator();
        if(!generator)
        {
            return NULL;
        }

        RRCCode* cCode = new RRCCode;
		cCode->Header = NULL;
		cCode->Source = NULL;
        string header = generator->GetHeaderCode();
        string source = generator->GetSourceCode();

        if(header.size())
        {
            cCode->Header = new char[header.size() + 1];
            strcpy(cCode->Header, header.c_str());
        }

        if(source.size())
        {
            cCode->Source = new char[source.size() + 1];
            strcpy(cCode->Source, source.c_str());
        }
        return cCode;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
	return NULL;
}

//Print functions ==========================================================
char* __stdcall getResultAsString(RRResultHandle result)
{
	try
    {
		stringstream resStr;
		//RRResult is a 2D matrix, and column headers (strings)
        //First header....
	    for(int i = 0; i < result->CSize; i++)
        {
        	resStr<<result->ColumnHeaders[i];
            if(i < result->CSize -1)
            {
            	resStr <<"\t";
            }
        }
        resStr<<endl;

        //Then the data
        int index = 0;
	    for(int j = 0; j < result->RSize; j++)
   	    {
		    for(int i = 0; i < result->CSize; i++)
    	    {
        		resStr<<result->Data[index++];
	            if(i < result->CSize -1)
    	        {
        	    	resStr <<"\t";
            	}
            }
	    	resStr <<"\n";
        }


		string strTmp = resStr.str();
    	char* resultChar = new char[strTmp.size() + 1];
        strcpy(resultChar, strTmp.c_str());
        return resultChar;

    }
    catch(Exception& ex)
    {
        stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
    return NULL;
}

char* __stdcall getMatrixAsString(RRMatrixHandle matrixHandle)
{
	try
    {
        stringstream ss;
        if(!matrixHandle)
        {
            ss<<"Null matrix in printMatrix...";

            return createText(ss.str());
        }

        RRMatrix& mat = *matrixHandle;
        ss<<"matrix dimension: "<<mat.RSize<<"x"<<mat.CSize<<" --\n";

        int index = 0;
        for(int row = 0; row < mat.RSize; row++)
        {
            for(int col = 0; col < mat.CSize; col++)
            {
                ss<<mat.Data[index++];
                if(col < mat.CSize + 1)
                {
                    ss<<"\t";
                }
            }
            ss<<endl;
        }
        return createText(ss.str());
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
}

void __stdcall printMatrix(RRMatrixHandle matrixHandle)
{
	try
    {
        gLog.SetCutOffLogLevel(lDebug2);

        if(!matrixHandle)
        {
            Log(lInfo)<<"Null matrix in printMatrix...";
            return;
        }

        RRMatrix& mat = *matrixHandle;
        stringstream ss;
        ss<<"matrix dimension: "<<mat.RSize<<"x"<<mat.CSize<<" --\n";

        int index = 0;
        for(int row = 0; row < mat.RSize; row++)
        {
            for(int col = 0; col < mat.CSize; col++)
            {
                ss<<mat.Data[index++];
                if(col < mat.CSize + 1)
                {
                    ss<<"\t";
                }
            }
            ss<<endl;
        }
        cout<<ss.str();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
}

void __stdcall printVector(RRVectorHandle vecHandle)
{
	try
    {
        gLog.SetCutOffLogLevel(lDebug2);
        if(!vecHandle)
        {
            Log(lInfo)<<"Null vector in printMatrix...";
            return;
        }

        RRVector& vec = *vecHandle;

        stringstream ss;
        ss<<"vector dimension: "<<vec.Size<<" \n";

        for(int index = 0; index < vec.Size; index++)
        {
            ss<<vec.Data[index];
            if(index < vec.Size + 1)
            {
                ss<<"\t";
            }
        }
        ss<<endl;
        cout<<ss.str();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
}

//Free Functions =====================================================
void __stdcall freeRRInstance(RRHandle handle)
{
	try
    {
        delete gRRHandle;
        gRRHandle = NULL;
        handle = NULL;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
}

bool __stdcall freeMatrix(RRMatrixHandle matrix)
{
	try
    {
        if(matrix)
        {
            delete [] (matrix->Data);
            delete matrix;
        }
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }

    return false;
}

bool __stdcall freeResult(RRResultHandle handle)
{
	try
    {
        delete handle;
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
    return false;
}

bool __stdcall freeText(char* text)
{
	try
    {
        if(text != ALLOCATE_API_ERROR_MSG)
        {
            delete [] text;
        }
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
    return false;
}

bool __stdcall freeStringList(RRStringListHandle sl)
{
	try
    {
    	delete sl;
    	return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
    return false;
}

bool __stdcall freeLabelStringList(RRLabelStringListHandle sl)
{
	try
    {
        delete sl;
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
    return false;
}

bool __stdcall freeVector(RRVectorHandle vector)
{
	try
    {
        if(vector)
        {
            delete [] vector->Data;
        }
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
    return false;
}

bool __stdcall freeCCode(RRCCodeHandle code)
{
	try
    {
        if(code)
        {
            delete code->Header;
            delete code->Source;
        }
		return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        SetAPIError(msg.str());
    }
    return false;
}
