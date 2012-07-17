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
#include "rr_version_info.h"
//---------------------------------------------------------------------------

using namespace std;
using namespace rr;
using namespace rr_c_api;
namespace rr_c_api
{
static  rr::RoadRunner*     gRRHandle       = NULL;
char*                       gLastError      = NULL;
}

char* rrCallConv getBuildDate()
{
    char* date = new char[strlen(__DATE__) + 1];
    strcpy(date, __DATE__);
    return date;
}

char* rrCallConv getVersion()
{
    return createText(RR_VERSION);
}

//char* rrCallConv getLatestLog()
//{
//    return createText(SVN_LASTLOG);
//}

//char* rrCallConv getLatestCommitAuthor()
//{
//    char* text = new char[strlen(SVN_LAST_COMMIT_AUTHOR) + 1];
//    strcpy(text, SVN_LAST_COMMIT_AUTHOR);
//    return text;
//}

RRHandle rrCallConv getRRInstance()
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
        setError(msg.str());
    }
  	return NULL;
}

char* rrCallConv getCopyright()
{
	try
    {
        char* text = NULL;
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
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
        setError(msg.str());
    }
	return NULL;
}

char* rrCallConv writeSBML()
{
	try
    {
        char* text = NULL;
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
        }
        else
        {
            string sbml = gRRHandle->writeSBML();
            text = new char[sbml.size() + 1];
            strcpy(text, sbml.c_str());
        }
        return text;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return NULL;
}

//Flags and Options
bool rrCallConv setComputeAndAssignConservationLaws(const bool& OnOrOff)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        gRRHandle->ComputeAndAssignConservationLaws(OnOrOff);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
     }

    return false;
}

bool rrCallConv setTempFolder(const char* folder)
{
	try
    {
    	if(!gRRHandle)
    	{
        	setError(ALLOCATE_API_ERROR_MSG);
        	return false;
    	}
	    return gRRHandle->SetTempFileFolder(folder);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}

char* rrCallConv getTempFolder()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
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
        setError(msg.str());
    }
	return NULL;
}

bool rrCallConv loadSBMLFromFile(const char* sbmlFileName)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        if(!gRRHandle->loadSBMLFromFile(sbmlFileName))
        {
            setError("Failed to load SBML semantics");
            return false;
        }
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return false;
}

bool rrCallConv loadSBML(const char* sbml)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        if(!gRRHandle->loadSBML(sbml))
        {
            setError("Failed to load SBML semantics");
            return false;
        }
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}

char* rrCallConv getSBML()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        string sbml = gRRHandle->getSBML();

        return createText(sbml);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return NULL;
}

bool rrCallConv setTimeStart(const double& timeStart)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
        gRRHandle->setTimeStart(timeStart);
    	return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());

    }

    return false;
}

bool rrCallConv setTimeEnd(const double& timeEnd)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        gRRHandle->setTimeEnd(timeEnd);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return false;
}

bool rrCallConv setNumPoints(const int& nrPoints)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        gRRHandle->setNumPoints(nrPoints);
	    return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}

bool rrCallConv getTimeStart(double& timeStart)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

		timeStart = gRRHandle->getTimeStart();
		return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}


bool rrCallConv getTimeEnd(double& timeEnd)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

		timeEnd = gRRHandle->getTimeEnd();
		return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}


bool rrCallConv getNumPoints(int& numPoints)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

		numPoints = gRRHandle->getNumPoints();
		return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}


bool  rrCallConv setSelectionList(const char* list)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        gRRHandle->setSelectionList(list);
        return true;

    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return false;
}

RRStringListHandle rrCallConv getSelectionList()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        StringList sNames = gRRHandle->getSelectionList();

        if(!sNames.Count())
        {
            return NULL;
        }

        return createList(sNames);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

RRResultHandle rrCallConv simulate()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
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
        setError(msg.str());
    }
	return NULL;
}

RRResultHandle rrCallConv simulateEx (const double& timeStart, const double& timeEnd, const int& numberOfPoints)
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
        setError(msg.str());
    }
	return NULL;
}

RRStringListHandle rrCallConv getReactionNames()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        StringList rNames = gRRHandle->getReactionNames();

        if(!rNames.Count())
        {
            return NULL;
        }


        return createList(rNames);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return NULL;
}

RRVectorHandle rrCallConv getRatesOfChange()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        vector<double> rates = gRRHandle->getRatesOfChange();

        if(!rates.size())
        {
            return NULL;
        }

        RRVector* list = new RRVector;
        list->Size = rates.size();
        list->Data = new double[list->Size];

        for(int i = 0; i < list->Size; i++)
        {
            list->Data[i] = rates[i];
            return list;
        }
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return NULL;
}


RRStringListHandle rrCallConv getRatesOfChangeNames()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        StringList rNames = gRRHandle->getRateOfChangeNames();

        if(!rNames.Count())
        {
            return NULL;
        }

        return createList(rNames);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return NULL;
}

bool rrCallConv getValue(const char* speciesID, double& value)
{
	try
    {
    	if(!gRRHandle)
    	{
        	setError(ALLOCATE_API_ERROR_MSG);
        	return false;
    	}
	    value = gRRHandle->getValue(speciesID);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return false;	//todo: how to indicate error???
}


RRMatrixHandle rrCallConv getUnScaledElasticityMatrix()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

		LIB_LA::DoubleMatrix tempMat = gRRHandle->getUnscaledElasticityMatrix();

        RRMatrixHandle matrix = createMatrix(tempMat);
	    return matrix;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return NULL;
}

RRMatrixHandle rrCallConv getScaledElasticityMatrix()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        LIB_LA::DoubleMatrix tempMat = gRRHandle->getScaledElasticityMatrix();


        RRMatrixHandle matrix = createMatrix(tempMat);
	    return matrix;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return NULL;
}

bool rrCallConv setValue(const char* speciesID, const double& val)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
	    return gRRHandle->setValue(speciesID, val);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return NULL;
}

RRMatrixHandle rrCallConv getStoichiometryMatrix()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
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
        setError(msg.str());
    }
	return false;
}

RRMatrixHandle rrCallConv getConservationMatrix()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        LIB_LA::DoubleMatrix tempMat = gRRHandle->getConservationMatrix();

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
        setError(msg.str());
    }
	return false;
}

RRMatrixHandle rrCallConv getLinkMatrix()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        LIB_LA::DoubleMatrix tempMat = gRRHandle->getLinkMatrix();
        
		return createMatrix(tempMat);
	}
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return false;
}

RRMatrixHandle rrCallConv getL0Matrix()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        LIB_LA::DoubleMatrix tempMat = gRRHandle->getL0Matrix();
        
		return createMatrix(tempMat);
	}
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return false;
}

RRMatrixHandle rrCallConv getNrMatrix()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        LIB_LA::DoubleMatrix tempMat = gRRHandle->getNrMatrix();
        
		return createMatrix(tempMat);
	}
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return false;
}

C_DECL_SPEC bool rrCallConv hasError()
{
    return (gLastError != NULL) ? true : false;
}

char* rrCallConv getLastError()
{
    return gLastError;
}

bool rrCallConv reset()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
        gRRHandle->reset();
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }

    return false;
}

int rrCallConv getNumberOfReactions()
{
 	try
    {
        if(!gRRHandle)
        {
           setError(ALLOCATE_API_ERROR_MSG);
           return -1;
        }
        return gRRHandle->getNumberOfReactions();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return -1;
}

bool rrCallConv getReactionRate(const int& rateNr, double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
        value = gRRHandle->getReactionRate(rateNr);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}

RRVectorHandle rrCallConv getReactionRates()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        vector<double> vec =  gRRHandle->getReactionRates();

        RRVector* aVec = createVector(vec);
        return aVec;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return NULL;
}

int rrCallConv getNumberOfBoundarySpecies()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return -1;
        }
        return gRRHandle->getNumberOfBoundarySpecies();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return -1;
}

RRStringListHandle rrCallConv getBoundarySpeciesNames()          // <- treat char* as you treat it in setSelectionList (char *)
{
	try
    {

        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        StringList bNames = gRRHandle->getBoundarySpeciesNames();

        if(!bNames.Count())
        {
            return NULL;
        }

        return createList(bNames);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
   	return NULL;
}

int rrCallConv getNumberOfFloatingSpecies()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return -1;
        }
        return gRRHandle->getNumberOfFloatingSpecies();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
   	return -1;
}

RRStringListHandle rrCallConv getFloatingSpeciesNames()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        StringList fNames = gRRHandle->getFloatingSpeciesNames();

        if(!fNames.Count())
        {
            return NULL;
        }

        return createList(fNames);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return false;
}

int rrCallConv getNumberOfGlobalParameters()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return -1;
        }
        return gRRHandle->getNumberOfGlobalParameters();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
   	return -1;
}

RRStringListHandle rrCallConv getGlobalParameterNames()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        StringList pNames = gRRHandle->getGlobalParameterNames();

        if(!pNames.Count())
        {
            return NULL;
        }

        return createList(pNames);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
   	return NULL;
}

RRVectorHandle rrCallConv getFloatingSpeciesConcentrations()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        vector<double> vec =  gRRHandle->getFloatingSpeciesConcentrations();
        RRVector* aVec = createVector(vec);
        return aVec;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return NULL;
}

RRVectorHandle rrCallConv getFloatingSpeciesInitialConcentrations()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        vector<double> vec =  gRRHandle->getFloatingSpeciesInitialConcentrations();
        RRVector* aVec = createVector(vec);
        return aVec;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return NULL;
}

bool rrCallConv setFloatingSpeciesByIndex (const int& index, const double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        gRRHandle->setFloatingSpeciesByIndex(index, value);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}

bool rrCallConv setBoundarySpeciesByIndex (const int& index, const double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        gRRHandle->setBoundarySpeciesByIndex(index, value);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}

bool rrCallConv setGlobalParameterByIndex(const int& index, const double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        gRRHandle->setGlobalParameterByIndex(index, value);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}

bool rrCallConv setFloatingSpeciesInitialConcentrations(RRVector* vec)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
        vector<double> tempVec;
        copyVector(vec, tempVec);
        gRRHandle->changeInitialConditions(tempVec);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}

bool rrCallConv oneStep(const double& currentTime, const double& stepSize, double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        value = gRRHandle->oneStep(currentTime, stepSize);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return false;
}


RRVectorHandle rrCallConv getGlobalParameterValues()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        vector<double> vec =  gRRHandle->getGlobalParameterValues();
        RRVector* aVec = createVector(vec);
        return aVec;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    	return NULL;
    }
}

RRArrayList2Handle rrCallConv getAvailableSymbols()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }


        ArrayList2 slSymbols = gRRHandle->getAvailableSymbols();
        slSymbols.Add(34.56);
        return createList(slSymbols);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    	return NULL;
    }
}

bool rrCallConv getBoundarySpeciesByIndex (const int& index, double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        value = gRRHandle->getBoundarySpeciesByIndex(index);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}

bool rrCallConv getFloatingSpeciesByIndex (const int& index, double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        value = gRRHandle->getFloatingSpeciesByIndex(index);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}

bool rrCallConv getGlobalParameterByIndex (const int& index, double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        value = gRRHandle->getGlobalParameterByIndex(index);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}



bool rrCallConv getuCC (const char* variable, const char* parameter, double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        value = gRRHandle->getuCC(variable, parameter);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}


bool rrCallConv getCC (const char* variable, const char* parameter, double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        value = gRRHandle->getCC(variable, parameter);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}


bool rrCallConv getuEE(const char* name, const char* species, double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        value = gRRHandle->getuEE(name, species);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}


bool rrCallConv getEE(const char* name, const char* species, double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        value = gRRHandle->getEE(name, species);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}

int rrCallConv getNumberOfDependentSpecies()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return -1;
        }

        return gRRHandle->getNumberOfDependentSpecies();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return -1;
}

int rrCallConv getNumberOfIndependentSpecies()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return -1;
        }

        return gRRHandle->getNumberOfIndependentSpecies();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return -1;
}

bool rrCallConv steadyState(double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
	   	value = gRRHandle->steadyState();
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
  	return false;
}

bool rrCallConv evalModel()
{
	try
	{
		if(!gRRHandle)
		{
			setError(ALLOCATE_API_ERROR_MSG);
		}
		gRRHandle->EvalModel();
        return true;
	}
	catch(Exception& ex)
	{
		stringstream msg;
		msg<<"RoadRunner exception: "<<ex.what()<<endl;
		setError(msg.str());
	}
    return false;
}

char* rrCallConv getParamPromotedSBML(const char* sArg)
{
	try
	{
		if(!gRRHandle)
		{
			setError(ALLOCATE_API_ERROR_MSG);
			return NULL;
		}

		string param =  gRRHandle->getParamPromotedSBML(sArg);

		char* text = createText(param.c_str());
		return text;
	}
	catch(Exception& ex)
	{
		stringstream msg;
		msg<<"RoadRunner exception: "<<ex.what()<<endl;
		setError(msg.str());
	}
	return NULL;
}

RRVectorHandle rrCallConv computeSteadyStateValues()
{
	try
	{
		if(!gRRHandle)
		{
			setError(ALLOCATE_API_ERROR_MSG);
			return NULL;
		}
		vector<double> vec =  gRRHandle->computeSteadyStateValues();

		RRVector* aVec = createVector(vec);
		return aVec;
	}
	catch(Exception& ex)
	{
		stringstream msg;
		msg<<"RoadRunner exception: "<<ex.what()<<endl;
		setError(msg.str());
	}
	return NULL;
}

bool rrCallConv setSteadyStateSelectionList(const char* list)
{
	try
	{
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
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
        setError(msg.str());
    }
	return false;
}

RRStringListHandle rrCallConv getSteadyStateSelectionList()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        ArrayList sNames = gRRHandle->getSteadyStateSelectionList();

        if(!sNames.Count())
        {
            return NULL;
        }

        return createList(sNames);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return NULL;
}

RRMatrixHandle rrCallConv getFullJacobian()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        LIB_LA::DoubleMatrix tempMat = gRRHandle->getFullJacobian();
        return createMatrix(tempMat);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return NULL;
}

RRMatrixHandle rrCallConv getReducedJacobian()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        LIB_LA::DoubleMatrix tempMat = gRRHandle->getReducedJacobian();
        return createMatrix(tempMat);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return NULL;
}


RRMatrixHandle rrCallConv getEigenvalues()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

		LIB_LA::DoubleMatrix tempMat = gRRHandle->getEigenvalues();
        return createMatrix(tempMat);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
	return NULL;
}


RRCCode* rrCallConv getCCode()
{
	try
    {
    	if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
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
        setError(msg.str());
    }
	return NULL;
}

//The latest.. ////////////////////////////////////////

bool rrCallConv getScaledFloatingSpeciesElasticity(const char* reacName, const char* specName, double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
        value = gRRHandle->getScaledFloatingSpeciesElasticity(reacName, specName);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return false;
}

RRStringListHandle rrCallConv getFloatingSpeciesInitialConditionNames()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        StringList aList = gRRHandle->getFloatingSpeciesInitialConditionNames();
        return createList(aList);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

RRVectorHandle rrCallConv getRatesOfChangeEx(const RRVectorHandle vec)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        vector<double> tempList = createVector(vec);
        tempList = gRRHandle->getRatesOfChangeEx(tempList);
        return createVector(tempList);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

RRVectorHandle rrCallConv getReactionRatesEx(const RRVectorHandle vec)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        vector<double> tempList = createVector(vec);
        tempList = gRRHandle->getReactionRatesEx(tempList);
        return createVector(tempList);;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

RRStringArrayList* rrCallConv getElasticityCoefficientNames()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        //RRArrayList<string> list = gRRHandle->getConcentrationControlCoefficientNames();
        StringArrayList aList = gRRHandle->getElasticityCoefficientNames();
        return createList(aList);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

RRStringListHandle rrCallConv getRateOfChangeNames()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }
        StringList aList = gRRHandle->getRateOfChangeNames();
        return createList(aList);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return false;
}

bool rrCallConv setCapabilities(const char* caps)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return false;
        }

        if(!caps)
        {
            return false;
        }
        gRRHandle->setCapabilities(caps);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return false;
}

char* rrCallConv getCapabilities()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        return createText(gRRHandle->getCapabilities());
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
RRStringListHandle rrCallConv getEigenValueNames()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        StringList aList = gRRHandle->getEigenValueNames();
        return createList(aList);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

RRStringArrayListHandle rrCallConv getFluxControlCoefficientNames()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        return createList(gRRHandle->getFluxControlCoefficientNames());
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

RRMatrixHandle rrCallConv getUnscaledConcentrationControlCoefficientMatrix()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        return createMatrix(gRRHandle->getUnscaledConcentrationControlCoefficientMatrix());
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}


RRMatrixHandle rrCallConv getScaledConcentrationControlCoefficientMatrix()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        return createMatrix(gRRHandle->getScaledConcentrationControlCoefficientMatrix());
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}


RRMatrixHandle rrCallConv getUnscaledFluxControlCoefficientMatrix()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        return createMatrix(gRRHandle->getUnscaledFluxControlCoefficientMatrix());
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}


RRMatrixHandle rrCallConv getScaledFluxControlCoefficientMatrix()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        return createMatrix(gRRHandle->getScaledFluxControlCoefficientMatrix());
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

RRStringArrayListHandle rrCallConv getUnscaledFluxControlCoefficientNames()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }

        return createList(gRRHandle->getUnscaledFluxControlCoefficientNames());
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

RRStringArrayList* rrCallConv getConcentrationControlCoefficientNames()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        RRArrayList<string> list = gRRHandle->getConcentrationControlCoefficientNames();
        return createList(list);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

RRStringArrayListHandle rrCallConv getUnscaledConcentrationControlCoefficientNames()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        return createList(gRRHandle->getUnscaledConcentrationControlCoefficientNames());
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

int rrCallConv getNumberOfCompartments()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        return gRRHandle->getNumberOfCompartments();
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return -1;
}

bool rrCallConv getCompartmentByIndex(const int& index, double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        value = gRRHandle->getCompartmentByIndex(index);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return false;
}

bool rrCallConv setCompartmentByIndex (const int& index, const double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        gRRHandle->setCompartmentByIndex(index, value);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return false;
}

RRStringListHandle rrCallConv getCompartmentNames()
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        return createList(gRRHandle->getCompartmentNames());
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

bool rrCallConv getRateOfChange(const int& index, double& value)
{
	try
    {
        if(!gRRHandle)
        {
            setError(ALLOCATE_API_ERROR_MSG);
            return NULL;
        }
        value = gRRHandle->getRateOfChange(index);
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return false;
}

//Print functions ==========================================================
char* rrCallConv printArrayList(const RRArrayList2Handle list)
{
	try
    {
        if(!list)
        {
            return NULL;
        }

        //Types of list items
        char*           cVal;
        int*            intVal;
        double*         dVal;
        RRArrayList2*   lVal; 		//list is nested list

		stringstream resStr;
        resStr<<"{";
	    for(int i = 0; i < list->ItemCount; i++)
        {
            switch(list->Items[i].ItemType)
            {
                case litString:
                    cVal = (char *) list->Items[i].pValue;
                    resStr<<"\""<< (cVal) <<"\"";
                break;

                case litInteger:
                    intVal = (int *) list->Items[i].pValue;
                    resStr<< (*intVal);
                break;

                case litDouble:
                    dVal = (double *) list->Items[i].pValue;
                    resStr<< (*dVal);
                break;

                case litArrayList:
                    lVal = (RRArrayList2 *) list->Items[i].pValue;
                    resStr<<printArrayList(lVal);
                break;
            }

            if(i < list->ItemCount -1)
            {
                resStr<<",";
            }
        }
        resStr<<"}";
		string strTmp = resStr.str();
    	char* resultChar = new char[strTmp.size() + 1];
        strcpy(resultChar, strTmp.c_str());
        return resultChar;

    }
    catch(Exception& ex)
    {
        stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
        return NULL;
    }
}

char* rrCallConv printList(const RRStringListHandle list)
{
	try
    {
        if(!list)
        {
            return NULL;
        }

		stringstream resStr;
		//RRResult is a 2D matrix, and column headers (strings)
        //First header....
	    for(int i = 0; i < list->Count; i++)
        {
        	resStr<<list->String[i];;
            if(i < list->Count -1)
            {
            	resStr <<endl;
            }
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
        setError(msg.str());
    }
    return NULL;
}

char* rrCallConv  printStringArrayList(const RRStringArrayList* list)
{
	try
    {
        if(!list)
        {
            return NULL;
        }

		stringstream resStr;
		//list  is actually a nested list

        resStr<<"{";
	    for(int i = 0; i < list->ItemCount; i++)
        {

            if(list->Items[i].Item != NULL)
            {
                resStr<<"\""<<list->Items[i].Item<<"\"";
                if(i < list->ItemCount -1)
                {
                    resStr<<",";
                }

            }
            else
            {
                resStr<<printStringArrayList(list->Items[i].SubList);   //Recursive call..
            }
        }
        resStr<<"}";
		string strTmp = resStr.str();
    	char* resultChar = new char[strTmp.size() + 1];
        strcpy(resultChar, strTmp.c_str());
        return resultChar;

    }
    catch(Exception& ex)
    {
        stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

char* rrCallConv printResult(const RRResultHandle result)
{
	try
    {
        if(!result)
        {
            return NULL;
        }
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
        setError(msg.str());
    }
    return NULL;
}

char* rrCallConv getMatrixAsString(const RRMatrixHandle matrixHandle)
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
        setError(msg.str());
    }
    return NULL;
}

char* rrCallConv printMatrix(const RRMatrixHandle matrixHandle)
{
	try
    {
        if(!matrixHandle)
        {
            return NULL;
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
        string msg(ss.str());
        return createText(msg);
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

char* rrCallConv printVector(RRVectorHandle vecHandle)
{
	try
    {
        if(!vecHandle)
        {
            setError("Null vector in printMatrix...");
            return NULL;
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
        return createText(ss.str());
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return NULL;
}

//Free Functions =====================================================
bool rrCallConv freeRRInstance(RRHandle handle)
{
	try
    {
        delete gRRHandle;
        gRRHandle = NULL;
        handle = NULL;
        return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return false;
}

bool rrCallConv freeMatrix(RRMatrixHandle matrix)
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
        setError(msg.str());
    }

    return false;
}

bool rrCallConv freeResult(RRResultHandle handle)
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
        setError(msg.str());
    }
    return false;
}

bool rrCallConv freeText(char* text)
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
        setError(msg.str());
    }
    return false;
}

bool rrCallConv freeStringList(RRStringListHandle sl)
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
        setError(msg.str());
    }
    return false;
}

bool rrCallConv freeLabelStringList(RRLabelStringListHandle sl)
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
        setError(msg.str());
    }
    return false;
}

bool rrCallConv freeVector(RRVectorHandle vector)
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
        setError(msg.str());
    }
    return false;
}

bool rrCallConv freeCCode(RRCCodeHandle code)
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
        setError(msg.str());
    }
    return false;
}

bool rrCallConv freeStringArrayList(RRStringArrayListHandle theList)
{
	try
    {
        if(!theList)
        {
            return true;
        }

        int itemCount = theList->ItemCount;
        for(int i = 0; i < itemCount; i++)
        {
            if(theList->Items[i].Item != NULL)
            {
                delete [] theList->Items[i].Item ;
            }
            else
            {
                //Item is a sublist
                freeStringArrayList(theList->Items[i].SubList);
            }
        }

        delete [] theList->Items;
    	delete theList;
    	return true;
    }
    catch(Exception& ex)
    {
    	stringstream msg;
    	msg<<"RoadRunner exception: "<<ex.what()<<endl;
        setError(msg.str());
    }
    return false;
}

void rrCallConv Pause()
{
    rr::Pause(true);
}


int rrCallConv getVectorLength (RRVectorHandle vector)
{
	if (vector == NULL)
		return -1;
	else
		return vector->Size;
}

bool rrCallConv getVectorElement (RRVectorHandle vector, int index, double& value)
{
	if (vector == NULL)
		return false;
	if ((index < 0) || (index >= vector->Size))
		return false;
	value = vector->Data[index];
	return true;
}


bool rrCallConv setVectorElement (RRVectorHandle vector, int index, double value)
{
	if (vector == NULL)
		return false;
	if ((index < 0) || (index >= vector->Size))
		return false;
	vector->Data[index] = value;
	return true;
}


int rrCallConv getStringListLength (RRStringListHandle stringList)
{
	if (stringList == NULL)
		return -1;
	return stringList->Count;
}


char* rrCallConv getStringListElement (RRStringListHandle stringList, int index)
{
	if (stringList == NULL)
		return NULL;
	if ((index < 0) || (index >= stringList->Count))
		return NULL;
	return stringList->String[index];
}

int rrCallConv  getMatrixNumRows (RRMatrixHandle m)
{
	if (m == NULL)
		return -1;
	return m->RSize;
}

int  rrCallConv  getMatrixNumCols (RRMatrixHandle m)
{
	if (m == NULL)
		return -1;
	return m->CSize;
}

bool rrCallConv getMatrixElement (RRMatrixHandle m, int r, int c, double& value)
{
	if (m == NULL)
		return false;
	if ((r < 0) || (c < 0) || (r >= m->RSize) || (c >= m->CSize))
		return false;
	value = m->Data[r*m->CSize + c];
	return true;
}


int rrCallConv  getResultNumRows (RRResultHandle result)
{
	if (result == NULL)
		return -1;
	return result->RSize;
}

int  rrCallConv  getResultNumCols (RRResultHandle result)
{
	if (result == NULL)
		return -1;
	return result->CSize;
}

bool  rrCallConv getResultElement (RRResultHandle result, int r, int c, double& value)
{
	if (result == NULL)
		return false;
	if ((r < 0) || (c < 0) || (r >= result->RSize) || (c >= result->CSize))
		return false;
	value = result->Data[r*result->CSize + c];
	return true;
}

char*  rrCallConv  getResultColumnLabel (RRResultHandle result, int column)
{
	if (result == NULL)
		return NULL;
	if ((column < 0) || (column >= result->CSize))
		return NULL;
	return result->ColumnHeaders[column];
}

char* rrCallConv  getCCodeHeader (RRCCodeHandle code)
{
	if (code == NULL)
		return NULL;
	return code->Header;
}

char* rrCallConv  getCCodeSource (RRCCodeHandle code)
{
	if (code == NULL)
		return NULL;
	return code->Source;
}


