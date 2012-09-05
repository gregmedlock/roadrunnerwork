/**
 * @file rr_c_types.h
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

/*! \mainpage cRoadRunner Library
 *
 * \section intro_sec Introduction
 *
 * RoadRunner is a high performance and portable simulation engine 
 * for systems and synthetic biology. To run a simple SBML model 
 * and generate time series data we would call:
 *
 \code
 RRResultHandle output;

 loadSBMLFromFile ("mymodel.xml");

 output = simulate (0, 10, 100);
 \endcode

 More complex example:

 \code
 #include <stdlib.h>
 #include <stdio.h>
 #include "rr_c_api.h"

 int main(int nargs, char** argv)
 {
        RRResultHandle efm, output, params;

        if (nargs < 2)
        {
            m1 = model1();
        }
        else
        {
            printf("loading model file %s\n", argv[1]);
            loadSBMLFromFile(argv[1]);
        }


        output = simualte (0, 100, 1000);  // start, end, num. points
        printf("output.tab has %i rows and %i columns\n", output->RSize, output->RCols);
        printResult (output);
        freeREsult (output);

        return 0;
 }
 \endcode
 * \section install_sec Installation
 *
 * Installation documentation is provided in the main google code page.

 \defgroup loadsave Read and Write models
 \brief Read and write models to files or strings. Support for SBML formats.

 \defgroup state Current state of system
 \brief Compute derivatives, fluxes, and other values of the system at the current state

 \defgroup reaction Reaction group
 \brief Get information about reaction rates

 \defgroup rateOfChange Rates of change group
 \brief Get information about rates of change

 \defgroup boundary Boundary species group
 \brief Get information about reaction rates

 \defgroup floating Floating species group
 \brief Get information about reaction rates

 \defgroup parameters Parameter group
 \brief set and get global and local parameters

 \defgroup compartment Compartment group
 \brief set and get information on compartments

 \defgroup simulation Time-course simulation
 \brief Deterministic, stochastic, and hybrid simulation algorithms

 \defgroup mca Metabolic Control Analysis
 \brief Calculate control coefficients and sensitivities

 \defgroup matrix Stoichiometry analysis
 \brief Linear algebra based methods for analyzing a reaction network
*/

#ifndef rr_c_typesH
#define rr_c_typesH

#if defined(__cplusplus)
extern "C"
{
#else
#include <stdio.h>
#include "rr_support/stdbool.h"
#endif
                                    /*Hand the client a HANDLE, i.e. ptr to structure */
typedef void* RRHandle;

typedef struct RRVector
{
    int             Size;
    double*         Data;
} *RRVectorHandle;


typedef struct RRLabelStringList
{
    int             Count;
    char*           Label;
    char**          String;
} *RRLabelStringListHandle;


typedef struct RRStringList
{
    int             Count;
    char**          String;
} *RRStringListHandle;

typedef struct RRMatrix
{
    int             RSize;
    int             CSize;
    double*         Data;
} *RRMatrixHandle;

typedef struct RRResult
{
    int             RSize;
    int             CSize;
    double*         Data;
    char**          ColumnHeaders;
} *RRResultHandle;

typedef struct RRCCode
{
    char*   Header;
    char*   Source;

} *RRCCodeHandle;


struct RRStringArrayList;

typedef struct RRStringArrayListItem
{
    char*                       Item;
    RRStringArrayList*          SubList;
} *RRStringArrayListItemHandle;


typedef struct RRStringArrayList
{
    int                         ItemCount;
    RRStringArrayListItemHandle Items;

}  *RRStringArrayListHandle;

enum ListItemType {litString, litInteger, litDouble,   litArrayList};

// The above enums correspond to the currently supported types in an RRArrayList2
// char, int, double, RRArrayList2
// The void pointer pValue need to be casted to corresponding type to retrieve its value

struct RRArrayList2;
typedef struct RRArrayList2Item
{
    ListItemType                ItemType;
    void*                       pValue;
} *RRArrayList2ItemHandle;


typedef struct RRArrayList2
{
    int                         ItemCount;
    RRArrayList2ItemHandle      Items;
}  *RRArrayList2Handle;

#if defined( __cplusplus)
}
#endif


#endif
