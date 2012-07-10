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
typedef void*                       RRHandle;

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


typedef struct RRSymbolLists
{
    int                 NumberOfLists;
    RRLabelStringList*  List;
} *RRSymbolListsHandle;

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


#if defined( __cplusplus)
}
#endif


#endif
