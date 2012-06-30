unit uRoadRunnerAPI;

{ Copyright 2012 Herbert M Sauro

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

   In plain english this means:

   You CAN freely download and use this software, in whole or in part, for personal,
   company internal, or commercial purposes;

   You CAN use the software in packages or distributions that you create.

   You SHOULD include a copy of the license in any redistribution you may make;

   You are NOT required include the source of software, or of any modifications you may
   have made to it, in any redistribution you may assemble that includes it.

   YOU CANNOT:

   redistribute any piece of this software without proper attribution;
}

interface

Uses SysUtils, Classes, Windows, uMatrix, Generics.Collections, IOUtils, uRRList;

type
  TAnsiCharArray = array[0..20000] of AnsiChar;
  PAnsiCharArray = ^TAnsiCharArray;
  TArrayOfAnsiCharArray = array of PAnsiCharArray;
  PArrayOfAnsiCharArray = ^TArrayOfAnsiCharArray;
  TDoubleArray = array of double;

  TArrayOfPAnsiCharArray = array of PAnsiCharArray;

  TRRResult = record
     RSize : integer;
     CSize : integer;
     Data : array of double;
     ColumnHeaders : ^PAnsiChar;
  end;
  PRRResultHandle = ^TRRResult;


  TRRLabeledStringList = record
    count : integer;
    labelStr : PAnsiChar;
    strList : TArrayOfPAnsiCharArray;
  end;
  PRRLabeledStringList = ^TRRLabeledStringList;


  TRRStringList = record
    count : integer;
    strList : TArrayOfPAnsiCharArray;
  end;
  PRRStringList = ^TRRStringList;


  TRRLabeledSymbolLists = record
    count : integer;
    list : array of TRRLabeledStringList;
  end;
  PRRLabeledSymbolLists = ^TRRLabeledSymbolLists;


  TRRDoubleVector = record
       count : integer;
       data : array of double;
  end;
  PRRDoubleVectorHandle =  ^TRRDoubleVector;


  TLabeledStringList = record
     labeStr : AnsiString;
     stringList : TStringList;
  end;

  TListOfLabeledStringLists = array of TLabeledStringList;

  TRRMatrix = record
    RSize : integer;
    CSize : integer;
    data : array of double;
  end;
  PRRMatrixHandle = ^TRRMatrix;

  TRRCCodeAPI = record
    Header : PAnsiChar;
    Source : PAnsiChar;
  end;
  TRRCCode = record
    Header : AnsiString;
    Source : AnsiString;
  end;
  PRRCCodeHandle = ^TRRCCodeAPI;

  TVoidCharFunc = function : PAnsiChar; stdcall;   //char* func(void)
  TVoidBoolFunc = function : boolean; stdcall; // bool func (void);
  TVoidIntFunc = function : integer; stdcall;
  TVoidDoubleFunc = function : double; stdcall;

  TBoolBoolFunc = function (var value : boolean) : boolean; stdcall;

  TPointerVoidFunc = function : Pointer; stdcall; //void* func(void)
  TCharBoolFunc = function (str : PAnsiChar) : bool; stdcall;  // bool func (char *)
  TDoubleBoolFunc = function (value : double) : bool; stdcall; // bool func (double)
  TIntBoolFunc = function (value : integer) : bool; stdcall;   // bool func (double)
  TIntDoubleFunc = function (index : integer) : double; stdcall;

  TVoidStringListFunc = function() : PRRStringList; stdcall;

  TGetCopyright = TVoidCharFunc;
  TGetRRInstance = TPointerVoidFunc;
  TGetCCode = function : PRRCCodeHandle; stdcall;
  TSetTimeStart = TDoubleBoolFunc;
  TSetTimeEnd = TDoubleBoolFunc;
  TSetNumPoints = TIntBoolFunc;
  TSimulateEx = function (var timeStart : double; var timeEnd : double; var numberOfPoints : integer) : PRRResultHandle;
  TGetStoichiometryMatrix = function : PRRMatrixHandle; stdcall;
  TFreeRRResult = function (ptr : PRRResultHandle) : boolean; stdcall;
  TFreeRRInstance = procedure (instance : Pointer); stdcall;

  TSetSelectionList = function (list : PAnsiChar) : bool; stdcall;
  TGetValue = function (speciesId : PAnsiChar; var value : double) : boolean; stdcall;
  TSetValue = function (speciesId : PAnsiChar; var value : double) : bool; stdcall;
  TGetReactionNames = TPointerVoidFunc;
  TReset = function : bool; stdcall;
  TFreeStringList = procedure (handle : Pointer); stdcall;
  TFreeRRMatrix = function (matrix : PRRMatrixHandle) : boolean; stdcall;
  TFreeRRDoubleVector = function (vector : PRRDoubleVectorHandle) : boolean ; stdcall;
  TOneStep = function (var currentTime : double; var stepSize : double) : double; stdcall;
  TSteadyState = function : double; stdcall;

var
   DLLLoaded : boolean;
   setTimeStart : TSetTimeStart;
   setTimeEnd : TSetTimeEnd;
   setNumberOfPoints : TSetNumPoints;

function  hasError : boolean;
function  getRRInstance : Pointer;
procedure freeRRInstance; overload;
procedure freeRRInstance (myInstance : Pointer); overload;
function  getLastError : AnsiString;
function  getBuildDate : AnsiString;
function  getRevision : integer;
function  getCopyright : AnsiString;
function  getTempFolder : AnsiString;
function  getCCode : TRRCCode;

function  loadSBML (sbmlStr : AnsiString) : boolean;
function  loadSBMLFromFile (fileName : AnsiString) : boolean;
function  getSBML : AnsiString;

function  getValue (Id : AnsiString) : double;
function  setValue (Id : AnsiString; value : double) : boolean;
function  reset : boolean;
function  simulate : TMatrix;
function  simulateEx (timeStart: double; timeEnd : double; numberOfPoints : integer)  : TMatrix;
function  oneStep (var currentTime : double; var stepSize : double) : double;
function  setSelectionList (strList : TStringList) : boolean;
function  getCapabilities : AnsiString;

function  getCompartmentNames : TStringList;
function  getReactionNames : TStringList;
function  getBoundarySpeciesNames : TStringList;
function  getFloatingSpeciesNames : TStringList;
function  getGlobalParameterNames : TStringList;

function  getNumberOfReactions : integer;
function  getNumberOfBoundarySpecies : integer;
function  getNumberOfFloatingSpecies : integer;
function  getNumberOfGlobalParameters : integer;
function  getNumberOfCompartments : integer;

function  setCompartmentByIndex     (index : integer; value : double) : boolean;
function  setFloatingSpeciesByIndex (index : integer; value : double) : boolean;
function  setBoundarySpeciesByIndex (index : integer; value : double) : boolean;
function  setGlobalParameterByIndex (index : integer; value : double) : boolean;

function  getCompartmentByIndex     (index : integer) : double;
function  getFloatingSpeciesByIndex (index : integer) : double;
function  getBoundarySpeciesByIndex (index : integer) : double;
function  getGlobalParameterByIndex (index : integer) : double;

function  getNumberOfDependentSpecies : integer;
function  getNumberOfIndependentSpecies : integer;

function  steadyState : double;
function  computeSteadyStateValues : TDoubleArray;

function  getAvailableSymbols : TRRLIst;

function  setComputeAndAssignConservationLaws (value : boolean) : boolean;

procedure setRoadRunnerLibraryName (newLibName : AnsiString);
function  loadRoadRunner (var errMsg : AnsiString) : boolean;
procedure releaseRoadRunnerLibrary;


implementation

type
  TLibGetAvailableSymbols = function : PRRLabeledSymbolLists; stdcall;
  TlibSetInitialConditions = function (vec : PRRDoubleVectorHandle) : bool; stdcall;
  TlibComputeSteadyStateValues = function : PRRDoubleVectorHandle;


var DLLHandle : Cardinal;
    libName : AnsiString = 'rr_c_API.dll';
    instance : Pointer = nil;

    libLoadSBML : TCharBoolFunc;
    libLoadSBMLFromFile : TCharBoolFunc;
    libGetSBML : TVoidCharFunc;

    libHasError : TVoidBoolFunc;
    libGetLastError : TVoidCharFunc;

    libGetBuildDate : TVoidCharFunc;
    libGetRevision : TVoidIntFunc;
    libGetCopyright : TGetCopyright;
    libGetTempFolder : TVoidCharFunc;
    libGetCCode : TGetCCode;

    libGetRRInstance : TGetRRInstance;
    libFreeRRInstance : TFreeRRInstance;
    libFreeResult : TFreeRRResult;

    libSimulate : TPointerVoidFunc;
    libSimulateEx : TSimulateEx;
    libGetValue : TGetValue;
    libSetValue : TSetValue;
    libSetSelectionList : TSetSelectionList;
    libGetReactionNames : TGetReactionNames;
    libReset : TReset;
    libGetCapabilities : TVoidCharFunc;

    libGetNumberOfReactions : TVoidIntFunc;
    libGetNumberOfBoundarySpecies : TVoidIntFunc;
    libGetNumberOfFloatingSpecies : TVoidIntFunc;
    libGetNumberOfGlobalParameters : TVoidIntFunc;
    libGetNumberOfCompartments : TVoidIntFunc;

    libSetCompartmentByIndex     : function (var index : integer; var value : double) : boolean; stdcall;
    libSetFloatingSpeciesByIndex : function (var index : integer; var value : double) : boolean; stdcall;
    libSetBoundarySpeciesByIndex : function (var index : integer; var value : double) : boolean; stdcall;
    libSetGlobalParameterByIndex : function (var index : integer; var value : double) : boolean; stdcall;

    libGetCompartmentByIndex     : function (var index : integer; var value : double) : boolean; stdcall;
    libGetGlobalParameterByIndex : function (var index : integer; var value : double) : boolean; stdcall;
    libGetFloatingSpeciesByIndex : function (var index : integer; var value : double) : boolean; stdcall;
    libGetBoundarySpeciesByIndex : function (var index : integer; var value : double) : boolean; stdcall;

    libGetNumberOfDependentSpecies : function : integer; stdcall;
    libGetNumberOfIndependentSpecies : function : integer; stdcall;

    libSteadyState : TSteadyState;
    libGetReactionRate : TIntDoubleFunc;
    libOneStep : TOneStep;

    libGetCompartmentNames     : TVoidStringListFunc;
    libGetBoundarySpeciesNames : TVoidStringListFunc;
    libGetFloatingSpeciesNames : TVoidStringListFunc;
    libGetGlobalParameterNames : TVoidStringListFunc;

    libSetSteadyStateSelectionList : TCharBoolFunc;
    libGetAvailableSymbols : TLibGetAvailableSymbols;
    libComputeSteadyStateValues : TlibComputeSteadyStateValues;
    libSetInitialConditions : TlibSetInitialConditions;
    libSetComputeAndAssignConservationLaws : TBoolBoolFunc;

    libGetStoichiometryMatrix :  TGetStoichiometryMatrix;

    libFreeStringList : TFreeStringList;     //
    libFreeMatrix : TFreeRRMatrix; //
    libFreeText : TCharBoolFunc;             //
    libFreeDoubleVector : TFreeRRDoubleVector; //

// Utility Routines
// --------------------------------------------------------------
function getArrayOfStrings (pList: PRRStringList) : TStringList;
var nStrings : integer;
    i, j : integer;
    element : PAnsiCharArray;
    str : AnsiString;
begin
  nStrings := pList^.count;
  result := TStringList.Create;
  for i := 0 to nStrings - 1 do
      begin
      element := pList^.strList[i];
      j := 0; str := '';
      while element[j] <> #0 do
          begin
          str := str + element[j];
          inc (j);
          end;
      result.Add (str);
      end;
end;

// --------------------------------------------------------------
// For doumentation, see the C API docs at:
//   http://code.google.com/p/roadrunnerwork/
// --------------------------------------------------------------

function getRRInstance : Pointer;
begin
  result := libGetRRInstance;
end;

procedure freeRRInstance (myInstance : Pointer);
begin
  if myInstance <> nil then
     libFreeRRInstance (myInstance);
end;

procedure freeRRInstance;
begin
  if instance <> nil then
     libFreeRRInstance (instance);
end;


function getBuildDate : AnsiString;
begin
  result := libGetBuildDate;
end;


function getRevision : integer;
begin
  result := libGetRevision;
end;


function getCopyright : AnsiString;
var p : PAnsiChar;
begin
  p := libGetCopyright();
  result := AnsiString (p);
end;

function  getTempFolder : AnsiString;
begin
  result := libGetTempFolder;
end;

function hasError : boolean;
begin
  result := libHasError;
end;


function getLastError : AnsiString;
begin
  result := libGetLastError;
end;

function getCCode : TRRCCode;
var p : PRRCCodeHandle;
begin
  p := libGetCCode;
  result.Header := p^.Header;
  result.Source := p^.Source;
end;

function setComputeAndAssignConservationLaws (value : boolean) : boolean;
begin
  result := libSetComputeAndAssignConservationLaws (value);
end;

function loadSBML (sbmlStr : AnsiString) : boolean;
begin
  result := libLoadSBML (PAnsiChar (sbmlStr));
end;


function loadSBMLFromFile (fileName : AnsiString) : boolean;
var str : AnsiString;
begin
  if FileExists (fileName) then
     begin
     str := TFile.ReadAllText(fileName);
     result := libLoadSBMLFromFile (PAnsiChar (str));
     end
  else
     raise Exception.Create ('Unable to locate SBML file [' + fileName + ']');
end;


function getSBML : AnsiString;
begin
  result := libGetSBML;
end;


function getValue (Id : AnsiString) : double;
begin
  if not libGetValue (PAnsiChar (Id), result) then
     raise Exception.Create ('Error in getVlaue');
end;


function setValue (Id : AnsiString; value : double) : boolean;
begin
  result := libSetValue (PAnsiChar (Id), value);
end;


function reset : boolean;
begin
  result := libReset;
end;


function getCapabilities : AnsiString;
begin
  result := libGetCapabilities;
end;


function setSelectionList (strList : TStringList) : boolean;
var pList : PArrayOfAnsiCharArray;
    list : TArrayOfAnsiCharArray;
    i, j : integer; l : integer;
    ch : AnsiChar; p : PAnsiCharArray;
    selectionList : AnsiString;
begin
  setLength (list, strList.Count);
  selectionList := strList[0];
  for i := 1 to strList.Count - 1 do
      selectionList := selectionList + ',' + strList[i];
  libSetSelectionList (PAnsiChar (selectionList));
end;


function simulate : TMatrix;
var RRResult : PRRResultHandle;
    i, j : integer;
    nr, nc : integer;
begin
  RRResult := libSimulate;
  try
     nr := RRResult^.RSize;
     nc := RRResult^.CSize;
     result := TMatrix.Create (nr, nc);
     for i := 0 to nr - 1 do
         for j := 0 to nc - 1 do
             result[i+1,j+1] := RRResult^.data[i*nc + j];
  finally
    libFreeResult (RRResult);
  end;
end;


function simulateEx (timeStart: double; timeEnd : double; numberOfPoints : integer)  : TMatrix;
var RRResult : PRRResultHandle;
    i, j : integer;
    nr, nc : integer;
begin
  RRResult := libSimulateEx (timeStart, timeEnd, numberOfPoints);
  try
     nr := RRResult^.RSize;
     nc := RRResult^.CSize;
     result := TMatrix.Create (nr, nc);
     for i := 0 to nr - 1 do
         for j := 0 to nc - 1 do
             result[i+1,j+1] := RRResult^.data[i*nc + j];
  finally
    libFreeResult (RRResult);
  end;
end;


function oneStep (var currentTime : double; var stepSize : double) : double;
begin
  result := libOneStep (currentTime, stepSize);
end;


function getReactionNames : TStringList;
var pList : PRRStringList;
begin
  pList := libGetReactionNames;
  try
    result := getArrayOfStrings(pList);
  finally
    libFreeStringList (pList);
  end;
end;


function getNumberOfReactions : integer;
begin
  result := libGetNumberOfReactions;
end;

function getNumberOfBoundarySpecies : integer;
begin
  result := libGetNumberOfBoundarySpecies;
end;


function getBoundarySpeciesNames : TStringList;
var p : PRRStringList;
begin
  p := libGetBoundarySpeciesNames;
  try
    if p = nil then
       result := TStringList.Create
    else
       result := getArrayOfStrings(p);
  finally
    libFreeStringList (p);
  end;
end;

function  getFloatingSpeciesNames : TStringList;
var p : PRRStringList;
begin
  p := libGetFloatingSpeciesNames;
  try
    if p = nil then
       result := TStringList.Create
    else
       result := getArrayOfStrings(p);
  finally
    libFreeStringList (p);
  end;
end;


function getGlobalParameterNames : TStringList;
var p : PRRStringList;
begin
  p := libGetGlobalParameterNames;
  try
    if p = nil then
       result := TStringList.Create
    else
       result := getArrayOfStrings (p);
  finally
    libFreeStringList (p);
  end;
end;


function getNumberOfFloatingSpecies : integer;
begin
  result := libGetNumberOfFloatingSpecies;
end;


function getNumberOfGlobalParameters : integer;
begin
  result := libGetNumberOfGlobalParameters;
end;


function getNumberOfCompartments : integer;
begin
  result := libGetNumberOfCompartments;
end;


function getCompartmentNames : TStringList;
var pList : PRRStringList;
begin
  pList := libGetCompartmentNames;
  try
    result := getArrayOfStrings(pList);
  finally
    libFreeStringList (pList);
  end;
end;

function setCompartmentByIndex (index : integer; value : double) : boolean;
begin
  result := libSetCompartmentByIndex (index, value);
end;


function setFloatingSpeciesByIndex (index : integer; value : double) : boolean;
begin
  result := libSetFloatingSpeciesByIndex (index, value);
end;


function setBoundarySpeciesByIndex (index : integer; value : double) : boolean;
begin
  result := libSetBoundarySpeciesByIndex (index, value);
end;


function setGlobalParameterByIndex (index : integer; value : double) : boolean;
begin
  result := libSetGlobalParameterByIndex (index, value);
end;


function getCompartmentByIndex (index : integer) : double;
begin
  if not libGetCompartmentByIndex (index, result) then
     raise Exception.Create ('Index out of range in getCompartmentByIndex');
end;


function getFloatingSpeciesByIndex (index : integer) : double;
begin
  if not libGetFloatingSpeciesByIndex (index, result) then
     raise Exception.Create ('Index out of range in getFloatingSpeciesByIndex');
end;


function getBoundarySpeciesByIndex (index : integer) : double;
begin
  if not libGetBoundarySpeciesByIndex (index, result) then
     raise Exception.Create ('Index out of range in getBoundarySpeciesByIndex');
end;


function getGlobalParameterByIndex (index : integer) : double;
begin
  if not libGetGlobalParameterByIndex (index, result) then
     raise Exception.Create ('Index out of range in getGlobalParameterByIndex');
end;


function getNumberOfDependentSpecies : integer;
begin
  result := libGetNumberOfDependentSpecies;
end;

function getNumberOfIndependentSpecies : integer;
begin
  result := libGetNumberOfIndependentSpecies;
end;


function steadyState : double;
begin
  result := libSteadyState;
end;


function computeSteadyStateValues : TDoubleArray;
var p : PRRDoubleVectorHandle; i : integer;
begin
  p := libComputeSteadyStateValues;
  try
    setLength (result, p^.count);
    for i := 0 to p^.count - 1 do
        result[i] := p^.data[i];
  finally
    libFreeDoubleVector (p);
  end;
end;

function setSteadyStateSelectionList (strList : TStringList) : boolean;
var i : integer;
    str : AnsiString;
begin
  if strList.Count > 0 then
     begin
     str := strList[0];
     for i := 1 to strList.Count - 1 do
         str := ' ' + strList[i];
     libSetSteadyStateSelectionList (PAnsiChar (str));
     end;
  result := true;
end;


  {st := getFluxControlCoefficientNames();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Flux Control Coefficients'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;}

  {st := getUnscaledConcentrationControlCoefficientNames();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Unscaled Concentration Control Coefficients'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;}



function getAvailableSymbols : TRRList;
var subList : TRRList; st : TStringList;
    i : integer; item : TRRListItem;
begin
  result := TRRList.Create;
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('time'));
  result.Add (TRRListItem.Create (subList));

  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Floating Species'));
  st := getFloatingSpeciesNames();
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;

  st := getBoundarySpeciesNames();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Boundary Species'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;

  {st := getFloatingSpeciesAmountNames();
  subList := TRRList.Create;
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;}

  {st := getBoundarySpeciesAmountNames();
  subList := TRRList.Create;
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  lresultist.Add (TRRListItem.Create (subList));
  st.Free;}

  st := getGlobalParameterNames();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Global Parameter Names'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;

  {st := getCompartmentNames();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Compartments'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;}

  st := getReactionNames();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Reaction Names'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;

  {st := getRatesOfChangeNames();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Rate of Change Names'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;}

  {st := getElasticityCoefficientNames();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Elasticity Coefficients'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;}

  {st := getUnscaledElasticityCoefficientNames();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Unscaled Elasticity Coefficients'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;}

  {st := getEigenValueNames();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Eigenvalues'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;}

  {p := libGetAvailableSymbols;
  setLength (result, p^.count);
  for i := 0 to p^.count - 1 do
      begin
      result[i].labelStr := p^.list[i].labelStr;
      result[i].stringList := getArrayOfStrings  (@(p^.list[i]));
      end;}
end;


function getReactionRate (index : integer) : double;
begin
  result := libGetReactionRate (index);
end;


function getStoichiometryMatrix : TMatrix;
var st : PRRMatrixHandle;
    nr, nc : integer;
    i, j : integer;
begin
  st := libGetStoichiometryMatrix;
  try
    nr := st^.RSize;
    nc := st^.CSize;
    result := TMatrix.Create (nr, nc);
    for i := 0 to nr - 1 do
        for j := 0 to nc - 1 do
            result[i+1,j+1] := st^.data[i*nc + j];
  finally
    libFreeMatrix (st);
  end;
end;


// ---------------------------------------------------------------------

procedure setRoadRunnerLibraryName (newLibName : AnsiString);
begin
  libName := newLibName;
end;

function loadSingleMethod (methodName : string) : Pointer;
begin
   result := GetProcAddress(dllHandle, PChar (methodName));
   if not Assigned (result) then
      raise Exception.Create ('Unable to locate ' + methodName);
end;

function loadMethods (var errMsg : AnsiString) : boolean;
begin
   result := true;
   try
   @libGetBuildDate  := loadSingleMethod ('getBuildDate');
   @libGetRevision   := loadSingleMethod ('getRevision');
   @libHasError      := loadSingleMethod ('hasError');
   @libGetLastError  := loadSingleMethod ('getLastError');
   @libGetRRInstance := loadSingleMethod ('getRRInstance');

   @libGetCopyright  := loadSingleMethod ('getCopyright');
   @libGetTempFolder := loadSingleMethod ('getTempFolder');
   @libGetCCode := loadSingleMethod ('getCCode');

   @libSetComputeAndAssignConservationLaws := loadSingleMethod ('setComputeAndAssignConservationLaws');

   @libLoadSBMLFromFile := loadSingleMethod ('loadSBMLFromFile');
   @libLoadSBML := loadSingleMethod ('loadSBML');
   @libGetSBML  := loadSingleMethod ('getSBML');

   @setTimeStart := loadSingleMethod ('setTimeStart');
   @setTimeEnd   := loadSingleMethod ('setTimeEnd');
   @setNumberOfPoints := loadSingleMethod ('setNumPoints');
   @libSimulate  := loadSingleMethod ('simulate');
   @libSimulateEx := loadSingleMethod ('simulateEx');
   @libOneStep   := loadSingleMethod ('oneStep');
   @libReset     := loadSingleMethod ('reset');
   @libGetCapabilities := loadSingleMethod ('getCapabilities');

   @libSetValue := loadSingleMethod ('setValue');
   @libGetValue := loadSingleMethod ('getValue');
   @libSetSelectionList := loadSingleMethod ('setSelectionList');

   @libGetNumberOfReactions        := loadSingleMethod ('getNumberOfReactions');
   @libGetNumberOfBoundarySpecies  := loadSingleMethod ('getNumberOfBoundarySpecies');
   @libGetNumberOfFloatingSpecies  := loadSingleMethod ('getNumberOfFloatingSpecies');
   @libGetNumberOfGlobalParameters := loadSingleMethod ('getNumberOfGlobalParameters');
   @libGetNumberOfCompartments     := loadSingleMethod ('getNumberOfCompartments');

   @libSetCompartmentByIndex       := loadSingleMethod ('setCompartmentByIndex');
   @libSetFloatingSpeciesByIndex   := loadSingleMethod ('setFloatingSpeciesByIndex');
   @libSetBoundarySpeciesByIndex   := loadSingleMethod ('setBoundarySpeciesByIndex');
   @libSetGlobalParameterByIndex   := loadSingleMethod ('setGlobalParameterByIndex');

   @libGetCompartmentByIndex       := loadSingleMethod ('getCompartmentByIndex');
   @libGetFloatingSpeciesByIndex   := loadSingleMethod ('getFloatingSpeciesByIndex');
   @libGetBoundarySpeciesByIndex   := loadSingleMethod ('getBoundarySpeciesByIndex');
   @libGetGlobalParameterByIndex   := loadSingleMethod ('getGlobalParameterByIndex');

   @libGetNumberOfDependentSpecies   := loadSingleMethod ('getNumberOfDependentSpecies');
   @libGetNumberOfIndependentSpecies := loadSingleMethod ('getNumberOfIndependentSpecies');

   @libSteadyState                  := loadSingleMethod ('steadyState');
   @libComputeSteadyStateValues     := loadSingleMethod ('computeSteadyStateValues');
   @libSetSteadyStateSelectionList  := loadSingleMethod ('setSteadyStateSelectionList');
   @libSetSteadyStateSelectionList  := loadSingleMethod ('setSteadyStateSelectionList');

   @libGetReactionRate := loadSingleMethod ('getReactionRate');

   @libGetCompartmentNames      := loadSingleMethod ('getCompartmentNames');
   @libGetReactionNames         := loadSingleMethod ('getReactionNames');
   @libGetBoundarySpeciesNames  := loadSingleMethod ('getBoundarySpeciesNames');
   @libGetFloatingSpeciesNames  := loadSingleMethod ('getFloatingSpeciesNames');
   @libGetGlobalParameterNames  := loadSingleMethod ('getGlobalParameterNames');
   @libGetAvailableSymbols      := loadSingleMethod ('getAvailableSymbols');

   @libGetStoichiometryMatrix   := loadSingleMethod ('getStoichiometryMatrix');

   @libFreeRRInstance := loadSingleMethod ('freeRRInstance');
   @libFreeResult     := loadSingleMethod ('freeResult');
   @libFreeMatrix     := loadSingleMethod ('freeMatrix');
   @libFreeText       := loadSingleMethod ('freeText');
   @libFreeStringList := loadSingleMethod ('freeStringList');


   //@libFreeDoubleVector := GetProcAddress (dllHandle, PChar ('freeDoubleVector'));
   //if not Assigned (libFreeDoubleVector) then
   //   begin errMsg := 'Unable to locate freeDoubleVector'; result := false; exit; end;
   except
     on E: Exception do
        begin
        errMsg := e.message;
        result := false;
        exit;
        end;
   end;
end;

function loadRoadRunner (var errMsg : AnsiString) : boolean;
var errStr : string;
    tempString: WideString;
    aString: PChar;
begin
  DLLLoaded := false;
  if FileExists (libName) then
     begin
     tempString := WideString (libName);
     DllHandle := LoadLibrary (PWideChar(tempString));

     if DllHandle <> 0 then
         begin
         if loadMethods (errMsg) then
            begin
            //instance := libGetRRInstance;
            DLLLoaded := True;
            result := true;
            end
         else
            result := false;
         end
     else
         begin
         errStr := SysErrorMessage(Windows.GetLastError);
         DLLLoaded := False;
         errMsg := 'Failed to load roadRunner at:[' + getCurrentDir + ']: ' + errStr;
         end;
     end
  else
     begin
     DLLLoaded := False;
     errMsg := 'Unable to locate roadRunner library at:[' + getCurrentDir + ']';
     end;
end;


procedure releaseRoadRunnerLibrary;
begin
  DLLLoaded := false;
  libFreeRRInstance (instance);
  freeLibrary (DLLHandle);
end;

end.