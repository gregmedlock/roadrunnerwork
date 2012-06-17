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

  TRRDataMatrix = record
    RSize : integer;
    CSize : integer;
    data : array of double;
  end;
  PRRDataMatrixHandle = ^TRRDataMatrix;

  TVoidCharFunc = function : PAnsiChar; stdcall;   //char* func(void)
  TVoidBoolFunc = function : boolean; stdcall; // bool func (void);
  TVoidIntFunc = function : integer; stdcall;
  TVoidDoubleFunc = function : double; stdcall;

  TPointerVoidFunc = function : Pointer; stdcall; //void* func(void)
  TCharBoolFunc = function (str : PAnsiChar) : bool; stdcall;  // bool func (char *)
  TDoubleBoolFunc = function (value : double) : bool; stdcall; // bool func (double)
  TIntBoolFunc = function (value : integer) : bool; stdcall;   // bool func (double)
  TIntDoubleFunc = function (index : integer) : double; stdcall;

  TVoidStringListFunc = function() : PRRStringList; stdcall;

  TGetCopyright = TVoidCharFunc;
  TGetRRInstance = TPointerVoidFunc;
  TSetTimeStart = TDoubleBoolFunc;
  TSetTimeEnd = TDoubleBoolFunc;
  TSetNumPoints = TIntBoolFunc;
  TSimulateEx = function (var timeStart : double; var timeEnd : double; var numberOfPoints : integer) : PRRResultHandle;
  TGetStoichiometryMatrix = function : PRRDataMatrixHandle; stdcall;
  TFreeRRResult = function (ptr : PRRResultHandle) : boolean; stdcall;
  TFreeRRInstance = procedure (instance : Pointer); stdcall;

  TSetSelectionList = function (list : PAnsiChar) : bool; stdcall;
  TGetValue = function (speciesId : PAnsiChar) : double; stdcall;
  TSetValue = function (speciesId : PAnsiChar; var value : double) : bool; stdcall;
  TGetReactionNames = TPointerVoidFunc;
  TReset = function : bool; stdcall;
  TFreeStringList = procedure (handle : Pointer); stdcall;
  TFreeRRDataMatrix = function (matrix : PRRDataMatrixHandle) : boolean; stdcall;
  TFreeRRDoubleVector = function (vector : PRRDoubleVectorHandle) : boolean ; stdcall;
  TOneStep = function (var currentTime : double; var stepSize : double) : double; stdcall;

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
function  getCopyright : AnsiString;

function  loadSBML (sbmlStr : AnsiString) : boolean;
function  loadSBMLFromFile (fileName : AnsiString) : boolean;

function  getValue (Id : AnsiString) : double;
function  setValue (Id : AnsiString; value : double) : boolean;
function  reset : boolean;
function  simulate : TMatrix;
function  simulateEx (timeStart: double; timeEnd : double; numberOfPoints : integer)  : TMatrix;
function  oneStep (var currentTime : double; var stepSize : double) : double;
function  setSelectionList (strList : TStringList) : boolean;
function  getReactionNames : TStringList;
function  getBoundarySpeciesNames : TStringList;
function  getFloatingSpeciesNames : TStringList;
function  getGlobalParameterNames : TStringList;
function  getNumberOfReactions : integer;
function  getNumberOfBoundarySpecies : integer;
function  getNumberOfFloatingSpecies : integer;
function  getNumberOfGlobalParameters : integer;
function  steadyState : double;
function  computeSteadyStateValues : TDoubleArray;

function  getAvailableSymbols : TRRLIst;

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

    libLoadSBML : TCharBoolFunc;            //
    libLoadSBMLFromFile : TCharBoolFunc;    //

    libHasError : TVoidBoolFunc;            //
    libGetLastError : TVoidCharFunc;        //

    libGetBuildDate : TVoidCharFunc;        //
    libGetCopyright : TGetCopyright;        //
    libGetRRInstance : TGetRRInstance;      //
    libFreeRRInstance : TFreeRRInstance;    //
    libFreeRRResult : TFreeRRResult;        //

    libSimulate : TPointerVoidFunc;         //
    libSimulateEx : TSimulateEx;            //
    libGetValue : TGetValue;                //
    libSetValue : TSetValue;                //
    libSetSelectionList : TSetSelectionList;//
    libGetReactionNames : TGetReactionNames;//
    libReset : TReset;                      //
    libGetNumberOfReactions : TVoidIntFunc; //
    libGetNumberOfBoundarySpecies : TVoidIntFunc;//
    libGetNumberOfFloatingSpecies : TVoidIntFunc;//
    libGetNumberOfGlobalParameters : TVoidIntFunc;//
    libSteadyState : TVoidDoubleFunc; //
    libGetReactionRate : TIntDoubleFunc; //
    libOneStep : TOneStep;         //
    libGetBoundarySpeciesNames : TVoidStringListFunc; //
    libGetFloatingSpeciesNames : TVoidStringListFunc; //
    libGetGlobalParameterNames : TVoidStringListFunc; //
    libSetSteadyStateSelectionList : TCharBoolFunc;
    libGetAvailableSymbols : TLibGetAvailableSymbols;
    libComputeSteadyStateValues : TlibComputeSteadyStateValues;
    libSetInitialConditions : TlibSetInitialConditions;

    libGetStoichiometryMatrix :  TGetStoichiometryMatrix;  //

    libFreeStringList : TFreeStringList;     //
    libFreeRRDataMatrix : TFreeRRDataMatrix; //
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


function getCopyright : AnsiString;
var p : PAnsiChar;
begin
  p := libGetCopyright();
  result := AnsiString (p);
end;


function hasError : boolean;
begin
  result := libHasError;
end;


function getLastError : AnsiString;
begin
  result := libGetLastError;
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


function getValue (Id : AnsiString) : double;
begin
  result := libGetValue (PAnsiChar (Id));
end;


function setValue (Id : AnsiString; value : double) : boolean;
begin
  result := libSetValue (PAnsiChar (Id), value);
end;


function reset : boolean;
begin
  result := libReset;
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
    libFreeRRResult (RRResult);
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
    libFreeRRResult (RRResult);
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
      result[i].labeStr := p^.list[i].labelStr;
      result[i].stringList := getArrayOfStrings  (@(p^.list[i]));
      end;}
end;


function getReactionRate (index : integer) : double;
begin
  result := libGetReactionRate (index);
end;


function getStoichiometryMatrix : TMatrix;
var st : PRRDataMatrixHandle;
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
    libFreeRRDataMatrix (st);
  end;
end;


// ---------------------------------------------------------------------

procedure setRoadRunnerLibraryName (newLibName : AnsiString);
begin
  libName := newLibName;
end;


function loadMethods (var errMsg : AnsiString) : boolean;
begin
   result := true;
   @libGetBuildDate := GetProcAddress(dllHandle, PChar ('getBuildDate'));
   if not Assigned (libGetBuildDate) then
      begin errMsg := 'Unable to locate getBuildDate'; result := false; exit; end;
   @libHasError := GetProcAddress(dllHandle, PChar ('hasError'));
   if not Assigned (libHasError) then
      begin errMsg := 'Unable to locate hasError'; result := false; exit; end;
   @libGetLastError := GetProcAddress(dllHandle, PChar ('getLastError'));
   if not Assigned (libGetLastError) then
      begin errMsg := 'Unable to locate getLastError'; result := false; exit; end;

   @libGetRRInstance := GetProcAddress(dllHandle, PChar ('getRRInstance'));
   if not Assigned (libGetRRInstance) then
      begin errMsg := 'Unable to locate getRRInstance'; result := false; exit; end;
   @libFreeRRInstance := GetProcAddress(dllHandle, PChar ('freeRRInstance'));
   if not Assigned (libFreeRRInstance) then
      begin errMsg := 'Unable to locate freeRRInstance'; result := false; exit; end;


   @libGetCopyright := GetProcAddress(dllHandle, PChar ('getCopyright'));
   if not Assigned (libGetCopyright) then
      begin errMsg := 'Unable to locate getCopyright'; result := false; exit; end;

   @libLoadSBMLFromFile := GetProcAddress (dllHandle, PChar ('loadSBMLFromFile'));
   if not Assigned (libLoadSBMLFromFile) then
      begin errMsg := 'Unable to locate loadSBMLFromFile'; result := false; exit; end;
   @libLoadSBML := GetProcAddress (dllHandle, PChar ('loadSBML'));
   if not Assigned (libLoadSBML) then
      begin errMsg := 'Unable to locate loadSBML'; result := false; exit; end;

   @setTimeStart := GetProcAddress (dllHandle, PChar ('setTimeStart'));
   if not Assigned (setTimeStart) then
      begin errMsg := 'Unable to locate setTimeStart'; result := false; exit; end;
   @setTimeEnd := GetProcAddress (dllHandle, PChar ('setTimeEnd'));
   if not Assigned (setTimeEnd) then
      begin errMsg := 'Unable to locate setTimeEnd'; result := false; exit; end;
   @setNumberOfPoints := GetProcAddress (dllHandle, PChar ('setNumPoints'));
   if not Assigned (setNumberOfPoints) then
      begin errMsg := 'Unable to locate setNumPoints'; result := false; exit; end;
   @libSimulate := GetProcAddress (dllHandle, PChar ('simulate'));
   if not Assigned (libSimulate) then
      begin errMsg := 'Unable to locate simulate'; result := false; exit; end;
   @libSimulateEx := GetProcAddress (dllHandle, PChar ('simulateEx'));
   if not Assigned (libSimulateEx) then
      begin errMsg := 'Unable to locate simulateEx'; result := false; exit; end;
   @libSetValue := GetProcAddress (dllHandle, PChar ('setValue'));
   if not Assigned (libSetValue) then
      begin errMsg := 'Unable to locate setValue'; result := false; exit; end;
   @libGetValue := GetProcAddress (dllHandle, PChar ('getValue'));
   if not Assigned (libGetValue) then
      begin errMsg := 'Unable to locate getValue'; result := false; exit; end;
   @libSetSelectionList := GetProcAddress (dllHandle, PChar ('setSelectionList'));
   if not Assigned (libSetSelectionList) then
      begin errMsg := 'Unable to locate setSelectionList'; result := false; exit; end;
   @libGetReactionNames := GetProcAddress (dllHandle, PChar ('getReactionNames'));
   if not Assigned (libGetReactionNames) then
      begin errMsg := 'Unable to locate getReactionNames'; result := false; exit; end;
   @libFreeStringList := GetProcAddress (dllHandle, PChar ('freeStringList'));
   if not Assigned (libFreeStringList) then
      begin errMsg := 'Unable to locate freeStringList'; result := false; exit; end;
   @libReset := GetProcAddress (dllHandle, PChar ('reset'));
   if not Assigned (libReset) then
      begin errMsg := 'Unable to locate reset'; result := false; exit; end;
   @libGetNumberOfReactions := GetProcAddress (dllHandle, PChar ('getNumberOfReactions'));
   if not Assigned (libGetNumberOfReactions) then
      begin errMsg := 'Unable to locate getNumberOfReactions'; result := false; exit; end;
   @libGetNumberOfBoundarySpecies := GetProcAddress (dllHandle, PChar ('getNumberOfBoundarySpecies'));
   if not Assigned (libGetNumberOfBoundarySpecies) then
      begin errMsg := 'Unable to locate getNumberOfBoundarySpecies'; result := false; exit; end;
   @libGetNumberOfFloatingSpecies := GetProcAddress (dllHandle, PChar ('getNumberOfFloatingSpecies'));
   if not Assigned (libGetNumberOfFloatingSpecies) then
      begin errMsg := 'Unable to locate getNumberOfFloatingSpecies'; result := false; exit; end;
   @libGetNumberOfGlobalParameters := GetProcAddress (dllHandle, PChar ('getNumberOfGlobalParameters'));
   if not Assigned (libGetNumberOfGlobalParameters) then
      begin errMsg := 'Unable to locate getNumberOfGlobalParameters'; result := false; exit; end;
   @libSteadyState := GetProcAddress (dllHandle, PChar ('steadyState'));
   if not Assigned (libSteadyState) then
      begin errMsg := 'Unable to locate steadyState'; result := false; exit; end;
   @libComputeSteadyStateValues := GetProcAddress (dllHandle, PChar ('computeSteadyStateValues'));
   if not Assigned (libComputeSteadyStateValues) then
      begin errMsg := 'Unable to locate computeSteadyStateValues'; result := false; exit; end;
   @libSetSteadyStateSelectionList := GetProcAddress (dllHandle, PChar ('setSteadyStateSelectionList'));
   if not Assigned (libSetSteadyStateSelectionList) then
      begin errMsg := 'Unable to locate setSteadyStateSelectionList'; result := false; exit; end;


   @libGetReactionRate := GetProcAddress (dllHandle, PChar ('getReactionRate'));
   if not Assigned (libGetReactionRate) then
      begin errMsg := 'Unable to locate getReactionRate'; result := false; exit; end;
   @libOneStep := GetProcAddress (dllHandle, PChar ('oneStep'));
   if not Assigned (libOneStep) then
      begin errMsg := 'Unable to locate oneStep'; result := false; exit; end;
   @libGetBoundarySpeciesNames := GetProcAddress (dllHandle, PChar ('getBoundarySpeciesNames'));
   if not Assigned (libGetBoundarySpeciesNames) then
      begin errMsg := 'Unable to locate getBoundarySpeciesNames'; result := false; exit; end;
   @libGetFloatingSpeciesNames := GetProcAddress (dllHandle, PChar ('getFloatingSpeciesNames'));
   if not Assigned (libGetFloatingSpeciesNames) then
      begin errMsg := 'Unable to locate getFloatingSpeciesNames'; result := false; exit; end;
   @libGetGlobalParameterNames := GetProcAddress (dllHandle, PChar ('getGlobalParameterNames'));
   if not Assigned (libGetGlobalParameterNames) then
      begin errMsg := 'Unable to locate getGlobalParameterNames'; result := false; exit; end;
   @libSetSteadyStateSelectionList := GetProcAddress (dllHandle, PChar ('setSteadyStateSelectionList'));
   if not Assigned (libSetSteadyStateSelectionList) then
      begin errMsg := 'Unable to locate setSteadyStateSelectionList'; result := false; exit; end;
   @libGetAvailableSymbols := GetProcAddress (dllHandle, PChar ('getAvailableSymbols'));
   if not Assigned (libGetAvailableSymbols) then
      begin errMsg := 'Unable to locate getAvailableSymbols'; result := false; exit; end;

   @libGetStoichiometryMatrix := GetProcAddress (dllHandle, PChar ('getStoichiometryMatrix'));
   if not Assigned (libGetStoichiometryMatrix) then
      begin errMsg := 'Unable to locate getStoichiometryMatrix'; result := false; exit; end;

   @libFreeRRResult := GetProcAddress (dllHandle, PChar ('freeRRResult'));
   if not Assigned (libFreeRRResult) then
      begin errMsg := 'Unable to locate freeRRResult'; result := false; exit; end;
   @libFreeRRDataMatrix := GetProcAddress (dllHandle, PChar ('freeRRDataMatrix'));
   if not Assigned (libFreeRRDataMatrix) then
      begin errMsg := 'Unable to locate freeRRDataMatrix'; result := false; exit; end;
   @libFreeText := GetProcAddress (dllHandle, PChar ('freeText'));
   if not Assigned (libFreeText) then
      begin errMsg := 'Unable to locate freeText'; result := false; exit; end;
   //@libFreeDoubleVector := GetProcAddress (dllHandle, PChar ('freeDoubleVector'));
   //if not Assigned (libFreeDoubleVector) then
   //   begin errMsg := 'Unable to locate freeDoubleVector'; result := false; exit; end;
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