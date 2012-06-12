unit uRoadRunnerAPI;

interface

Uses SysUtils, Classes, Windows, uMatrix;

type
  TAnsiCharArray = array[0..10] of AnsiChar;
  PAnsiCharArray = ^TAnsiCharArray;
  TArrayOfAnsiCharArray = array of PAnsiCharArray;
  PArrayOfAnsiCharArray = ^TArrayOfAnsiCharArray;

  TArrayOfPAnsiCharArray = array of PAnsiCharArray;

  TVoidCharFunc = function: PAnsiChar; stdcall;   //char* func(void)
  TPointerVoidFunc = function : Pointer; stdcall; //void* func(void)
  TCharBoolFunc = function (str : PAnsiChar) : bool; stdcall;  // bool func (char *)
  TDoubleBoolFunc = function (value : double) : bool; stdcall; // bool func (double)
  TIntBoolFunc = function (value : integer) : bool; stdcall;   // bool func (double)
  TVoidBoolFunc = function : boolean; stdcall; // bool func (void);
  TVoidIntFunc = function : integer; stdcall;
  TVoidDoubleFunc = function : double; stdcall;
  TIntDoubleFunc = function (index : integer) : double; stdcall;

  TGetCopyright = TVoidCharFunc;
  TGetRRInstance = TPointerVoidFunc;
  TDeleteRRInstance = function (p : Pointer) : bool; stdcall;
  TLoadSBML = TCharBoolFunc;
  TSetTimeStart = TDoubleBoolFunc;
  TSetTimeEnd = TDoubleBoolFunc;
  TSetNumPoints = TIntBoolFunc;
  TSimulate = TPointerVoidFunc;
  TFreeRRResult = TVoidBoolFunc;

  TSetSelectionList = function (list : PAnsiChar) : bool; stdcall;
  TGetValue = function (speciesId : PAnsiChar) : bool; stdcall;
  TSetValue = function (speciesId : PAnsiChar; var value : double) : bool; stdcall;
  TGetReactionNames = TPointerVoidFunc;
  TReset = function : bool; stdcall;
  TFreeStringList = procedure (handle : Pointer); stdcall;
  TOneStep = function (var currentTime : double; var stepSize : double) : double; stdcall;

  TRRResult = record
     RSize : integer;
     CSize : integer;
     Data : array of double;
     ColumnHeaders : ^PAnsiChar;
  end;
  PRRResult = ^TRRResult;


  TRRStringList = record
    count : integer;
    strList : TArrayOfPAnsiCharArray;
  end;
  PRRStringList = ^TRRStringList;


var
   DLLLoaded : boolean;
   setTimeStart : TSetTimeStart;
   setTimeEnd : TSetTimeEnd;
   setNumberOfPoints : TSetNumPoints;

   libLoadSBML : TLoadSBML;

function  getCopyright : AnsiString;
function  loadSBML (sbmlStr : AnsiString) : boolean;
function  simulate : TMatrix;
function  setSelectionList (strList : TStringList) : boolean;
function  getReactionNames : TStringList;


procedure setRoadRunnerLibraryName (newLibName : AnsiString);
function  loadRoadRunner (var errMsg : AnsiString) : boolean;
procedure releaseRoadRunnerLibrary;

implementation

var DLLHandle : Cardinal;
    libName : AnsiString = 'rr_c_API.dll';
    instance : Pointer;

    libGetCopyright : TGetCopyright;
    libGetRRInstance : TGetRRInstance;
    libDeleteRRInstance : TDeleteRRInstance;
    libSimulate : TSimulate;
    libFreeRRResult : TFreeRRResult;
    libGetValue : TGetValue;
    libSetValue : TSetValue;
    libSetSelectionList : TSetSelectionList;
    libGetReactionNames : TGetReactionNames;
    libReset : TReset;
    libGetNumberOfReactions : TVoidIntFunc;
    libGetNumberOfBoundarySpecies : TVoidIntFunc;
    libGetNumberOfFloatingSpecies : TVoidIntFunc;
    libGetNumberOfGlobalParameterNames : TVoidIntFunc;
    libSteadyState : TVoidDoubleFunc;
    libGetReactionRate : TIntDoubleFunc;
    libOneStep : TOneStep;
    libGetBoundarySpeciesNames : TVoidCharFunc;
    libGetFloatingSpeciesNames : TVoidCharFunc;
    libGetGlobalParameterNames : TVoidCharFunc;
    libSetSteadyStateSelectionList : TCharBoolFunc;

    libFreeStringList : TFreeStringList;


//bool                    __stdcall   setInitialConditions(RRDoubleVector* vec);     // <- might be called changeInitialConditions in roadRunner
//RSymbolListsHandle     __stdcall   getAvailableSymbols();              // <- You'll have to decide what type to return
//RRDoubleVectorHandle    __stdcall   computeSteadyStateValues();


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


function getCopyright : AnsiString;
var p : PAnsiChar;
begin
  p := libGetCopyright();
  result := AnsiString (p);
end;


function loadSBML (sbmlStr : AnsiString) : boolean;
begin
  result := libLoadSBML (PAnsiChar (sbmlStr));
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
var RRResult : PRRResult;
    i, j : integer;
    nr, nc : integer;
begin
  RRResult := libSimulate;
  nr := RRResult^.RSize;
  nc := RRResult^.CSize;
  result := TMatrix.Create (nr, nc);
  for i := 0 to nr - 1 do
      for j := 0 to nc - 1 do
          result[i+1,j+1] := RRResult^.data[i*nc + j];
  libFreeRRResult;
end;


function getReactionNames : TStringList;
var pList : PRRStringList;
begin
  pList := libGetReactionNames;
  result := getArrayOfStrings(pList);
  libFreeStringList (pList);
end;


procedure setRoadRunnerLibraryName (newLibName : AnsiString);
begin
  libName := newLibName;
end;


function loadMethods (var errMsg : AnsiString) : boolean;
begin
   result := true;
   @libGetRRInstance := GetProcAddress(dllHandle, PChar ('getRRInstance'));
   if not Assigned (libGetRRInstance) then
      begin errMsg := 'Unable to locate getRRInstance'; result := false; exit; end;

   @libGetCopyright := GetProcAddress(dllHandle, PChar ('getCopyright'));
   if not Assigned (libGetCopyright) then
      begin errMsg := 'Unable to locate getCopyright'; result := false; exit; end;

   @setTimeStart := GetProcAddress (dllHandle, PChar ('setTimeStart'));
   if not Assigned (setTimeStart) then
      begin errMsg := 'Unable to locate setTimeStart'; result := false; exit; end;
   @setTimeEnd := GetProcAddress (dllHandle, PChar ('setTimeEnd'));
   if not Assigned (setTimeEnd) then
      begin errMsg := 'Unable to locate setTimeEnd'; result := false; exit; end;
   @setNumberOfPoints := GetProcAddress (dllHandle, PChar ('setNumPoints'));
   if not Assigned (setNumberOfPoints) then
      begin errMsg := 'Unable to locate setNumPoints'; result := false; exit; end;
   @libLoadSBML := GetProcAddress (dllHandle, PChar ('loadSBML'));
   if not Assigned (libLoadSBML) then
      begin errMsg := 'Unable to locate loadSBML'; result := false; exit; end;
   @libSimulate := GetProcAddress (dllHandle, PChar ('simulate'));
   if not Assigned (setNumberOfPoints) then
      begin errMsg := 'Unable to locate simulate'; result := false; exit; end;
   @libFreeRRResult := GetProcAddress (dllHandle, PChar ('freeRRResult'));
   if not Assigned (libFreeRRResult) then
      begin errMsg := 'Unable to locate freeRRResult'; result := false; exit; end;
   @libSetValue := GetProcAddress (dllHandle, PChar ('setValue'));
   if not Assigned (libFreeRRResult) then
      begin errMsg := 'Unable to locate setValue'; result := false; exit; end;
   @libGetValue := GetProcAddress (dllHandle, PChar ('getValue'));
   if not Assigned (libFreeRRResult) then
      begin errMsg := 'Unable to locate getValue'; result := false; exit; end;
   @libSetSelectionList := GetProcAddress (dllHandle, PChar ('setSelectionList'));
   if not Assigned (libFreeRRResult) then
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
   @libGetNumberOfGlobalParameterNames := GetProcAddress (dllHandle, PChar ('getNumberOfGlobalParameterNames'));
   if not Assigned (libGetNumberOfGlobalParameterNames) then
      begin errMsg := 'Unable to locate getNumberOfGlobalParameterNames'; result := false; exit; end;
   @libSteadyState := GetProcAddress (dllHandle, PChar ('steadyState'));
   if not Assigned (libSteadyState) then
      begin errMsg := 'Unable to locate steadyState'; result := false; exit; end;
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
            instance := libGetRRInstance;
            DLLLoaded := True;
            result := true;
            end
         else
            result := false;
         end
     else
         begin
         errStr := SysErrorMessage(GetLastError);
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
  libDeleteRRInstance (instance);
  freeLibrary (DLLHandle);
end;

end.