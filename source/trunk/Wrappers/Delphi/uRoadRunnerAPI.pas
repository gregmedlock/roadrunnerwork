unit uRoadRunnerAPI;

interface

Uses SysUtils, Classes, Windows, uMatrix;

type
  TAnsiCharArray = array[0..10] of AnsiChar;
  PAnsiCharArray = ^TAnsiCharArray;
  TArrayOfAnsiCharArray = array of PAnsiCharArray;
  PArrayOfAnsiCharArray = ^TArrayOfAnsiCharArray;

  TArrayOfPAnsiCharArray = array of PAnsiCharArray;

  TCharVoidFunc = function: PAnsiChar; stdcall;   //char* func(void)
  TPointerVoidFunc = function : Pointer; stdcall; //void* func(void)
  TCharBoolFunc = function (str : PAnsiChar) : bool; stdcall;  // bool func (char *)
  TDoubleBoolFunc = function (value : double) : bool; stdcall; // bool func (double)
  TIntBoolFunc = function (value : integer) : bool; stdcall;   // bool func (double)
  TVoidBoolFunc = function : boolean; stdcall; // bool func (void);

  TGetCopyright = TCharVoidFunc;
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
  TFreeStringList = procedure (handle : Pointer); stdcall;

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

libFreeStringList : TFreeStringList;


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