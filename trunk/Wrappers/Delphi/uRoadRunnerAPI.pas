unit uRoadRunnerAPI;

interface

Uses SysUtils, Windows;

var
   DLLLoaded : boolean;

function  getCopyright : AnsiString;

procedure setRoadRunnerLibraryName (newLibName : AnsiString);
function  loadRoadRunner (var errMsg : AnsiString) : boolean;
procedure releaseRoadRunnerLibrary;

implementation

type
  TCharVoidFunc = function: PAnsiChar; stdcall;   //char* func(void)
  TPointerVoidFunc = function : Pointer; stdcall; //void* func(void)

  TGetCopyright = TCharVoidFunc;
  TGetRRInstance = TPointerVoidFunc;


var DLLHandle : Cardinal;
    libName : AnsiString = 'rr_c_API.dll';
    instance : Pointer;

    libGetCopyright : TGetCopyright;
    libGetRRInstance : TGetRRInstance;

function getCopyright : AnsiString;
var p : PAnsiChar;
begin
  p := libGetCopyright();
  result := AnsiString (p);
end;

procedure setRoadRunnerLibraryName (newLibName : AnsiString);
begin
  libName := newLibName;
end;

procedure loadMethods;
begin
   @libGetRRInstance := GetProcAddress(dllHandle, PChar ('getRRInstance'));
   if not Assigned (libGetRRInstance) then
      exit;

   @libGetCopyright := GetProcAddress(dllHandle, PChar ('getCopyright'));
   if not Assigned (libGetCopyright) then
      exit;
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
         loadMethods;
         instance := libGetRRInstance;
         DLLLoaded := True;
         result := true;
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
  freeLibrary (DLLHandle);
end;

end.