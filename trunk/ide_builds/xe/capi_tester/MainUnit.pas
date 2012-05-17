unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, ExtCtrls, StdActns;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Memo1: TMemo;
    FileNameE: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ActionList1: TActionList;
    LoadDLL: TAction;
    UnloadDLL: TAction;
    SelectDLLA: TAction;
    FunctionList: TListBox;
    FileOpen1: TFileOpen;
    Button4: TButton;
    LoadFunctionsA: TAction;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Button5: TButton;
    APIFuncs: TActionList;
    CharStarVoidA: TAction;
    procedure FileOpen1Accept(Sender: TObject);
    procedure FileOpen1BeforeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LoadDLLExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure LoadFunctionsAExecute(Sender: TObject);
    procedure UnloadDLLExecute(Sender: TObject);
    procedure FunctionListClick(Sender: TObject);
    procedure CharStarVoidAExecute(Sender: TObject);
    procedure APIFuncsUpdate(Action: TBasicAction; var Handled: Boolean);
  private
    { Private declarations }
    procedure CheckDLL(fName: string);
  public
    { Public declarations }
  end;
var
    Form1: TForm1;
    mIsLoaded: BOOL;
    dllHandle: THandle;

implementation

{$R *.dfm}
const DLLName = 'rrC_API.dll';

type
TCharVoidFunc   = function: PAnsiChar;  stdcall;        //char* func(void)
TIntVoidFunc    = function: Integer;    stdcall;        //int   func(void)

procedure TForm1.FileOpen1Accept(Sender: TObject);
begin
    CheckDLL(FileOpen1.Dialog.FileName);
end;

procedure TForm1.FileOpen1BeforeExecute(Sender: TObject);
begin
    FileOpen1.Dialog.Filter :=  'DLL''s (*.dll)|*.DLL';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    FileNameE.Text := DLLName;
    CheckDLL(FileNameE.Text);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if(Key = VK_ESCAPE)
    then
        Close();
end;

procedure TForm1.LoadDLLExecute(Sender: TObject);
var
    tempString: WideString;
    test: Integer;
    aString: PChar;

begin
    tempString := WideString(FileNameE.Text);
    aString := PWideChar(tempString);
    dllHandle := LoadLibrary(aString);

    if(dllHandle <> 0)
    then
    begin
        Memo1.Lines.Add('C DLL was loaded.');
        mIsLoaded := true;
    end
    else
        Memo1.Lines.Add('Library was NOT loaded..');
end;

procedure TForm1.LoadFunctionsAExecute(Sender: TObject);
var
    GetCopy: TCharVoidFunc;
    GetNumber: TIntVoidFunc;
    FPointer: TFarProc;

begin
    FPointer := GetProcAddress(dllHandle, PChar ('getCopyright'));
    if FPointer <> nil then
    begin
        GetCopy := TCharVoidFunc(FPointer);
        Memo1.Lines.Add('Loaded C function char* getCopyright()');
        FunctionList.Items.AddObject('getCopyright', TObject(FPointer));
    end;

//    FPointer := GetProcAddress(dllHandle, PChar ('_getRRInstance'));
//    if FPointer <> nil then
//    begin
//        GetCopy := TCharVoidFunc(FPointer);
//        Memo1.Lines.Add('Loaded C function char* getCopyright()');
//        FunctionList.Items.AddObject('getCopyright', TObject(FPointer));
//    end;
    LoadFunctionsA.Enabled := false;
end;

procedure TForm1.UnloadDLLExecute(Sender: TObject);
begin
    FreeLibrary(dllHandle);
    mIsLoaded := false;
    FunctionList.Clear();
    LoadDLL.Enabled := true;

    Memo1.Lines.Add('Unloaded C DLL');

end;

procedure TForm1.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
    LoadFunctionsA.Enabled := mIsLoaded;
    LoadDLL.Enabled := not mIsLoaded;
    UnloadDLL.Enabled := mIsLoaded;
end;

procedure TForm1.APIFuncsUpdate(Action: TBasicAction; var Handled: Boolean);
var
  I: Integer;
begin
    if not mIsLoaded  then
    for I := 0 to APIFuncs.ActionCount -1 do
    TAction(APIFuncs.Actions[I]).Enabled := false;

end;

procedure TForm1.FunctionListClick(Sender: TObject);
var itemNr: Integer;
    funcPtr: TCharVoidFunc;
begin
    //Get currently selected item
    itemNr := FunctionList.ItemIndex;
    funcPtr := TCharVoidFunc(FunctionList.Items.Objects[itemNr]);
    if funcPtr <> nil then
        CharStarVoidA.Enabled := true;
end;

procedure TForm1.CharStarVoidAExecute(Sender: TObject);
var itemNr: Integer;
    funcPtr: TCharVoidFunc;
    aResult: AnsiString;
begin
    itemNr := FunctionList.ItemIndex;
    funcPtr := TCharVoidFunc(FunctionList.Items.Objects[itemNr]);
    if funcPtr <> nil then
    begin
        aResult := funcPtr();
        Memo1.Lines.Add(aResult);

    end;
end;

procedure TForm1.CheckDLL(fName: string);
begin
    if(FileExists(fName))
    then
    begin
        FileNameE.Text := fName;
        LoadDLL.Enabled := true;
    end;
end;

end.
