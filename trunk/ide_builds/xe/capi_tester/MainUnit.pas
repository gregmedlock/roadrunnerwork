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
    ListBox1: TListBox;
    FileOpen1: TFileOpen;
    procedure FileOpen1Accept(Sender: TObject);
    procedure FileOpen1BeforeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LoadDLLExecute(Sender: TObject);
  private
    { Private declarations }
    procedure CheckDLL(fName: string);
  public
    { Public declarations }
  end;
//    function GetCopyright(): PChar; stdcall;
var
  Form1: TForm1;

implementation

{$R *.dfm}
const DLLName = 'rrC_API.dll';

type
TCharFunc = function: PChar; stdcall;
TIntFunc = function: Integer; stdcall;

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

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if(Key = VK_ESCAPE)
    then
        Close();
end;

procedure TForm1.LoadDLLExecute(Sender: TObject);
    var
    handle: THandle;
    tempString: WideString;
    test: Integer;
    aString: PChar;
    GetCopy: TCharFunc;
    GetNumber: TIntFunc;
    FPointer: TFarProc;
    aString2: ShortString;
    number: Integer;

begin
    aString2 := StrAlloc(512);
    tempString := WideString(FileNameE.Text);
    aString := PWideChar(tempString);
    handle := LoadLibrary(aString);

    if(handle <> 0)
    then
    begin
        Memo1.Lines.Add('Library was loaded.');
        Memo1.Lines.Add('Current DLL Build info is...');
        FPointer := GetProcAddress(handle, PChar ('_getCopyright'));
        if FPointer <> nil then
        begin
            GetCopy := TCharFunc(fPointer);
            aString := 'empty';
            aString := PChar( GetCopy());
            StrPas(aString);
            Memo1.Lines.Add();
        end;
        FPointer := GetProcAddress(handle, PChar ('_GetNumber'));
        if FPointer <> nil then
        begin
            GetNumber := TIntFunc(FPointer);
            number := GetNumber();
            Memo1.Lines.Add(IntToStr(number));
        end;

    end
    else
        Memo1.Lines.Add('Library was NOT loaded..');
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
