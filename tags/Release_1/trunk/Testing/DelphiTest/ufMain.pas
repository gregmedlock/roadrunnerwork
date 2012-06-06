unit ufMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uRoadRunnerAPI, ExtCtrls, IOUtils, Grids;

type
  TForm2 = class(TForm)
    Button1: TButton;
    pnlBottm: TPanel;
    lblProgress: TLabel;
    btnGetCopyright: TButton;
    btnLoadSBML: TButton;
    grid: TStringGrid;
    btnGetReactionNames: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btnGetCopyrightClick(Sender: TObject);
    procedure btnLoadSBMLClick(Sender: TObject);
    procedure btnGetReactionNamesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

Uses uMatrix;

procedure TForm2.btnGetCopyrightClick(Sender: TObject);
begin
  lblProgress.caption := string (getCopyright);
end;

procedure TForm2.btnGetReactionNamesClick(Sender: TObject);
var i : integer; strList : TStringList;
begin
  strList := getReactionNames;
  for i := 0 to strList.Count - 1 do
      grid.Cells[0, i+2] := strList[i];
  strList.Free;
end;

procedure TForm2.btnLoadSBMLClick(Sender: TObject);
var str : AnsiString;
    m : TMatrix;
    i, j : integer;
    list : TStringList;
begin
  list := TStringList.Create;
  str := AnsiString (TFile.ReadAllText('feedback.xml'));
  if not loadSBML(str) then
     lblProgress.Caption := 'Failed to load SBML model';
  list.Add ('time');
  list.Add ('S1');
  list.Add ('S2');
  setSelectionList (list);
  m := simulate();
  grid.ColCount := m.c + 1;
  grid.RowCount := m.r + 1;
  for i := 1 to m.r do
      for j := 1 to m.c do
          grid.Cells [j-1, i] := Format ('%8.5g', [m[i,j]]);
  list.free;
end;


procedure TForm2.Button1Click(Sender: TObject);
var errMsg : AnsiString;
begin
  if loadRoadRunner (errMsg) then
     lblProgress.caption := 'RoadRunner Loaded'
  else
     lblProgress.caption := string (errMsg);
end;

end.
