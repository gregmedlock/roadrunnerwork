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
    procedure Button1Click(Sender: TObject);
    procedure btnGetCopyrightClick(Sender: TObject);
    procedure btnLoadSBMLClick(Sender: TObject);
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
  lblProgress.caption := getCopyright;
end;

procedure TForm2.btnLoadSBMLClick(Sender: TObject);
var str : AnsiString;
    m : TMatrix;
    i, j : integer;
begin
  str := TFile.ReadAllText('feedback.xml');
  if not loadSBML(str) then
     lblProgress.Caption := 'Failed to load SBML model';
  m := simulate();
  grid.ColCount := m.c + 1;
  grid.RowCount := m.r + 1;
  for i := 1 to m.r do
      for j := 1 to m.c do
          grid.Cells [j-1, i] := Format ('%8.5g', [m[i,j]]);
end;


procedure TForm2.Button1Click(Sender: TObject);
var errMsg : AnsiString;
begin
  if loadRoadRunner (errMsg) then
     lblProgress.caption := 'RoadRunner Loaded'
  else
     lblProgress.caption := errMsg;
end;

end.
