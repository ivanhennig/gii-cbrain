unit ucommongrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  Grids, MaskEdit, ucommon, ufunctions, superobject;

type
  TCanEditEvent = procedure(Sender: TObject; Col, Row: Longint;
    var CanEdit: Boolean) of object;

  { TStringGrid }

  TStringGrid = class(Grids.TStringGrid)
  private
    FOnCanEdit: TCanEditEvent;
  protected
    function CanEditShow: Boolean; override;
  public
    property OnCanEdit: TCanEditEvent read FOnCanEdit write FOnCanEdit;
  end;


  { TfrmCommonGrid }

  TfrmCommonGrid = class(TfrmCommon)
    grid: TStringGrid;
    procedure gridKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }

  public
    { public declarations }
    FRow0 : Integer;
    FColsNumbers : TIntArray;
    FColsStrings : TIntArray;

    procedure serialize; override;
    procedure unserialize; override;
  end;

implementation

{$R *.lfm}

{ TStringGrid }

function TStringGrid.CanEditShow: Boolean;
begin
  Result:=inherited CanEditShow;
  if Result and Assigned(FOnCanEdit) then
     FOnCanEdit(Self, Col, Row, Result);
end;

{ TfrmCommonGrid }


procedure TfrmCommonGrid.gridKeyPress(Sender: TObject; var Key: char);
begin
  if (in_array(grid.Col,FColsNumbers)) and not (Key in [#8, '0' .. '9']) then
  begin
    Key:=#0;
  end;
  if (in_array(grid.Col,FColsStrings)) and not (Key in [#8, '0' .. '9', 'a'..'z', 'A'..'Z', '_', '-']) then
  begin
    Key:=#0;
  end;
end;

procedure TfrmCommonGrid.serialize;
var
  i,c:Integer;
  lSO : ISuperObject;
  lSA : ISuperObject;
begin
  inherited serialize;
  if (Assigned(FJson)) then
  begin
    lSO := FJson.Instance;
  end else
  begin
    lSO := FSO;
  end;
  lSO.Clear();
  for i := 0 to grid.RowCount-1 do
  begin
    lSA := SO('[]');
    for c := 0 to grid.ColCount-1 do
    begin
       lSA.AsArray.Add( grid.Cells[c,i] );
    end;
    lSO.AsArray.Add(lSA);
  end;
end;


procedure TfrmCommonGrid.unserialize;
var
  i, c:Integer;
  lArrRow : TSuperArray;
  lSO     : ISuperObject;
begin
  inherited unserialize;
  if (Assigned(FJson)) then
  begin
    lSO := FJson.Instance;
  end else
  begin
    lSO := FSO;
  end;


  if (lSO.AsArray.Length>0) then
  begin
    grid.RowCount := lSO.AsArray.Length;
  end;


  for i := FRow0 to lSO.AsArray.Length-1 do
  begin
    lArrRow := lSO.AsArray.O[i].AsArray;
//    grid.RowCount := grid.RowCount+1;

    if (grid.Columns.Count=0) then//Colunas dyn
    begin
      grid.ColCount := lArrRow.Length;
    end;

    for c := grid.FixedCols to lArrRow.Length-1 do
    begin
      grid.Cells[c,i] := lArrRow.S[c];
    end;
  end;
end;

end.

