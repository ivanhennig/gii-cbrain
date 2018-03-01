unit udlgsymboleditunordered;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  Grids, StdCtrls, ucommongrid, superobject, ustrings, LCLType;

type

  { TdlgSymbolEditUnordered }

  TdlgSymbolEditUnordered = class(TfrmCommonGrid)
    procedure FormCreate(Sender: TObject);
    procedure gridColRowDeleted(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure gridColRowInserted(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure gridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure gridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure gridValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);

    procedure gridHackCanEdit(Sender: TObject; Col, Row: Longint;
          var CanEdit: Boolean);
  private
    { private declarations }
  public
    { public declarations }
    procedure serialize; override;
    procedure unserialize; override;
  end;

implementation

{$R *.lfm}

{ TdlgSymbolEditUnordered }

procedure TdlgSymbolEditUnordered.FormCreate(Sender: TObject);
begin
  inherited;
  btnOk.Caption:= SOKSaveSymbols;
  //Bloquear algumas celulas
  grid.OnCanEdit := @gridHackCanEdit;
end;

procedure TdlgSymbolEditUnordered.gridColRowDeleted(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  if (not IsColumn) then
  begin
    grid.ColCount:=grid.ColCount-1;
  end;
end;

procedure TdlgSymbolEditUnordered.gridColRowInserted(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  if (not IsColumn) then
  begin
    grid.ColCount:=grid.ColCount+1;
  end;
end;

procedure TdlgSymbolEditUnordered.gridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  if ((aCol>0) and (aRow>0) and (aRow>=aCol)) then
  begin
    grid.Canvas.Brush.Color:=clBtnFace;
    grid.Canvas.FillRect(aRect);
    if (aRow=aCol) then
    begin
      grid.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, '1.0');
    end else
    begin
      grid.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, '-');
    end;
  end;
end;

procedure TdlgSymbolEditUnordered.gridSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  //Controla a navegação somente pelas celulas editaveis
  if ((grid.Col>0) and (grid.Row>0) and (grid.Row>=grid.Col)) then
  begin
    //if (grid.Row=grid.RowCount-1) then
    //begin
    //  grid.Row:=grid.Row-1;
    //end;

    if (grid.Col<grid.ColCount) then
    begin
      grid.Col:=grid.Col+1;
    end else
    begin
      grid.Col:=0;
    end;
  end;
end;

procedure TdlgSymbolEditUnordered.gridValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
begin
  grid.Cells[aRow,aCol]:=NewValue;
end;

procedure TdlgSymbolEditUnordered.gridHackCanEdit(Sender: TObject; Col,
  Row: Longint; var CanEdit: Boolean);
begin
  if ((grid.Col>0) and (grid.Row>0) and (grid.Row>=grid.Col)) then
  begin
    CanEdit:=false;
  end;
end;

procedure TdlgSymbolEditUnordered.serialize;
begin
  inherited serialize;

end;

procedure TdlgSymbolEditUnordered.unserialize;
begin
  FRow0:=0;//Iniciar na linha fixed
  inherited unserialize;
end;

end.

