unit udglsymboleditordered;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ucommongrid,
  ufunctions, ustrings, Grids;

type

  { TdlgSymbolOrderedEdit }

  TdlgSymbolOrderedEdit = class(TfrmCommonGrid)
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure gridValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TdlgSymbolOrderedEdit }

procedure TdlgSymbolOrderedEdit.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if (ModalResult=mrOK) then
  begin
    serialize();
  end;
end;

procedure TdlgSymbolOrderedEdit.FormCreate(Sender: TObject);
begin
  inherited;
  btnOk.Caption:= SOKSaveSymbols;
  array_push(FColsNumbers,0);
end;

procedure TdlgSymbolOrderedEdit.gridValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
begin
  if (NewValue='') then Exit;

  if ((aRow>1) and (aCol=0)) then
  begin
    if (StrToInt( grid.Cells[0,aRow-1] ) >= StrToInt(NewValue)) then
    begin
      MessageDlg(SErrMustBeGreaterThanPrevious, mtWarning, [mbOK],0);

      Abort;
    end;
  end;
end;


end.

