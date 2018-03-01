unit udglsymboledit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ucommongrid,
  superobject, ustrings;

type

  { TdlgSymbolEdit }

  TdlgSymbolEdit = class(TfrmCommonGrid)
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;


implementation

uses ufunctions;

{$R *.lfm}

{ TdlgSymbolEdit }

procedure TdlgSymbolEdit.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (ModalResult=mrOK) then
  begin
    serialize();
  end;
end;

procedure TdlgSymbolEdit.FormCreate(Sender: TObject);
begin
  inherited;
  btnOk.Caption:= SOKSaveSymbols;
  array_push(FColsStrings,0);
end;

end.

