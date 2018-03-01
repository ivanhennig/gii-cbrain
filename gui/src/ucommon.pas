unit ucommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  superobject, ufunctions;

type

  { TfrmCommon }

  TfrmCommon = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    FJson : TJson;
    FSO : ISuperObject;
    procedure serialize(); virtual;
    procedure unserialize(); virtual;
    constructor Create(TheOwner: TComponent); override;
    constructor Create(aOwner: TComponent; aJson : TJson); overload;
    constructor Create(aOwner: TComponent; aSO : ISuperObject); overload;

  end;

implementation

{$R *.lfm}

{ TfrmCommon }

procedure TfrmCommon.btnOkClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TfrmCommon.btnCancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TfrmCommon.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (ModalResult=mrOK) then
  begin
    serialize();
  end;
end;

procedure TfrmCommon.FormCreate(Sender: TObject);
var
  lTop,lLeft: Integer;
begin
  {$IFDEF WINDOWS}
  lLeft:= btnOk.Left;
  lTop := btnOk.Top;
  btnOk.Left := btnCancel.Left;
  btnOk.Top  := btnCancel.Top;
  btnCancel.Left := lLeft;
  btnCancel.Top  := lTop;
  {$ENDIF}
end;

procedure TfrmCommon.serialize;
begin

end;

procedure TfrmCommon.unserialize;
begin

end;

constructor TfrmCommon.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  raise Exception.Create('Must use overloaded version');
end;

constructor TfrmCommon.Create(aOwner: TComponent; aJson: TJson);
begin
  inherited Create(aOwner);
  FJson := aJson;
  unserialize();
end;

constructor TfrmCommon.Create(aOwner: TComponent; aSO: ISuperObject);
begin
  inherited Create(aOwner);
  FSO := aSO;
  unserialize();
end;

end.

