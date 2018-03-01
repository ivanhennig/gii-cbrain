unit udlgnumberedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, Spin, ucommon, ustrings, superobject;

type

  { TdlgNumberEdit }

  TdlgNumberEdit = class(TfrmCommon)
    DividerBevel1: TDividerBevel;
    edtNumberHigh: TFloatSpinEdit;
    edtNumberLow: TFloatSpinEdit;
    lblNumberHigh: TLabel;
    lblNumberLow: TLabel;
    mmoTipNumberLinearFunction: TMemo;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure lblTipNumberLinearFunctionClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure serialize; override;
    procedure unserialize; override;
  end;


implementation

{$R *.lfm}

{ TdlgNumberEdit }

procedure TdlgNumberEdit.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  inherited;
  //Validação dos campos
  if (ModalResult=mrOK) then
  begin
    if (edtNumberLow.Value <= 0.00 ) then
    begin
      MessageDlg(SValidation,Format(SValidationNumberLess,['zero', lblNumberLow.Caption]), mtError, [mbOK], 0);
      edtNumberLow.SetFocus;
      CanClose := false;
      Exit;
    end;
    if (edtNumberHigh.Value <= edtNumberLow.Value ) then
    begin
      MessageDlg(SValidation,Format(SValidationNumberGreat,[edtNumberLow.Value, lblNumberLow.Caption]), mtError, [mbOK], 0);
      edtNumberLow.SetFocus;
      CanClose := false;
      Exit;
    end;
  end;
end;

procedure TdlgNumberEdit.FormCreate(Sender: TObject);
begin
  inherited;
  Self.Caption  := SParamNumberLinearInfo;
  btnOk.Caption := SOKSaveDef;
end;

procedure TdlgNumberEdit.lblTipNumberLinearFunctionClick(Sender: TObject);
begin

end;

procedure TdlgNumberEdit.serialize;
var
   lSO : ISuperObject;
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
  lSO.D['lowbound'] := edtNumberLow.Value;
  lSO.D['highbound'] := edtNumberHigh.Value;
end;

procedure TdlgNumberEdit.unserialize;
var
  lSO : ISuperObject;
begin
  inherited unserialize;
  if (Assigned(FJson)) then
  begin
    lSO := FJson.Instance;
  end else
  begin
    lSO := FSO;
  end;
  edtNumberLow.Value  := lSO.D['lowbound'];
  edtNumberHigh.Value := lSO.D['highbound'];
end;

end.

