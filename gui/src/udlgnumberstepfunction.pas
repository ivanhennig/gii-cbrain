unit udlgnumberstepfunction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  Buttons, Spin, StdCtrls, ucommon, ustrings;

type

  { TdlgNumberStepFunction }

  TdlgNumberStepFunction = class(TfrmCommon)
    DividerBevel1: TDividerBevel;
    edtNumberStepParam: TFloatSpinEdit;
    lblThreshold: TLabel;
    mmoTipNumberStepFunction: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TdlgNumberStepFunction }

procedure TdlgNumberStepFunction.FormCreate(Sender: TObject);
begin
  inherited;
  Self.Caption  := SParamNumberStepInfo;
  btnOk.Caption := SOKSaveDef;
end;

end.

