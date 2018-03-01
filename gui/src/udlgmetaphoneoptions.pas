unit udlgmetaphoneoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, Spin, ucommon, superobject;

type

  { TdlgMetaphoneOptions }

  TdlgMetaphoneOptions = class(TfrmCommon)
    DividerBevel1: TDividerBevel;
    cbMetaphoneLanguage: TComboBox;
    lblMetaphoneLanguage: TLabel;
    mmoTipMetaphone: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure serialize; override;
    procedure unserialize; override;
  end;



implementation

{$R *.lfm}

{ TdlgMetaphoneOptions }

procedure TdlgMetaphoneOptions.FormCreate(Sender: TObject);
begin

end;

procedure TdlgMetaphoneOptions.serialize;
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
  lSO.S['language'] := cbMetaphoneLanguage.Text;
end;

procedure TdlgMetaphoneOptions.unserialize;
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
  cbMetaphoneLanguage.ItemIndex := cbMetaphoneLanguage.Items.IndexOf(lSO.S['language']);
end;

end.

