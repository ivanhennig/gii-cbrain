unit usettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, FileUtil, DividerBevel, Forms, Controls, Graphics,
  Dialogs, Buttons, EditBtn, StdCtrls, Spin, ucommon, ustrings;

type

  { TfrmSettings }

  TfrmSettings = class(TfrmCommon)
    cbSettingUseLocalService: TComboBox;
    divSettingsMain: TDividerBevel;
    edtSettingsRpcUrl: TEdit;
    edtSettingsRpcPort: TSpinEdit;
    edtSettingsPathPHP: TFileNameEdit;
    edtSettingsPathMongo: TFileNameEdit;
    lblSettingsUseLocalServices: TLabel;
    lblProjectPort: TLabel;
    lblSettingsUrlService: TLabel;
    lblSettingsPHP: TLabel;
    lblSettingsMongo: TLabel;
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    procedure serialize; override;
    procedure unserialize; override;
  end;

implementation

{$R *.lfm}

{ TfrmSettings }

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  inherited;
  btnOk.Caption:=SOKSaveSettings;
end;

procedure TfrmSettings.btnOkClick(Sender: TObject);
begin
  inherited;
end;

procedure TfrmSettings.serialize;
begin
  inherited serialize;
  FSO.B['uselocalservice'] := cbSettingUseLocalService.ItemIndex = 1;
  FSO.S['pathphp'] := edtSettingsPathPHP.Text;
  FSO.S['pathmongod'] := edtSettingsPathMongo.Text;
  FSO.S['rpcurl'] := edtSettingsRpcUrl.Text;
  FSO.I['rpcport'] := edtSettingsRpcPort.Value;
end;

procedure TfrmSettings.unserialize;
begin
  inherited unserialize;

  cbSettingUseLocalService.Items.Clear;
  cbSettingUseLocalService.Items.Add(SNo);
  cbSettingUseLocalService.Items.Add(SYes);
  cbSettingUseLocalService.ItemIndex := ifthen( FSO.B['uselocalservice'], 1, 0);

  edtSettingsPathPHP.Text            := FSO.S['pathphp'];
  edtSettingsPathMongo.Text          := FSO.S['pathmongod'];
  edtSettingsRpcUrl.Text             := FSO.S['rpcurl'];
  edtSettingsRpcPort.Value           := FSO.I['rpcport'];
end;

end.

