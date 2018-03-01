unit uproject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, Spin, ucommon, ujsonrpc, superobject, ustrings;

type

  { TfrmProject }

  TfrmProject = class(TfrmCommon)
    cbProjectDriver: TComboBox;
    edtProjectHost: TEdit;
    lblMain: TDividerBevel;
    lblExternalConnSettings: TDividerBevel;
    edtProjectConnectionName: TEdit;
    edtProjectPort: TSpinEdit;
    edtProjectName: TEdit;
    edtProjectDescription: TEdit;
    edtUsername: TEdit;
    edtPassword: TEdit;
    lblDescription: TLabel;
    lblDriver: TLabel;
    lblName: TLabel;
    lblProjectConnectionName: TLabel;
    lblProjectHost: TLabel;
    lblProjectPort: TLabel;
    lblProjectUsername: TLabel;
    lblProjectPassword: TLabel;
    procedure btnOkClick(Sender: TObject);
    procedure cbProjectDriverChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure lblProjectPortClick(Sender: TObject);
  private
    procedure populatecbDriver(e, r : String);
    procedure doRefresh();
  public
    procedure serialize; override;
    procedure unserialize; override;
  end;

implementation

{$R *.lfm}

{ TfrmProject }

procedure TfrmProject.cbProjectDriverChange(Sender: TObject);
begin
  doRefresh();
end;

procedure TfrmProject.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  //Validação dos campos
  if (ModalResult=mrOK) then
  begin
    if (Trim(edtProjectName.Text)='') then
    begin
      MessageDlg(SValidation,Format(SValidationMessageEmpty,[lblName.Caption]), mtError, [mbOK], 0);
      edtProjectName.SetFocus;
      CanClose := false;
      Abort;
    end;
  end;
  inherited;
end;

procedure TfrmProject.btnOkClick(Sender: TObject);
begin
  inherited;
end;

procedure TfrmProject.FormCreate(Sender: TObject);
begin
  inherited;
  btnOk.Caption:=SOKSaveProject;
end;

procedure TfrmProject.lblProjectPortClick(Sender: TObject);
begin

end;

procedure TfrmProject.populatecbDriver(e, r: String);
var
  lIndex: Integer;
  i: Integer;
  lDrivers: ISuperObject;
begin
  if (e<>'') then
  begin
    MessageDlg(SErrServiceNotRunning + #13 + e, mtWarning, [mbOK],0);
    Exit;
  end;

  lDrivers := SO(r);
  if (Assigned(lDrivers) and (lDrivers.IsType(stArray)) ) then
  begin
    for i := 0 to lDrivers.AsArray.Length-1 do
    begin
      cbProjectDriver.Items.Add(lDrivers.AsArray.S[i] );
    end;
    lIndex := cbProjectDriver.Items.IndexOf(FJson.S['projectdriver']);
    if (lIndex>0) then
    begin
      cbProjectDriver.ItemIndex:=lIndex;
    end;
  end;

  doRefresh();
end;

procedure TfrmProject.doRefresh;
var
  lEnabled : Boolean;
begin
  lEnabled := cbProjectDriver.ItemIndex > 0;
  lblProjectUsername.Enabled:=lEnabled;
  edtUsername.Enabled:=lEnabled;
  lblProjectPassword.Enabled:=lEnabled;
  edtPassword.Enabled:=lEnabled;
  lblProjectConnectionName.Enabled:=lEnabled;
  edtProjectConnectionName.Enabled:=lEnabled;
  lblProjectPort.Enabled:=lEnabled;
  edtProjectPort.Enabled:=lEnabled;
  lblProjectHost.Enabled:=lEnabled;
  edtProjectHost.Enabled:=lEnabled;
end;

procedure TfrmProject.serialize;
begin
  inherited serialize;
  FJson.S['projectname']        := edtProjectName.Text;
  FJson.S['projectdescription'] := edtProjectDescription.Text;

  if (cbProjectDriver.ItemIndex > 0 ) then
  begin
    FJson.S['projectdriver']   := cbProjectDriver.Text;
    FJson.S['projectusername'] := edtUsername.Text;
    FJson.S['projectpassword'] := edtPassword.Text;
  end;

  FJson.S['projectconnectionname'] := edtProjectConnectionName.Text;
  FJson.I['projectport']           := edtProjectPort.Value;
  FJson.S['projecthost']           := edtProjectHost.Text;
end;

procedure TfrmProject.unserialize;
begin
  inherited unserialize;
  edtProjectName.Text           := FJson.S['projectname'];
  edtProjectDescription.Text    := FJson.S['projectdescription'];
  edtUsername.Text              := FJson.S['projectusername'];
  edtPassword.Text              := FJson.S['projectpassword'];
  edtProjectConnectionName.Text := FJson.S['projectconnectionname'];
  edtProjectPort.Value          := FJson.I['projectport'];
  edtProjectHost.Text           := FJson.S['projecthost'];

  cbProjectDriver.Items.Add(SDisabled);
  cbProjectDriver.ItemIndex:=0;
  TRpc.Create('getDrivers', [], @populatecbDriver);//RPC Async

  doRefresh();
end;

end.

