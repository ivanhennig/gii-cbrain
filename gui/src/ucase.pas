unit ucase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, EditBtn, ExtCtrls, Spin, ucommon, ustrings;

type

  { TfrmCase }

  TfrmCase = class(TfrmCommon)
    cbSimilarityFunctions: TComboBox;
    cbType: TComboBox;
    chkCSVFirstLineTitle: TCheckBox;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    edtCaseFilename: TFileNameEdit;
    edtCaseName: TEdit;
    edtCaseDescription: TEdit;
    edtCaseRelationName: TEdit;
    edtCaseCSVSep: TEdit;
    edtCaseMinScore: TFloatSpinEdit;
    lblCaseCSVSep: TLabel;
    lblCaseFileName: TLabel;
    lblCaseQuery: TLabel;
    lblCaseRelationName: TLabel;
    lblDescription: TLabel;
    lblName: TLabel;
    lblCaseGlobalSim: TLabel;
    lblType: TLabel;
    lblRelevanceScore: TLabel;
    mmoCaseSQLQuery: TMemo;
    pnlCaseDB: TPanel;
    pnlCaseCSV: TPanel;
    procedure cbTypeChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);

  private
    { private declarations }
    procedure doRefresh();
    procedure populatecbType();
    procedure populatecbSimilarity();
  public
    procedure serialize(); override;
    procedure unserialize(); override;
  end;


implementation

{$R *.lfm}

{ TfrmCase }


procedure TfrmCase.cbTypeChange(Sender: TObject);
begin
  doRefresh();
end;

procedure TfrmCase.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (ModalResult=mrOK) then
  begin
    if (Trim(edtCaseName.Text)='') then
    begin
      MessageDlg(SValidation,Format(SValidationMessageEmpty,[lblName.Caption]), mtError, [mbOK], 0);
      edtCaseName.SetFocus;
      CanClose := false;
      Abort;
    end;
    if (cbType.ItemIndex<=0) then//Não foi selecionado nenhum tipo de acesso a dados
    begin
      MessageDlg(SValidation,Format(SValidationMessageEmpty,[lblType.Caption]), mtError, [mbOK], 0);
      cbType.SetFocus;
      CanClose := false;
      Abort;
    end else if (cbType.ItemIndex=1) then//CSV
    begin
      if (edtCaseCSVSep.Text='') then
      begin
        MessageDlg(SValidation,Format(SValidationMessageEmpty,[lblCaseCSVSep.Caption]), mtError, [mbOK], 0);
        edtCaseCSVSep.SetFocus;
        CanClose := false;
        Abort;
      end;
    end else if (cbType.ItemIndex=2) then//Table
    begin
      if (edtCaseRelationName.Text='') then
      begin
        MessageDlg(SValidation,Format(SValidationMessageEmpty,[lblCaseRelationName.Caption]), mtError, [mbOK], 0);
        edtCaseRelationName.SetFocus;
        CanClose := false;
        Abort;
      end;
    end else if (cbType.ItemIndex=2) then//Query
    begin
      if (mmoCaseSQLQuery.Lines.Text='') then
      begin
        MessageDlg(SValidation,Format(SValidationMessageEmpty,[lblCaseQuery.Caption]), mtError, [mbOK], 0);
        mmoCaseSQLQuery.SetFocus;
        CanClose := false;
        Abort;
      end;
    end;

    if (edtCaseMinScore.Value = 0.0) then
    begin
      MessageDlg(SValidation,Format(SValidationMessageEmpty,[lblRelevanceScore.Caption]), mtError, [mbOK], 0);
      edtCaseMinScore.SetFocus;
      CanClose := false;
      Abort;
    end;
  end;
  inherited;
end;

procedure TfrmCase.FormCreate(Sender: TObject);
begin
  inherited;
  btnOk.Caption:=SOKSaveCase;
end;

procedure TfrmCase.doRefresh;
begin
  lblCaseFileName.Enabled:=False;
  lblCaseFileName.FocusControl.Enabled:=False;
  lblCaseRelationName.Enabled:=False;
  lblCaseRelationName.FocusControl.Enabled:=False;
  lblCaseQuery.Enabled:=False;
  lblCaseQuery.FocusControl.Enabled:=False;
  pnlCaseCSV.Visible:=False;
  pnlCaseCSV.Color:=clDefault;
  pnlCaseCSV.Top:=155;
  pnlCaseCSV.Left:=0;
  pnlCaseDB.Visible:=False;
  pnlCaseDB.Color:=clDefault;
  pnlCaseDB.Top:=155;
  pnlCaseDB.Left:=0;
  case cbType.ItemIndex of
    1 :
      begin
        pnlCaseCSV.Visible:=True;
        lblCaseFileName.Enabled:=True;
        lblCaseFileName.FocusControl.Enabled:=True;
      end;
    2 :
      begin
        pnlCaseDB.Visible:=True;
        lblCaseRelationName.Enabled:=True;
        lblCaseRelationName.FocusControl.Enabled:=True;
      end;
    3 :
      begin
        pnlCaseDB.Visible:=True;
        lblCaseQuery.Enabled:=True;
        lblCaseQuery.FocusControl.Enabled:=True;
      end;
  end;
end;

procedure TfrmCase.populatecbType;
begin
  cbType.Items.Clear;
  cbType.Items.Add(SSelectOne);
  cbType.Items.Add(SCaseTypeFile);
  cbType.Items.Add(SCaseTypeRelation);
  cbType.Items.Add(SCaseTypeQuery);

  cbType.ItemIndex:=0;
end;

procedure TfrmCase.populatecbSimilarity;
begin
  cbSimilarityFunctions.Items.Clear;
  cbSimilarityFunctions.Items.Add(SGlobalSimNearest);
  cbSimilarityFunctions.Items.Add(SGlobalSimEuclidean);
  cbSimilarityFunctions.ItemIndex:=0;
end;

procedure TfrmCase.serialize;
begin
  inherited serialize;
  FJson.S['casename']:=edtCaseName.Text;
  FJson.S['casedescription']:=edtCaseDescription.Text;
  FJson.S['caserelation']:=edtCaseRelationName.Text;
  FJson.S['casequery']:=mmoCaseSQLQuery.Lines.Text;
  FJson.S['casefilename']:=edtCaseFilename.Text;
  FJson.D['casemin_score']:=edtCaseMinScore.Value;
  case cbType.ItemIndex of
    1 : FJson.S['casetype']:='file';
    2 : FJson.S['casetype']:='relation';
    3 : FJson.S['casetype']:='query';
  end;

  if (cbSimilarityFunctions.Text = SGlobalSimEuclidean) then
  begin
    FJson.S['caseglobalsim']:='euclidean';
  end else if (cbSimilarityFunctions.Text = SGlobalSimNearest) then
  begin
    FJson.S['caseglobalsim']:='nearest';
  end;



  FJson.B['casecsvfirstlinetitle']:=chkCSVFirstLineTitle.Checked;
  FJson.S['casecsvsep']           :=edtCaseCSVSep.Text;
  //
  FJson.Instance.Delete('project');
end;

procedure TfrmCase.unserialize;
begin
  inherited unserialize;
  edtCaseName.Text           := FJson.S['casename'];
  edtCaseDescription.Text    := FJson.S['casedescription'];
  edtCaseRelationName.Text   := FJson.S['caserelation'];
  mmoCaseSQLQuery.Lines.Text := FJson.S['casequery'];
  edtCaseFilename.Text       := FJson.S['casefilename'];
  edtCaseMinScore.Value      := FJson.D['casemin_score'];
  populatecbType();
  populatecbSimilarity();
  case FJson.S['casetype'] of
    'file' : cbType.ItemIndex:=1;
    'relation' : cbType.ItemIndex:=2;
    'query' : cbType.ItemIndex:=3;
  end;

  case FJson.S['caseglobalsim'] of
    'nearest'   : cbSimilarityFunctions.ItemIndex:=0;
    'euclidean' : cbSimilarityFunctions.ItemIndex:=1;
  end;

  chkCSVFirstLineTitle.Checked := FJson.B['casecsvfirstlinetitle'];
  edtCaseCSVSep.Text           := FJson.S['casecsvsep'];

  doRefresh();
end;

end.

