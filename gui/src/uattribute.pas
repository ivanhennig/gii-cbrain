unit uattribute;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SdfData, FileUtil, DividerBevel, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Buttons, Spin, ExtCtrls, ucommon, ustrings, ucommongrid,
  udglsymboledit, superobject, ufunctions, udglsymboleditordered,
  udlgnumberedit, udlgmetaphoneoptions, udlgsymboleditunordered,
  udlgnumberstepfunction;

type

  { TfrmAttribute }

  TfrmAttribute = class(TfrmCommon)
    btnEditSymbols: TSpeedButton;
    cbIndex: TComboBox;
    cbSimilarityFunctions: TComboBox;
    cbType: TComboBox;
    cbDataType: TComboBox;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    edtAttrDescription: TEdit;
    edtAttrName: TComboBox;
    edtWeight: TFloatSpinEdit;
    lblWeight: TLabel;
    lblIndex: TLabel;
    lblDescription: TLabel;
    lblSimilarityLocal: TLabel;
    lblType: TLabel;
    lblDataType: TLabel;
    lblName: TLabel;
    btnSimilarityParams: TSpeedButton;
    procedure btnEditSymbolsClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnSimilarityParamsClick(Sender: TObject);
    procedure cbDataTypeChange(Sender: TObject);
    procedure cbIndexChange(Sender: TObject);
    procedure cbSimilarityFunctionsChange(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);

    procedure FormShow(Sender: TObject);
  private
    FCaseJson : TJson;

    procedure showDlgMetaphone;
    procedure showDlgNumberLinear();
    procedure showDlgNumberStep();
    procedure showDlgSymbolOrdered();
    procedure showDlgSymbolUnordered();
    procedure showDlgSymbol();
    procedure doRefresh();
    procedure doPopulateSimilarityFunctions();
    procedure doRefreshSimilarityParams;
    procedure validateSimilarityParams;
  public

    procedure serialize(); override;
    procedure unserialize(); override;
    constructor Create(aOwner: TComponent; aJson, aCaseJson: TJson); overload;
  end;


implementation

{$R *.lfm}

{ TfrmAttribute }

procedure TfrmAttribute.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  //Validação dos campos
  if (ModalResult=mrOK) then
  begin
    if (Trim(edtAttrName.Text)='') then
    begin
      MessageDlg(SValidation,Format(SValidationMessageEmpty,[lblName.Caption]), mtError, [mbOK], 0);
      edtAttrName.SetFocus;
      CanClose := false;
      Abort;
    end;

    if (cbType.ItemIndex <= 0) then
    begin
      MessageDlg(SValidation,Format(SValidationMessageEmpty,[lblType.Caption]), mtError, [mbOK], 0);
      cbType.SetFocus;
      CanClose := false;
      Abort;
    end;
    if (cbDataType.ItemIndex <= 0) then
    begin
      MessageDlg(SValidation,Format(SValidationMessageEmpty,[lblDataType.Caption]), mtError, [mbOK], 0);
      cbDataType.SetFocus;
      CanClose := false;
      Abort;
    end;

    if (cbSimilarityFunctions.Enabled and (cbSimilarityFunctions.ItemIndex<=0) and (cbSimilarityFunctions.Text <> SDefault)) then
    begin
      MessageDlg(SValidation,Format(SValidationMessageEmpty,[lblSimilarityLocal.Caption]), mtError, [mbOK], 0);
      cbSimilarityFunctions.SetFocus;
      CanClose := false;
      Abort;
    end;


    if (edtWeight.Enabled) then
    begin
      if ((edtWeight.Value < 0.1) or (edtWeight.Value > 1.0)) then
      begin
        MessageDlg(SValidation,SValidationWeight, mtError, [mbOK], 0);
        edtWeight.SetFocus;
        CanClose := false;
        Abort;
      end;
    end;


    validateSimilarityParams;
    serialize();
  end;
end;

procedure TfrmAttribute.FormCreate(Sender: TObject);
begin
  inherited;
  btnOk.Caption:=SOKSaveAttr;
end;


procedure TfrmAttribute.serialize;
var
  lManterParams : String;
begin
  FJson.S['attrname']:= edtAttrName.Text;
  FJson.S['attrdescription']:= edtAttrDescription.Text;

  case cbType.ItemIndex of
    1 : FJson.S['type']:='input';
    2 : FJson.S['type']:='output';
    3 : FJson.S['type']:='input_output';
  end;
  case cbDataType.ItemIndex of
    1 : FJson.S['datatype']:='string';
    2 : FJson.S['datatype']:='number';
    3 : FJson.S['datatype']:='symbol';
    4 : FJson.S['datatype']:='symbolordered';
    5 : FJson.S['datatype']:='symbolunordered';
    6 : FJson.S['datatype']:='phrase';
  end;
  FJson.B['index']:=(cbIndex.Text=SYes);

  if (cbSimilarityFunctions.Text = SFunctionStringSensitive) then
  begin
    FJson.S['function']:='case';
  end else if (cbSimilarityFunctions.Text = SFunctionStringInsensitive) then
  begin
    FJson.S['function']:='icase';
  end else if (cbSimilarityFunctions.Text = SFunctionEquality) then
  begin
    FJson.S['function']:='equal';
  end else if (cbSimilarityFunctions.Text = SFunctionNumberLinear) then
  begin
    FJson.S['function']:='linear';
  end else if (cbSimilarityFunctions.Text = SFunctionStringMetaphone) then
  begin
    FJson.S['function']:='metaphone';
  end else if (cbSimilarityFunctions.Text = SFunctionStringIntersectMetaphone) then
  begin
    FJson.S['function']:='intersect_metaphone';
  end else if (cbSimilarityFunctions.Text = SFunctionStringIntersect) then
  begin
    FJson.S['function']:='intersect';
  end else
  begin
    FJson.S['function']:='';
  end;

  FJson.D['weight']:=(edtWeight.Value);

  //Limpeza parametros

  if (cbDataType.Text = SDataTypeSymbol) then
  begin
    lManterParams := 'symbol_params';
  end else if (cbDataType.Text = SDataTypeSymbolOrdered) then
  begin
    lManterParams := 'symbolordered_params';
  end else if (cbDataType.Text = SDataTypeNumber) then
  begin
    lManterParams := 'number_params';
  end else if (cbDataType.Text = SDataTypeSymbolUnordered) then
  begin
    lManterParams := 'symbolunordered_params';
  end else if ((cbSimilarityFunctions.Text = SFunctionStringMetaphone) or (cbSimilarityFunctions.Text = SFunctionStringIntersectMetaphone)) then
  begin
    lManterParams := 'metaphone_params';
  end;
  if (lManterParams<>'symbol_params') then FJson.Instance.Delete('symbol_params');
  if (lManterParams<>'symbolordered_params') then FJson.Instance.Delete('symbolordered_params');
  if (lManterParams<>'number_params') then FJson.Instance.Delete('number_params');
  if (lManterParams<>'symbolunordered_params') then FJson.Instance.Delete('symbolunordered_params');
  if (lManterParams<>'metaphone_params') then FJson.Instance.Delete('metaphone_params');
end;
procedure TfrmAttribute.unserialize;
var
  lIndex : Integer;
  lCSV : TSdfDataSet;
  i : Word;
begin
  if (Assigned(FCaseJson) and (FCaseJson.S['casetype']='file')) then
  begin
    lCSV := TSdfDataSet.Create(Self);
    lCSV.FirstLineAsSchema := FCaseJson.B['casecsvfirstlinetitle'];
    lCSV.Delimiter         := FCaseJson.S['casecsvsep'][1];

    lCSV.FileName          := FCaseJson.S['casefilename'];
    try
      lCSV.Open;
      if (lCSV.FieldCount>0) then
      begin
        if (FCaseJson.B['casecsvfirstlinetitle']) then
        begin
          for i := 0 to lCSV.FieldCount-1 do
          begin
            edtAttrName.Items.Add(StringReplace(lCSV.Fields[i].FieldName,'"','',[rfReplaceAll]) );
          end;
        end else
        begin
          for i := 0 to lCSV.FieldCount-1 do
          begin
            edtAttrName.Items.Add(IntToStr(i));
          end;
        end;
      end;
    finally
      lCSV.Close;
      lCSV.Free;
    end;
  end;


  edtAttrName.Text       :=FJson.S['attrname'];
  edtAttrDescription.Text:=FJson.S['attrdescription'];


  //Populando os combos
  cbType.Items.Clear;
  cbType.Items.Add(SSelectOne);
  cbType.Items.Add(STypeInput);
  cbType.Items.Add(STypeOutput);
  cbType.Items.Add(STypeInputOutput);
  case FJson.S['type'] of
    'input'  : cbType.ItemIndex:=1;
    'output' : cbType.ItemIndex:=2;
    'input_output' : cbType.ItemIndex:=3;
  else
    cbType.ItemIndex:=0;
  end;

  cbDataType.Items.Clear;
  cbDataType.Items.Add(SSelectOne);
  cbDataType.Items.Add(SDataTypeString);
  cbDataType.Items.Add(SDataTypeNumber);
  cbDataType.Items.Add(SDataTypeSymbol);
  cbDataType.Items.Add(SDataTypeSymbolOrdered);
  cbDataType.Items.Add(SDataTypeSymbolUnordered);
  cbDataType.Items.Add(SDataTypePhrase);
  case FJson.S['datatype'] of
    'string'          : cbDataType.ItemIndex:=1;
    'number'          : cbDataType.ItemIndex:=2;
    'symbol'          : cbDataType.ItemIndex:=3;
    'symbolordered'   : cbDataType.ItemIndex:=4;
    'symbolunordered' : cbDataType.ItemIndex:=5;
    'phrase'          : cbDataType.ItemIndex:=6;
  else
    cbDataType.ItemIndex:=0;
  end;


  cbIndex.Items.Clear;
//  cbIndex.Items.Add(SSelectOne);
  cbIndex.Items.Add(SNo);
  cbIndex.Items.Add(SYes);

  cbIndex.ItemIndex:=0;
  if (FJson.Exists('index')) then
  begin
    if (FJson.B['index']) then
    begin
      cbIndex.ItemIndex:=1;
    end else
    begin
      cbIndex.ItemIndex:=2;
    end;
  end;
  edtWeight.Value := FJson.D['weight'];

  doPopulateSimilarityFunctions();
  //Populando combo com funçoes
  case FJson.S['function'] of
    'case'                : lIndex := cbSimilarityFunctions.Items.IndexOf( SFunctionStringSensitive );
    'icase'               : lIndex := cbSimilarityFunctions.Items.IndexOf( SFunctionStringInsensitive );
    'equal'               : lIndex := cbSimilarityFunctions.Items.IndexOf( SFunctionEquality );
    'linear'              : lIndex := cbSimilarityFunctions.Items.IndexOf( SFunctionNumberLinear );
    'metaphone'           : lIndex := cbSimilarityFunctions.Items.IndexOf( SFunctionStringMetaphone );
    'intersect_metaphone' : lIndex := cbSimilarityFunctions.Items.IndexOf( SFunctionStringIntersectMetaphone );
    'intersect'           : lIndex := cbSimilarityFunctions.Items.IndexOf( SFunctionStringIntersect );
  else
    lIndex := 0;
  end;
  cbSimilarityFunctions.ItemIndex:=lIndex;

  doRefresh();
end;

constructor TfrmAttribute.Create(aOwner: TComponent; aJson, aCaseJson: TJson);
begin
  inherited Create(aOwner, aJson);
  FCaseJson := aCaseJson;
end;


procedure TfrmAttribute.FormShow(Sender: TObject);
begin
  unserialize();
end;

procedure TfrmAttribute.showDlgSymbolOrdered;
var
  lSO : ISuperObject;
  lDlg : TdlgSymbolOrderedEdit;
begin
  if (FJson.Exists('symbolordered_params')) then
  begin
    lSO := FJson.O['symbolordered_params'];
  end else
  begin
    lSO := SO('[]');
  end;

  lDlg := TdlgSymbolOrderedEdit.Create(Self, lSO );
  try
    if (lDlg.ShowModal = mrOK) then
    begin
      FJson.O['symbolordered_params'] := lSO;
    end;
  finally
    lDlg.Free;
  end;

end;

procedure TfrmAttribute.showDlgSymbolUnordered;
var
  lSO : ISuperObject;
  lDlg : TdlgSymbolEditUnordered;
begin
  if (FJson.Exists('symbolunordered_params')) then
  begin
    lSO := FJson.O['symbolunordered_params'];
  end else
  begin
    lSO := SO('[]');
  end;

  lDlg := TdlgSymbolEditUnordered.Create(Self, lSO );
  try
    if (lDlg.ShowModal = mrOK) then
    begin
      FJson.O['symbolunordered_params'] := lSO;
    end;
  finally
    lDlg.Free;
  end;
end;

procedure TfrmAttribute.showDlgSymbol;
var
  lSO : ISuperObject;
  lDlg : TdlgSymbolEdit;
begin
  if (FJson.Exists('symbol_params')) then
  begin
    lSO := FJson.O['symbol_params'];
  end else
  begin
    lSO := SO('[]');
  end;

  lDlg := TdlgSymbolEdit.Create(Self, lSO );
  try
    if (lDlg.ShowModal = mrOK) then
    begin
      FJson.O['symbol_params'] := lSO;
    end;
  finally
    lDlg.Free;
  end;
end;
procedure TfrmAttribute.showDlgNumberLinear;
var
  lSO : ISuperObject;
  lDlg : TdlgNumberEdit;
begin
  if (FJson.Exists('number_params')) then
  begin
    lSO := FJson.O['number_params'];
  end else
  begin
    lSO := SO();
  end;

  lDlg := TdlgNumberEdit.Create(Self, lSO );
  try
    if (lDlg.ShowModal = mrOK) then
    begin
      FJson.O['number_params'] := lSO;
    end;
  finally
    lDlg.Free;
  end;
end;

procedure TfrmAttribute.showDlgNumberStep;
var
  lSO : ISuperObject;
  lDlg : TdlgNumberStepFunction;
begin
  if (FJson.Exists('numberstep_params')) then
  begin
    lSO := FJson.O['numberstep_params'];
  end else
  begin
    lSO := SO();
  end;

  lDlg := TdlgNumberStepFunction.Create(Self, lSO );
  try
    if (lDlg.ShowModal = mrOK) then
    begin
      FJson.O['numberstep_params'] := lSO;
    end;
  finally
    lDlg.Free;
  end;
end;

procedure TfrmAttribute.showDlgMetaphone;
var
  lSO : ISuperObject;
  lDlg : TdlgMetaphoneOptions;
begin
  if (FJson.Exists('metaphone_params')) then
  begin
    lSO := FJson.O['metaphone_params'];
  end else
  begin
    lSO := SO();
  end;

  lDlg := TdlgMetaphoneOptions.Create(Self, lSO );
  try
    if (lDlg.ShowModal = mrOK) then
    begin
      FJson.O['metaphone_params'] := lSO;
    end;
  finally
    lDlg.Free;
  end;
end;

procedure TfrmAttribute.doRefresh;
var
  lInput : Boolean;

begin
  //Opcoes reasoning somente quando tipo for input
  lInput:=(cbType.Text = STypeInput) or (cbType.Text = STypeInputOutput);
  lblIndex.Enabled:=lInput;
  lblIndex.FocusControl.Enabled:=lInput;



  lblSimilarityLocal.Enabled:=lInput;
  lblSimilarityLocal.FocusControl.Enabled:=lInput;
  lblWeight.Enabled:=lInput;
  lblWeight.FocusControl.Enabled:=lInput;

  doRefreshSimilarityParams();//Habilita ou desabilita botao de parametros
end;

procedure TfrmAttribute.doPopulateSimilarityFunctions;
begin
  //Popula combo de funcoes
  cbSimilarityFunctions.Items.Clear;
  if (cbDataType.Text = SDataTypeString) then
  begin
    cbSimilarityFunctions.Items.Add(SSelectOne);
    cbSimilarityFunctions.Items.Add(SFunctionStringSensitive);
    cbSimilarityFunctions.Items.Add(SFunctionStringInsensitive);
    cbSimilarityFunctions.Items.Add(SFunctionStringMetaphone);
  end else if ((cbDataType.Text = SDataTypeNumber) or (cbDataType.Text = SDataTypeSymbolOrdered)) then
  begin
    cbSimilarityFunctions.Items.Add(SSelectOne);
    cbSimilarityFunctions.Items.Add(SFunctionEquality);
    cbSimilarityFunctions.Items.Add(SFunctionNumberLinear);
    cbSimilarityFunctions.Items.Add(SFunctionNumberStep);
  end else if (cbDataType.Text = SDataTypeSymbolUnordered) then
  begin
    cbSimilarityFunctions.Items.Add(SDefault);
  end else if (cbDataType.Text = SDataTypeSymbol) then
  begin
    cbSimilarityFunctions.Items.Add(SDefault);
  end else if (cbDataType.Text = SDataTypePhrase) then
  begin
    cbSimilarityFunctions.Items.Add(SSelectOne);
    cbSimilarityFunctions.Items.Add(SFunctionStringIntersect);
    cbSimilarityFunctions.Items.Add(SFunctionStringIntersectMetaphone);
  end;
  cbSimilarityFunctions.ItemIndex:=0;
end;



procedure TfrmAttribute.btnOkClick(Sender: TObject);
begin
  inherited;
end;

procedure TfrmAttribute.btnEditSymbolsClick(Sender: TObject);
begin
  if (cbDataType.Text = SDataTypeSymbol) then
  begin
    showDlgSymbol();
  end else if (cbDataType.Text = SDataTypeSymbolOrdered) then
  begin
    showDlgSymbolOrdered();
  end else if (cbDataType.Text = SDataTypeSymbolUnordered) then
  begin
    showDlgSymbolUnordered();
  end;
end;

procedure TfrmAttribute.validateSimilarityParams();
begin
  //Validacao das funcoes que precisam parametroa
  if (cbDataType.Text = SDataTypeSymbol) then
  begin
    if (not FJson.Exists('symbol_params')) then
    begin
      MessageDlg(SValidation,Format(SValidationMessageEmpty,[SParamSymbolInfo]), mtError, [mbOK], 0);
      Abort;
    end;
  end else if (cbDataType.Text = SDataTypeSymbolOrdered) then
  begin
    if (not FJson.Exists('symbolordered_params')) then
    begin
      MessageDlg(SValidation,Format(SValidationMessageEmpty,[SParamSymbolOrderedInfo]), mtError, [mbOK], 0);
      Abort;
    end;
  end else if (cbSimilarityFunctions.Text = SFunctionNumberLinear) then
  begin
    if (not FJson.Exists('number_params')) then
    begin
      MessageDlg(SValidation,Format(SValidationMessageEmpty,[SParamNumberLinearInfo]), mtError, [mbOK], 0);
      Abort;
    end;
  end else if (cbSimilarityFunctions.Text = SFunctionNumberStep) then
  begin
    if (not FJson.Exists('numberstep_params')) then
    begin
      MessageDlg(SValidation,Format(SValidationMessageEmpty,[SParamNumberLinearInfo]), mtError, [mbOK], 0);
      Abort;
    end;
  end else if (cbDataType.Text = SDataTypeSymbolUnordered) then
  begin
    if (not FJson.Exists('symbolunordered_params')) then
    begin
      MessageDlg(SValidation,Format(SValidationMessageEmpty,[SParamSymbolUnorderedInfo]), mtError, [mbOK], 0);
      Abort;
    end;
  end else if ((cbSimilarityFunctions.Text = SFunctionStringMetaphone) or (cbSimilarityFunctions.Text = SFunctionStringIntersectMetaphone)) then
  //Ao usar metaphone é necessário definir idioma, pode ser usado em frase e palavra
  begin
    if (not FJson.Exists('metaphone_params')) then
    begin
      MessageDlg(SValidation,Format(SValidationMessageEmpty,[SParamMetaphoneInfo]), mtError, [mbOK], 0);
      Abort;
    end;
  end;


end;

procedure TfrmAttribute.doRefreshSimilarityParams;
begin
  btnSimilarityParams.Caption := SParamNoOptions;
  btnSimilarityParams.Enabled := False;

  btnEditSymbols.Visible := False;


  //Habilita botao se o combo esta habilitado
  if (cbDataType.Text = SDataTypeSymbol) then
  begin
    btnEditSymbols.Visible := True;
  end;
  if (cbDataType.Text = SDataTypeSymbolOrdered) then
  begin
    btnEditSymbols.Visible := True;
  end;
  if (cbDataType.Text = SDataTypeSymbolUnordered) then
  begin
    btnEditSymbols.Visible := True;
  end;
  if ((cbSimilarityFunctions.Text = SFunctionStringMetaphone) or (cbSimilarityFunctions.Text = SFunctionStringIntersectMetaphone)) then
  //Ao usar metaphone é necessário exibir botão para definir idioma
  begin
    btnSimilarityParams.Caption := SParamMetaphoneInfo;
    btnSimilarityParams.Enabled := cbSimilarityFunctions.Enabled;
  end;
  if (cbSimilarityFunctions.Text = SFunctionNumberLinear) then
  begin
    btnSimilarityParams.Caption  := SParamNumberLinearInfo;
    btnSimilarityParams.Enabled  := cbSimilarityFunctions.Enabled;
  end;
  if (cbSimilarityFunctions.Text = SFunctionNumberStep) then
  begin
    btnSimilarityParams.Caption  := SParamNumberStepInfo;
    btnSimilarityParams.Enabled  := cbSimilarityFunctions.Enabled;
  end;
end;

procedure TfrmAttribute.btnSimilarityParamsClick(Sender: TObject);
begin
  if (cbSimilarityFunctions.Text = SFunctionNumberLinear) then
  begin
    showDlgNumberLinear();
  end else if (cbSimilarityFunctions.Text = SFunctionNumberStep) then
  begin
    showDlgNumberStep();
  end else if ((cbSimilarityFunctions.Text = SFunctionStringMetaphone) or (cbSimilarityFunctions.Text = SFunctionStringIntersectMetaphone)) then
  begin
    showDlgMetaphone();
  end;
end;

procedure TfrmAttribute.cbDataTypeChange(Sender: TObject);
begin
  doPopulateSimilarityFunctions();//Popula as funcoes de similaridade
  doRefresh();
end;

procedure TfrmAttribute.cbIndexChange(Sender: TObject);
begin
  doRefresh();
end;

procedure TfrmAttribute.cbSimilarityFunctionsChange(Sender: TObject);
begin
  doRefresh();
end;

procedure TfrmAttribute.cbTypeChange(Sender: TObject);
begin
  doRefresh();
end;




end.

