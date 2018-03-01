unit uinputform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, Spin, ucommon, superobject, ujsonrpc, ustrings, ucaseresult;

type

  { TComboValue }

  TComboValue = class
    Value : String;
    constructor Create(aValue : String);
  end;

  { TfrmInputForm }

  TfrmInputForm = class(TfrmCommon)
    procedure btnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    function serializeControls: String;
    procedure btnOkClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    FTop : Integer;
    procedure callbackPHPExec(e, r : String);
    procedure addSymbolControl(aName: String; aArray: TSuperArray; aVal : String = '');
    procedure addUnorderedSymbolControl(aName: String; aArray: TSuperArray; aVal : String = '');
    procedure addTextControl(aName: String; aVal : String = '');
    procedure addMemoControl(aName: String; aVal : String = '');
    procedure addNumberControl(aName: String; aMin, aMax: Double; aVal : Double = 0);
    procedure addLabel(aCaption : String);
    { private declarations }
  public
    //FCaseInput : String;
    procedure createControls(aOnlyInputs: Boolean=true; aCaseInput : String = '{}');
  end;

implementation

{$R *.lfm}

{ TComboValue }

constructor TComboValue.Create(aValue: String);
begin
  Value:=aValue;
end;

{ TfrmInputForm }

procedure TfrmInputForm.FormCreate(Sender: TObject);
begin
  inherited;
  btnOk.Caption:=SOKRetrieve;
end;

procedure TfrmInputForm.createControls(aOnlyInputs: Boolean; aCaseInput: String
  );
var
  lArray : TSuperArray;
  i : Byte;
  lJson : ISuperObject;
  lKey : String;
begin
  lJson := SO(aCaseInput);

  if (not FJson.Exists('attrs')) then Exit;
  lArray := FJson.Instance.A['attrs'];
  if (lArray.Length = 0) then Exit;
  FTop:=20;
  for i := 0 to lArray.Length-1 do
  begin
    if (aOnlyInputs) then
    begin
      //Somente atributos de entrada devem aparecer aqui
      if (lArray[i].S['type']='output') then
      begin
        Continue;
      end;
    end;
    lKey := lArray[i].S['attrname'];

    addLabel(lArray[i].S['attrdescription']);
    if (lArray[i].S['datatype'] = 'string') then
    begin
      addTextControl(lArray[i].S['attrname'], lJson.S[lKey]);
      Inc(FTop,30);

    end else if (lArray[i].S['datatype'] = 'phrase') then
    begin
      addMemoControl(lArray[i].S['attrname'], lJson.S[lKey]);
      Inc(FTop,110);
    end else if (lArray[i].S['datatype'] = 'number') then
    begin
      addNumberControl(
        lArray[i].S['attrname'],
        lArray[i].O['number_params'].D['lowbound'],
        lArray[i].O['number_params'].D['highbound'],
        lJson.D[lKey]
        );
      Inc(FTop,30);
    end else if (lArray[i].S['datatype'] = 'symbolordered') then
    begin
      addSymbolControl(
        lArray[i].S['attrname'],
        lArray[i].A['symbolordered_params'],
        lJson.S[lKey]
      );
      Inc(FTop,30);
    end else if (lArray[i].S['datatype'] = 'symbol') then
    begin
      addSymbolControl(
        lArray[i].S['attrname'],
        lArray[i].A['symbol_params'],
        lJson.S[lKey]
      );
      Inc(FTop,30);
    end else if (lArray[i].S['datatype'] = 'symbolunordered') then
    begin
      addUnorderedSymbolControl(
        lArray[i].S['attrname'],
        lArray[i].A['symbolunordered_params'],
        lJson.S[lKey]
      );
      Inc(FTop,30);
    end;


  end;
  Self.Repaint;
end;

procedure TfrmInputForm.btnCancelClick(Sender: TObject);
begin
  //inherited;
  Self.Close;
end;

procedure TfrmInputForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction:=caFree;
end;

procedure TfrmInputForm.FormShow(Sender: TObject);
begin
  createControls();
end;

function TfrmInputForm.serializeControls() : String;
var
  i : Byte;
  lJson : ISuperObject;
  lLabel : TLabel;
  lKey : String;
begin
  lJson := SO();
  for i := 0 to Self.ControlCount-1 do
  begin
    lKey := Copy(Self.Controls[i].Name,2,Length(Self.Controls[i].Name));

    if (Self.Controls[i] is TLabel) then
    begin
      lLabel:=TLabel(Self.Controls[i]);
    end else if (Self.Controls[i] is TMemo) then
    begin
      lJson.S[lKey] := TMemo(Self.Controls[i]).Lines.Text;
    end else if (Self.Controls[i] is TEdit) then
    begin
      lJson.S[lKey] := TEdit(Self.Controls[i]).Text;
    end else if (Self.Controls[i] is TFloatSpinEdit) then
    begin
      lJson.D[lKey] := TFloatSpinEdit(Self.Controls[i]).Value;
    end else if (Self.Controls[i] is TComboBox) then
    begin
      if (TComboBox(Self.Controls[i]).ItemIndex<0) then
      begin
        MessageDlg(SValidation,Format(SValidationMessageEmpty,[lLabel.Caption]), mtError, [mbOK], 0);
        TWinControl(Self.Controls[i]).SetFocus;
        Abort;
      end;
      lJson.S[lKey] := TComboValue(TComboBox(Self.Controls[i]).Items.Objects[TComboBox(Self.Controls[i]).ItemIndex]).Value;
    end;
  end;
  //public function exec($project_json, $casename, $casedata)
  Result := lJson.AsJSon();

end;

procedure TfrmInputForm.btnOkClick(Sender: TObject);
var
  lSerializedControls : String;
begin
  lSerializedControls := serializeControls();
  btnOk.Enabled:=False;
  btnCancel.Enabled:=False;
  TRpc.Create('query', [FJson.AsJson, lSerializedControls], @callbackPHPExec);//RPC Async
end;

procedure TfrmInputForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  inherited;
end;

procedure TfrmInputForm.addSymbolControl(aName: String; aArray: TSuperArray;
  aVal: String);
var
  lInput : TComboBox;
  i : Byte;
begin
  lInput := TComboBox.Create(Self);
  lInput.Parent   := Self;
  lInput.Name     := 'i'+aName;
  lInput.AutoSize := false;
  lInput.Left     := 170;
  lInput.Width    := 400;
  lInput.Top      := FTop;
  lInput.Style    := csDropDownList;
  lInput.AddItem(SSelectOne,TComboValue.Create(''));
  for i := 0 to aArray.Length-1 do
  begin
    if (aArray[i].AsArray.S[0]='') then Continue;
    if (aArray[i].AsArray.S[1]='') then Continue;

    lInput.AddItem(aArray[i].AsArray.S[1],TComboValue.Create(aArray[i].AsArray.S[0]));
  end;
  if (aVal<>'') then
  begin
    lInput.ItemIndex:=lInput.Items.IndexOf(aVal);
  end;

end;

procedure TfrmInputForm.addUnorderedSymbolControl(aName: String;
  aArray: TSuperArray; aVal: String);
var
  lInput : TComboBox;
  i : Byte;
begin
  lInput := TComboBox.Create(Self);
  lInput.Parent   := Self;
  lInput.Name     := 'i'+aName;
  lInput.AutoSize := false;
  lInput.Left     := 170;
  lInput.Width    := 400;
  lInput.Top      := FTop;
  lInput.Style    := csDropDownList;
  lInput.AddItem(SSelectOne,TComboValue.Create(''));
  for i := 0 to aArray.Length-1 do
  begin
    if (aArray[i].AsArray.S[0]='') then Continue;
    lInput.AddItem(aArray[i].AsArray.S[0],TComboValue.Create(aArray[i].AsArray.S[0]));
  end;
  if (aVal<>'') then
  begin
    lInput.ItemIndex:=lInput.Items.IndexOf(aVal);
  end;
end;

procedure TfrmInputForm.addTextControl(aName: String; aVal: String);
var
  lInput : TEdit;
begin
  lInput := TEdit.Create(Self);
  lInput.Parent   := Self;
  lInput.Name     := 'i'+aName;
  lInput.AutoSize := false;
  lInput.Alignment:= taLeftJustify;
  lInput.Left     := 170;
  lInput.Width    := 400;
  lInput.Top      := FTop;
  lInput.Text     := aVal;
end;

procedure TfrmInputForm.addMemoControl(aName: String; aVal: String);
var
  lInput : TMemo;
begin
  lInput := TMemo.Create(Self);
  lInput.Parent   := Self;
  lInput.Name     := 'i'+aName;
  lInput.AutoSize := false;
  lInput.Alignment:= taLeftJustify;
  lInput.Left     := 170;
  lInput.Width    := 400;
  lInput.Height   := 100;
  lInput.Top      := FTop;
  lInput.Text     := aVal;
end;

procedure TfrmInputForm.addNumberControl(aName: String; aMin, aMax: Double;
  aVal: Double);
var
  lInput : TFloatSpinEdit;
begin
  lInput := TFloatSpinEdit.Create(Self);
  lInput.Parent   := Self;
  lInput.Name     := 'i'+aName;
  lInput.AutoSize := false;
  lInput.Alignment:= taRightJustify;
  lInput.Left     := 170;
  lInput.Width    := 400;
  lInput.MinValue := 0;
  lInput.MaxValue := 0;
  lInput.Top      := FTop;
  lInput.Value    := aVal;
end;

procedure TfrmInputForm.addLabel(aCaption: String);
var
  lLabel : TLabel;
begin
  lLabel := TLabel.Create(Self);
  lLabel.AutoSize := false;
  lLabel.Alignment:= taRightJustify;
  lLabel.Left     := 10;
  lLabel.Width    := 150;
  lLabel.Top      := FTop;
  lLabel.Caption  := aCaption;
  lLabel.Visible  := True;
  lLabel.Parent   := Self;
end;

procedure TfrmInputForm.callbackPHPExec(e, r: String);
var
  frmCaseResult : TfrmCaseResult;
  lSO : ISuperObject;
begin
  btnOk.Enabled:=True;
  btnCancel.Enabled:=True;
  if (e<>'') then
  begin
    ShowMessage(e);
    Exit;
  end;

  lSO := SO(r);
  if (lSO.AsArray.Length=0) then
  begin
    ShowMessage(SNoResults);
    Exit;
  end;

  frmCaseResult := TfrmCaseResult.Create(Self);
  frmCaseResult.FCaseDefinition := FJson;
  frmCaseResult.Show;
  frmCaseResult.populateGrid(lSO);

end;

end.

