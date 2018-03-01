unit ucaseresult;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, strutils, db, BufDataset, memds, FileUtil, Forms,
  Controls, Graphics, Dialogs, DBGrids, StdCtrls, superobject, ufunctions,
  ustrings, Grids, ComCtrls, Buttons;

type

  { TfrmCaseResult }

  TfrmCaseResult = class(TForm)
    ds: TDataSource;
    grid: TDBGrid;
    lblWait: TLabel;
    mem: TMemDataset;
    btnReuseAdapt: TSpeedButton;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    procedure btnReuseAdaptClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure gridDblClick(Sender: TObject);
    procedure gridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
  private
    { private declarations }
  public
    FCaseDefinition : TJson;
    FCaseInput : String;
    procedure populateGrid(lSO: ISuperObject);
    procedure showReuseForm();
  end;


implementation

uses ureuseform;

{$R *.lfm}

{ TfrmCaseResult }

procedure TfrmCaseResult.Button1Click(Sender: TObject);
begin

end;

procedure TfrmCaseResult.btnReuseAdaptClick(Sender: TObject);
begin
  showReuseForm();
end;

procedure TfrmCaseResult.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure TfrmCaseResult.FormCreate(Sender: TObject);
begin

end;

procedure TfrmCaseResult.gridDblClick(Sender: TObject);
begin
  showReuseForm();
end;

procedure TfrmCaseResult.gridDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin

end;

procedure TfrmCaseResult.populateGrid(lSO: ISuperObject);
var
  lArrayNames : ISuperObject;
  lField : TFieldDef;
  lColName :SOString;
  lColValue : SOString;
  i, c : Integer;
  lDisplayWidth : Integer;

  lField_id : TField;
begin
  grid.Visible:=false;
  lblWait.Visible:=true;

  if (lSO.AsArray.Length>0) then
  begin
    lArrayNames := lSO.AsArray[0].AsObject.GetNames;
  end;

  if (mem.Active) then
  begin
    mem.Clear;
    mem.Close;
  end;
  mem.FieldDefs.Clear;
  //Criando um campo que indica a origem, se da base de conhecimento ou base de casos
  lField := mem.FieldDefs.AddFieldDef;
  lField.DisplayName := '_source';
  lField.DataType := ftString;
  lField.Size := Max(Length(SGridStoredBase),Length(SGridCaseBase));

  lField := mem.FieldDefs.AddFieldDef;
  lField.DisplayName := '_score';
  lField.DataType := ftString;
  lField.Size := 20;

  for c := 0 to lArrayNames.AsArray.Length-1 do
  begin
    if (lArrayNames.AsArray.S[c]='_id') then continue;
    if (lArrayNames.AsArray.S[c]='_score') then continue;
    if (lArrayNames.AsArray.S[c]='_json') then continue;



    lField := mem.FieldDefs.AddFieldDef;
    lField.DisplayName := lArrayNames.AsArray.S[c];
    lField.DataType := ftString;
    lField.Size := 2048;
  end;
  //Criando um campo para guardar o _id
  lField := mem.FieldDefs.AddFieldDef;
  lField.DisplayName := '_id';
  lField.DataType := ftString;
  lField.Size := 2048;
  //Criando um campo para guardar o json de resultado
  lField := mem.FieldDefs.AddFieldDef;
  lField.DisplayName := '_json';
  lField.DataType := ftString;
  lField.Size := 2048;


  mem.Open;
  mem.DisableControls;

  mem.Fields.FieldByName('_source').DisplayLabel:=SGridColSource;
  mem.Fields.FieldByName('_score').DisplayLabel:=SGridColScore;



  //Ocultando coluna json
  mem.Fields[mem.FieldCount-1].Visible:=False;
  try
    for i := 0 to lSO.AsArray.Length-1 do
    begin
      lArrayNames := lSO.AsArray[i].AsObject.GetNames;
      mem.Append;

      for c := 0 to lArrayNames.AsArray.Length-1 do
      begin
        lColName := lArrayNames.AsArray.S[c];
        lColValue := lSO.AsArray.O[i].S[lColName];
        mem.FieldByName(lColName).AsString:= lColValue;
        if (i=0) then
        begin
          mem.FieldByName(lColName).DisplayWidth:=Length(lColValue);
        end else
        begin
          mem.FieldByName(lColName).DisplayWidth:=Max(mem.FieldByName(lColName).DisplayWidth,Length(lColValue));
        end;
        //mem.FieldByName(lColName).Size:=mem.FieldByName(lColName).DisplayWidth;

      end;
      //ShowMessage(lSO.AsArray.O[i].AsJSon());
      mem.Fields[mem.FieldCount-1].AsString:=lSO.AsArray.O[i].AsJSon();
      //Ocultando campo do mongo que indica a origem
      lField_id := mem.FindField('_id');
      if (Assigned(lField_id)) then
      begin
        lField_id.Visible:=false;
      end;
      if (Assigned(lField_id) and (lField_id.AsString<>'')) then
      begin
        mem.FieldByName('_source').AsString:= SGridStoredBase;
      end else
      begin
        mem.FieldByName('_source').AsString:= SGridCaseBase;
      end;
      mem.Post;
    end;
  finally
    mem.EnableControls;
  end;

  lblWait.Visible:=false;
  grid.Visible:=True;
end;

procedure TfrmCaseResult.showReuseForm;
var
  frmReuseForm : TfrmReuseForm;
begin
  frmReuseForm             := TfrmReuseForm.Create(Self, FCaseDefinition);
  frmReuseForm.FCaseResult := mem.Fields[mem.FieldCount-1].AsString;
  frmReuseForm.Show;
end;

end.

