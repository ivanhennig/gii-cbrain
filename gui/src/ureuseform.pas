unit ureuseform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  uinputform, ujsonrpc, ustrings;

type

  { TfrmReuseForm }

  TfrmReuseForm = class(TfrmInputForm)
    procedure btnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    FCaseResult : String;
    procedure callbackPHPExec(e, r : String);
  end;


implementation

{$R *.lfm}

{ TfrmReuseForm }

procedure TfrmReuseForm.btnOkClick(Sender: TObject);
var
  lSerializedControls : String;
begin
  lSerializedControls := serializeControls();
  //  inherited;
  //storeInternal($case_definition_json, $input_casedata_json, $output_casedata_json)
  btnOk.Enabled:=False;
  btnCancel.Enabled:=False;
  TRpc.Create('store', [FJson.AsJson, lSerializedControls], @callbackPHPExec);//RPC Async
end;

procedure TfrmReuseForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction:=caFree;
end;

procedure TfrmReuseForm.FormShow(Sender: TObject);
begin
  //inherited;
  createControls(False, FCaseResult);
end;

procedure TfrmReuseForm.FormCreate(Sender: TObject);
begin
  inherited;
  btnOk.Caption:=SOKReuse;
end;

procedure TfrmReuseForm.callbackPHPExec(e, r: String);
begin
  btnOk.Enabled:=True;
  btnCancel.Enabled:=True;
  if (e<>'') then
  begin
    ShowMessage(e);
    Exit;
  end;
  Self.Close;
end;


end.

