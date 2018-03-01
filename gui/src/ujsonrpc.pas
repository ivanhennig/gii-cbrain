unit ujsonrpc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, superobject, ustrings, ufunctions, fphttpclient;

type

  TRpcCallback = procedure(e,r: String) of Object;

  { TRpc }

  TRpc = class(TThread)
  private
    FRequestID : Integer;
    FError     : String;
    FResult    : String;
    FMethod    : String;
    FParams    : ISuperObject;
    FCallback  : TRpcCallback;
    procedure DoCallback;
  public
    constructor Create(amethod: String; aparams: array of const; acallback: TRpcCallback);
  protected
    procedure Execute; override;
  end;


implementation

{ TRpc }

procedure TRpc.DoCallback;
begin
  if (Assigned(FCallback)) then
  begin
    FCallback(FError,FResult);
  end;
end;

constructor TRpc.Create(amethod: String; aparams: array of const;
  acallback: TRpcCallback);
begin
  FRequestID := 1;
  FreeOnTerminate := True;
  FMethod   := amethod;
  FParams   := SA(aparams);
  FCallback := acallback;
  inherited Create(True);
  Resume;
end;

procedure TRpc.Execute;
var
  lHttp : TFPHTTPClient;
  lJson : ISuperObject;
  lResult : String;
  lRequest : String;
  lSettings : ISuperObject;
begin
  lSettings:=getSettings;

  Inc( FRequestID );
  lHttp := TFPHTTPClient.Create(Nil);
  try
    try
      lJson := SO();
      lJson.S['id']     := IntToStr( FRequestID );
      lJson.S['method'] := FMethod;
      lJson.O['params'] := FParams;
      lRequest := lJson.AsJSon();
      lHttp.RequestBody := TStringStream.Create(  lRequest );
      lResult := lHttp.Post(lSettings.S['rpcurl']+'/rpc.php?class=Main');
      if (lResult='') then
      begin
        FError:=SErrEmptyResponse;
        Exit;
      end;

      lJson.Clear();
      lJson := SO(lResult);
      if (Assigned(lJson.O['error']) and (not lJson.O['error'].IsType(stNull))) then
      begin
        FError := lJson.O['error'].S['message'];
        Exit;
      end;
      if (Assigned(lJson.O['result'])) then
      begin
        FResult :=lJson.O['result'].AsJSon();
      end else
      begin
        FError := lResult;
      end;
    except
      on E:Exception do
      begin
        FError := E.ClassName + ': '+ e.Message;
      end;
    end;
  finally
    lHttp.RequestBody.Free;
    lHttp.Free;
    Synchronize(@DoCallback);
  end;
end;

end.

