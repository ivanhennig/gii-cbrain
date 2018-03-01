unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, FileUtil, LazFileUtils, Forms, Controls, Graphics,
  Dialogs, Menus, ComCtrls, ExtCtrls, ActnList, Buttons, VirtualTrees, uproject,
  usettings, uattribute, superobject, ufunctions, ustrings, ucase, uinputform,
  ufunctionssys, LCLType, StdCtrls, PopupNotifier;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actEditProject: TAction;
    actEditCase: TAction;
    actEditAttribute: TAction;
    actInputForm: TAction;
    actStartInternalDB: TAction;
    actRestartDaemon: TAction;
    actRemoveAttr: TAction;
    actStopDaemon: TAction;
    actStartDaemon: TAction;
    actNewAttribute: TAction;
    actOpenProject: TAction;
    actSaveAs: TAction;
    actSave: TAction;
    actRemoveProject: TAction;
    actRemoveCase: TAction;
    actNewProject: TAction;
    actNewCase: TAction;
    ActionList1: TActionList;
    imgsMenu: TImageList;
    imgsTree: TImageList;
    mmoDB: TMemo;
    mmoPHP: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    mnuNewAttribute: TMenuItem;
    mnuAttribute: TMenuItem;
    mnuRemoveCase: TMenuItem;
    mnuCase: TMenuItem;
    mnuProjectEdit: TMenuItem;
    mnuMain: TMainMenu;
    mnuSep3: TMenuItem;
    mnuSep2: TMenuItem;
    mnuOnlineHelp: TMenuItem;
    mnuAbout: TMenuItem;
    mnuHelp: TMenuItem;
    mnuSettings: TMenuItem;
    mnuNewCase: TMenuItem;
    mnuProject: TMenuItem;
    mnuExit: TMenuItem;
    mnuFile: TMenuItem;
    mnuProjectNew: TMenuItem;
    openDlg: TOpenDialog;
    saveProject: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    vt: TVirtualStringTree;

    procedure actEditAttributeExecute(Sender: TObject);
    procedure actEditAttributeUpdate(Sender: TObject);
    procedure actEditCaseExecute(Sender: TObject);
    procedure actEditCaseUpdate(Sender: TObject);
    procedure actEditProjectExecute(Sender: TObject);
    procedure actEditProjectUpdate(Sender: TObject);
    procedure actInputFormExecute(Sender: TObject);
    procedure actInputFormUpdate(Sender: TObject);
    procedure actNewAttributeExecute(Sender: TObject);
    procedure actNewAttributeUpdate(Sender: TObject);
    procedure actNewCaseExecute(Sender: TObject);
    procedure actNewCaseUpdate(Sender: TObject);
    procedure actNewProjectExecute(Sender: TObject);
    procedure actOpenProjectExecute(Sender: TObject);
    procedure actRemoveAttrExecute(Sender: TObject);
    procedure actRemoveAttrUpdate(Sender: TObject);
    procedure actRemoveCaseExecute(Sender: TObject);
    procedure actRemoveCaseUpdate(Sender: TObject);
    procedure actRemoveProjectExecute(Sender: TObject);
    procedure actRemoveProjectUpdate(Sender: TObject);
    procedure actRestartDaemonExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveAsUpdate(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure actStartDaemonExecute(Sender: TObject);
    procedure actStartDaemonUpdate(Sender: TObject);
    procedure actStartInternalDBExecute(Sender: TObject);
    procedure actStopDaemonExecute(Sender: TObject);
    procedure actStopDaemonUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);

    procedure FormCreate(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuProjectCloseClick(Sender: TObject);
    procedure mnuProjectEditClick(Sender: TObject);
    procedure mnuProjectNewClick(Sender: TObject);
    procedure mnuSettingsClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure vtChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtClick(Sender: TObject);
    procedure vtGetCursor(Sender: TBaseVirtualTree; var aCursor: TCursor);
    procedure vtGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure vtGetLineStyle(Sender: TBaseVirtualTree; var Bits: Pointer);
    procedure vtGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vtHotChange(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode);
    procedure vtPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  private
    { private declarations }
    FProcessPHP : TProcess;
    FProcessDB  : TProcess;

    procedure doOpenFile(aPath: String);
  public
    { public declarations }
    function isProjectNode(anode : PVirtualNode) : Boolean;
    function isCaseNode(anode : PVirtualNode) : Boolean;
    function isAttrNode(anode : PVirtualNode) : Boolean;

    function getProjectNodeSelected() : PVirtualNode;
    function getProjectNode(anode : PVirtualNode) : PVirtualNode;
    function getCaseNodeSelected() : PVirtualNode;
    function getCaseNode(anode : PVirtualNode) : PVirtualNode;

    function getCaseDataFromSelected() : TJson;

    procedure touchProject(anode : PVirtualNode; achanged : Boolean = True);

    function newProject(adata: TJson): PVirtualNode;
    function newCase(aparent: PVirtualNode; adata: TJson): PVirtualNode;
    function newAttr(awhere: PVirtualNode; adata: TJson): PVirtualNode;

    function prepareJson() : ISuperObject;

    procedure doStartServices();

  end;

var
  frmMain: TfrmMain;
  i:Int64;
const
  IMG_FOLDER_OPEN  = 0;
  IMG_FOLDER_CLOSE = 1;
  IMG_FILE         = 2;
  LVL_PROJ         = 0;
  LVL_CASE         = 1;
  LVL_ATTR         = 2;

  COL_WEIGHT       = 4;
  COL_EDIT         = 1;
  COL_DELETE       = 2;
  COL_DESC         = 3;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmMain.mnuProjectCloseClick(Sender: TObject);
begin

end;

procedure TfrmMain.mnuProjectEditClick(Sender: TObject);
begin

end;

procedure TfrmMain.mnuProjectNewClick(Sender: TObject);
begin

end;

procedure TfrmMain.mnuSettingsClick(Sender: TObject);
var
  lSO : ISuperObject;
begin
  lSO:=getSettings();
  with TfrmSettings.Create(Self,lSO) do
  begin
    try
      if (ShowModal = mrOK) then
      begin
        lSO.SaveTo(Common.UserDir + '.giiCBRain.json', true);
        doStartServices();
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
var
  Buffer: string;
  BytesAvailable: DWord;
  BytesRead:LongInt;
begin
  if Assigned(FProcessDB) then
  begin
    if FProcessDB.Running then
    begin
      BytesAvailable := FProcessDB.Output.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable>0 do
      begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := FProcessDB.OutPut.Read(Buffer[1], BytesAvailable);
        mmoDB.Lines.Text := mmoDB.Lines.Text + copy(Buffer,1, BytesRead);
        BytesAvailable := FProcessDB.Output.NumBytesAvailable;
      end;
    end;

    if (Assigned(FProcessDB.Stderr)) then
    begin
      BytesAvailable := FProcessDB.Stderr.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable>0 do
      begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := FProcessDB.Stderr.Read(Buffer[1], BytesAvailable);
        mmoDB.Lines.Text := mmoDB.Lines.Text + copy(Buffer,1, BytesRead);
        BytesAvailable := FProcessDB.Stderr.NumBytesAvailable;
      end;
    end;
  end;
  if Assigned(FProcessPHP) then
  begin
    if FProcessPHP.Running then
    begin
      BytesAvailable := FProcessPHP.Output.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable>0 do
      begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := FProcessPHP.OutPut.Read(Buffer[1], BytesAvailable);
        mmoPHP.Lines.Text := mmoPHP.Lines.Text + copy(Buffer,1, BytesRead);
        BytesAvailable := FProcessPHP.Output.NumBytesAvailable;
      end;
    end;

    BytesAvailable := FProcessPHP.Stderr.NumBytesAvailable;
    BytesRead := 0;
    while BytesAvailable>0 do
    begin
      SetLength(Buffer, BytesAvailable);
      BytesRead := FProcessPHP.Stderr.Read(Buffer[1], BytesAvailable);
      mmoPHP.Lines.Text := mmoPHP.Lines.Text + copy(Buffer,1, BytesRead);
      BytesAvailable := FProcessPHP.Stderr.NumBytesAvailable;
    end;
  end;
end;

procedure TfrmMain.vtChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin

end;

procedure TfrmMain.vtClick(Sender: TObject);
var
  x,y : integer;
  lTmp : THitInfo;

  lJson : TJson;
begin
  x := vt.ScreenToClient(Mouse.CursorPos).X;
  y := vt.ScreenToClient(Mouse.CursorPos).Y;
  vt.GetHitTestInfoAt(x,y,true,lTmp);
  if not Assigned(lTmp.HitNode) then Exit;
  lJson := PJson(vt.GetNodeData(lTmp.HitNode))^;
  if not Assigned(lJson) then Exit;
  if (lTmp.HitColumn = COL_EDIT) then
  begin
    if (isProjectNode(lTmp.HitNode)) then
    begin
      actEditProjectExecute(nil);
    end else if (isCaseNode(lTmp.HitNode)) then
    begin
      actEditCaseExecute(nil);
    end else if (isAttrNode(lTmp.HitNode)) then
    begin
      actEditAttributeExecute(nil);
    end;
  end else if (lTmp.HitColumn = COL_DELETE) then
  begin
    if (isProjectNode(lTmp.HitNode)) then
    begin
      actRemoveProjectExecute(nil);
    end else if (isCaseNode(lTmp.HitNode)) then
    begin
      actRemoveCaseExecute(nil);
    end else if (isAttrNode(lTmp.HitNode)) then
    begin
      actRemoveAttrExecute(nil);
    end;
  end;
end;

procedure TfrmMain.vtGetCursor(Sender: TBaseVirtualTree; var aCursor: TCursor);
begin
 {
  if (Sender.FCurrentHotColumn=COL_EDIT) then
  begin
    aCursor:=crHandPoint;
  end else begin
    aCursor:=crDefault;
  end;
  }
end;




procedure TfrmMain.vtGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  lLevel :Byte;
begin
  lLevel := Sender.GetNodeLevel(Node);
  if (Column = 0) then
  begin
    case lLevel of
      LVL_PROJ : ImageIndex := IMG_FOLDER_OPEN;
      LVL_CASE : ImageIndex := IMG_FILE;
      LVL_ATTR : ImageIndex := IMG_FILE;
    end;
  end else if (Column = COL_EDIT) then
  begin
    ImageIndex := 3;
  end else if (Column = COL_DELETE) then
  begin
    ImageIndex := 4;
  end;
end;

procedure TfrmMain.vtGetLineStyle(Sender: TBaseVirtualTree; var Bits: Pointer);
begin

end;

procedure TfrmMain.vtGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  lJson : TJson;
  lLvl  : Byte;
begin
  CellText:='';
  lLvl  := Sender.GetNodeLevel(Node);
  lJson := PJson(Sender.GetNodeData(Node))^;
  if (Column = 0) then
  begin
    case lLvl of
      LVL_PROJ : CellText:=lJson.S['projectname'];
      LVL_CASE : CellText:=lJson.S['casename'];
      LVL_ATTR : CellText:=lJson.S['attrname'];
    end;
  end else if (Column = COL_DESC) then
  begin
    case lLvl of
      LVL_PROJ : CellText:=lJson.S['projectdescription'];
      LVL_CASE : CellText:=lJson.S['casedescription'];
      LVL_ATTR : CellText:=lJson.S['attrdescription'];
    end;
  end else if (Column = COL_WEIGHT) then
  begin
    case lLvl of
//      LVL_PROJ : CellText:=lJson.S['projectdescription'];
//      LVL_CASE : CellText:=lJson.S['casedescription'];
      LVL_ATTR :
        begin
          //Exibição do peso somente nos atributos de entrada
          if ((lJson.S['type']='input') or (lJson.S['type']='input_output')) then
          begin
            CellText:=lJson.S['weight'];
          end else
          begin
            CellText:='-';
          end;


        end;
    end;
  end;
end;

procedure TfrmMain.vtHotChange(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode);
begin

end;

procedure TfrmMain.vtPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  lJson : TJson;
  lLvl  : Byte;
begin
  lLvl  := Sender.GetNodeLevel(Node);
  lJson := PJson(Sender.GetNodeData(Node))^;
  if (Column = 0) then
  begin
    case lLvl of
      LVL_PROJ : begin
        if (lJson.FChanged) then
        begin
          TargetCanvas.Font.Style:=[fsBold];
        end;
      end;
//      LVL_CASE : CellText:=lJson.S['casename'];
//      LVL_ATTR : CellText:=lJson.S['attrname'];
    end;
  end else if (Column = 1) then
  begin
    TargetCanvas.Font.Style:=[fsItalic];
//    case lLvl of
//      LVL_PROJ : CellText:=lJson.S['projectdescription'];
//      LVL_CASE : CellText:=lJson.S['casedescription'];
//      LVL_ATTR : CellText:=lJson.S['attrdescription'];
//    end;
  end;
end;

function TfrmMain.isProjectNode(anode: PVirtualNode): Boolean;
begin
  Result:=(Assigned(anode)) and (vt.GetNodeLevel(anode)=LVL_PROJ);
end;

function TfrmMain.isCaseNode(anode: PVirtualNode): Boolean;
begin
  Result:=(Assigned(anode)) and (vt.GetNodeLevel(anode)=LVL_CASE);
end;

function TfrmMain.isAttrNode(anode: PVirtualNode): Boolean;
begin
  Result:=(Assigned(anode)) and (vt.GetNodeLevel(anode)=LVL_ATTR);
end;

function TfrmMain.getProjectNodeSelected: PVirtualNode;
begin
  Result:=getProjectNode(vt.GetFirstSelected());
end;

function TfrmMain.getProjectNode(anode: PVirtualNode): PVirtualNode;
begin
  Result := nil;
  if (isProjectNode(anode)) then
  begin
    Result := anode;
  end else if (isCaseNode(anode)) then
  begin
    Result := anode^.Parent;
  end else if (isAttrNode(anode)) then
  begin
    Result := anode^.Parent^.Parent;
  end;
end;

function TfrmMain.getCaseNodeSelected: PVirtualNode;
begin
  Result:=getCaseNode(vt.GetFirstSelected());
end;

function TfrmMain.getCaseNode(anode: PVirtualNode): PVirtualNode;
begin
  Result := nil;
  if (isCaseNode(anode)) then
  begin
    Result := anode;
  end else if (isAttrNode(anode)) then
  begin
    Result := anode^.Parent;
  end;
end;



function TfrmMain.getCaseDataFromSelected: TJson;
var
  lProjData : TJson;
begin
  Result    := PJson( vt.GetNodeData( getCaseNodeSelected()))^;
  lProjData := PJson( vt.GetNodeData( getProjectNodeSelected()))^;
  Result.Instance.O['project'] := lProjData.Instance;
  Result.Instance.O['project'].Delete('cases');
end;

procedure TfrmMain.touchProject(anode: PVirtualNode; achanged: Boolean);
var
  lNode : PVirtualNode;
  lJson : PJson;
begin
  lNode := getProjectNode(anode);
  lJson := vt.GetNodeData(lNode);
  lJson^.FChanged:=achanged;

  vt.Invalidate;
end;


function TfrmMain.newProject(adata: TJson): PVirtualNode;
var
  i,ii : Integer;
  lNodeCase : PVirtualNode;
  lArrCase, lArrAttr : TSuperArray;
  lJsonCase, lJsonAttr : TJson;

  lData : PJson;
begin
  Result := vt.InsertNode(Nil,amInsertAfter);
  lData  := vt.GetNodeData(Result);
  lData^ := adata;

  if (adata.Exists('cases')) then
  begin
    lArrCase := adata.Instance.A['cases'];
    for i:=0 to lArrCase.Length-1 do
    begin
      lJsonCase := TJson.Create(lArrCase.O[i]);
      lNodeCase := newCase(Result, lJsonCase);

      if (lJsonCase.Exists('attrs')) then
      begin
        lArrAttr := lJsonCase.Instance.A['attrs'];
        for ii:=0 to lArrAttr.Length-1 do
        begin
          lJsonAttr := TJson.Create(lArrAttr.O[ii]);
          newAttr(lNodeCase, lJsonAttr);
        end;
        //lJsonCase.Instance.Delete('attrs');//Pode ser deletado pois um novo json é montado baseado no tree
      end;

    end;
    //adata.Instance.Delete('cases');//Pode ser deletado pois um novo json é montado baseado no tree
  end;
  vt.Selected[Result]:=true;
end;
function TfrmMain.newCase(aparent: PVirtualNode; adata: TJson): PVirtualNode;
var
  lJson : PJson;
begin
  if (not Assigned(aparent)) then
  begin
    raise Exception.Create(SErrSelectProject);
  end;
  if (vt.GetNodeLevel(aparent) > 0) then
  begin
    raise Exception.Create(SErrSelectProject);
  end;

  Result := vt.InsertNode(aparent, amAddChildLast);
  lJson  := vt.GetNodeData( Result );
  lJson^ := adata;

  vt.Expanded[aparent]:=True;


end;

function TfrmMain.newAttr(awhere: PVirtualNode; adata: TJson): PVirtualNode;
var
  lJson : PJson;
begin
  if (isCaseNode(awhere)) then
  begin
    Result := vt.InsertNode(awhere, amAddChildLast);
  end else
  begin
    Result := vt.InsertNode(awhere, amInsertAfter);
  end;
  lJson  := vt.GetNodeData( Result );
  lJson^ := adata;
end;

function TfrmMain.prepareJson: ISuperObject;
var
  lJson  : ISuperObject;
  lArrCase, lArrAttr : ISuperObject;
  lCase, lAttr : ISuperObject;
  lData  : PJson;
  lProjectNode, lCaseNode, lAttrNode : PVirtualNode;
begin
  Result := nil;
  lProjectNode := getProjectNodeSelected();
  if (Assigned(lProjectNode)) then
  begin
    lData  := vt.GetNodeData(lProjectNode);
    lJson  := lData^.Instance.Clone;
    if (lProjectNode^.ChildCount>0) then
    begin
      lArrCase := SO('[]');
      lCaseNode := lProjectNode^.FirstChild;
      while Assigned(lCaseNode) do
      begin
        lArrAttr := SO('[]');
        lAttrNode := lCaseNode^.FirstChild;
        while Assigned(lAttrNode) do
        begin
          lAttr := PJson(vt.GetNodeData(lAttrNode))^.Instance;
          lArrAttr.AsArray.Add( lAttr );
          lAttrNode := lAttrNode^.NextSibling;
        end;
        lCase := PJson(vt.GetNodeData(lCaseNode))^.Instance;
        lCase.O['attrs'] := lArrAttr;
        lArrCase.AsArray.Add( lCase );
        lCaseNode := lCaseNode^.NextSibling;
      end;
      lJson.O['cases'] := lArrCase;
    end;
    Result := lJson;
  end;
end;

procedure TfrmMain.doStartServices;
var
  lJson : ISuperObject;
begin
  lJson := getSettings;
  if (lJson.B['uselocalservice']) then
  begin
    actStartDaemonExecute(nil);
    actStartInternalDBExecute(nil);
  end else
  begin
    actStopDaemonExecute(nil);
  end;
  StatusBar1.SimpleText:='';
  StatusBar1.Update;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  //Setando o titulo das colunas
  vt.Header.Columns.Items[0].Text:= STreeColProjects;
  vt.Header.Columns.Items[1].Text:= STreeColEdit;
  vt.Header.Columns.Items[2].Text:= STreeColDelete;
  vt.Header.Columns.Items[3].Text:= STreeColDescription;
  vt.Header.Columns.Items[4].Text:= STreeColWeigth;

  Self.Text:=Application.Title;


  vt.NodeDataSize  := SizeOf(TJson);

  if (Paramcount>0) then
  begin
    doOpenFile(ParamStr(1));
  end;

  doStartServices();
end;



procedure TfrmMain.actNewCaseExecute(Sender: TObject);
begin
  with TfrmCase.Create(Self, TJson.Create ) do
  begin
    try
      if (ShowModal = mrOK) then
      begin
        newCase(vt.GetFirstSelected(), FJson);
        touchProject(vt.GetFirstSelected(), True);
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.actEditProjectExecute(Sender: TObject);
var
  lData : PJson;
begin
  lData := vt.GetNodeData( getProjectNodeSelected() );
  with TfrmProject.Create(Self, lData^ ) do
  begin
    try
      if (ShowModal = mrOK) then
      begin
        touchProject(vt.GetFirstSelected(), True);
      end;
    finally
      Free;
    end;
  end;

end;

procedure TfrmMain.actEditProjectUpdate(Sender: TObject);
begin
   TAction(Sender).Enabled := Assigned(getProjectNodeSelected());
end;

procedure TfrmMain.actInputFormExecute(Sender: TObject);
begin
  TfrmInputForm.Create(Self, getCaseDataFromSelected() ).Show;
end;

procedure TfrmMain.actInputFormUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= vt.GetNodeLevel(vt.GetFirstSelected()) >= 1;
end;

procedure TfrmMain.actNewAttributeExecute(Sender: TObject);
var
  lNode :PVirtualNode;
begin
  with TfrmAttribute.Create(Self, TJson.Create, getCaseDataFromSelected() ) do
  begin
    try
      if (ShowModal = mrOK) then
      begin
        lNode := newAttr( vt.GetFirstSelected(), FJson);
        touchProject(vt.GetFirstSelected(), True);

        vt.Selected[lNode] := True;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.actNewAttributeUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= vt.GetNodeLevel(vt.GetFirstSelected()) >= 1;
end;

procedure TfrmMain.actEditCaseExecute(Sender: TObject);
begin
  if (isProjectNode(vt.GetFirstSelected())) then Exit;

  with TfrmCase.Create(Self, PJson(vt.GetNodeData( getCaseNodeSelected() ))^ ) do
  begin
    try
      if (ShowModal = mrOK) then
      begin
        touchProject(vt.GetFirstSelected());
      end;
    finally
      Free;
    end;
  end;

end;

procedure TfrmMain.actEditCaseUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= vt.GetNodeLevel(vt.GetFirstSelected()) >= 1;
end;

procedure TfrmMain.actEditAttributeExecute(Sender: TObject);
begin
  if (isAttrNode(vt.GetFirstSelected())) then
  begin
    with TfrmAttribute.Create(Self, PJson(vt.GetNodeData( vt.GetFirstSelected()))^, getCaseDataFromSelected() ) do
    begin
      try
        if (ShowModal = mrOK) then
        begin
          touchProject(vt.GetFirstSelected(), True);
        end;
      finally
        Free;
      end;
    end;
  end;
end;

procedure TfrmMain.actEditAttributeUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= vt.GetNodeLevel(vt.GetFirstSelected()) >= 2;
end;

procedure TfrmMain.actNewCaseUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(getProjectNodeSelected());
end;
procedure TfrmMain.actRemoveCaseUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= Assigned(getCaseNodeSelected());
end;

procedure TfrmMain.actSaveAsExecute(Sender: TObject);
var
  lJson  : ISuperObject;
  lData  : TJson;
begin


    lData  := PJson(vt.GetNodeData( vt.GetFirstSelected()))^;
    lJson  := prepareJson();
    if (saveProject.Execute) then
    begin
      lData.FPath:=saveProject.FileName;
    end;
    if (lJson.SaveTo(lData.FPath,True) = 0) then
    begin
      raise Exception.Create(SErrCantSave);
    end;
    touchProject(vt.GetFirstSelected(), False);

end;

procedure TfrmMain.actSaveAsUpdate(Sender: TObject);
begin
   TAction(Sender).Enabled:=Assigned(getProjectNodeSelected());
end;

procedure TfrmMain.actSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=Assigned(getProjectNodeSelected());
end;

procedure TfrmMain.actStartDaemonExecute(Sender: TObject);
var
//  lOutput : TStringList;
//  lError  : TStringList;
  lSOSettings : ISuperObject;
  lServicePath : UTF8String;
  l_output : String;
begin
  exit;
  StatusBar1.SimpleText:=SServicePHPStarting;
  StatusBar1.Update;

  lSOSettings:=getSettings;
  if (lSOSettings.S['pathphp']='') then
  begin
    MessageDlg(SErrSettingsPathPHPMissing, mtWarning, [mbOK],0);
    mnuSettingsClick(nil);
    Exit;
  end;

  lServicePath:=ExtractFileDir(ParamStr(0))+'\service\';
  if (not DirectoryExistsUTF8(lServicePath)) then
  begin
    lServicePath:=ExtractFileDir(ParamStr(0))+'\..\..\service\';
    if (not DirectoryExistsUTF8(lServicePath)) then
    begin
      MessageDlg(SErrSettingsPathPHPWrong, mtWarning, [mbOK],0);
      Exit;
    end;
  end;

//  lOutput := TStringList.Create;
//  lError  := TStringList.Create;
  if (not Assigned(FProcessPHP)) then
  begin
    FProcessPHP := TProcess.Create(nil);
  end;
  if (FProcessPHP.Running) then
  begin
    MessageDlg(SErrAlreadyRunning, mtError, [mbOK],0);
    Exit;
  end;
  try
    try
      {$IFDEF LINUX}
      RunCommand('which php', l_output);
      if (l_output <> '') then
      begin
        FProcessPHP.CommandLine := l_output+' -S 127.0.0.1:'+lSOSettings.S['rpcport']+' -t "'+lServicePath+'"';
      end else
      begin
        raise Exception.Create('PHP not found');
      end;
      {$Else}
      FProcessPHP.CommandLine := lSOSettings.S['pathphp']+' -S 127.0.0.1:'+lSOSettings.S['rpcport']+' -t "'+lServicePath+'"';
      {$ENDIF}
      FProcessPHP.Options     := FProcessPHP.Options + [poNoConsole, poUsePipes];
      FProcessPHP.Execute;
    except
      MessageDlg(SErrSettingsPathPHPWrong, mtWarning, [mbOK],0);
    end;
//    lOutput.LoadFromStream( FProcessPHP.Output );
//    lError.LoadFromStream( FProcessPHP.Stderr );
//    ShowMessage(lError.Text);
  finally
//    lOutput.Free;
//    lError.Free;
  end;
end;

procedure TfrmMain.actStartDaemonUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= not Assigned(FProcessPHP) or not FProcessPHP.Running;
end;

procedure TfrmMain.actStartInternalDBExecute(Sender: TObject);
var
//  lOutput : TStringList;
//  lError  : TStringList;
  lSOSettings : ISuperObject;
  lPathData : String;
  l_output : String;
begin
  StatusBar1.SimpleText:=SServiceDBStarting;
  StatusBar1.Update;
  if (not Assigned(FProcessDB)) then
  begin
    FProcessDB := TProcess.Create(Application);
  end;
  if (FProcessDB.Running) then
  begin
    MessageDlg(SErrAlreadyRunning, mtError, [mbOK],0);
    Exit;
  end;

  lSOSettings:=getSettings;
  if (lSOSettings.S['pathmongod']='') then
  begin
    MessageDlg(SErrSettingsPathMongoMissing, mtWarning, [mbOK],0);
    mnuSettingsClick(nil);
    Exit;
  end;

//  lPathData := Common.UserDir + '.giiRBCData';
  lPathData := ExtractFileDir(ParamStr(0))+'\data';

  if (not DirectoryExistsUTF8(lPathData)) then
  begin
    if (not CreateDirUTF8(lPathData)) then
    begin
      MessageDlg(Format(SErrCreatingDir,[lPathData]), mtWarning, [mbOK],0);
      Exit;
    end;
  end;

//  lOutput := TStringList.Create;
//  lError  := TStringList.Create;
  try
    try
      {$IFDEF LINUX}
      RunCommand('which mongod', l_output);
      if (l_output <> '') then
      begin
        FProcessPHP.CommandLine := l_output + ' --smallfiles --dbpath "'+lPathData+'"';
      end else
      begin
        raise Exception.Create('MongoDB not found');
      end;
      {$Else}
      FProcessDB.CommandLine := lSOSettings.S['pathmongod'] + ' --smallfiles --dbpath "'+lPathData+'"';
      {$ENDIF}
      FProcessDB.Options     := FProcessDB.Options + [poNoConsole,poUsePipes];
      FProcessDB.Execute;
    except
      MessageDlg(SErrSettingsPathMongoWrong, mtWarning, [mbOK],0);
    end;
//    lOutput.LoadFromStream( FProcessPHP.Output );
//    lError.LoadFromStream( FProcessPHP.Stderr );
//    ShowMessage(lError.Text);
  finally
//    lOutput.Free;
  end;
end;

procedure TfrmMain.actStopDaemonExecute(Sender: TObject);
begin
  if (Assigned(FProcessPHP)) then
  begin
    if (FProcessPHP.Running) then
    begin
      FProcessPHP.CloseInput;
      FProcessPHP.CloseOutput;
      FProcessPHP.CloseStderr;
      FProcessPHP.Terminate(0);
      FProcessPHP.WaitOnExit;
    end;
    FreeAndNil(FProcessPHP);
  end;
  if (Assigned(FProcessDB)) then
  begin
    if (FProcessDB.Running) then
    begin
      FProcessDB.CloseInput;
      FProcessDB.CloseOutput;
      FProcessDB.CloseStderr;
      FProcessDB.Terminate(0);
      FProcessDB.WaitOnExit;
    end;
    FreeAndNil(FProcessDB);
  end;

end;

procedure TfrmMain.actStopDaemonUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= Assigned(FProcessPHP) and FProcessPHP.Running;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if MessageDlg(SConfirmExit, mtConfirmation, [mbYes, mbNo],0) = mrNo then
  begin
    CanClose:=False;
  end;

  actStopDaemonExecute(Nil);
end;



procedure TfrmMain.actRemoveProjectExecute(Sender: TObject);
var
  lData : TJson;
begin
  if MessageDlg(SConfirmRemove, mtConfirmation, [mbYes, mbNo],0) = mrNo then Exit;

  lData := PJson(vt.GetNodeData( getProjectNodeSelected() ))^;
  if (lData.FPath<>'') then
  begin
    if MessageDlg(Format(SConfirmDeleteFile,[lData.FPath]), mtConfirmation, [mbYes, mbNo],0) = mrYes then
    begin
      if (not DeleteFileUTF8(lData.FPath)) then
      begin
         raise Exception.Create(SErrCantDelete);
      end;
    end;
  end;
  vt.DeleteNode( getProjectNodeSelected() );

end;

procedure TfrmMain.actRemoveProjectUpdate(Sender: TObject);
begin
   TAction(Sender).Enabled := Assigned(getProjectNodeSelected());
end;

procedure TfrmMain.actRestartDaemonExecute(Sender: TObject);
begin
  actStopDaemonExecute(nil);
  actStartDaemonExecute(nil);
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
var
  lJson  : ISuperObject;
  lData  : PJson;
begin
  lData := vt.GetNodeData(getProjectNodeSelected());
  lJson := prepareJson();
  if (lData^.FPath='') then
  begin
    if (saveProject.Execute) then
    begin
      lData^.FPath:=saveProject.FileName;
    end;
  end;
  if (lJson.SaveTo(lData^.FPath,True) = 0) then
  begin
    raise Exception.Create(SErrCantSave);
  end;
  touchProject(vt.GetFirstSelected(), False);

end;



procedure TfrmMain.actNewProjectExecute(Sender: TObject);
begin
  with TfrmProject.Create(Self, TJson.Create ) do
  begin
    try
      if (ShowModal = mrOK) then
      begin
        touchProject( newProject(FJson), True);
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.actOpenProjectExecute(Sender: TObject);
begin
  if openDlg.Execute then
  begin
    doOpenFile(openDlg.FileName);
  end;
end;

procedure TfrmMain.actRemoveAttrExecute(Sender: TObject);
begin
  if (not isAttrNode(vt.GetFirstSelected())) then Exit;

  if MessageDlg(SConfirmRemove, mtConfirmation, [mbYes, mbNo],0) = mrNo then Exit;

  //Avisar ao projeto sobre alteração
  touchProject(vt.GetFirstSelected(),True);

  vt.DeleteNode( vt.GetFirstSelected() );
end;

procedure TfrmMain.actRemoveAttrUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= vt.GetNodeLevel(vt.GetFirstSelected()) >= 2;
end;

procedure TfrmMain.doOpenFile(aPath : String);
var
  lData : TJson;
  lJson : ISuperObject;
begin
  if (not FileExistsUTF8(aPath)) then
  begin
    MessageDlg(Format(SErrOpening,[aPath]), mtWarning, [mbOK],0);
    Exit;
  end;
  lJson := TSuperObject.ParseFile(aPath, False);
  if (Assigned(lJson)) then
  begin
    lData := TJson.Create(lJson);
    lData.FPath:=aPath;
    newProject(lData);
  end;
end;

procedure TfrmMain.actRemoveCaseExecute(Sender: TObject);
begin
  if (isProjectNode(vt.GetFirstSelected())) then Exit;

  if MessageDlg(SConfirmRemove, mtConfirmation, [mbYes, mbNo],0) = mrNo then Exit;

  //Avisar ao projeto sobre alteração
  touchProject(vt.GetFirstSelected(),True);

  vt.DeleteNode( getCaseNodeSelected() );

end;



end.

