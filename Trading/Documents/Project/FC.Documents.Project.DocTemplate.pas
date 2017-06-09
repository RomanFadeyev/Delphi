{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Шаблон документа "проект"

 History:
-----------------------------------------------------------------------------}

unit FC.Documents.Project.DocTemplate;
{$I Compiler.inc}

interface
  uses Classes, SysUtils, ActnList,
       Documents.Definitions, Documents.Obj,
       FC.Definitions,FC.Documents.Project.Document,
       FC.fmUIDataStorage;

type
  TProjectDocTemplate = class(Documents.Obj.TDocTemplate)
  private
    procedure OnCreateProjectExecute(aAction:TCustomAction);
    procedure OnOpenProjectExecute(aAction:TCustomAction);
  protected
    function OnCreateDocumentInstance: IDocument; override;
  public
    constructor Create;
    destructor Destroy; override;

    function OpenDocument(const aDocumentPath: string): IDocument; override;    
  end;

implementation
  uses BaseUtils, FC.Messages;

{ TProjectDocTemplate }

constructor TProjectDocTemplate.Create;
begin
  inherited;
  ActionTarget.RegisterAction(UIDataStorage.acFileNewProject,nil,OnCreateProjectExecute);
  ActionTarget.RegisterAction(UIDataStorage.acFileOpenProject,nil,OnOpenProjectExecute);

  DocTypeList.AddDocType(TDocType.Create('Project files','*.fcproj',IStockProject));
end;

destructor TProjectDocTemplate.Destroy;
begin
  inherited;
end;

function TProjectDocTemplate.OnCreateDocumentInstance: IDocument;
begin
  result:=TProjectDocument.Create(self);
end;

procedure TProjectDocTemplate.OnCreateProjectExecute(aAction: TCustomAction);
begin
  //Если есть открытый проект - закрываем
  if DocumentCount>0 then
  begin
    if not GetDocument(0).Close then
      abort;
  end;

  try
    NewDocument;
  except
    on E:Exception do
      raise EBase.Create(SCannotCreateNewDocument,E);
  end;
end;

procedure TProjectDocTemplate.OnOpenProjectExecute(aAction: TCustomAction);
begin
  try
    OpenDocument('');
  except
    on E:Exception do
      raise EBase.Create(SCannotOpenDocument,E);
  end;
end;

function TProjectDocTemplate.OpenDocument(const aDocumentPath: string): IDocument;
begin
  //Если есть открытый проект - закрываем
  if DocumentCount>0 then
  begin
    if not GetDocument(0).Close then
      exit;
  end;

  result:=inherited OpenDocument(aDocumentPath);
end;

initialization
  DocManager.AddDocTemplate(TProjectDocTemplate.Create);
end.
