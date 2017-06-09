{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Сериализатор документа "проект"

 History:
-----------------------------------------------------------------------------}

unit FC.Documents.Project.Serialization;
{$I Compiler.inc}

interface
  uses Classes,SysUtils,Serialization,Application.Definitions;

type
  TProjectDocSerialize = class (TSerialize)
  protected
    function HeaderSignature: string; override;
    function HeaderVersion: string; override;
  end;

implementation
  uses BaseUtils;
{ TProjectDocSerialize }

function TProjectDocSerialize.HeaderVersion: string;
begin
  result:=Workspace.ProductName+' '+'Project File';
end;

function TProjectDocSerialize.HeaderSignature: string;
var
  aList: TStringList;
begin
  aList:=TStringList.Create;
  //Берем ProductVersion кроме Private Build
  try
    SplitString(Workspace.ProductVersion,aList,'.');
    result:=aList[0]+'.'+aList[1]+'.'+aList[2]+'.0';
  finally
    aList.Free;
  end;
end;

end.
