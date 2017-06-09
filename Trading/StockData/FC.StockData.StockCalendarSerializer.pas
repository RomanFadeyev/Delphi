{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Календарь событий

 History:
-----------------------------------------------------------------------------}
unit FC.StockData.StockCalendarSerializer;
{$I Compiler.inc}

interface
  uses Classes, Windows, SysUtils, Controls, Contnrs,Collections.Map,
       StockChart.Definitions,
       FC.Definitions, FC.Definitions.EconomicMacroIndicators;

type
  TStockFilterItemData  = record
    ID: integer;
    DateTime: TDateTime;
    Country: string;
    Class_: integer;
    Indicator: string;
    Priority: string;
    Period: string;
    PreviousValue: string;
    ForecastValue: string;
    FactValue: string;
    PFChangeType: TSCCalendarChangeType;
    FFChangeType: TSCCalendarChangeType;
    Source: string;
    Notes: string;
  end;

  TStockFilterDelegate = function (var aItem: TStockFilterItemData): boolean of object;

  TStockCalendarSerializer = class
    procedure SaveToFile(const aData: IStockCalendar; const aFileName: string);
    procedure SaveToStream(const aData: IStockCalendar; aStream: TStream);
    procedure LoadFromFile(const aData: IStockCalendarWriteable; const aFileName:string;
                           const aCheckItemsOrdered: boolean=false; const aFilterDelegate: TStockFilterDelegate=nil);
  end;


implementation
  uses BaseUtils, SystemService,DateUtils, Application.Definitions,FC.DataUtils;


function ApplyPattern(const aText,aPatterm: string): boolean;
var
  j,n: integer;
begin
  result:=false;
  n:=Length(aPatterm);

  j:=StrIPos(aPatterm,aText);
  if (j>0) then
    if ((j=1) or (not IsCharAlpha(aText[j-1]))) and
       ((j+n>=Length(aText)) or  (not IsCharAlpha(aText[j+n]))) then
      result:=true;
end;

function ApplyPatterns(const aText,aPatterns: string): boolean;
var
  s,sleft: string;
begin
  s:=aPatterns;
  repeat
    SplitString(s,SLeft,s,'|');
    result:=ApplyPattern(aText,sleft);
    if result then
      break;
  until s='';
end;

{ TStockCalendarSerializer }

procedure TStockCalendarSerializer.LoadFromFile(const aData: IStockCalendarWriteable; const aFileName:string;
                                                const aCheckItemsOrdered: boolean=false; const aFilterDelegate: TStockFilterDelegate=nil);
var
  aFileData: TStrings;
  itc: TStockMacroIndicatorCountry;
  it: TStockMacroIndicator;
  its: TStockMacroIndicatorSet;
  i: integer;
  s,s1,aCountry,aIndicator,s4,s5,s6,s7,s8,s9,s10,s11,s12: string;
  x: TStockMacroIndicator;
  aPF,aFF: TSCCalendarChangeType;
  aDateTime, aLastDateTime: TDateTime;
  aCanAdd : boolean;
  aFilterData :TStockFilterItemData;
begin
  aFileData:=TStringList.Create;
  aLastDateTime:=0;
  try
    aFileData.LoadFromFile(aFileName);
    for i := 0 to aFileData.Count - 1 do
    begin
      s:=aFileData[i];
      SplitString(s,s1,s,#9); //Date
      SplitString(s,aCountry,s,#9); //Country
      SplitString(s,aIndicator,s,#9); //Indicator
      SplitString(s,s4,s,#9); //Priority
      SplitString(s,s5,s,#9); //Period
      SplitString(s,s6,s,#9); //Previous
      SplitString(s,s7,s,#9); //Forecast
      SplitString(s,s8,s,#9); //Fact
      SplitString(s,s9,s,#9); //PFTrend
      SplitString(s,s10,s,#9); //FFTrend
      SplitString(s,s11,s,#9); //Source
      SplitString(s,s12,s,#9); //Notes

      aPF:=cttUnknown;
      aFF:=cttUnknown;

      x:=cicOther;
      if aIndicator='' then
        raise EStockError.CreateFmt('Empty economic indicator name in line %d',[i+1]);

      //Country
      for itc := low(TStockMacroIndicatorCountry) to high(TStockMacroIndicatorCountry) do
      begin
        if ApplyPatterns(aCountry,StockMacroIndicatorCountryNames[itc].Patterns) then
        begin
          its:=GetAllCountryIndicators(itc);
          for it := low(TStockMacroIndicator) to high(TStockMacroIndicator) do
            if it in its then
              if ApplyPattern(aIndicator,StockMacroIndicatorNames[it].Name) then
              begin
                x:=it;
                break;                                                              
              end;
          break;
        end;                                                            
      end;


      if SameText(s9,'BETTER') then
        aPF:=cttBetter
      else if SameText(s9,'WORSE') then
        aPF:=cttWorse
      else if SameText(s9,'SAME') then
        aPF:=cttEqual
      else if SameText(s9,'EQUAL') then                              
        aPF:=cttEqual;

      if SameText(s10,'BETTER') then
        aFF:=cttBetter
      else if SameText(s10,'WORSE') then
        aFF:=cttWorse
      else if SameText(s10,'SAME') then
        aFF:=cttEqual
      else if SameText(s10,'EQUAL') then
        aFF:=cttEqual;

      aDateTime:=StrToDateTimeWithTimeZone(s1);
      if aCheckItemsOrdered then
        if CompareDateTime(aLastDateTime,aDateTime)>0 then
          raise EStockError.CreateFmt('Items are not ordered by time. Error found in line %d',[i+1]);
      aLastDateTime:=aDateTime;

      if Assigned(aFilterDelegate) then
      begin
        aFilterData.ID:=-1;
        aFilterData.DateTime:=aDateTime;
        aFilterData.Country:=aCountry;
        aFilterData.Class_:=integer(x);
        aFilterData.Indicator:=aIndicator;
        aFilterData.Priority:=s4;
        aFilterData.Period:=s5;
        aFilterData.PreviousValue:=s6;
        aFilterData.ForecastValue:=s7;
        aFilterData.FactValue:=s8;
        aFilterData.PFChangeType:=aPF;
        aFilterData.FFChangeType:=aFF;
        aFilterData.Source:=s11;
        aFilterData.Notes:=s12;
        aCanAdd:=aFilterDelegate(aFilterData);

        if aCanAdd then
          aData.Add(aFilterData.ID,
                    aFilterData.DateTime,
                    aFilterData.Country,
                    aFilterData.Class_,
                    aFilterData.Indicator,
                    aFilterData.Priority,
                    aFilterData.Period,
                    aFilterData.PreviousValue,
                    aFilterData.ForecastValue,
                    aFilterData.FactValue,
                    aFilterData.PFChangeType,
                    aFilterData.FFChangeType,
                    aFilterData.Source,
                    aFilterData.Notes);
        
      end
      else begin
        aData.Add(-1, aDateTime,aCountry,integer(x),aIndicator,s4,s5,s6,s7,s8,aPF,aFF,s11,s12);
      end;
    end;
  finally
    aFileData.Free;
  end;
end;

procedure TStockCalendarSerializer.SaveToStream(const aData: IStockCalendar; aStream: TStream);
var
  i: integer;
  s: string;
  aFileData: TStrings;
begin
  TWaitCursor.SetUntilIdle;

  aFileData:=TStringList.Create;
  try
    for i := 0 to aData.Count-1 do
    begin
      with aData.GetItem(i) do
        s:=DateTimeToStrWithTimeZone(GetDateTime)+#9+ //Date
          GetCountry+#9+ //Country
          GetIndicator+#9+ //Indicator
          GetPriority+#9+  //Priority
          GetPeriod+#9+    //Period
          GetPreviousValue+#9+ //Previous
          GetForecastValue+#9+ //Forecast
          GetFactValue+#9+//Fact
          CalendarChangeTypeNames[GetPFChangeType]+#9+ //PFTrend
          CalendarChangeTypeNames[GetFFChangeType]+#9+//FFTrend
          GetSource+#9+//Source
          GetNotes; //Notes

      aFileData.Add(s);
    end;

    aFileData.SaveToStream(aStream);
  finally
    aFileData.Free;
  end;
end;

procedure TStockCalendarSerializer.SaveToFile(const aData: IStockCalendar; const aFileName: string);
var
  aStream: TStream;
begin
  aStream:=TFileStream.Create(aFileName,fmCreate);
  try
    SaveToStream(aData,aStream);
  finally
    aStream.Free;
  end;
end;

end.
