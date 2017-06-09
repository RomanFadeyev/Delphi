{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Источники данных, использующее для историю котировок
            (IStockDataStorage)

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.EHC.StockDataSource;
{$I Compiler.inc}

interface
  uses SysUtils,Classes,DB, AppEvnts, Controls, Serialization, FC.Definitions,
       FC.StockData.StockDataSource,StockChart.Definitions,FC.Singletons,
       FC.StockData.HC.StockDataSource;

type
  //---------------------------------------------------------------------------
  //Реализация DataSource для History Center

  TStockDataSource_EHC=class(TStockDataSource_HistoryCenter)
  end;

implementation
  uses Math,BaseUtils, SystemService, DateUtils, FC.DataUtils;


{ TStockDataSource_EHC }

end.
