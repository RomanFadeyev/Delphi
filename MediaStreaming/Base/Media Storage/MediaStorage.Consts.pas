unit MediaStorage.Consts;

interface

const
  //названия атрибутов ---------------------------------------------------------
  MediaSignature = $56444848;//'VDHH';

type
  TrsEventType = cardinal;

const
  EventMotionAlarm : TrsEventType = $4D4E414D; //'MNAM;' 1296974157
  EventOnline      : TrsEventType = $4F4E4C4E; //'ONLN;' 1330531406

//const
  //ConfigurationFileId = 'AdmConfig ({28239C53-7EA7-4CFC-A2DB-AB96FB76BF88})';

implementation
  uses SysUtils,uSysUtilsDB;



end.
