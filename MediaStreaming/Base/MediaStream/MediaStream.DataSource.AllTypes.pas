unit MediaStream.DataSource.AllTypes;

interface
  uses  MediaStream.DataSource.Base,
        MediaStream.DataSource.File_,
{$IFNDEF DISABLE_HH}
        MediaStream.DataSource.HH,
{$ENDIF}
{$IFNDEF DISABLE_HIKVISION}
        MediaStream.DataSource.HikVision,
{$ENDIF}
        MediaStream.DataSource.Ms3s,
        MediaStream.DataSource.Proxy,
        MediaStream.DataSource.RTSP,
        MediaStream.DataSource.Stub,
        MediaStream.DataSource.WaveIn;



implementation

end.

