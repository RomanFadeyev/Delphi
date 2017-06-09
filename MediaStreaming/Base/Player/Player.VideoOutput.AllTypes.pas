unit Player.VideoOutput.AllTypes;


interface
  uses Windows,Messages, SysUtils,Classes,Player.VideoOutput.Base,MediaProcessing.Definitions;


implementation
  uses {$IFNDEF DISABLE_PLAYER_HH}Player.VideoOutput.HH, {$ENDIF}
       Player.VideoOutput.BMP,
       Player.VideoOutput.YUV420,
       Player.VideoOutput.MJPEG,
       Player.VideoOutput.Mpeg4, //Частный случай AvcAny
       Player.VideoOutput.H264,  //Частный случай AvcAny
       Player.VideoOutput.AvcAny,
       {$IFNDEF DISABLE_PLAYER_VFW}Player.VideoOutput.VFW, {$ENDIF}
       Math,uTrace;


initialization
  //Важен порядок регистрации!!
  PlayerVideoOutputDecoderFactory.UnregisterDecoderClass(TVideoOutputDecoder_AvcAny);
  PlayerVideoOutputDecoderFactory.UnregisterDecoderClass(TVideoOutputDecoder_VFW);

  PlayerVideoOutputDecoderFactory.RegisterDecoderClass(TVideoOutputDecoder_AvcAny);
  PlayerVideoOutputDecoderFactory.RegisterDecoderClass(TVideoOutputDecoder_VFW);
end.
