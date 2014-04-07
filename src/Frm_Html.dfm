object FrmHtml: TFrmHtml
  Left = 0
  Top = 0
  Width = 443
  Height = 277
  Align = alClient
  TabOrder = 0
  object html_Viewer: THTMLViewer
    Left = 0
    Top = 0
    Width = 443
    Height = 277
    OnHotSpotCovered = EOnHotSpotCovered
    OnImageRequest = EOnImage
    TabOrder = 0
    Align = alClient
    ShowHint = True
    BorderStyle = htNone
    HistoryMaxCount = 0
    DefFontName = 'Times New Roman'
    DefPreFontName = 'Courier New'
    NoSelect = True
    CharSet = DEFAULT_CHARSET
    PrintMarginLeft = 2.000000000000000000
    PrintMarginRight = 2.000000000000000000
    PrintMarginTop = 2.000000000000000000
    PrintMarginBottom = 2.000000000000000000
    PrintScale = 1.000000000000000000
    OnMouseMove = EOnHint
    OnhtStreamRequest = EOnStyle
  end
  object tmr_Hint: TTimer
    Enabled = False
    Interval = 200
    OnTimer = EOnTimer
    Left = 3
    Top = 63
  end
end
