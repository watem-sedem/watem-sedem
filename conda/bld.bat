
c:\lazarus\lazbuild.exe cn_ws\cn_ws.lpr
:: strip debug symbols
c:\lazarus\fpc\3.2.0\bin\x86_64-win64\strip.exe cn_ws\cn_ws.exe
copy cn_ws\cn_ws.exe %LIBRARY_BIN%
