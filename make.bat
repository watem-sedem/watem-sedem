for /f "tokens=*" %%A in ('git describe --abbrev^=4 --always --tags') do set var=%%A
echo '%var%' > watem_sedem\version.inc
c:\lazarus\lazbuild.exe watem_sedem\cn_ws.lpr
:: strip debug symbols
c:\lazarus\fpc\3.2.0\bin\x86_64-win64\strip.exe cn_ws\cn_ws.exe
echo 'DEVELOP' > watem_sedem\version.inc
