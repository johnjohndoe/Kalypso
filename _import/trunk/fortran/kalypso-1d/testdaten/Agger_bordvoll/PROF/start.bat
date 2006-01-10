@echo Bitte warten...
@echo Berechnung laeuft.
if exist C:\WspWin\f77l3.eer copy C:\WspWin\f77l3.eer C:\WspWin\DemoBCE\prof\f77l3.eer
if exist C:\WspWin\lf90.eer copy C:\WspWin\lf90.eer C:\WspWin\DemoBCE\prof\lf90.eer
if exist C:\WspWin\f77l3.eer copy C:\WspWin\f77l3.eer C:\WspWin\DemoBCE\dath\f77l3.eer
if exist C:\WspWin\lf90.eer copy C:\WspWin\lf90.eer C:\WspWin\DemoBCE\dath\lf90.eer
pause

C:\WspWin\wsp272.exe < C:\WspWin\DemoBCE\prof\bat.001>C:\WspWin\DemoBCE\dath\error2.log
pause

if exist C:\WspWin\DemoBCE\prof\bat.001 del C:\WspWin\DemoBCE\prof\bat.001>NUL
if exist C:\WspWin\DemoBCE\prof\Agger.001 del C:\WspWin\DemoBCE\prof\Agger.001>NUL
if exist C:\WspWin\DemoBCE\prof\qwert.001 del C:\WspWin\DemoBCE\prof\qwert.001>NUL
if exist C:\WspWin\DemoBCE\prof\psiver.001 del C:\WspWin\DemoBCE\prof\psiver.001>NUL
copy C:\WspWin\DemoBCE\\dath\Agge C:\WspWin\DemoBCE\\dath\Agtb0001.003
copy C:\WspWin\DemoBCE\\dath\Agger.qb1 C:\WspWin\DemoBCE\\dath\Agwl0001.003
if not exist C:\WspWin\DemoBCE\\dath\Agger.qb1 del C:\WspWin\DemoBCE\\dath\Agwl0001.003>NUL
del C:\WspWin\DemoBCE\dath\*.tab
del C:\WspWin\DemoBCE\dath\*.wsl
copy C:\WspWin\DemoBCE\prof\start.bat C:\WspWin\DemoBCE\prof\start$.bat
if exist C:\WspWin\DemoBCE\prof\start$.bat del C:\WspWin\DemoBCE\prof\start$.bat
@echo on
@echo SCHLIESSEN SIE JETZT DIE DOS-BOX!
pause

