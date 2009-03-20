REM dr_wavos starten
bin\dr_wavos_kalypso.exe elbe\bin\objekte_elbe.dat < input.par > output.log

REM Ergebnisse zwischenspeichern
xcopy "elbe\vorher" "elbe\vorher_save" /y/i

REM shiftvor starten
bin\shiftvor.exe elbe\bin\objekte_shiftvor.dat < shiftvor.par > shiftvor.log
