Netzinstllation von Kalypso

wird möglichst aktuell gehalten

Installation:
Es reicht den Link 'kalypso.lnk' zu kopieren (z.B. aufs Desktop) und anzupassen.
Anpassung des Links (über Rechtsklick - Eigenschaften):
lediglich der lokale Arbeitsbereich (eigene Datenablage muss) noch richtig konfiguriert werden. Dazu muss %YOURWORKSPACE% unter 'Ziel' durch ein (beliebiges) lokales Verzeichnis ersetzt werden.
z.B.: Q:\lhz04085\work\kalypso_deploy\kalypso\eclipse.exe -data C:\Programme\Kalypso\workspace -dev lib -consoleLog -vmargs -Xmx512m

Alle eigenen Arbeitsdateien werden jetzt in diesem Verzeichnis abgelegt.
