----------------------------------
Kalypso_add.cfg:
----------------------------------
----------------------------------

�ber diese zus�tzliche Konfigurationsdatei im /PROF/ Ordner ("Kalypso_add.cfg") 
k�nnen weitere Einstellungen vorgenommen werden, die so in der WspWin-Oberfl�che
nicht zur Verf�gung stehen. Diese Datei wird nur gelesen, wenn sie existiert.


So kann man:
++++++++++++++
Die Rechenvariante mit Konstantem Reibungsgef�lle starten,
- BERECHNUNGSMODUS = REIB_KONST
- VERZOEGERUNGSVERLUST = NON    (nur in Kombi mit Konstantem Reibungsgef�lle zul�ssig!!)

  Achtung: Wenn kein Bedarf bitte die "!" vor den Einstellungen lassen,
  sonst werden die Einstellungen aus der Oberfl�che �berschrieben.


mit extremen Rauheiten nach Aguirre-Fuentes rechnen. 
- USE_EXTREM_ROUGH = true


mit einer Internen KM-Berechnung arbeiten:
- CALC_KM_INTERN = true
