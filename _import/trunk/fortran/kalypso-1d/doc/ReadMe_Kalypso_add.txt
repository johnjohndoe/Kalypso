----------------------------------
Kalypso_add.cfg:
----------------------------------
----------------------------------

Über diese zusätzliche Konfigurationsdatei im /PROF/ Ordner ("Kalypso_add.cfg") 
können weitere Einstellungen vorgenommen werden, die so in der WspWin-Oberfläche
nicht zur Verfügung stehen. Diese Datei wird nur gelesen, wenn sie existiert.


So kann man:
++++++++++++++
Die Rechenvariante mit Konstantem Reibungsgefälle starten,
- BERECHNUNGSMODUS = REIB_KONST
- VERZOEGERUNGSVERLUST = NON    (nur in Kombi mit Konstantem Reibungsgefälle zulässig!!)

  Achtung: Wenn kein Bedarf bitte die "!" vor den Einstellungen lassen,
  sonst werden die Einstellungen aus der Oberfläche überschrieben.


mit extremen Rauheiten nach Aguirre-Fuentes rechnen. 
- USE_EXTREM_ROUGH = true


mit einer Internen KM-Berechnung arbeiten:
- CALC_KM_INTERN = true
