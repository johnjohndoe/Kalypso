Rechenkern aktualisieren
========================
- neue Version zusätzlich hinzufügen: Kalypso-1D_x_y_z_w.exe (hier: Version x_y_z_w)
- in der Enumeration org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation.ExeVersion die Versionummer als zusätzliche Konstante in einführen
- in der Schema-Datei /KalypsoModelWspmTuhhSchema/src/org/kalypso/model/wspm/tuhh/schema/schemata/wspmTuhhSteadyState.xsd im Typ WaterlevelParameterType die neue Version eintragen
- die Version der Polynom-Exe ebenfalls hochzählen (notfalls die letzte einfach kopieren mit neuer Nummer); die Versionenummer der Polynom.exe und der Kalypso-1D.exe gehen in Kalypso Hand-in-Hand.