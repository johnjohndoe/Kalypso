	Die statische Bibliothek wspprj.lib
-----------------------------------
zu beachtendes:

1)
Alle von dieser Bibliothek benutzten Resourcen befinden sich in der Datei commonMfc.rc ( geh�rt zur commonMfc.lib )  -> Module die die wspprj.lib benutzen m�ssen diese Resourcendatei mit in die eigene .rc includieren.
Es wurde alles in die commonMfc.rc gepackt, weil 
- sonst die meissten Module sowohl die commonMfc.rc UND die wspprj.rc includieren m�ssten; 
- der Programmierer bei einer Datei mehr beachten m�sste, keine IDs doppelt zu vergeben
- der VC++ ResourcenEditor mit einem Include schon nicht richtig zurechtkommt
