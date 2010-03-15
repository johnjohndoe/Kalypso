========================================================================
       STATISCHE BIBLIOTHEK : CommonMfc
========================================================================


Diese statische Bibliothek enthält eine Reihe von Hilfsklassen als Erweiterung zu MFC.

Alle Komponenten, die diese Bibiliothek benutzen wollen müssen

- überall wo benötigt die Datei commonMfc.h includieren
- in ihren Resourcen die Datei commonMfc.rc includieren 
	( und den Bereich 0x4000 - 0x4fff in ihren Resourcen-IDs freihalten )