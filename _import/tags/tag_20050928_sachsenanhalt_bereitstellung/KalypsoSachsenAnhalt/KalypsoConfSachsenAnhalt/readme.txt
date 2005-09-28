--------------------------------------------------------------------------
Kalypso Konfigurations und Deployment Projekt     Sachsen-Anhalt/Magdeburg
--------------------------------------------------------------------------

KalypsoConfSachsenAnhalt
 |-- \deploy                   Zwischenstation: da werden die temp-Dateien für's Deploy hinkopiert
 |-- \deployable-server
   |-- \webdav              	Der KalypsoConf pseudo-Service
     |-- data					z.Z. leer
     |-- schemata
     |-- srvconf
     |-- vorhersageconf
     |-- kalypso-client.ini
     |-- schemaCatalog.ini
   |-- build.xml                 deploy von allen services
   |-- cvs-co.bat
   |-- deploy-properties
   |-- ssh8888.bat
 |-- \deployable-tomcatStuff     
   |-- bin
   |-- common
   |-- conf
   |-- TomcatService.reg
   |-- build.xml
 |-- \doc
   |-- tecdoc


* Die Variable ${workspace} muss Ant bekannt sein (Siehe Eclipse Preferences, Ant, Runtime)
  - sie muss den Wert ${workspace_loc} haben wenn der Deploy aus Eclipse stattfindet
  - sie muss den konkreten Wert (da wo der Workspace im Filesystem liegt) haben, im Fall 
    eines Deploy aus der Kommandozeile
* Die default targets der einzelene build-Dateien sind für ein Kundendeploy ausgelegt
  Man soll berücksichtigen dass die Skripts auch für den internen Gebrauch (Deploy und Test
  bei der Hydroinformatik/BCE) gedacht sind. Deswegen tauchen beide Arten von Targets.
* die zwischenstation \deploy dient als temporäre Ablage aller Deploy-Dateien.
* Sehr Wichtig: der Stand all dieser Dateien muss der Stand der LHZ 
  wiederspiegeln, d.h. man sollte keine TUHH oder BCE abhängige Änderungen 
  vornehmen, weil sonst ein Deploy vor Ort zu Inkonsistenz führen konnte. 

Dienste
===========
Beschreibung:
- Erzeugt eine WAR pro Dienst (benutzt im Hintergrund build.xml von jedem Dienst)


Daten und Konfiguration
===========================
Alle Dateien liegen im Webdav-Verzeichnis. Beim Deploy werden allerdings die Dateien
direkt hinkopiert (nicht über Webdav).

Vor dem echten Deploy:
- die Modelle werden aus dem CVS rausgecheckt
   * KalypsoBode (in Bode umgenannt)
   * KalypsoSaale (in Saale umgenannt)
- die Modelle werden aufgeräumt:
   * '.prognose' und 'Rechenvarianten' Verzeichnisse leeren
   * nach '.calculation'-Dateien suchen und entfernen
   * nach 'CVS'-Verzeichnisse suchen und entfernen


========================================================================
 NOTA:
========================================================================
* Servername: der Name der Kalypso Server muss an verschiedene Stellen
  berücksichtigt werden. Unter anderem in:
  - kalypso-client.ini
  - %CATALINA_HOME%/conf/catalina.policy
  
* Freigaben: folgende Freigaben müssen z.Z. (leider) definiert sein:
  - KalypsoModelle$ --> data/mirrored/prognose
  Grund: es liegt an die ModellNature die die Dateien z.Z. noch über File-Copy
  synchronisiert.
