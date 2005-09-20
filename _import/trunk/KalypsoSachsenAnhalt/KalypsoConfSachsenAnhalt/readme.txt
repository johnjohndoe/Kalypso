--------------------------------------------------------------------------
Kalypso Konfigurations und Deployment Projekt     Sachsen-Anhalt/Magdeburg
--------------------------------------------------------------------------

KalypsoConfSachsenAnhalt
 |-- \deploy                   Zwischenstation: da werden die temp-Dateien f�r's Deploy hinkopiert
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
* Die default targets der einzelene build-Dateien sind f�r ein Kundendeploy ausgelegt
  Man soll ber�cksichtigen dass die Skripts auch f�r den internen Gebrauch (Deploy und Test
  bei der Hydroinformatik/BCE) gedacht sind. Deswegen tauchen beide Arten von Targets.
* die zwischenstation \deploy dient als tempor�re Ablage aller Deploy-Dateien.
* Sehr Wichtig: der Stand all dieser Dateien muss der Stand der LHZ 
  wiederspiegeln, d.h. man sollte keine TUHH oder BCE abh�ngige �nderungen 
  vornehmen, weil sonst ein Deploy vor Ort zu Inkonsistenz f�hren konnte. 

Dienste
===========
Beschreibung:
- Erzeugt eine WAR pro Dienst (benutzt im Hintergrund build.xml von jedem Dienst)


Daten und Konfiguration
===========================
Alle Dateien liegen im Webdav-Verzeichnis. Beim Deploy werden allerdings die Dateien
direkt hinkopiert (nicht �ber Webdav).

Vor dem echten Deploy:
- die Modelle werden aus dem CVS rausgecheckt
   * KalypsoBode (in Bode umgenannt)
   * KalypsoSaale (in Saale umgenannt)
- die Modelle werden aufger�umt:
   * '.prognose' und 'Rechenvarianten' Verzeichnisse leeren
   * nach '.calculation'-Dateien suchen und entfernen
   * nach 'CVS'-Verzeichnisse suchen und entfernen


========================================================================
 NOTA:
========================================================================
* Servername: der Name der Kalypso Server muss an verschiedene Stellen
  ber�cksichtigt werden. Unter anderem in:
  - kalypso-client.ini
  - %CATALINA_HOME%/conf/catalina.policy
  
* Freigaben: folgende Freigaben m�ssen z.Z. (leider) definiert sein:
  - KalypsoModelle$ --> data/mirrored/prognose
  Grund: es liegt an die ModellNature die die Dateien z.Z. noch �ber File-Copy
  synchronisiert.
