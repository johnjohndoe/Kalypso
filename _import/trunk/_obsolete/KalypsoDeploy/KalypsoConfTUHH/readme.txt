--------------------------------------------------------------------------
Kalypso Konfigurations und Deployment Projekt              Sachsen/Dresden
--------------------------------------------------------------------------

KalypsoConfSachsen
 |--  \deploy                    da werden die deployed Dateien hinkopiert
 |--  \deployable-client
   |--  \client                  beinhaltet die RCP
   |--  build.xml                deploy von feature und plugins
 |--  \deployable-data
   |-- \data                     data Verzeichnis auf dem Server
   |-- build.xml                 deploy vom data-Verzeichnis + live checkout von den Modellen: Spree und WeisseElster
 |--  \deployable-services
   |-- \echte-psicompact         beinhaltet die echte PSICompact.jar
   |-- \KalypsoConf              Der KalypsoConf pseudo-Service
   |-- build.xml                 deploy von allen services
   |-- build-KalypsoConf.xml     sub-build file, wird von build.xml benutzt
 |-- \doc
   |-- tecdoc
 |-- \tomcatStuff                                  

* Die Variable ${workspace} muss Ant bekannt sein (Siehe Eclipse Preferences,
  Ant, Runtime)
* Die default targets der einzelene build-Dateien sind f�r 
  ein Kundendeploy ausgelegt
* Alle Deploy-Dateien werden in das 'deploy'-Verzeichnis unter 
  KalypsoConfSachsen erzeugt (ist CVS-ignorisiert)
* Wichtig: wenn �nderungen in Dateien des data-Verzeichnis gemacht 
  werden m�ssen, dann zuerst in deployable-data/data/...  und 
  anschliessend auf dem Server deployen.
* Wichtig: wenn �nderungen in Dateien des KalypsoConf-Verzeichnis 
  gemacht werden m�ssen, dann zuerst in deployable-services/KalypsoConf/... 
  und anschliessend auf dem Server deployen.
* Sehr Wichtig: der Stand all dieser Dateien muss der Stand der LfUG 
  wiederspiegeln, d.h. man sollte keine TUHH oder BCE abh�ngige �nderungen 
  vornehmen, weil sonst ein Deploy vor Ort zu Inkonsistenz f�hren konnte. 
  Beispiel: der Servername ist LFUG-KV-01 und ist in mehrere Dateien zu 
  finden. Also nie mit PC242 oder was auch immer �ndern.

Dienste
===========
- build.xml benutzen (in KalypsoConfSachsen/deployable-services)

Beschreibung:
- Erzeugt eine WAR pro Dienst (benutzt build.xml von jedem Dienst)
- Ersetzt die PSICompact.jar mit der echte Version

NB: KalypsoConf wird auch als Dienst ber�cksichtigt, liegt aber direkt in
das KalypsoConfSachsen Projekt (im Verzeichnis: deployable-services/KalypsoConf)


Daten und Konfiguration
===========================

  data
    |-- conf
    |-- mirrored
      |-- prognose
     
- Direkte Kopie von data/conf, data/mirrored
- Modelle werden aus dem CVS rausgecheckt
   * KalypsoSpree (in Spree umgenannt)
   * KalypsoWeisseElster (in WeisseElster umgenannt)
- Modelle werden aufger�umt:
   * '.prognose' und 'Rechenvarianten' Verzeichnisse leeren
   * nach '.calculation'-Dateien suchen und entfernen
   * nach 'CVS'-Verzeichnisse suchen und entfernen


========================================================================
 NOTA:
========================================================================
* Servername: der Name der Kalypso Server muss an verschiedene Stellen
  ber�cksichtigt werden. Unter anderem in:
  - KalypsoConf/kalypso-client.ini
  - %CATALINA_HOME%/conf/catalina.policy
  
* Freigaben: folgende Freigaben m�ssen definiert sein:
  - kalypso$
  - KalypsoModelle$ --> data/mirrored/prognose
