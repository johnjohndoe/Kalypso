 KalypsoBuild - Zentrales Buildsystem f�r die Kalypso-Projekte
---------------------------------------------------------------

Soweit m�glich sollte jedes Source-Projekt �ber eine build.xml-Datei
verf�gen. Standardm��ig wird diese Datei im etc/ant-Verzeichnis
des Projektes abgelegt.

KalypsoBuild stellt zwei Buildbausteine zur Verf�gung:

 - common-project: f�r ein standard Java-Project, diese
 build.xml kann direkt in die build.xml des Projektes
 importiert werden. Siehe bestehende Projekte f�r Beispiele.

 - common-binding: f�r ein standard XML-Binding Project,
 hier auch kann dieses Skript direkt in das Binding Skript
 importiert werden.
 
Dadurch werden die Skripts einheitlicher und �bersichtlicher.

Ein weiteres wichtiges Projekt ist KalypsoCoreServices welches
auch grundliegende Build-Einstellungen f�r alle Kalypso
Services-Projekte zur Verf�gung stellt.

---------------------------------------------------------------

Es gibt gruns�tzlich 3 M�gligkeiten die Skripts durchf�hren zu k�nnen:
1. zentral (z.B. nightly Build) --> siehe central-build/build.xml
2. �ber die Eclipse Oberfl�che (Wichtig: die Variable 'workspace' muss mit 
   die Eclipse 'workspace_loc' definiert sein (Window/Preferences/Ant/Runtime/Properties...))
3. manuell (z.B. �ber Kommandozeile) (Wichtig: hier auch die Property 
   'workspace' richtig setzen)
   
Class Files werden von Eclipse erzeugt (Konvention: in build/bin) und 
von der Compile-Target der einzelnen Build-Skripts (in build/class).

Das Verzeichnis 'compile-libs' beinhaltet die ben�tigte Libs (die algemein sind) um
die Projekte zu kompilieren.

---------------------------------------------------------------

Quellen die von Eclipse abh�ngen k�nnen im Grunde auch hiermit kompiliert
werden (Siehe compile-libs). Es ist aber nicht vorgesehen GUI-Quellen
die von Eclipse abh�ngen zu kompilieren weil es unterschiedliche
Platformen gibt: SWT f�r Windows, f�r Linux, usw. Deswegen wird z.Z. das
KalypsoUI Projekt von diesem build-system nicht direkt unterst�tzt.
