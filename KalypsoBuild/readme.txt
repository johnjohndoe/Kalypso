 KalypsoBuild - Zentrales Buildsystem für die Kalypso-Projekte
---------------------------------------------------------------

Soweit möglich sollte jedes Source-Projekt über eine build.xml-Datei
verfügen. Standardmäßig wird diese Datei im etc/ant-Verzeichnis
des Projektes abgelegt.

KalypsoBuild stellt zwei Buildbausteine zur Verfügung:

 - common-project: für ein standard Java-Project, diese
 build.xml kann direkt in die build.xml des Projektes
 importiert werden. Siehe bestehende Projekte für Beispiele.

 - common-binding: für ein standard XML-Binding Project,
 hier auch kann dieses Skript direkt in das Binding Skript
 importiert werden.
 
Dadurch werden die Skripts einheitlicher und übersichtlicher.

Ein weiteres wichtiges Projekt ist KalypsoCoreServices welches
auch grundliegende Build-Einstellungen für alle Kalypso
Services-Projekte zur Verfügung stellt.

---------------------------------------------------------------

Es gibt grunsätzlich 3 Mögligkeiten die Skripts durchführen zu können:
1. zentral (z.B. nightly Build) --> siehe central-build/build.xml
2. über die Eclipse Oberfläche (Wichtig: die Variable 'workspace' muss mit 
   die Eclipse 'workspace_loc' definiert sein (Window/Preferences/Ant/Runtime/Properties...))
3. manuell (z.B. über Kommandozeile) (Wichtig: hier auch die Property 
   'workspace' richtig setzen)
   
Class Files werden von Eclipse erzeugt (Konvention: in build/bin) und 
von der Compile-Target der einzelnen Build-Skripts (in build/class).

Das Verzeichnis 'compile-libs' beinhaltet die benötigte Libs (die algemein sind) um
die Projekte zu kompilieren.

---------------------------------------------------------------

Quellen die von Eclipse abhängen können im Grunde auch hiermit kompiliert
werden (Siehe compile-libs). Es ist aber nicht vorgesehen GUI-Quellen
die von Eclipse abhängen zu kompilieren weil es unterschiedliche
Platformen gibt: SWT für Windows, für Linux, usw. Deswegen wird z.Z. das
KalypsoUI Projekt von diesem build-system nicht direkt unterstützt.
