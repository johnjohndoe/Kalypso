KalypsoBuild

Um einen Nightly Build durchf�hren zu k�nnen, wurden die ant-files der 
einzelnen Projekte angepasst. Sie sollten immer die folgenden vier targets enthalten:
- compile
- deploy
- junit-test
- clean

Es gibt gruns�tzlich 3 M�gligkeiten die Skripts durchf�hren zu k�nnen:
1. zentral (z.B. nightly Build) --> siehe central-build/build.xml
2. �ber die Eclipse Oberfl�che (Wichtig: die Variable 'workspace' muss mit 
   die Eclipse 'workspace_loc' definiert sein (Window/Preferences/Ant/Runtime/Properties...))
3. manuell (z.B. �ber Kommandozeile) (Wichtig: hier auch die Property 'workspace' richtig 
   setzen)
   
Class Files werden von Eclipse erzeugt (Konvention: in build/bin) und von der Compile-Target
der einzelnen Build-Skripts (in build/class).

Das Verzeichnis 'lib/compile' beinhaltet die ben�tigte Libs (die algemein sind) um
die Projekte zu kompilieren. Sollte Libs sehr projektspezifisch sein, dann am
bestens diese Libs in einem gesonderte Verzeichnis im Projekt ablegen (Konvention:
lib/compile).

KalypsoUI besitzt z.Z. kein build.xml

