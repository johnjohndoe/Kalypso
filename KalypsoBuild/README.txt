KalypsoBuild

Um einen Nightly Build durchführen zu können, wurden die ant-files der 
einzelnen Projekte angepasst. Sie sollten immer die folgenden vier targets enthalten:
- compile
- deploy
- junit-test
- clean

Es gibt grunsätzlich 3 Mögligkeiten die Skripts durchführen zu können:
1. zentral (z.B. nightly Build) --> siehe central-build/build.xml
2. über die Eclipse Oberfläche (Wichtig: die Variable 'workspace' muss mit 
   die Eclipse 'workspace_loc' definiert sein (Window/Preferences/Ant/Runtime/Properties...))
3. manuell (z.B. über Kommandozeile) (Wichtig: hier auch die Property 'workspace' richtig 
   setzen)
   
Class Files werden von Eclipse erzeugt (Konvention: in build/bin) und von der Compile-Target
der einzelnen Build-Skripts (in build/class).

Das Verzeichnis 'lib/compile' beinhaltet die benötigte Libs (die algemein sind) um
die Projekte zu kompilieren. Sollte Libs sehr projektspezifisch sein, dann am
bestens diese Libs in einem gesonderte Verzeichnis im Projekt ablegen (Konvention:
lib/compile).

KalypsoUI besitzt z.Z. kein build.xml

