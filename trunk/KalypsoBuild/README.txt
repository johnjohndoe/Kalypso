KalypsoBuild

Um einen Nightly Build durchf�hren zu k�nnen, wurden die ant-files der einzelnen Projekte angepasst. Sie sollten immer die folgenden vier targets enthalten:
- compile
- deploy
- junit-test
- clean

Die ant-files lessen Einstellungen aus der Datei projectbuild.properties im Wurzelverzeichniss.

Mit dem ant Skript in ./KalypsoBuildBase kann das Basissystem erstellt werden.

HINWEISS!!!
Um die Bindings mit ant �bersetzen zu k�nnen, habe ich die Dateien:
- xml-apis.jar
- xercesImpl.jar
vgl. http://www.mail-archive.com/ant-user@jakarta.apache.org/msg22957.html