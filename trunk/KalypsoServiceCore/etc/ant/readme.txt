Erl�uterung:
catalina-ant.jar - f�r die Taskdefs im build.xml bez. Tomcat (Start, Deploy, ...)

project-build.xml - um nur das KalypsoCoreServices Projekt zu builden
service-build.xml - wird von die einzelnen Services importiert (beinhaltet algemeine Build-Einstellungen)

Gernot: sollte auch die Endorsed Property f�r die JVM gesetzt werden? 
z.B.: -Djava.endorsed.dirs=
M�glicherweise n�tig, um die jaxp Implementation in der J2SDK1.4 zu �berschreiben?