ANT Properties die vom Entwickler in die Eclipse Preferences eingestellt werden müssen:

- project		die Eclipse project_path variable
- workspace		die Eclipse workspace_loc variable

Gernot: sollte auch die Endoresd Property für die JVM gesetzt werden? 
z.B.: -Djava.endorsed.dirs=
Möglicherweise nötig, um die jaxp Implementation in der J2SDK1.4 zu überschreiben?