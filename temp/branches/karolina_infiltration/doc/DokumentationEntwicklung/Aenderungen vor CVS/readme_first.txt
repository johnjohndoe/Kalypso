Änderungen am NA_Modell


_000 an alle .f90-Dateien rangehängt.

-> kompiliert (test_komp1)

alle Ziffern in Dateinamen weggenommen:
	boden4_n	-	boden_n
	boden6		-	boden
	bodf_n2		-	bodf_n
	bodf3		-	bodf
	checkn3		-	checkn
	gebiet6		-	gebiet
	gerinne3	-	gerinne
	gwsp3_st	-	gewsp_st
	gwsp6		-	gwsp
	incept6		-	incept
	inp_hydro3	-	inp_hydro
	inp_zft6	-	inp_zft
	input6		-	input
	lcase10		-	lcase
	outbil_3	-	outbil
	psynt6		-	psynt
	snow1		-	snow	

-> versucht zu kompilieren (test_komp2)

Problem mit "duplicare definition" aufgetreten

Dateien erneut kopiert (nur .f90 + include) in test_komp3

Problem: in boden_n tauchen die beiden subroutinen 
		bodf
		incept
auf. Deswegen wohl der Fehler beim kompilieren. Problem wird anschaulich im analyzer.
Projekt test2.prv in datei/test2 öffnen. (function list)

Problembehebung:
"boden_n" und "boden" sind zwei Subroutinen, die aus "gebiet" aufgerufen werden mit einer if-Abfrage.
"boden_n" ist eine einfache Berechnung ohne Hydrotope.
-> "boden_n" in "boden_alt" umbenannt
In "boden_n" werden die beiden Subroutinen "incept" und "bodf" aufgerufen.
Diesen beiden auch umbenannt in "incept_alt" und "bodf_alt"!
Subroutinen werden nur von "boden_n" aufgerufen.
Subroutine bleibt aber erhalten (man weiss ja nie...)!
In "gebiet" auch "boden_n" in "boden_alt" umbenannt.

-> erneut kompiliert (test_komp4) und es klappt!!!!!!!

Über analyzer geprüft, nur noch 3 einzelstehende Subroutinen:
	bodf
	outarcview_tg
	outnas

"bodf" scheint nach Abgleich mit "bodf_n" eine alte Datei zur Bodenfeuchteberechnung zu sein. 
Wird ausgegliedert in "alt"

Die beiden anderen Dateien wurde zuletzt von Pasche bearbeietet. Es wird mit ihm abgeklärt, wofür die sind.
Solange verbleiben sie!

Neuer Ordner angelegt "kalypso00"
Dort die Dateien reinkopiert.
Erneut kompiliert mit "kalypso00.exe"

Dies ist die jungfräuliche Version des NA-Modells!

Stand: 07.05.02 Wiebke Klauder