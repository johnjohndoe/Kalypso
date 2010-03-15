#ifndef _LEIBO_H_
#define _LEIBO_H_

/* Modul : Fl�chenberechnung Dr.Leibo    15.07.19996

	Aufruf:
  ----------

	FlaechenImLaengsprofilBerechnen(LAENGSPROFIL *lp),

	wobei in lp die Datens�tze	querprof1 und querprof2 (Typ.GELAENDE)
	= Daten aus einer Profildatei-Datensatz:	'GELAENDE' enthalten sein m�ssen.
	R�ckgabe der berechneten Fl�chendaten in:   lp->flaeche

  In der Struktur << LAENGSPROFIL >> sind
  Abtragsfl�chen  mit ( - ) und  Auftragsfl�chen mit ( + ) gespeichert.

  summe ( Abtrag - Auftrag)      ist  mit Vorzeichen gespeichert.
  sum_auftrag                    ist ohne Vorzeichen gespeichert.
  sum_abtrag                     ist ohne Vorzeichen gespeichert.



	RETURN :  0 = Fehler
				 1 = OK
/*---------------------   Prototypen  f�r leibo.cpp ------------------------*/
// Exportierte Funktionern
int FlaechenImLaengsprofilBerechnen(LAENGSPROFIL *LP); // in Flaeche.cpp und volume2.cpp
int SchnittPunktzweierStrecken(double TOL,double x1,double y1,double x2,double y2,
										 double x3,double y3,double x4,double y4,double *X, double *Y); // in wspwin/List.cpp

// werden nicht exportiert
void RiemannschesIntegral(int I,int *p,double *x,double *y,double *X,double *Y, double *F);// nur in FlaechenImLaengsprofilBerechnen

/*---------------------   Prototypen  ----------------------------------------*/

#endif // _LEIBO_H_