////////////////
// wsplp2qp.h //
////////////////

#ifndef _WSPLP2QP_H_INCLUDED_
#define _WSPLP2QP_H_INCLUDED_

/*******************************************************
  
 Wasserspiegel aus L�ngsschnittdatei
 in alle Querprofile �bertragen

 DIRECTORY *dirp := Pfad der L�ngsschnittdatei
 char * filename := Dateiname der L�ngsschnittdatei

  R�ckgabe: 1 = OK
            0 = Fehler


*******************************************************/

int InsertWspLpToQuerprof(WSP_PROFIL_LISTE *pWPL,DIRECTORY *dirp,
                          char* filename,char *BerVariante,int datensatz); // wspm001, wspwin



/*******************************************************
Wasserspiegel (aus L�ngsschnittdatei:Referenz Dateiname)
in allen Querprofilen l�schen

 DIRECTORY *dirp := Pfad der L�ngsschnittdatei
 char * filename := Dateiname der L�ngsschnittdatei

  R�ckgabe: 1 = OK
            0 = Fehler
*******************************************************/
int DeleteWspInQuerprof(WSP_PROFIL_LISTE *pWPL,DIRECTORY *dirp, 
						  char* filename,char *BerVariante); // wspm001



/*******************************************************
Alle Wasserspiegel (aus L�ngsschnittdatei:Referenz Dateiname)
in allen Querprofilen l�schen

 DIRECTORY *dirp := Pfad der L�ngsschnittdatei
 char * filename := Dateiname der L�ngsschnittdatei

  R�ckgabe: 1 = OK
            0 = Fehler
*******************************************************/
int DeleteAlleWspInQuerprof(WSP_PROFIL_LISTE *pWPL,DIRECTORY *dirp,
                            char* filename,int datensatz); // wspwin



int WspToUGrenze(WSP_PROFIL_LISTE *pWPL,DIRECTORY *dirp,char* filename); // wspwin

#endif _WSPLP2QP_H_INCLUDED_