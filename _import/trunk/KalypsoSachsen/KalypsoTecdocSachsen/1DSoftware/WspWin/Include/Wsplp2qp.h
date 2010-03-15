////////////////
// wsplp2qp.h //
////////////////

#ifndef _WSPLP2QP_H_INCLUDED_
#define _WSPLP2QP_H_INCLUDED_

/*******************************************************
  
 Wasserspiegel aus Längsschnittdatei
 in alle Querprofile übertragen

 DIRECTORY *dirp := Pfad der Längsschnittdatei
 char * filename := Dateiname der Längsschnittdatei

  Rückgabe: 1 = OK
            0 = Fehler


*******************************************************/

int InsertWspLpToQuerprof(WSP_PROFIL_LISTE *pWPL,DIRECTORY *dirp,
                          char* filename,char *BerVariante,int datensatz); // wspm001, wspwin



/*******************************************************
Wasserspiegel (aus Längsschnittdatei:Referenz Dateiname)
in allen Querprofilen löschen

 DIRECTORY *dirp := Pfad der Längsschnittdatei
 char * filename := Dateiname der Längsschnittdatei

  Rückgabe: 1 = OK
            0 = Fehler
*******************************************************/
int DeleteWspInQuerprof(WSP_PROFIL_LISTE *pWPL,DIRECTORY *dirp, 
						  char* filename,char *BerVariante); // wspm001



/*******************************************************
Alle Wasserspiegel (aus Längsschnittdatei:Referenz Dateiname)
in allen Querprofilen löschen

 DIRECTORY *dirp := Pfad der Längsschnittdatei
 char * filename := Dateiname der Längsschnittdatei

  Rückgabe: 1 = OK
            0 = Fehler
*******************************************************/
int DeleteAlleWspInQuerprof(WSP_PROFIL_LISTE *pWPL,DIRECTORY *dirp,
                            char* filename,int datensatz); // wspwin



int WspToUGrenze(WSP_PROFIL_LISTE *pWPL,DIRECTORY *dirp,char* filename); // wspwin

#endif _WSPLP2QP_H_INCLUDED_