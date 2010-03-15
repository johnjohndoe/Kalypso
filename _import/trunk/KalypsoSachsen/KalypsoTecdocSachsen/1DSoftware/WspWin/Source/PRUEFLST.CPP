/*            PRUEFLST.CPP                     18.07.97       */
#include <windows.h>
#include "xvt.h"
#include "resource.h"
#include "wspwin.h"
#include "prueflst.h"

#define ENDE 84
#define SPACE 32


extern char *start_dir;
long *Dlg168ListSel;
BOOL punktbefehl;
BOOL Dlg168Canceled;
BOOL DefaultLesen=FALSE;

// vorwärtsdeklarationen
int WritePrListHeader(FILE *f,char *BerVar,BOOL bew,BOOL pkt,BOOL protokol); // hier
BOOL GetPrueflistenString(long id,char *string,int len); // hier
int WritePrListItems(FILE *f,long *sel); // hier


int WritePrueflistenDatei(char *BerVar,BOOL bew)
{
 /* Hauptfunktion zur Erstellung einer Prüflistendatei      18.07.97

	 Parameter:
	 char *BerVar : Pfad + Dateiname der ausgewählten Berechnungsvariante (dlg203)
	 BOOL bew     : mit oder ohne Ausdruck der *.bew -Datei

	 Rückgabe: TRUE wenn Prüflistendatei erfolgreich geschrieben
				  FALSE bei : - Fehler
								  - Abbruch des Auswahldialoges Dlg168
								  - keine Auswahl in Listbox Dlg168  getroffen
 */
 char *tmp,*path,*name;
 FILE *PrfList,*PrfList_neu_1,*PrfList_neu_2;

 if ( strlen(BerVar) >60)
  {
    char buf[200];//Dick 26.11.99
      xvt_res_get_str(STR_LISTAUSW_NOTE,buf,sizeof(buf));
      xvt_dm_post_note("%s",buf);
	//xvt_dm_post_error("Länge der Berechnungsvariante(incl.Pfad) zu lang!");
	return FALSE;
  }

 Dlg168ListSel = new long[MAX_SEL_LIST_LENGTH];
 char tmp2[255];
 tmp = new char[255];
 path = new char[255];
 name = new char[15];

 strcpy(tmp,start_dir);
 strcat(tmp,"\\wsp.prf");
 if ((PrfList = fopen(tmp,"w+"))==NULL)
  {
	delete[] Dlg168ListSel;
	delete[] tmp;
	delete[] name;
	delete[] path;
	return FALSE;
  }
 strcpy(tmp2,start_dir);
 strcat(tmp2,"\\wspruef.ctr");
 if ((PrfList_neu_1 = fopen(tmp2,"w+"))==NULL)
  {
	delete[] Dlg168ListSel;
	delete[] tmp;
	delete[] name;
	delete[] path;
	return FALSE;
  }
 strcpy(tmp2,start_dir);
 strcat(tmp2,"\\prf.ctr");
 if ((PrfList_neu_2 = fopen(tmp2,"w+"))==NULL)
  {
	delete[] Dlg168ListSel;
	delete[] tmp;
	delete[] name;
	delete[] path;
	return FALSE;
  }
 Dlg168Canceled = FALSE;
 DefaultLesen=TRUE;
 xvt_dlg_create_res(WD_MODAL, 168, EM_ALL, DLG_168_eh, 0L);
 if (Dlg168Canceled)
  {
	fclose (PrfList);
	fclose (PrfList_neu_1);
	fclose (PrfList_neu_2);
	delete[] Dlg168ListSel;
	delete[] tmp;
	delete[] name;
	delete[] path;
	return FALSE;
  }

 if (!WritePrListHeader(PrfList,BerVar,bew,punktbefehl,FALSE))
  {
    char buf[200];//Dick 26.11.99
      xvt_res_get_str(STR_PRUEFLST_NOTE3,buf,sizeof(buf));
      xvt_dm_post_note("%s",buf);
	//xvt_dm_post_error("Fehler:Kann Prüflistenkopf nicht schreiben");
	fclose (PrfList);
	delete[] Dlg168ListSel;
	delete[] tmp;
	delete[] name;
	delete[] path;
	return FALSE;
  }

 if (!WritePrListItems(PrfList,Dlg168ListSel))
  {
    char buf[200];//Dick 26.11.99
      xvt_res_get_str(STR_PRUEFLST_NOTE2,buf,sizeof(buf));
      xvt_dm_post_note("%s",buf);
	//xvt_dm_post_error("Fehler:Kann Prüfliste nicht schreiben");
	fclose (PrfList);
	delete[] Dlg168ListSel;
	delete[] tmp;
	delete[] name;
	delete[] path;
	return FALSE;
  }
  if (!WritePrListHeader(PrfList_neu_1,BerVar,bew,punktbefehl,TRUE))
  {
    char buf[200];//Dick 26.11.99
      xvt_res_get_str(STR_PRUEFLST_NOTE3,buf,sizeof(buf));
      xvt_dm_post_note("%s",buf);
	//xvt_dm_post_error("Fehler:Kann Prüflistenkopf nicht schreiben");
	fclose (PrfList);
	delete[] Dlg168ListSel;
	delete[] tmp;
	delete[] name;
	delete[] path;
	return FALSE;
  }

 if (!WritePrListItems(PrfList_neu_2,Dlg168ListSel))
  {
    char buf[200];//Dick 26.11.99
      xvt_res_get_str(STR_PRUEFLST_NOTE2,buf,sizeof(buf));
      xvt_dm_post_note("%s",buf);
	//xvt_dm_post_error("Fehler:Kann Prüfliste nicht schreiben");
	fclose (PrfList);
	fclose (PrfList_neu_1);
	fclose (PrfList_neu_2);
	delete[] Dlg168ListSel;
	delete[] tmp;
	delete[] name;
	delete[] path;
	return FALSE;
  }
 fclose (PrfList);
 fclose (PrfList_neu_1);
 fclose (PrfList_neu_2);
 delete[] Dlg168ListSel;
 delete[] tmp;
 delete[] name;
 delete[] path;
 return TRUE;
}
/*********************************************************************************/
int WritePrListHeader(FILE *f,char *BerVar,BOOL bew,BOOL pkt,BOOL protokol)
{
 /*
	 Hilfsfunktion zur Erstellung von Vergleichslisten: -->WSPVGL.EXE    18.07.97

	 Schreibt den Kopf: Datei1,Datei2 ... in die Datei FILE *f


	 Rückgabe : TRUE  wenn OK
					FALSE bei Fehler : File *f ==NULL

 */
 char *line,*tmp,*path,*name,*p;

 if (f==NULL) return FALSE;

 line = new char[255];
 tmp = new char[255];
 path = new char[255];
 name = new char[15];

 strcpy(tmp,BerVar); p = strrchr(tmp,'\\');p++; strcpy(name,p);

 strcpy(path,BerVar);
 p = strrchr(path,'\\');
 if (p) p[0]='\0';

 memset(line,SPACE,255);
 strcpy(tmp,BerVar); p = strrchr(tmp,'\\');p++; strcpy(name,p);
 name[2]='w';
 name[3]='s';
 strcpy(line,"Datei1  :");
 strcat(line,path);
 strcat(line,"\\");
 strcat(line,name);
 fprintf( f ,line);
 fprintf( f ,"\n");

 memset(line,SPACE,255);
 strcpy(tmp,BerVar); p = strrchr(tmp,'\\');p++; strcpy(name,p);
 name[2]='e';
 name[3]='r';
 strcpy(line,"Datei2  :");
 strcat(line,path);
 strcat(line,"\\");
 strcat(line,name);
 fprintf( f ,line);
 fprintf( f ,"\n");

 memset(line,SPACE,255);
 strcpy(tmp,BerVar); p = strrchr(tmp,'\\'); p++;strcpy(name,p);
 name[2]='b';
 name[3]='e';
 strcpy(line,"Datei3  :");
 if (bew)
 {
  strcat(line,path);
  strcat(line,"\\");
  strcat(line,name);
 }
 else strcat(line,"#");
 fprintf( f ,line);
 fprintf( f ,"\n");

 memset(line,SPACE,255);
 strcpy(tmp,BerVar); p = strrchr(tmp,'\\');p++; strcpy(name,p);
// name[2]='p';
// name[3]='r';
 name[2]='0';
 name[3]='0';
 name[(strlen(name)-3)]='\0';
 strcat(name,"prf");
 strcpy(line,"Ausgabe :");
// strcat(line,path);  //vorläufig noch ohne Pfad
// strcat(line,"\\");
 strcat(line,name);
 fprintf( f ,line);
 fprintf( f ,"\n");
 if(protokol)
     {
      memset(line,SPACE,255);
      strcpy(tmp,BerVar); p = strrchr(tmp,'\\');p++; strcpy(name,p);
      name[2]='0';
      name[3]='0';
      name[(strlen(name)-3)]='\0';
      strcat(name,"log");
      strcpy(line,"Protokoll :");
      // strcat(line,path);  //vorläufig noch ohne Pfad
      // strcat(line,"\\");
      strcat(line,name); 
      fprintf( f ,line);
      fprintf( f ,"\n");
     }
 memset(line,SPACE,255);
 strcpy(line,"Fehler  :");
 strcat(line,"prf.log");
 fprintf( f ,line);
 fprintf( f ,"\n");

 memset(line,SPACE,255);    // mit/ohne Punktbefehl
 strcpy(line,".NS.    :");
 if (pkt)  line[9] = '1';
 else line[9] = '0';
 line[10]='\0';
 fprintf( f ,line);
 fprintf( f ,"\n");

 delete[] line;
 delete[] tmp;
 delete[] name;
 delete[] path;
 return TRUE;
}
/*********************************************************************************/
BOOL GetPrueflistenString(long id,char *string,int len)
/*
	Hilfsfunktion zur Erstellung von Vergleichslisten: -->WSPVGL.EXE    18.07.97

	Gibt in Abhängigkeit von id (z.B. als Rückgabewert der Auswahl aus einer
	Listbox) den zugehörigen String zurück.
	In int:len wird die Länge des übergebenen Strings angegeben, min:10 Zeichen

	Rückgabe: TRUE, wenn es zu id einen String gibt
				 FALSE,wenn kein String vorhanden, oder übergebener String
						 zu klein ist
*/
{
 if (len < 10) return FALSE;
 switch (id)
 {
  case 0:  strcpy(string,"WSP-L");return TRUE;
  case 1:  strcpy(string,"EH");return TRUE;
  case 2:  strcpy(string,"WTIEFE");return TRUE;
  case 3:  strcpy(string,"BL");return TRUE;
  case 4:  strcpy(string,"BF");return TRUE;
  case 5:  strcpy(string,"BR");return TRUE;
  case 6:  strcpy(string,"UL");return TRUE;
  case 7:  strcpy(string,"UF");return TRUE;
  case 8:  strcpy(string,"UR");return TRUE;
  case 9:  strcpy(string,"KSTL");return TRUE;
  case 10:  strcpy(string,"KSTF");return TRUE;
  case 11:  strcpy(string,"KSTR");return TRUE;
  case 12:  strcpy(string,"HZV");return TRUE;
  case 13:  strcpy(string,"FL");return TRUE;
  case 14:  strcpy(string,"FF");return TRUE;
  case 15:  strcpy(string,"FR");return TRUE;
  case 16:  strcpy(string,"VL");return TRUE;
  case 17:  strcpy(string,"VF");return TRUE;
  case 18:  strcpy(string,"VR");return TRUE;
  case 19:  strcpy(string,"hydrL");return TRUE;
  case 20:  strcpy(string,"hydrF");return TRUE;
  case 21:  strcpy(string,"hydrR");return TRUE;
  case 22:  strcpy(string,"Froude");return TRUE;
  case 23:  strcpy(string,"Alpha");return TRUE;
  case 24:  strcpy(string,"AlphaS");return TRUE;
  case 25:  strcpy(string,"IE");return TRUE;
  case 26:  strcpy(string,"QL");return TRUE;
  case 27:  strcpy(string,"QF");return TRUE;
  case 28:  strcpy(string,"QR");return TRUE;
  case 29:  strcpy(string,"TauL");return TRUE;
  case 30:  strcpy(string,"TauF");return TRUE;
  case 31:  strcpy(string,"TauR");return TRUE;
  case 32:  strcpy(string,"LambL");return TRUE;
  case 33:  strcpy(string,"LambF");return TRUE;
  case 34:  strcpy(string,"LambR");return TRUE;
  case 35:  strcpy(string,"ZETA");return TRUE;
  case 36:  strcpy(string,"LL");return TRUE;
  case 37:  strcpy(string,"LF");return TRUE;
  case 38:  strcpy(string,"LR");return TRUE;
  case 39:  strcpy(string,"ZS");return TRUE;
  case 40:  strcpy(string,"Uferl");return TRUE;  // ?? "UferL"
  case 41:  strcpy(string,"UferR");return TRUE;
  case 42:  strcpy(string,"RandL");return TRUE;
  case 43:  strcpy(string,"RandR");return TRUE;
  case 44:  strcpy(string,"wtuL");return TRUE;
  case 45:  strcpy(string,"wtuR");return TRUE;
  case 46:  strcpy(string,"FreibL");return TRUE;
  case 47:  strcpy(string,"FreibR");return TRUE;
  case 48:  strcpy(string,"IDP");return TRUE;
  case 49:  strcpy(string,"IVZ");return TRUE;
  case 50:  strcpy(string,"MFB");return TRUE;
  case 51:  strcpy(string,"DKUK");return TRUE;
  case 52:  strcpy(string,"DKOK");return TRUE;
  case 53:  strcpy(string,"BP");return TRUE;
  case 54:  strcpy(string,"UP");return TRUE;
  case 55:  strcpy(string,"FP");return TRUE;
  case 56:  strcpy(string,"WSP-Diff");return TRUE;
  case 57:  strcpy(string,"EH-Diff");return TRUE;
  case 58:  strcpy(string,"IE-M");return TRUE;
  case 59:  strcpy(string,"htrL");return TRUE;
  case 60:  strcpy(string,"htrR");return TRUE;
  case 61:  strcpy(string,"LamTrL");return TRUE; //LambTrL
  case 62:  strcpy(string,"LamTrR");return TRUE; //LambTrR
  case 63:  strcpy(string,"vtrL");return TRUE;
  case 64:  strcpy(string,"vtrR");return TRUE;
  case 65:  strcpy(string,"ktrL");return TRUE;
  case 66:  strcpy(string,"ktrR");return TRUE;

  case 67:  strcpy(string,"ThetaL");return TRUE;
  case 68:  strcpy(string,"ThetaF");return TRUE;
  case 69:  strcpy(string,"ThetaR");return TRUE;
  case 70:  strcpy(string,"PhiehL");return TRUE;
  case 71:  strcpy(string,"PhiehF");return TRUE;
  case 72:  strcpy(string,"PhiehR");return TRUE;
  case 73:  strcpy(string,"MGehL");return TRUE;
  case 74:  strcpy(string,"MGehF");return TRUE;
  case 75:  strcpy(string,"MGehR");return TRUE;
  case 76:  strcpy(string,"MGehG");return TRUE;
  case 77:  strcpy(string,"PhimpL");return TRUE;
  case 78:  strcpy(string,"PhimpF");return TRUE;
  case 79:  strcpy(string,"PhimpR");return TRUE;
  case 80:  strcpy(string,"MGmpL");return TRUE;
  case 81:  strcpy(string,"MGmpF");return TRUE;
  case 82:  strcpy(string,"MGmpR");return TRUE;
  case 83:  strcpy(string,"MGmpG");return TRUE;
  case ENDE:strcpy(string,"ENDE");return TRUE; //=84

  default:return FALSE;
 }
}

/*********************************************************************************/
int GetPrListSelectedItems(WINDOW win,long *sel)
{
 /*
	  Hilfsfunktion zur Erstellung von Vergleichslisten: -->WSPVGL.EXE    18.07.97

	  GetPrListSelectedItems liest aus der Listbox "WINDOW win" alle
	  ausgewählten Elemente und schreibt die Indices (2., 5., 11.-Element gewählt)
	  in den übergebenen Array: sel.
	  Das Ende der Liste enthält -1 als Endekennzeichen.

	  long *sel ist ein Array mit MAX_SEL_LIST_LENGTH (70) Elementen
				:=  long sel[MAX_SEL_LIST_LENGTH];

	  Rückgabe: FALSE = keine Elemente markiert oder sonst. Fehler
					>0  Anzahl der markierten Listbox-Einträge
 */
 SLIST list;
 SLIST_ELT e;
 int i=0;

 if ((i=xvt_list_count_sel(win))==0)	return FALSE;
 if (i>MAX_SEL_LIST_LENGTH) return FALSE;

 if ((list = xvt_slist_create()) ==NULL) return FALSE;

 for (i=0;i<MAX_SEL_LIST_LENGTH;i++)  sel[i]=-1;  //initialisieren

 i=0;
 list = xvt_list_get_sel(win);
 for ( e=xvt_slist_get_first(list); e ;e=xvt_slist_get_next(list,e))
  {
	sel[i] = *(xvt_slist_get_data( e ));
	i++;
  }
 xvt_slist_destroy(list);
 return i;
}
/*********************************************************************************/
int WritePrListItems(FILE *f,long *sel)
 /*
	  Hilfsfunktion zur Erstellung von Vergleichslisten: -->WSPVGL.EXE    18.07.97

	 long *sel ist ein Array mit MAX_SEL_LIST_LENGTH (70) Elementen
				:=  long sel[MAX_SEL_LIST_LENGTH];

	 Format: 1-9 Kommentar 10-20 Steuersymbole 21 - ... Kommentar
			  [0-8 Kommentar 9-19 Steuersymbole  20 - ... Kommentar]
 */
{
 int pos = 0;
 char *str,*line;
 char *p9,*p21;

 if (f==NULL) return FALSE;
 str = new char[200];
 line = new char[255];

 while ( sel[pos] != -1 )
  {
	if (GetPrueflistenString(sel[pos],str,15) )
	  {
		memset(line,SPACE,255);
		line[8] = ':'; // bis hierhin Kommentar
		p9 = &line[9];
		strcpy( p9 , str);
		p9[strlen(str)]=' ' ;
		line[20]= ':'; // ab hier Kommentar
		p21 = &line[21];
		xvt_res_get_str( ((int)sel[pos])+1400,str,150);
		strcpy(p21,str);

		fprintf( f , "%s\n",line);
	  }
	pos++;
  }
 // Ende - Kennzeichen schreiben
  memset(line,SPACE,255);
  line[8] = ':'; // bis hierhin Kommentar
  line[9]=  'E';
  line[10]= 'N';
  line[11]= 'D';
  line[12]= 'E';
  line[20]= ':'; // ab hier Kommentar
  p21 = &line[21];
  strcpy(p21,"Abbruch-Kennwert");
  fprintf( f , "%s\n",line);
 // Ende - Kennzeichen schreiben


 delete[] line;
 delete[] str;
 return TRUE;
}
/*********************************************************************************/
/*********************************************************************************/
int WriteDefPrueflistenDatei(void)
{
 /* Funktion zur Erstellung einer DefaultPrüflistendatei      13.01.99

	 

	 Rückgabe: TRUE wenn Prüflistendatei erfolgreich geschrieben
				  FALSE bei : - Fehler
								  - Abbruch des Auswahldialoges Dlg168
								  - keine Auswahl in Listbox Dlg168  getroffen
 */
 char *tmp;
 FILE *PrfList;
 int pos=0;
 

 Dlg168ListSel = new long[MAX_SEL_LIST_LENGTH];
 tmp = new char[255];
 


 Dlg168Canceled = FALSE;
 DefaultLesen=FALSE;
 xvt_dlg_create_res(WD_MODAL, 168, EM_ALL, DLG_168_eh, 0L);
 if (Dlg168Canceled)
  {
	
	delete[] Dlg168ListSel;
	delete[] tmp;
	
	return FALSE;
  }


 strcpy(tmp,start_dir);
 strcat(tmp,"\\wsp_def.prf");
 if ((PrfList = fopen(tmp,"w+"))==NULL)
  {
	delete[] Dlg168ListSel;
	delete[] tmp;
    char buf[200];//Dick 26.11.99
      xvt_res_get_str(STR_PRUEFLST_NOTE,buf,sizeof(buf));
      xvt_dm_post_note("%s",buf);
	//xvt_dm_post_error("Voreinstellung für Prüflisten kann nicht geschrieben werden!");
	return FALSE;
  }

 
 
  fprintf(PrfList,"%d\n",punktbefehl);
  while(Dlg168ListSel[pos]!=-1)
      {
       fprintf(PrfList,"%d\n",Dlg168ListSel[pos]);
       pos++;
      }
  fprintf(PrfList,"%d",-1);

 fclose (PrfList);
 delete[] Dlg168ListSel;
 delete[] tmp;
 
 return TRUE;
}
/*********************************************************************************/
int ReadDefPrueflistenDatei(int *def_list)
{
 /* Funktion zur Erstellung einer DefaultPrüflistendatei      13.01.99

	 Parameter:
	 int *def_list : Listenelementnummern für Vorselektieren
	 

	 Rückgabe: TRUE wenn Prüflistendatei erfolgreich gelesen
				  FALSE bei : - Fehler
*/
 char *tmp;
 FILE *PrfList;
 int pos=0;
 

 
 tmp = new char[255];
 

 strcpy(tmp,start_dir);
 strcat(tmp,"\\wsp_def.prf");
 if ((PrfList = fopen(tmp,"r"))==NULL)
  {	
	delete[] tmp;	
	return FALSE;
  }

 
  while(fscanf(PrfList,"%d",def_list+pos)!=EOF)
      {
       pos++;
      }
  

 fclose (PrfList);
 delete[] tmp;
 
 return TRUE;
}
/*********************************************************************************/

