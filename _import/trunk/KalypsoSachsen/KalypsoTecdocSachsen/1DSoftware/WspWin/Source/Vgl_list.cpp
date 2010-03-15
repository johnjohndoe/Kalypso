/*            VGL_LIST.CPP                     18.07.97       */
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "prueflst.h"
#include "vgl_list.h"

extern char *start_dir;
long *Dlg169ListSel;
BOOL Dlg169Canceled;

int WriteVglListenDatei(char *BerVar1,char *BerVar2,char *BerVar3)
{
 /* Hauptfunktion zur Erstellung einer Vergleichslistendatei      18.07.97

	 Parameter:
	 char *BerVar1-3 : Pfad + Dateiname der ausgewählten Berechnungsvarianten (dlg203)

	 Rückgabe: TRUE wenn Vergleichslistendatei erfolgreich geschrieben
				  FALSE bei : - Fehler
								  - Abbruch des Auswahldialoges Dlg168
								  - keine Auswahl in Listbox Dlg168  getroffen
 */
 char *tmp;
 FILE *PrfList,*PrfList_neu_1,*PrfList_neu_2;

 if ( (strlen(BerVar1) >60)||(strlen(BerVar2) >60)||(strlen(BerVar3) >60) )
  {
	xvt_dm_post_error("Länge der Berechnungsvariante(incl.Pfad) zu lang!");
	return FALSE;
  }

 Dlg169ListSel = new long[MAX_SEL_LIST_LENGTH];
 tmp = new char[255];

 strcpy(tmp,start_dir);
 strcat(tmp,"\\wsp.vgl");
 if ((PrfList = fopen(tmp,"w+"))==NULL)
  {
	delete[] Dlg169ListSel;
	delete[] tmp;
	return FALSE;
  }
 strcpy(tmp,start_dir);
 strcat(tmp,"\\wspvgl.ctr");
 if ((PrfList_neu_1 = fopen(tmp,"w+"))==NULL)
  {
	delete[] Dlg169ListSel;
	delete[] tmp;
	return FALSE;
  }
 strcpy(tmp,start_dir);
 strcat(tmp,"\\vgl.ctr");
 if ((PrfList_neu_2 = fopen(tmp,"w+"))==NULL)
  {
	delete[] Dlg169ListSel;
	delete[] tmp;
	return FALSE;
  }

 Dlg169Canceled = FALSE;
 xvt_dlg_create_res(WD_MODAL, 169, EM_ALL, DLG_169_eh, 0L);
 if (Dlg169Canceled)
  {
	fclose (PrfList);
	delete[] Dlg169ListSel;
	delete[] tmp;
	return FALSE;
  }

 if (!WriteVglListHeader(PrfList,BerVar1,BerVar2,BerVar3))
  {
	xvt_dm_post_error("Fehler:Kann Vergleichslistenkopf nicht schreiben");
	fclose (PrfList);
	fclose (PrfList_neu_1);
	fclose (PrfList_neu_2);
	delete[] Dlg169ListSel;
	delete[] tmp;
	return FALSE;
  }

 if (!WriteVglListItems(PrfList,Dlg169ListSel))
  {
	xvt_dm_post_error("Fehler:Kann Vergleichsliste nicht schreiben");
	fclose (PrfList);
	fclose (PrfList_neu_1);
	fclose (PrfList_neu_2);
	delete[] Dlg169ListSel;
	delete[] tmp;
	return FALSE;
  }
 if (!WriteVglListHeader(PrfList_neu_1,BerVar1,BerVar2,BerVar3))
  {
	xvt_dm_post_error("Fehler:Kann Vergleichslistenkopf nicht schreiben");
	fclose (PrfList);
	fclose (PrfList_neu_1);
	fclose (PrfList_neu_2);
	delete[] Dlg169ListSel;
	delete[] tmp;
	return FALSE;
  }

 if (!WriteVglListItems(PrfList_neu_2,Dlg169ListSel))
  {
	xvt_dm_post_error("Fehler:Kann Vergleichsliste nicht schreiben");
	fclose (PrfList);
	fclose (PrfList_neu_1);
	fclose (PrfList_neu_2);
	delete[] Dlg169ListSel;
	delete[] tmp;
	return FALSE;
  }

 fclose (PrfList);
 fclose (PrfList_neu_1);
 fclose (PrfList_neu_2);
 delete[] Dlg169ListSel;
 delete[] tmp;
 return TRUE;
}
/*********************************************************************************/
int WriteVglListHeader(FILE *f,char *BerVar1,char *BerVar2,char *BerVar3)
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



 memset(line,SPACE,255);
 strcpy(path,BerVar1); p = strrchr(path,'\\'); if (p) p[0]='\0'; // Pfad
 strcpy(tmp,BerVar1); p = strrchr(tmp,'\\');p++; strcpy(name,p);
 name[2]='e';
 name[3]='r';
 strcpy(line,"Datei1  :");
 strcat(line,path);
 strcat(line,"\\");
 strcat(line,name);
 fprintf( f ,line);
 fprintf( f ,"\n");

 memset(line,SPACE,255);
 strcpy(path,BerVar2); p = strrchr(path,'\\'); if (p) p[0]='\0'; // Pfad
 strcpy(tmp,BerVar2); p = strrchr(tmp,'\\');p++; strcpy(name,p);
 name[2]='e';
 name[3]='r';
 strcpy(line,"Datei2  :");
 strcat(line,path);
 strcat(line,"\\");
 strcat(line,name);
 fprintf( f ,line);
 fprintf( f ,"\n");

  memset(line,SPACE,255);

    if(BerVar3[0]!='#')
  {
	strcpy(path,BerVar3); p = strrchr(path,'\\'); if (p) p[0]='\0'; // Pfad
	strcpy(tmp,BerVar3);
	p = strrchr(tmp,'\\');
	p++;strcpy(name,p);
	name[2]='e';
	name[3]='r';
  }
  else
	{
	 strcpy(path," ");
	 strcpy(name,"#");
	 }

  strcpy(line,"Datei3  :");

  if(BerVar3[0]!='#')
   {
	strcat(line,path);
	strcat(line,"\\");
  }
  strcat(line,name);
  fprintf( f ,line);
  fprintf( f ,"\n");

 memset(line,SPACE,255);
 strcpy(path,BerVar1); p = strrchr(path,'\\'); if (p) p[0]='\0'; // Pfad
 strcpy(tmp,BerVar1); p = strrchr(tmp,'\\');p++; strcpy(name,p);
// name[2]='p';
// name[3]='r';
 name[2]='0';
 name[3]='0';
 strcpy(line,"Ausgabe :");
// strcat(line,path);  //Ausgabe vorläufig ohne Pfad
// strcat(line,"\\");
 name[(strlen(name)-3)]='\0';
 strcat(name,"vgl");
 strcat(line,name);
 fprintf( f ,line);
 fprintf( f ,"\n");

 memset(line,SPACE,255);
 strcpy(line,"Fehler  :");
 strcat(line,"vgl.log");
 fprintf( f ,line);
 fprintf( f ,"\n");

 fprintf(f,"CC *** Kennwerte für den Wertevergleich ***\n");

 delete[] line;
 delete[] tmp;
 delete[] name;
 delete[] path;
 return TRUE;
}
/*********************************************************************************/
int GetVglListSelectedItems(WINDOW win,long *sel)
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
int WriteVglListItems(FILE *f,long *sel)
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
 char *p9;

 if (f==NULL) return FALSE;
 str = new char[200];
 line = new char[255];

 while ( sel[pos] != -1 )
  {
	if (xvt_res_get_str( ((int)sel[pos])+1500,str,150) )
	  {
		memset(line,SPACE,255);
		line[8] = ':'; // bis hierhin Kommentar
		p9 = &line[9];
		strcpy( p9 , str);
		p9[strlen(str)]=' ' ;
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
  line[13]= '\0';
  fprintf( f , "%s\n",line);
 // Ende - Kennzeichen schreiben

 delete[] line;
 delete[] str;
 return TRUE;
}
/*********************************************************************************/
/*********************************************************************************/

