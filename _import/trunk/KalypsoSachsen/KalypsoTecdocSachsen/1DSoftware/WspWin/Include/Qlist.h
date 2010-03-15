/////////////
// QList.h //
/////////////
// gehört zu qwert.h !

#ifndef _QLIST_H_
#define _QLIST_H_

void Delete_Qwert_Datei(void); // oft
void MakeNewNode(void); // wspw122
void MakeNewQSatz(int anzahl); // dito
void ReadWerte(int n1); // hier
void ReadWerteWSF(int n1);// hier
void lese_qwert_datei(void); // oft
void lese_ereignisse(void); // oft
int TesteEndeQwert(void); // oft
void LeseSlistInQwert(SLIST list, int nr); // wspw122
void win122_get_scroll_daten(Scroller *pscr); // wspw122
void display_win122_edit(WINDOW *win,Scroller *pscr); // dito
void win122_save_scroll_daten(Scroller *pscr,char str[11],int nr,int mode=0);//Dick 2.02.99 mode -> Y(==0) oder Z(==1)  // wspw122
int schreibe_qwert_datei(void); // dito

/********   Funktionen: TABELLE ***********************/
TABELLE* MakeNewTabelle(int anzahl); // lese_str, util2, wspd307
void dlg206_get_daten(WINDOW *win,TABELLE *ptr_anfang,int position); // wspd307
void dlg206_save_daten(WINDOW *win,TABELLE *ptr_anfang,int position); // dito
void destroy_tabelle(void); // oft
int write_tabelle(FILE *,TABELLE *ptr_anfang,char *titel); // util2
int read_tabelle(FILE *,TABELLE *ptr_anfang); // dito
BOOLEAN teste_k(TABELLE *ptr_anfang, double station); // util2

int teste_anzahl_hq(void); // dito


#endif _QLIST_H_