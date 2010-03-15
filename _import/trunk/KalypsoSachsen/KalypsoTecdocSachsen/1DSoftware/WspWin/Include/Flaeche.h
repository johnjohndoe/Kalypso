/*********************************************************

	Datenschnittstelle zum Modul "Flächenberechnung"

	Hinweis: -alle Module werden in einer DLL implementiert
				-Memory für Struktur GELAENDE,LAENGSPROFIL
				 alloziiert auf GobalenHeap

*********************************************************/
#ifndef _FLAECHE_H
#define _FLAECHE_H

long XVT_CALLCONV1 DLG_165_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent);
#define DLG_165 165

/********   Prototypen   *************/
void InitStrData(STR_DATA*); // flaeche, jabron2, volume1, wspm001
void DeleteStrData(STR_DATA *); // flaeche, wspm001
int Save_Global_StrData(STR_DATA *); // flaeche, jabron2, volume1, wspm001
int Get_Global_StrData(STR_DATA *) ; // dito
int DeleteGel2InStrData(STR_DATA *); // flaeche, wspm001
int DeleteFlaecheInStrData(STR_DATA *); // flaeche, wspm001
int Test_Flaechen_Data(STR_DATA *); // flaeche, wspm001
int Make_FlaechenBerechnung(STR_DATA *); // flaeche, wspm001

int Make_GelaendeVerknuepfung(int ,double ); // flaeche, wspm001
int ProfilFlaechenBerechnung(class List *,double); // flaeche, wspm001

#endif

