///////////////
// waspila.h //
///////////////

#ifndef _WASPILA_H_INCLUDED_
#define _WASPILA_H_INCLUDED_

int lese_start_datei(char* start_datei); // wspm001
void lese_geometrie(int anzahl_profs); // wspm001
int lese_profil(char* profil_name, int anzahl_profs); // hier
int lese_charakter_und_rauheits_profil(char *charakter_name, char *profil_name); // hier

#endif _WASPILA_H_INCLUDED_