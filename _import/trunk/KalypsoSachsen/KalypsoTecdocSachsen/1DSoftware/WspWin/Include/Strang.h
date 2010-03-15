//////////////
// strang.h //
//////////////

#ifndef _STRANG_H_
#define _STRANG_H_

struct STRANG;

void MakeNewStrang(int); // oft
STRANG* MakeNewStrang(int,STRANG *strang); // in wspm001
void dlg136_get_daten(int);  // wspd136, w120, aufnehm
void dlg136_save_daten(int);  // util, wspd136
void destroy_strang_tabelle(void); // oft 
void destroy_strang_tabelle(STRANG *strang); // wspm001
void make_sort_strangtabelle(void); // wspd135, wspm001
void sort_new_profil( STRANG** strang_anfang, STRANG** strang_ende, 
                     char stationStr[20], char vzkStr[20], char pkStr[20], char name[15], 
                     BOOL vorwaerts ); // aufnehm, wspd136, wspw120, waspila
void vzk_einlesen(void); // readporf
void anhaengen(void); // wspd136, d148
void wandle_abstand_in_string(void); // aufnehm, wspd136, wspw120., waspila
void wandle_abstand_in_string(STRANG *alt_strang); // dito
void delete_strang_entry_neu(void); // wspd136
void StrangUpdateIndex(void); // wspw120, waspila
void change_strang_entry(void); // wspd136


#endif // _STRANG_H_