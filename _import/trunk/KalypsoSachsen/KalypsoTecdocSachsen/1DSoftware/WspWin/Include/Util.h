#ifndef _UTIL_H
#define _UTIL_H

struct neuer_datensatz // util, wspm001, wspw116, wspw120
{
  WINDOW listbox_win;
  char *datensatz;
  short id;
  short typen[100];
};

void convert_profil_str(char *profilstr); // wspd136
/* konvertiert Profilstring aus der Vernetzungsdatei
(listbox 86 in wspd136) in profilstring für Edit-Felder	in wspd136        */

int compare_station(char *station); // wspd136
int anzahl_station(char *station); // dito
/*********************************************************/
int get_profildatei_names(char *lpstrFile); // aufnehm, wspd135, wspm001

neuer_datensatz display_new_datensatz(WINDOW win); // wspw116, wspw120
// set - Funktionen
void wsp121_set_title(WINDOW win); // wspw121
void wsp121_save_title(void); // wspw121
void zeige_slist(SLIST list); // aufnehm, profproj, qwert, wspd211
void read_res_datentypen(SLIST ,int ); // list, readprof, wspw116, w120
void add_new_datensatz(WINDOW win_list2); // wspw120, wspw116
void delete_datensatz(WINDOW win); // wspw116, w120
int check_border(void); // wspw120, wspw116
void header_zusammensetzten(void); // wspw121
BOOL CopyDir(char* quelle,char* ziel); // wspd147, d148
BOOLEAN Verzeichnis_waehlen(HWND hwnd,HINSTANCE hInst,char *szTempName,char *szInitialDir,char *szDirName); // wspd148
void ProjektZustandProfilOpen(char *strProjekt,char *strZustand,char *strProfil); // wspdde

int  write_datplt(FILE *in,char *filename,BOOLEAN* exist_plot ,int read_first_line); // readprof

#endif


