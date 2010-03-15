////////////////
// readprof.h //
////////////////

#ifndef _READPROF_H_INCLUDED_
#define _READPROF_H_INCLUDED_

typedef struct _PROFIL
{
  char name[20];
  double station;     
  int pk;
  int vzk;
  char dateiname[20];
  
}PROFIL;

void test_line9(char str[25]); // util, wspd166, m001, w116, w120, waspila
void write_line9(char str[25]); // wspw120, waspila
void ds_save_anzahl( PROFILDATA* pData );
int read_profildatei( WSP_PROFIL_LISTE *ptrWPL, DIRECTORY* dir, char* filename, BOOLEAN setGlobals = TRUE );
int save_profildatei( WSP_PROFIL_LISTE* ptrWPL, BOOLEAN getGlobals = TRUE );
int read_profil_dat(STRANG*);  // STR-Datei lesen // oft
int save_str_datei(void);   // STR-Datei sichern // dito

int read_Plot_DB( FILE* in, WSP_PROFIL_LISTE* ptrWPL ); // wspd162
int save_Plot_DB( WSP_PROFIL_LISTE* ptrWPL ); // wspw121

int change_Plot_profildatei(WSP_PROFIL_LISTE *ptrWPL); // wspd162


#endif _READPROF_H_INCLUDED_