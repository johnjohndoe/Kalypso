// Aufnehm.h: Include für aufnehm.cpp

#ifndef _AUFNEHM_H_
#define _AUFNEHM_H_

struct STRANG;

#define AUFNEHM_BUFFER_SIZE 8096

int profile_aufnehmen(STRANG *alte_strang=NULL); // wspd136 und wspm001
void selektion(void); // wspd136
int auswahl2( char szFiles[AUFNEHM_BUFFER_SIZE], int multi, int stringtable ); // aufnehm, read_cfg, readprof, wspd210, wspm001, profverl

#endif _AUFNEHM_H_