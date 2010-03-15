////////////////
// verluste.h //
////////////////

#ifndef _VERLUSTE_H_INCLUDED_
#define _VERLUSTE_H_INCLUDED_

struct VerlustDatei // wspd215, d216
 {
  char stationx[15];
  char verlustart[5][16];
  char verlust[5][15];
  VerlustDatei *next_verlust;
 };

 void Schreibe_Verlust_Datei_Knauf(char *buffer,char* counter); // util2
 void Schreibe_Verlust_Datei_BCE(void); // wspwin


#endif _VERLUSTE_H_INCLUDED_