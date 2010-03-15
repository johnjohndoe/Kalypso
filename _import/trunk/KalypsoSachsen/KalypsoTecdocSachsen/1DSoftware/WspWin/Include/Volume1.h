#ifndef _VOLUME1_H
#define _VOLUME1_H

#include "volume2.h"

//************Prototypen****************************
int MakeVolumenBerechnung(WSP_PROFIL_LISTE *); // wspm001
int VolMakeGelDaten(WSP_PROFIL_LISTE *,VOLUMEN *); // hier

int GetProfilName(WSP_SLIST *liste,double station,char *profil_name); // hier

int VolReadGelaendeWerte(WSP_PROFIL_LISTE *,STR_DATA *,STR_DATA *); // hier
//**************************************************

int VolPrintProtokoll(FILE *out,VOLUMEN *vol,double abstand,double station,int action); // hier
void VolWriteHeader(FILE *,STR_DATA *data,STR_DATA *ref); // hier

#endif

