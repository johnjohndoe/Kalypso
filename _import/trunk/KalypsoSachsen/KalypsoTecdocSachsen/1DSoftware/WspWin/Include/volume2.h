#ifndef _VOLUME2_H
#define _VOLUME2_H

typedef struct _VOLUMEN
{
	 GELAENDE *gel1,  // 1. Gelände
				 *gel2,  // 2. Gelände
         *tr1,   // 1. Trennfläche
         *tr2;   // 2. Trennfläche
   LAENGSPROFIL  *lpdaten;
   double y1,z1,  // letztes Wertepaar
			  y2,z2;
   double ity1,itz1,ity2,itz2;
} VOLUMEN;


//************Prototypen****************************
int VolCountStationen(GELAENDE *,double); // nur in volumen2.h
void InitVolumen(VOLUMEN *); // nur volume1.cpp
VOLUMEN *VolInitLPDaten(VOLUMEN *vol); // nur volume1.cpp
VOLUMEN *DeleteVolumen(VOLUMEN *); // nur volume1.cpp
VOLUMEN *VolDeleteLPDaten(VOLUMEN *); // nur volume1.cpp
int MakeVolumeDatenLinks(VOLUMEN *); // nur volume1.cpp
int MakeVolumeDatenFluss(VOLUMEN *); // nur volume1.cpp
int MakeVolumeDatenRechts(VOLUMEN *);  // nur volume1.cpp
void error_txt(LAENGSPROFIL *lp,int z); //intern nur zum debuggen
GELAENDE* DeleteBCE_NAN(GELAENDE *querprof);  // flaeche.cpp


//**************************************************
#endif // _VOLUME2_H