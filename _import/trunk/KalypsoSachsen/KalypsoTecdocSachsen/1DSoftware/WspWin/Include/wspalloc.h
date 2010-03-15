#ifndef _WSPALLOC_H_
#define _WSPALLOC_H_

/********************************************************

	 Speicherverwaltung zum Modul Massenberechnung

*********************************************************/

/*********************************************************************
	MakeLNode(LAENGSPROFIL *) :

	Speicher reservieren für ein Objekt des Typs LAENGSPROFIL

*********************************************************************/
LAENGSPROFIL*  MakeLNode(LAENGSPROFIL *lp); // in flaeche.cpp und volume2.cpp


/*********************************************************************
	Init_LGelaende(GELAENDE *)

	Initialisierung eines Objektes vom Typ: GELAENDE
*********************************************************************/
void Init_LGelaende(GELAENDE *ptr_gelaende); // nur hier



/*********************************************************************
  MakeLKoord(GELAENDE *ptr_anfang,int anzahl)

  Speicher reservieren für <anzahl> Objekte vom Typ: GELAENDE
*********************************************************************/
GELAENDE* MakeLKoord(GELAENDE *ptr_anfang,int anzahl); // in flaeche.cpp, volume1/2.cpp


/*********************************************************************
	AppendLKoord(GELAENDE *ptr, int anzahl)

	'anzahl' neue Elemente vom Typ:GELAENDE an ptr anhängen
*********************************************************************/
GELAENDE* AppendLKoord(GELAENDE *ptr_anfang,int anzahl); // in volume2.cpp



/*********************************************************************
	DeleteLKoord(GELAENDE *ptr)

	Freigabe des allozierten Speichers für ein Objekt vom Typ:GELAENDE
*********************************************************************/
GELAENDE* DeleteLKoord(GELAENDE *ptr); // in volum2.cpp



/*********************************************************************
	 DeleteLNode(LAENGSPROFIL *lp)

	 Freigabe des allozierten Speichers für ein Objekt vom
	 Typ: LAENGSPROFIL und aller darunterliegenden Objekte
	 des Typs GELAENDE
*********************************************************************/
void  DeleteLNode(LAENGSPROFIL *lp); // in flaeche.cpp, volume2.cpp


#endif // _WSPALLOC_H_