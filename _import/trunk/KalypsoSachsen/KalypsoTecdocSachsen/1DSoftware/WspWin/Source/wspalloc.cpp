/********************************************************

	 Speicherverwaltung zum Modul Massebrechnung

*********************************************************/
#include <windows.h>
#include "xvt.h"

#include "global_types.h"

#include "wspalloc.h"

/*********************************************************************
	MakeLNode(LAENGSPROFIL *) :

	Speicher reservieren für ein Objekt des Typs LAENGSPROFIL
*********************************************************************/
LAENGSPROFIL* MakeLNode(LAENGSPROFIL *lp)
{
  LAENGSPROFIL *ptr_lp;

  ptr_lp            = new LAENGSPROFIL;  //Daten werden auf GlobalHeap angelegt(laut BORLAND Handbuch)
  if (ptr_lp!=NULL)
	{
	 ptr_lp->querprof1 = NULL;//new GELAENDE;
	 ptr_lp->querprof2 = NULL;//new GELAENDE;
	 ptr_lp->flaeche   = NULL;

	 ptr_lp->station    = 0.0;
	 ptr_lp->summe      = 0.0;
	 ptr_lp->sum_auftrag= 0.0;
	 ptr_lp->sum_abtrag = 0.0;
	 lp=ptr_lp;
	}
  return lp;
}
/*********************************************************************
	Init_LGelaende(GELAENDE *)

	Initialisierung eines Objektes vom Typ:GELAENDE
*********************************************************************/
void Init_LGelaende(GELAENDE *ptr_gelaende)
{
  ptr_gelaende->y = 0.0;
  ptr_gelaende->z = 0.0;
  ptr_gelaende->index=0;
  ptr_gelaende->next_ds = NULL;
  ptr_gelaende->pre_ds  = NULL;
}

/*********************************************************************
  MakeLKoord(GELAENDE *ptr_anfang,int anzahl)

  neuen Speicher reservieren für <anzahl> Objekte vom Typ: GELAENDE .
  Zum nachträglichen Anhängen AppendLKoord() benutzen !

*********************************************************************/
GELAENDE* MakeLKoord(GELAENDE *ptr_anfang,int anzahl)
{
 GELAENDE *ptr_gelaende,
			 *p_t,
			 *p_h;

 p_t = p_h = NULL;
 for (int i=1;i<=anzahl;i++)
	  {
	  ptr_gelaende = new GELAENDE;  //Daten werden auf GlobalHeap angelegt(BORLAND)
	  Init_LGelaende(ptr_gelaende);
	  if (!p_h)
		 {
		  p_h = ptr_gelaende;
		  p_t = ptr_gelaende;
		  ptr_gelaende->pre_ds = NULL;
		  ptr_gelaende->index=1;
		 }
	  else
		 {
		  p_t->next_ds = ptr_gelaende;
		  ptr_gelaende ->index = p_t->index+1;
		  ptr_gelaende->pre_ds = p_t;
		  p_t = ptr_gelaende;
		  p_t ->next_ds = NULL;
		 }
	  }
 ptr_anfang = p_h;
  /*hier sollte noch eine Fehlerbehandlung eingebaut werden
	 und der Rückgabewert angepaßt werden                   */
 return p_h;
}
/*********************************************************************
 GELAENDE * AppendLKoord(GELAENDE *ptr_anfang,int anzahl)

	<anzahl> zusätzliche Elemente vom Typ: GELAENDE
	an das Ende von ptr_anfang anhängen
*********************************************************************/
GELAENDE* AppendLKoord(GELAENDE *ptr_anfang,int anzahl)
{
 GELAENDE *ptr_gelaende,
			 *p_t;
 if (!ptr_anfang) return NULL;

 p_t = ptr_anfang;
 while (p_t->next_ds !=NULL)
	 p_t = p_t->next_ds;
 for (int i=1;i<=anzahl;i++)
	 {
	  ptr_gelaende = new GELAENDE;  //Daten werden auf GlobalHeap angelegt(BORLAND)
	  Init_LGelaende(ptr_gelaende);
	  if (p_t)
		 {
		  p_t->next_ds = ptr_gelaende;
		  ptr_gelaende ->index = p_t->index+1;
		  ptr_gelaende->pre_ds = p_t;
		  ptr_gelaende->next_ds = NULL;
		  p_t = p_t->next_ds;
		 }
	 }
 return ptr_anfang;
}

/*********************************************************************
			GELAENDE * DeleteLKoord(GELAENDE *ptr)
					 Speicher wieder freigeben
*********************************************************************/
GELAENDE* DeleteLKoord(GELAENDE *ptr)
{
 GELAENDE *p1,*p2;
 p1 = p2 = ptr;
 while (p1)
  {
	p2 = p1->next_ds;
	delete p1;
	p1=p2;
  }
 ptr=NULL;
 return ptr;
}
/*********************************************************************
	 DeleteLNode(LAENGSPROFIL *lp)

	 Freigabe des allozierten Speichers für ein Objekt vom
	 Typ: LAENGSPROFIL und aller darunterliegenden Objekte
	 des Typs GELAENDE
*********************************************************************/
void DeleteLNode(LAENGSPROFIL *lp)
{
 GELAENDE *p1,*p2;

 p1=p2=lp->querprof1;
 while (p1)
  {
	p2 = p1->next_ds;
	delete p1;
	p1=p2;
  }
 p1=p2=lp->querprof2;
 while (p1)
  {
	p2 = p1->next_ds;
	delete p1;
	p1=p2;
  }
 p1=p2=lp->flaeche;
 while (p1)
  {
	p2 = p1->next_ds;
	delete p1;
	p1=p2;
  }
 lp->querprof1 = NULL;
 lp->querprof2 = NULL;
 lp->flaeche = NULL;
 delete lp;
 lp = NULL;
}
/*****************************************************************************/
