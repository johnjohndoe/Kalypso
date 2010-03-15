#include <windows.h>
#include "xvt.h"

#include "resource.h"

#include "global_defs.h"
#include "global_types.h"

#include "wspalloc.h"
#include "leibo.h"
#include "Volume2.h"



#define LINKS  0
#define FLUSS  1
#define RECHTS 2

int VolCountStationen(GELAENDE *gelaende,double station)
{/* wieviel Werte gibt es bis zum ersten Auftreten von station (z.B. Trennfläche)
	 ->wird station auf MAXDOUBLE gesetzt so werden alle Stationen gezählt
 */
 GELAENDE *tmp;
 int zaehler=0;
 tmp = gelaende;
 while ((tmp!=NULL)&&(tmp->y <= station))
	{
	 zaehler++;
	 if(tmp->y == station) return zaehler;
	 tmp = tmp->next_ds;
	}
 return zaehler;
}
/*-------------------------------------------------------------------------------*/
void InitVolumen(VOLUMEN *vol)
 {
  vol->gel1=vol->gel2=vol->tr1=vol->tr2=NULL;
  vol->y1=vol->y2=vol->z1=vol->z2 = BCE_NAN;
  vol->ity1=vol->ity2=vol->itz1=vol->itz2=BCE_NAN;
  vol->lpdaten = MakeLNode(vol->lpdaten);
}
/*-------------------------------------------------------------------------------*/
VOLUMEN *  VolInitLPDaten(VOLUMEN *vol)
{
 if (vol!=NULL)  vol->lpdaten = MakeLNode(vol->lpdaten);
 return vol;
}
/*-------------------------------------------------------------------------------*/
VOLUMEN *  DeleteVolumen(VOLUMEN *vol)
{
 if (vol!=NULL)
 {
  if (vol->gel1) DeleteLKoord(vol->gel1);
  if (vol->gel2) DeleteLKoord(vol->gel2);
  if (vol->tr1)  DeleteLKoord(vol->tr1);
  if (vol->tr2)  DeleteLKoord(vol->tr2);
  vol->gel1=vol->gel2=vol->tr1=vol->tr2=NULL;

  if (vol->lpdaten) 	DeleteLNode(vol->lpdaten);
  vol->lpdaten =NULL;
  vol->y1=vol->y2=vol->z1=vol->z2 = BCE_NAN;
  vol->ity1=vol->ity2=vol->itz1=vol->itz2=BCE_NAN;
  //delete vol;
  //return NULL;
  /*Achtung: Win95 ::delete vol muss in dem Programmodul
    vorgenommen werden,indem es mit new vol angelegt wurde
    bzw. im Debug-Modus wird eine Fehlermeldung ausgegeben*/
  return vol;
 }
 return NULL;
}
/*-------------------------------------------------------------------------------*/
VOLUMEN *  VolDeleteLPDaten(VOLUMEN *vol)
{
 if (vol!=NULL)
 {
  if (vol->lpdaten) 	DeleteLNode(vol->lpdaten);
  vol->lpdaten =NULL;
  vol->y1=vol->y2=vol->z1=vol->z2 = BCE_NAN;
  vol->ity1=vol->ity2=vol->itz1=vol->itz2=BCE_NAN;
  return vol;
 }
 return NULL;
}
/*-------------------------------------------------------------------------------*/
int  MakeVolumeDatenLinks(VOLUMEN *vol)
{
 /*
	Rückgabe :  0 = Fehler in Modul 'FlaechenImLaengsprofilBerechnen()'
					1 = OK
 */

 GELAENDE *g1,*g2,*q1,*q2;
 int i,anzahl;
 g1=vol->gel1;
 g2=vol->gel2;

	 anzahl = VolCountStationen(g1,vol->tr1->y); //linke Trennfläche-Gelände 1
	 if (anzahl==1)   //linke Trennfl. auf erster Station =default-Fall
	  {
		vol->lpdaten->summe =0.0;
		vol->lpdaten->sum_auftrag =0.0;
		vol->lpdaten->sum_abtrag =0.0;
		vol->lpdaten->flaeche=NULL;
		return 1;
	  }
	 else if (anzahl)
	  {
		vol->lpdaten->querprof1=MakeLKoord(vol->lpdaten->querprof1,anzahl);
		q1=vol->lpdaten->querprof1;

		for (i=0;i<anzahl;i++)
		 {  // Daten kopieren : 1.Gelände
		  if ((g1)&&(q1))
			{
			 q1->y     = g1->y;
			 q1->z     = g1->z;
			 q1->index = g1->index;
			 if (g1->next_ds !=NULL)
				  g1 = g1->next_ds;
			 if (q1->next_ds !=NULL)
				  q1 = q1->next_ds;
			}
		 }
		if (g1)  // letztes Wertepaar merken
		  { vol->y1 = g1->pre_ds->y;	 vol->z1 = g1->pre_ds->z;
		  }

		anzahl = VolCountStationen(g2,vol->tr1->y); //linke Trennfläche- Gelände 2
		if (anzahl) //#2
		 {
		  vol->lpdaten->querprof2=MakeLKoord(vol->lpdaten->querprof2,anzahl);
		  q2=vol->lpdaten->querprof2;
		  for (i=0;i<anzahl;i++)
			{     // Daten kopieren
			 if ((g2)&&(q2))
			  {
				q2->y     = g2->y;
				q2->z     = g2->z;
				q2->index = g2->index;
			 if (g2->next_ds !=NULL)
				  g2 = g2->next_ds;
				if (q2->next_ds!=NULL)
					q2 = q2->next_ds;
			  }
			}
		  if (q2->y < vol->y1)   // ** interpolieren **
			 {
			  vol->lpdaten->querprof2=AppendLKoord(vol->lpdaten->querprof2,1);
			  if (q2->next_ds !=NULL)
					q2 = q2->next_ds;
			  q2->y=vol->y1;   // y-Wert Trennfl.

			  if (g2->pre_ds->z == g2->z)  //1.Fall: Geländewerte auf gleicher Höhe
				  q2->z=g2->z;
			  else   //2.Fall: Geländewerte auf unterschiedlicher Höhe
				{
				 if (g2->y != q2->pre_ds->y)
				  q2->z= (((g2->z - q2->pre_ds->z) * (vol->y1 - q2->pre_ds->y))
							 /(g2->y - q2->pre_ds->y)) + q2->pre_ds->z;
				 else q2->z=g2->z;//else kann nicht sein
				}
			  vol->ity1 = q2->y; //zwischenspeichern
			  vol->itz1 = q2->z;
			 }


		  vol->lpdaten->querprof2=AppendLKoord(vol->lpdaten->querprof2,1);
		  if (q2->next_ds !=NULL)
				{
				 q2->next_ds->y = vol->y1;  // Trennfläche von 1.Gelände
				 q2->next_ds->z = vol->z1;
				}
		 /* Flächenberechnung Dr. Leibo */
		  xvt_scr_set_busy_cursor();
		  if (! FlaechenImLaengsprofilBerechnen(vol->lpdaten))
			 {
              char buf[200];
              xvt_res_get_str(STR_FLAECHE_NOTE,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf); 
              
			  //xvt_dm_post_error("Eine Profildatei enthält Fehler.Flächenberechnung für dieses Profil nicht möglich.Daten in Datei:'CONVERT.ERR'abgelegt!");
			  error_txt(vol->lpdaten,LINKS); //intern nur zum debuggen

			  DeleteLNode(vol->lpdaten);
			  vol->lpdaten = NULL;
			  return 0;
			 }

		 } //-if (anzahl) #2
	  }
 return 1;
}

/*-------------------------------------------------------------------------------*/
int  MakeVolumeDatenFluss(VOLUMEN *vol)
{
 /*
	Rückgabe :  0 = Fehler in Modul 'FlaechenImLaengsprofilBerechnen()'
					1 = OK
 */

 GELAENDE *g1,*g2,*q1,*q2;
 int i,t1,t2,t3,t4,anzahl=0;

 g1=vol->gel1;
 g2=vol->gel2;

	 t1 = VolCountStationen(g1,vol->tr1->y); //linke Trennfläche-Gelände 1
	 t2 = VolCountStationen(g1,vol->tr1->next_ds->y); //rechte Trennfläche-Gelände 1
	 if (t2>=t1)
		anzahl = (t2-t1)+1;
	 if (anzahl > 0)
	  {
		vol->lpdaten->querprof1=MakeLKoord(vol->lpdaten->querprof1,anzahl);
		q1=vol->lpdaten->querprof1;
		for (i=1;i<t1;i++)  // g1 auf linke Trennfl. setzen
		  g1 = g1->next_ds;
		for (i=1;i<=anzahl;i++) // Daten kopieren: 1. Gelände
		 {
		  if ((g1)&&(q1))
			{
			 q1->y     = g1->y;
			 q1->z     = g1->z;
			 q1->index = g1->index;
			 if (q1->next_ds !=NULL)
				 q1 = q1->next_ds;
			 if (g1->next_ds !=NULL)
				 g1 = g1->next_ds;

			}
		 }
		if (g1)  // letztes Wertepaar merken
		  { vol->y1 = g1->pre_ds->y;	 vol->z1 = g1->pre_ds->z;
		  }
		t3 = VolCountStationen(g2,vol->tr1->y); //linke Trennfläche Gelände1 bzgl. Gelände 2
		t4 = VolCountStationen(g2,vol->tr1->next_ds->y); //rechte Trennfläche Gel.1 bzgl. Gelände 2
		if (t4>t3)  anzahl = t4-t3+1;
		else anzahl =0;
		vol->lpdaten->querprof2=MakeLKoord(vol->lpdaten->querprof2,anzahl+2); //incl. 2*Trennfl.
		q2=vol->lpdaten->querprof2;
		for (i=1;i<t3;i++)  // g2 auf linke Trennfl.Gelände1 setzen
		  g2 = g2->next_ds;

		 // 2 Punkte einfügen:

		//1.Punkt = linke Trennfläche 1.Gelände
		q2->y = vol->tr1->y;
		q2->z = vol->tr1->z;
		q2 = q2->next_ds;

		if (vol->tr1->y != g2->y  ) 		// 2. Punkt
		  {
			 vol->lpdaten->querprof2=AppendLKoord(vol->lpdaten->querprof2,1); // 2.Punkt
			 q2->y=vol->tr1->y;
					//1.Fall: Geländewerte auf gleicher Höhe
			  if (vol->tr1->y < g2->y  )
				{
				 if (g2->z == g2->pre_ds->z)  // gleiche Höhe
					q2->z=g2->z;
				 else //2.Fall: Geländewerte auf unterschiedlicher Höhe -> interpolieren
				 {
				  if (g2->y != g2->pre_ds->y)
					 q2->z= (((g2->z - g2->pre_ds->z) * (vol->tr1->y - g2->pre_ds->y))
									/ (g2->y - g2->pre_ds->y)) + g2->pre_ds->z;
				  else ; //kann nicht sein
				 }
				}
			  else if (vol->tr1->y > g2->y  )
				{
				 if (g2->z == g2->next_ds->z)  // gleiche Höhe
					q2->z=g2->z;
				 else //2.Fall: Geländewerte auf unterschiedlicher Höhe -> interpolieren
				 {
				  if (g2->y != g2->next_ds->y)
					 q2->z= (((g2->z - g2->next_ds->z) * (vol->tr1->y - g2->next_ds->y))
									/ (g2->y - g2->next_ds->y)) + g2->next_ds->z;
				 }
				}

			  vol->ity1 = q2->y;  //Punkt merken
			  vol->itz1 = q2->z;
			  q2 = q2->next_ds;
		  }

		for (i=0;i<anzahl;i++)// Daten kopieren
			{
			 if ((g2)&&(q2))
			  {
//				if ((g2->y >= vol->tr1->y)&&(g2->y <= vol->tr2->y))
				 { // liegen Punkte von g2 innerhalb der Trennflächen von g1
				  q2->y     = g2->y;
				  q2->z     = g2->z;
				  q2->index = g2->index;
				  if (g2->next_ds!=NULL)
					 g2 = g2->next_ds;
				  if (q2->next_ds!=NULL)
					 q2 = q2->next_ds;
				 }
			  }
			}
		if (g2->y < vol->tr1->next_ds->y)
		  {
			vol->lpdaten->querprof2=AppendLKoord(vol->lpdaten->querprof2,1); // 2.Punkt
			q2->y = vol->tr1->next_ds->y;

			if (g2->z == g2->next_ds->z)  // gleiche Höhe
				q2->z=g2->z;
			else
        q2->z = (((g2->z - g2->next_ds->z)*(g2->next_ds->y - vol->tr1->next_ds->y ))
          /(g2->next_ds->y - g2->y)) + g2->next_ds->z;
      vol->ity2 = q2->y;    //Punkt merken
      vol->itz2 = q2->z;
      q2 = q2->next_ds;
		  }
		q2->y = vol->tr1->next_ds->y; // rechte Trennfl.
		q2->z = vol->tr1->next_ds->z;

		 /* Flächenberechnung Dr. Leibo */
		  xvt_scr_set_busy_cursor();
		  if (! FlaechenImLaengsprofilBerechnen(vol->lpdaten))
			 {
              char buf[200];
              xvt_res_get_str(STR_FLAECHE_NOTE,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
			  //xvt_dm_post_error("Eine Profildatei enthält Fehler.Flächenberechnung für dieses Profil nicht möglich.Daten in Datei:'CONVERT.ERR'abgelegt!");
			  error_txt(vol->lpdaten,FLUSS); //intern nur zum debuggen

			  DeleteLNode(vol->lpdaten);
			  vol->lpdaten = NULL;
			  return 0;
			 }

	  } // -if (anzahl >0)
 return 1;
}
/*-------------------------------------------------------------------------------*/
int  MakeVolumeDatenRechts(VOLUMEN *vol)
{
 /*
	Rückgabe :  0 = Fehler in Modul 'FlaechenImLaengsprofilBerechnen()'
					1 = OK
 */

 GELAENDE *g1,*g2,*q1,*q2;
 int i,
	  t1,t2,t3,t4,
	  anzahl=0;

 g1=vol->gel1;
 g2=vol->gel2;

 t1 = VolCountStationen(g1,vol->tr1->next_ds->y); //rechte Trennfläche-Gelände 1
 t2 = VolCountStationen(g1,MAXDOUBLE); //alle Stationen bis Ende

 if (t2>t1)
		anzahl = (t2-t1)+1;
 if (anzahl>0)
  {
	 vol->lpdaten->querprof1=MakeLKoord(vol->lpdaten->querprof1,anzahl);
	 q1=vol->lpdaten->querprof1;

	 for (i=1;i<t1;i++)  // g1 auf rechte Trennfl. setzen
		  g1 = g1->next_ds;
	 for (i=1;i<=anzahl;i++) // Daten kopieren: 1. Gelände
		 {
		  if ((g1)&&(q1))
			{
			 q1->y     = g1->y;
			 q1->z     = g1->z;
			 q1->index = g1->index;
			 if (g1->next_ds !=NULL)
				 g1 = g1->next_ds;
			 if (q1->next_ds !=NULL)
				 q1 = q1->next_ds;
			}
		 }
	 if (g1)  // letztes Wertepaar merken
		  { vol->y1 = g1->pre_ds->y;	 vol->z1 = g1->pre_ds->z;
		  }

	 t3 = VolCountStationen(g2,vol->tr1->next_ds->y); //rechte Trennfläche v.Gel.1 bzgl. Gelände 2
	 t4 = VolCountStationen(g2,MAXDOUBLE);           // alle Stationen
	 if (t4>=t3)  anzahl = t4-t3;
	 else anzahl =0;

	 vol->lpdaten->querprof2=MakeLKoord(vol->lpdaten->querprof2,anzahl+1); //incl. Trennfl.
	 q2=vol->lpdaten->querprof2;
	 for (i=0;i<t3;i++)  // g2 auf linke Trennfl.Gelände1 setzen
		 if (g2->next_ds!=NULL)
			  g2 = g2->next_ds;

	 // 2 Punkte einfügen:

	 //1.Punkt = rechte Trennfläche 1.Gelände
	 q2->y = vol->tr1->next_ds->y;
	 q2->z = vol->tr1->next_ds->z;
	 if (q2->next_ds !=NULL)
		q2 = q2->next_ds;

	 if (vol->tr1->next_ds->y < g2->y  ) 		// 2. Punkt
		  {
			 vol->lpdaten->querprof2=AppendLKoord(vol->lpdaten->querprof2,1); // 2.Punkt
			 q2->y=vol->tr1->next_ds->y;
					//1.Fall: Geländewerte auf gleicher Höhe
			 if (g2->z == g2->pre_ds->z)  // gleiche Höhe
				  q2->z=g2->z;
			 else //2.Fall: Geländewerte auf unterschiedlicher Höhe -> interpolieren
				 {
				  if (g2->y != g2->pre_ds->y)
					 q2->z= (((g2->z - g2->pre_ds->z) * (vol->tr1->next_ds->y - g2->pre_ds->y))
									/ (g2->y - g2->pre_ds->y)) + g2->pre_ds->z;
				  else ; //kann nicht sein
				 }
			 vol->ity1 = q2->y;  //Punkt merken
			 vol->itz1 = q2->z;
			 q2 = q2->next_ds;
		  }
	 for (i=0;i<anzahl;i++)// Daten kopieren
		{
		 if ((g2)&&(q2))
			  {
				q2->y     = g2->y;
				q2->z     = g2->z;
				q2->index = g2->index;
				if (g2->next_ds !=NULL)
					g2 = g2->next_ds;
				if (q2->next_ds !=NULL)
					q2 = q2->next_ds;
			  }
		}
	 /* Flächenberechnung Dr. Leibo */
	 xvt_scr_set_busy_cursor();
	 if (! FlaechenImLaengsprofilBerechnen(vol->lpdaten))
			 {
              char buf[200];
              xvt_res_get_str(STR_FLAECHE_NOTE,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
			  //xvt_dm_post_error("Eine Profildatei enthält Fehler.Flächenberechnung für dieses Profil nicht möglich.Daten in Datei:'CONVERT.ERR'abgelegt!");
			  error_txt(vol->lpdaten,RECHTS); //intern nur zum debuggen

			  DeleteLNode(vol->lpdaten);
			  vol->lpdaten = NULL;
			  return 0;
			 }
  }  //--if (anzahl>0)
 else if (t2==t1) //Trennfl. auf letzter Station
  {
		vol->lpdaten->summe =0.0;
		vol->lpdaten->sum_auftrag =0.0;
		vol->lpdaten->sum_abtrag =0.0;
		vol->lpdaten->flaeche=NULL;
		return 1;
  }
 return 1;
}

/*-------------------------------------------------------------------------------*/
void  error_txt(LAENGSPROFIL *lp,int z)         //intern nur zum debuggen
{
  GELAENDE *tmp;
  FILE *out;
  out=fopen ("convert.err","a" );
  if (out !=NULL)
	{
	  switch (z)
		{
		 case 0: fprintf(out,"\tStation:%4.2lf -->LINKS\n",lp->station);
			break;
		 case 1: fprintf(out,"\tStation:%4.2lf -->FLUSS\n",lp->station);
			break;
		 case 2: fprintf(out,"\tStation:%4.2lf -->RECHTS\n",lp->station);
			break;
		};
			  tmp =lp->querprof1;
			  while (tmp !=NULL)
				{
				 fprintf(out," %6.2lf",tmp->y);
				 tmp = tmp->next_ds;
				}
			  fprintf(out,"\n");
			  tmp =lp->querprof1;
			  while (tmp !=NULL)
				{
				 fprintf(out," %6.2lf",tmp->z);
				 tmp = tmp->next_ds;
				}
			  fprintf(out,"\n\n");

			  tmp =lp->querprof2;
			  while (tmp !=NULL)
				{
				 fprintf(out," %6.2lf",tmp->y);
				 tmp = tmp->next_ds;
				}
			  fprintf(out,"\n");
			  tmp =lp->querprof1;
			  while (tmp !=NULL)
				{
				 fprintf(out," %6.2lf",tmp->z);
				 tmp = tmp->next_ds;
				}
			  fprintf(out,"\n-----------------------------------\n\n");
			  fclose(out);
	 }
}

/*-------------------------------------------------------------------------------*/
GELAENDE*  DeleteBCE_NAN(GELAENDE *querprof)
{
	GELAENDE *tmp,*res;

	tmp = querprof;
	while (tmp)
	 {
	  if ((tmp->y==BCE_NAN)||(tmp->z==BCE_NAN))// Tupel löschen
		{
		 if ((tmp->pre_ds!=NULL)&&(tmp->next_ds!=NULL))
		  {
			res = tmp->pre_ds;
			res->next_ds = tmp->next_ds;
			tmp->next_ds->pre_ds=res;
			delete tmp;
            tmp=res;//Dick 30.04.99
		  }
		 else
		  {
			if (tmp->pre_ds==NULL)
			  {
				querprof = tmp->next_ds;
				querprof->pre_ds=NULL;
				delete tmp;
                tmp=querprof;//Dick 30.04.99
                continue;
			  }
			else if (tmp->next_ds==NULL)
			  {
				res = tmp->pre_ds;
				res->next_ds = NULL;
				delete tmp;
                tmp=NULL;//Dick 30.04.99
			  }
		  }
		}
      if(tmp!=NULL)//Dick 30.04.99
          tmp=tmp->next_ds;
	 }
 return querprof;
}
