/****************************************************************************
*             LIST .CPP                                                     *
*             14.10.1994                                                    *
****************************************************************************/
#include "global_defs.h"

#include <iostream.h>
#include <stdio.h>
#include <dos.h>
#include "windows.h"
#include <tchar.h>

#include <string.h>
#include <math.h>

#include "xvt.h"


#include "global_types.h"
#include "global_vars.h"
#include "..\..\wspdlg\Include\export.h"
#include "slist.h"

#include "l_typen.h"
#include "resource.h"

#include "util.h"
#include "list.h"
#include "paint.h"
#include "typen.h"
#include "leibo.h"

#include "strang.h"

#define OFFSET 1
#define FAKTOR 2
#define NEU    3
#define ANFANG 1
#define ENDE   2

PNT profil_pnt[STRANGANZAHL];
Rem_Station rem_station;

extern  Scroller scr;
SLIST title_list;
extern  int typ[TYPE_SIZE];//Dick 8.12.98
extern  int ds_info[TYPE_SIZE];
extern int anzahl_ds;

extern WINDOW win_list2;//Listbutton33 in Alpha/Grafik nur in: DeleteStation
extern Paint *paint;
ZOOM zoom_info;//extern ZOOM zoom_info; Ref.war in zoom.cpp
extern MinMax pmm;
extern BOOLEAN checkbox,checkbox_alle, checkbox_spiegel;
extern int draw_liste[50];
extern SLIST slist_comment;
extern BOOLEAN fehl_korr=FALSE;//Hilfsmerker für die Fehlerkorrektur beim Trennflächen
//im ZOOM-Modus
extern WINDOW scroll_116,WIN_117,WIN130;

extern char Plot_Ini_Datei[151];//Dick 16.12.98

extern BOOLEAN berechnen,editieren;
extern struct _MMP mmp;//Dick 30.04.99
extern BOOLEAN win117_mouse_move;//Dick 30.04.99
extern BOOLEAN win117_mouse_move_aktiv;//Dick 30.04.99
/***************************************************/
List::List()
{
  ptr_anfang = new Profildatei;
  ptr_anfang->next =NULL;
  ptr_profil=ptr_ende=ptr_anfang;
  
  ptr_anfang->datensatz=new Koord;
  ptr_anfang->datensatz->next_ds=NULL;
  ptr_anfang->datensatz->pre_ds=NULL;
  pp =ptr_anfang->datensatz;
  pp->ds_nr=0;
  pp->attr=0;
  pp->status=-1;
  pp->pre_x =0;
  pp->pre_y =0;
  p_h = p_t = pp;
  
  ptr_anfang->ds_nummer=0;
  ptr_anfang->profiltyp=0;
  ptr_anfang->status=-1;
  
  slist_dummy=NULL;
  dummy_typ=0;
}

List::~List(void)
{
  if (ptr_anfang!=NULL)
    while (ptr_anfang)
    {
      ptr_profil = ptr_anfang;  // retten von: ptr_anfang
      
      if (ptr_anfang!=NULL)
      {
        ptr_anfang = ptr_anfang->next;
        if (ptr_profil->datensatz!=NULL)
        {
          p_h=pp=ptr_profil->datensatz;
          p_h=p_h->next_ds;
          while (pp)
          {
            delete pp;
            pp=p_h;
            if (p_h!=NULL)
              p_h=p_h->next_ds;
          }
        }
        pp=p_h=p_t=NULL;
        ptr_profil->datensatz=NULL;
        
        if (ptr_profil!=NULL)
        {
          delete ptr_profil;
          ptr_profil=NULL;
        }
      } //-if...
    } //-while...
    if (slist_dummy!=NULL)
    {
      DeleteWspSList((_WSP_SLIST*)slist_dummy);
      slist_dummy=NULL;
      dummy_typ=0;
    }
}

/***************************************************/
Profildatei* List::get_anfang(void)
{
  return ptr_anfang;
}
/***************************************************/
Koord* List::get_zoom_ptr(int typ)
{
  if (typ ==ANFANG)
    return zoom_left_x;
  else if (typ ==ENDE)
    return zoom_right_x;
  else return NULL;
}
/***************************************************/
void List::DeleteList(void)
{
  if (ptr_anfang!=NULL)
    while (ptr_anfang)
    {
      ptr_profil = ptr_anfang;
      ptr_anfang = ptr_anfang->next;
      p_h = ptr_profil->datensatz;
      while (p_h)
      {
        pp=p_h;
        p_h = p_h->next_ds;
        pp=NULL; //!!
        delete pp;
      }
      pp=p_t=NULL;
    }
    ptr_anfang=ptr_profil=ptr_ende=NULL;
}
/***************************************************/
void List::DeleteNode( int nummer, int* ds_info, int* typ )
// Parameter:
//      int nummer: Nummer des zu löschenden Datensatzes
//      int* ds_info, *typ; Zeiger auf ds_info[TYPE_SIZE], typ[TYPE_SIZE]
{
  Profildatei* p_help = NULL;
  
  ptr_profil = ptr_anfang;
  
  while( ( ptr_profil->ds_nummer < nummer - 1 ) && ptr_profil )
    ptr_profil = ptr_profil->next;
  
  if( ( ptr_profil == ptr_anfang ) && ( nummer == 1 ) && ( ptr_profil->profiltyp == 1 ) )  //erster Datensatz:Gelaende
  {
    char buf[200];
    xvt_res_get_str(STR_DELETE_NODE,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf); //"Dieser Datensatz kann nicht gelöscht werden!"
  }
  else if( ptr_profil )
  {
    if( ptr_profil->next )
      p_help = ptr_profil->next;
    if( p_help->next )
    {
      ptr_profil->next = p_help->next;
      while( ptr_profil->next )
      {
        ptr_profil = ptr_profil->next;
        ptr_profil->ds_nummer--;
      }
      
      for( int i = nummer; i <= ds_info[0]; i++ )
      {
        ds_info[i] = ds_info[i + 1];
        typ[i] = typ[i + 1];       
      }
      
      ds_info[0]--;
    }
    else
    {
      for ( int i = nummer; i <= ds_info[0]; i++ )
        ds_info[i] = ds_info[i + 1];
      ds_info[0]--;
      ptr_ende = ptr_profil;
      ptr_profil->next = NULL;
    }
  }
  else 
  {  
    char buf[200];
    xvt_res_get_str(STR_DATSATZ_NOTEXIST,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf); //"Dieser Datensatz existiert nicht!"
    return;
  }
  p_h = p_help->datensatz;
  while( p_h )
  {
    pp = p_h;
    p_h = p_h->next_ds;
    delete pp;
  }
  delete p_h;
}; // DeleteNode

void List::MakeNewNode( int n1 )
// neuen Knoten einrichten -für n1=-1 wird neue list generiert
{
  ptr_profil = new Profildatei;
  ptr_profil->daten_info[0][0]='\0';
  ptr_profil->daten_info[1][0]='\0';
  ptr_profil->daten_info[2][0]='\0';
  ptr_profil->status = -1;
  ptr_profil->datensatz = new Koord;
  pp = ptr_profil->datensatz;
  pp->ds_nr = 1;
  pp->x = BCE_NAN;
  pp->y = BCE_NAN;
  pp->pre_x = 0;
  pp->pre_y = 0;
  pp->attr = 0;
  pp->status = -1;
  pp->x_pnt = pp->y_pnt = 0;
  pp->next_ds = NULL;
  pp->pre_ds = NULL;
  p_h = p_t = pp;
  
  if( n1 > 0 )
    ptr_profil->ds_nummer = n1;
  else
    ptr_profil->ds_nummer = 1;
  
  if( !ptr_anfang || n1 < 0 )
    ptr_anfang = ptr_profil;
  else
    ptr_ende->next = ptr_profil;
  
  ptr_ende = ptr_profil;
  ptr_ende ->next = NULL;
}; // MakeNewNode

/***************************************************/
void List::MakeNewKoord(int anzahl)
{
  for (int i=2;i<=anzahl;i++)
  {
    pp = new Koord;
    if (pp!=NULL)
    {
      pp->ds_nr = i;
      pp->x=BCE_NAN;
      pp->y=BCE_NAN;
      pp->pre_x=0;
      pp->pre_y=0;
      pp->x_pnt =0; // kompatibilität zu list_dll 
      pp->y_pnt =0; // kompatibilität zu list_dll 
      
      pp->attr=0;
      pp->status =-1;
      
      p_t->next_ds = pp;
      p_t->next_ds->pre_ds = p_t;
      p_t = pp;
      p_t->next_ds = NULL;
    }
  }
}

/**************************************************/
void List::NewKoord(int ds_nummer, int Pos)   //neuen Knoten ans Ende ode Pos von Datensatz ds_nummer anhängen
{
  Koord *help, *help2,*pp_help;
  Profildatei *profil_help;
  profil_help = ptr_anfang;
  while ((profil_help->ds_nummer < ds_nummer)&&(profil_help !=NULL))
    profil_help = profil_help->next;
  help = profil_help->datensatz;
  help2 = profil_help->datensatz;
  
  if(help2!=NULL)//Dick 10.08.98
  {
    while (help2->next_ds != NULL)
      help2 = help2->next_ds;
    if(help2->ds_nr==1 && help2->x==BCE_NAN)
      return; 
    
    if (help2->ds_nr>=KOORDINPROFILANZAHL-1)
    {
      //xvt_dm_post_error("Es können nicht mehr als 200 y-Werte angelegt werden !");
      char buf[200];
      xvt_res_get_str(STR_200_WERTE,buf,sizeof(buf));
      xvt_dm_post_note("%s",buf);
      return;
    }
  }
  //else //Dick 10.08.98
  //{
  pp_help = new Koord;
  pp_help->x=BCE_NAN;          // initialisieren
  pp_help->y=BCE_NAN;
  pp_help->pre_x=0;
  pp_help->pre_y=0;
  pp_help->status =-1;
  pp_help->attr=0;
  pp_help->ds_nr=0;//Dick 5.02.99
  pp_help->next_ds = NULL;
  if(Pos>=0) //Wenn anstat Sortierung verwendet wird 
  {
    ds_info[profil_help->ds_nummer]++;
    pp_help->attr=1;
  }
  
  // Position :
  if (Pos == 1 || help == NULL)
  {
    pp_help->pre_ds = NULL;
    pp_help->next_ds = help;
    if (help != NULL)
      help->pre_ds = pp_help;
    profil_help->datensatz = pp_help;
    while(pp_help!=NULL)
    {        
      pp_help->ds_nr ++;
      pp_help=pp_help->next_ds;
    }
    
    return;
  }
  
  int i = 1;
  
  while (help->next_ds != NULL)
  {
    i++;
    if (i == Pos)
      break;
    help = help->next_ds;
  }
  
  
  help2 = help->next_ds;
  pp_help->pre_ds = help;
  help->next_ds = pp_help;
  pp_help->ds_nr = help->ds_nr+1;
  if (help2 != NULL)
    help2->pre_ds = pp_help;
  pp_help->next_ds = help2;
  pp_help=pp_help->next_ds;
  while(pp_help!=NULL)
  {        
    pp_help->ds_nr ++;
    pp_help=pp_help->next_ds;
  }
  //profil_help->datensatz = pp_help;
  //}
  delete [] pp_help;
}
/***************************************************/

void List::InitKoord(int ds_nummer,double wert)
/*Füllen eines Datensatzes mit y-Werten*/

{
  Koord *help;
  ptr_profil = ptr_anfang;
  while ((ptr_profil->ds_nummer < ds_nummer)&&(ptr_profil !=NULL))
    ptr_profil = ptr_profil->next;
  help = ptr_profil->datensatz;
  while (help!=NULL)
  {
    help->y  = wert;
    help=help->next_ds;
  }
}

/***************************************************/
void List::WriteTypDaten(int nummer,int typ,char *name) // ungleich zu list_dll.cpp
{
  /* char *name  wird momentan nur für GELAENDE2 benötigt*/
  char *s1, *s2;
  char*tmp = NULL;
  s1="\0";
  s2="\0";
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->ds_nummer < nummer))
    ptr_profil = ptr_profil->next;
  
  ptr_profil->profiltyp = typ;
  
  for (int i=0;i<150;i++)
    ptr_profil->daten_info[2][i]=' ';
  ptr_profil->daten_info[2][150]='\0';
  
  for (i=2;i<26;i=i+3)                  // Zeile 3: 8 beliebige Integerzahlen ,
    ptr_profil->daten_info[2][i]='0';  // 9.Ziffer:Datenblockabhängig,bei GELAENDE=0
  
  ptr_profil->daten_info[0][0]=ptr_profil->daten_info[1][0]='\0';
  
  switch (typ)
  {
  case GELAENDEHOEHE:
    s1 = "GELAENDE"; s2="HOEHE";
    ptr_profil->daten_info[2][26] ='0';
    break;
  case TRENNFLAECHEN:
    s1="TRENNFLAECHEN";  s2="[-]";
    ptr_profil->daten_info[2][26] ='0';
    MakeNewKoord(2);
    break;
  case RAUHIGKEIT:
    s1="RAUHEIT";   s2="KS";
    ptr_profil->daten_info[2][26] ='0';
    break;
  case RAUHIGKEIT_KST:
    s1="RAUHEIT";   s2="KST";
    ptr_profil->daten_info[2][26] ='0';
    break;
  case DURCHST_BEREICH:
    s1="DURCHSTROEMTE";  s2="BEREICHE";
    ptr_profil->daten_info[2][26] ='0';
    ptr_profil->daten_info[2][27] ='\0';
    MakeNewKoord(2);
    break;
  case UK_BRUECKE:
    s1="UK-BRUECKE";     s2="\0";
    ptr_profil->daten_info[2][26] ='0';
    break;
  case OK_BRUECKE:
    s1="OK-BRUECKE";
    
    if( LWA_PROJEKT )
      s2="0.5 0.8\0";//Dick 23.08.99
    else
      s2="\0";
    
    ptr_profil->daten_info[2][26] ='0';
    break;
  case AXM:
    s1="AX";         s2="[-]";
    ptr_profil->daten_info[2][26] ='0';
    break;
  case AYM:
    s1="AY";         s2="[-]";
    ptr_profil->daten_info[2][26] ='0';
    break;
  case DPM:
    s1="DP";         s2="[-]";
    ptr_profil->daten_info[2][26] ='0';
    break;
  case OK_WEHRS:
    s1="OK-WEHR";  s2="\0";
    ptr_profil->daten_info[2][26] ='0';
    break;
    /* nur LWA*/
  case KREISSEGM:
    s1="KREISSEGMENT";    s2="\0";
    ptr_profil->daten_info[2][25]='1';
    ptr_profil->daten_info[2][26]='5';
    //ptr_profil->daten_info[2][26] ='0';
    MakeNewKoord(3);
    break;
  case SVA_WERT:
    s1="SVA-WERT";    s2="[-]";
    ptr_profil->daten_info[2][26] ='0';
    break;
  case OK_GELAENDE:
    s1="OK-GELAENDE";   s2="HOEHE";
    ptr_profil->daten_info[2][26] ='0';
    break;
  case KASTEN:
    s1="KASTENPROFIL";    s2="\0";
    ptr_profil->daten_info[2][26] ='0';
    MakeNewKoord(4);
    break;
  case SCHUETZ:
    s1="SCHUETZ";
    s2="0.8 0.0 SCHMIDT 90.0000 \0";//Bley 9.1.2001
    MakeNewKoord(1);
    break;
  case LWA_FELDER:
    s1="LWA-FELDER";    s2="[-]";
    ptr_profil->daten_info[2][26] ='0';
    break;
  case GAUSSRUECK:
  /*   s1="GAUSSPROFIL";   s2="MIT RUECKSPRUENGEN";
  ptr_profil->daten_info[2][26] ='0';
    break; */
  case GAUSS:
    s1="LAGE-KOORDINATEN";    s2="*STATIONSWERT-RECHTSWERT-HOCHWERT";
    ptr_profil->daten_info[2][26] ='0';
    MakeNewKoord(3);
    break;
    /*nur BCE*/
  case PUNKT_NR:
    s1="PUNKT-NR";      s2="\0";
    ptr_profil->daten_info[2][26] ='0';
    break;
    /*  case STATION:
    s1="STATION";    s2="WSP-HOEHE NN+m";
    ptr_profil->daten_info[2][26] ='0';
    MakeNewKoord(2);
    break;*/
  case STATION:
    //      s1="STATION";    
    s1="WSP-HOEHE NN+m";
    //s2="WSP-HOEHE NN+m";
    if (name !=NULL) 
    {
      tmp=new char[255];
      // strcpy(tmp,"WSP-HOEHE NN+m");
      // strcat(tmp," aus Ber.variante:");
      strcpy(tmp,name);
      s2 = tmp;
    }
    else 
      //      s2="WSP-HOEHE NN+m";
      s2="\0";
    ptr_profil->daten_info[2][26] ='0';
    MakeNewKoord(2);
    break;
  case STATION_FIX: //Dick 5.02.99
    
    s1="WSP-Messung NN+m";
    
    if (name !=NULL) 
    {
      tmp=new char[255];
      
      strcpy(tmp,name);
      s2 = tmp;
    }
    else 
      s2="\0";
    ptr_profil->daten_info[2][26] ='0';
    MakeNewKoord(2);
    break;
  case WASSERSP1:
    s1="WASSERSPIEGEL MNN.";      s2="HQ1-BEST";
    ptr_profil->daten_info[2][26] ='0';
    MakeNewKoord(2);
    break;
  case WASSERSP100:
    s1="WASSERSPIEGEL MNN.";      s2="HQ100-BEST";
    ptr_profil->daten_info[2][26] ='0';
    MakeNewKoord(2);
    break;
  case WASSERSP5:
    s1="WASSERSPIEGEL MNN.";      s2="HQ5-BEST";
    ptr_profil->daten_info[2][26] ='0';
    MakeNewKoord(2);
    break;
  case BORDVOLL:
    s1="BORDVOLL";  s2="\0";
    ptr_profil->daten_info[2][26] ='0';
    MakeNewKoord(2);
    break;
  case MODELLGRENZEN:
    s1="MODELLGRENZEN"; s2="\0";
    ptr_profil->daten_info[2][26]='0';
    MakeNewKoord(2);
    break;
  case TRENN_WEHR:
    s1="TRENNLINIE";     s2="WEHR";
    ptr_profil->daten_info[2][26] ='0';
    MakeNewKoord(2);
    break;
    
  case MAUL:
    s1="MAULPROFIL";           s2="[-]";
    ptr_profil->daten_info[2][26] ='9';
    MakeNewKoord(5);
    break;
  case EIPROFIL:
    s1="EI";       s2="[-]";
    ptr_profil->daten_info[2][26] ='8';
    MakeNewKoord(5);
    break;
  case KREIS:
    s1="KREIS";          s2="[-]";
    ptr_profil->daten_info[2][26] ='7';
    MakeNewKoord(4);
    break;
  case ARMCO84:
    s1="ARMCO84";     s2="MA\0";
    ptr_profil->daten_info[2][25] ='2';
    ptr_profil->daten_info[2][26] ='0';
    MakeNewKoord(6);
    break;
  case ARMCO71:
    s1="ARMCO71";       s2="[-]";
    ptr_profil->daten_info[2][25] ='1';
    ptr_profil->daten_info[2][26] ='9';
    MakeNewKoord(5);
    break;
  case NWRINNE:
    s1="NW-RINNE";
    s2="[-]";
    ptr_profil->daten_info[2][25] ='1';
    ptr_profil->daten_info[2][26] ='6';
    MakeNewKoord(3);
    break;
    
  case FUELLHOEHE:
    break;
    
  case NAU_MED:
    break;
    
  case TRAPEZ:
    s1="TRAPEZ";
    s2="[-]";
    ptr_profil->daten_info[2][26] ='6';
    MakeNewKoord(6);
    break;
  case GELAENDE2:
    s1 = "2.GELAENDE";
    s2 = name;
    //s2="\0";
    ptr_profil->daten_info[2][26] ='0';
    break;
  case FLAECHE:
    s1 = "FLAECHE";
    s2="\0";
    ptr_profil->daten_info[2][26] ='0';
    break;
  case COMMENT:
    s1="Kommentar:";
    s2="[-]";
    ptr_profil->daten_info[2][25]='1';
    ptr_profil->daten_info[2][26]='7';
    break;
    
  case UNKNOWN:
    s1="unbekannt";
    s2="\0";
    ptr_profil->daten_info[2][26] ='0';
    break;
  case RECHTSWERT:
    s1="RECHTSWERT";    s2="[-]";
    ptr_profil->daten_info[2][26] ='0';
    break;
  case HOCHWERT:
    s1="HOCHWERT";
    s2="[-]";
    ptr_profil->daten_info[2][26] ='0';
    break;
  case BUHNE:
    s1="BUHNEN";
    s2="";
    ptr_profil->daten_info[2][26]='0';
    break;
  default:break;
  }
  strcpy(ptr_profil->daten_info[0],s1);  
  strcpy(ptr_profil->daten_info[1],s2);
  if (tmp !=NULL) delete[] tmp;
}
/***************************************************/
void List::Sort(double x,double y,unsigned ds_not)
{
  Koord *help,*new_pp;
  
  ptr_profil = ptr_anfang;
  
  while ((ptr_profil !=NULL)&& ((ptr_profil->profiltyp < MAUL)||(ptr_profil->profiltyp ==UNKNOWN))    )
  {
    if(ptr_profil->ds_nummer !=(INT)ds_not)
    {
      switch (ptr_profil->profiltyp)
      {
      case TRENNFLAECHEN:
      case BORDVOLL:
      case MODELLGRENZEN:
        {
        }
        break;
      case DURCHST_BEREICH:
        {
        }
        break;
      default:
        {
          new_pp = new Koord;
          
          pp=ptr_profil->datensatz;
          
          if (ptr_profil->profiltyp > 1)   // nicht GELAENDE
            y=BCE_NAN;
          new_pp->x = x;
          new_pp->y = y;
          new_pp->pre_x = 0;
          new_pp->pre_y = 0;
          ds_info[ptr_profil->ds_nummer]++;
          
          
          while ((pp->next_ds != NULL)&&(pp->x<=x))
            pp = pp->next_ds;
          help = pp;
          if ((pp->next_ds==NULL)&&(pp->x<=x))   // ListenENDE
          {
            pp->next_ds=new_pp;
            new_pp->next_ds = NULL;
            new_pp->pre_ds=help;
            new_pp->ds_nr = pp->ds_nr +1;
          }
          else
          {
            if (pp->pre_ds!=NULL)
            {
              pp->pre_ds->next_ds = new_pp;
              new_pp->next_ds = pp;
              new_pp->pre_ds = pp->pre_ds;
              pp->pre_ds = new_pp;
              new_pp->ds_nr = pp->ds_nr ;
            }
            else  // Listenanfang !
            {
              if (ptr_profil->datensatz == pp)
              {
                ptr_profil->datensatz = new_pp;
                new_pp->next_ds = pp;
                pp->pre_ds = new_pp;
                new_pp->pre_ds = NULL;
                new_pp->ds_nr = pp->ds_nr;
              }
            }
            while (pp!=NULL)
            {
              if (pp->next_ds ==NULL)
                pp->ds_nr = pp->pre_ds->ds_nr+1;
              else
                pp->ds_nr =pp->next_ds->ds_nr;
              pp = pp->next_ds;
            }
          }
          
        }
        break;
      };
    }
    ptr_profil = ptr_profil->next;
  }
  
  
  
  if (x < pmm.minX)
  {
    GetMinMax(&pmm,scr.datensatz);
    Update_Scr(0,&scr);
  }
  
}
/****************************************************************************/
void List::CopyStation(int ds_nummer)
/*
kopiert Stationswerte von Gelände-NN nach Datensatz: ds_nummer
*/
{
  Koord *help;
  BOOLEAN weiter=TRUE;
  
  ptr_profil = ptr_anfang;
  help = ptr_profil->datensatz;   // zeigt auf Gelände-NN
  
  while ((ptr_profil !=NULL)&&(ptr_profil->ds_nummer < ds_nummer))
    ptr_profil = ptr_profil->next;
  if (ptr_profil!=NULL)
    pp=ptr_profil->datensatz;     // zeigt auf gewählten Datensatz
  
  while (weiter)
  {
    if (help !=NULL)
    {
      if (pp!=NULL)
      {
        pp->x = help->x;
        pp = pp->next_ds;
        help = help->next_ds;
      }
      else weiter =FALSE;
    }
    else weiter = FALSE;
  }
  
}
/***************************************************/
int List::GetSonderProfilTyp(char * typ)
{
/* Rückgabe: FALSE falls im Profil kein Sonderprofil
(Typ:60-80) vorhanden, ansonsten TRUE und in char *typ
steht der Profiltypname
  */
  Profildatei *ptr_help;
  
  
  int typ_nr=0;
  char buf[50];
  
  typ[0]='\0';  //init!
  
  ptr_help = ptr_anfang;
  while (ptr_help !=NULL)
  {
    if ((ptr_help->profiltyp >= MAUL)&&(ptr_help->profiltyp <= TRAPEZ))
    {
      typ_nr = ptr_help->profiltyp;
      if((xvt_res_get_str( 1000+ typ_nr, buf, sizeof(buf)))==NULL)
        xvt_dm_post_error("Can't read string resource");
      else
        strcpy(typ,buf);
    }
    ptr_help = ptr_help->next;
  }
  return typ_nr;
}
/***************************************************/

/*!
 * Rückgabe:Position (ds_nr) des Stationswertes in der Liste
 * Stationen mit Höhenwert:BCE_NAN werden ignoriert !!!!
 *
 * @param station : gesuchte x-Koordinate im Datensatz
 * @param ds_nr : Nummer des Datensatz
 *
 * @return int  : Nummer der Koordinate
 */
int List::ExistStation(double station,int ds_nr)
{
  Profildatei* pd = ptr_anfang;
  
  while( pd && pd->ds_nummer < ds_nr )
    pd = pd->next;
  
  Koord* ptr_k=pd->datensatz;
  
  while( ptr_k )
  {
    if( fabs( ptr_k->x - station ) < PRECISION_KRD && ptr_k->y != BCE_NAN )
      return ptr_k->ds_nr;

    ptr_k = ptr_k->next_ds;
  }
  
  return 0;
}
/***************************************************/
/***************************************************/
int List::ExistStationX(double station,int ds_nr)
{
/* Rückgabe:Position (ds_nr) des Stationswertes in der Liste

  Stationen mit Höhenwert:BCE_NAN werden NICHT ignoriert !!!!   */
  
  int gefunden=-1;
  Koord *ptr_k;
  Profildatei *pd;
  
  pd = ptr_anfang;
  
  while ((pd !=NULL)&&(pd->ds_nummer < ds_nr))
    pd = pd->next;
  
  ptr_k=pd->datensatz;
  
  while(ptr_k !=NULL)
  {
    if ((ptr_k->x == station)&&(gefunden==-1))
      gefunden = ptr_k->ds_nr;
    ptr_k = ptr_k->next_ds;
  }
  
  return gefunden;
}
/***************************************************/
void List::Delete_Min_Koord(int ds_nr)
{
  /* BCE_NAN -Werte aus Datensatz löschen und ds_info[] aktualisieren */
  Koord *ptr_k;
  Profildatei *pd;
  
  pd = ptr_anfang;
  while ((pd !=NULL)&&(pd->ds_nummer < ds_nr))
    pd = pd->next;
  ptr_k=pd->datensatz;
  while(ptr_k !=NULL)
  {
    if ((ptr_k !=NULL)&&(ptr_k->y==BCE_NAN))
    {
      DeleteKoord(ptr_k->ds_nr, ds_nr,ptr_k->x,ptr_k->y);
      ptr_k=pd->datensatz;
    }
    else
      ptr_k = ptr_k->next_ds;
  }
}
/***************************************************/
void  List::DeleteKoord(int ds)
{
  Koord *help;
  ptr_profil = ptr_anfang;
  
  while ((ptr_profil !=NULL)&&(ptr_profil->ds_nummer < ds))
    ptr_profil = ptr_profil->next;
  if (ptr_profil && ptr_profil->datensatz )
  {
    pp=ptr_profil->datensatz;
    while (pp !=NULL)
    {
      help = pp;
      pp = pp->next_ds;
      delete help;
    }
    ptr_profil->datensatz=NULL;
  }
}
/***************************************************/
int List::DeleteKoord(int pos,int ds_nummer,double x,double y)
{
  Koord *ptr_k,*help;
  Profildatei *pd;
  int del=0;
  pd = ptr_anfang;
  while ((pd !=NULL)&&(pd->ds_nummer < ds_nummer))
    pd = pd->next;
  if (pd !=NULL)
  {
    ptr_k=pd->datensatz;
    
    while ((ptr_k->ds_nr < pos)&&(ptr_k->next_ds !=NULL))
      ptr_k= ptr_k->next_ds;
    
    {
      if ((ptr_k->next_ds == NULL)&&(ptr_k->pre_ds!=NULL))  // letztes Element
      {
        if (ptr_k->x == x)
        {
          help = ptr_k;
          ptr_k = ptr_k->pre_ds;
          ptr_k->next_ds = NULL;
          delete help;
          ds_info[ds_nummer]--;
          if (ds_info[ds_nummer] <=15)
            scr.anzahl--;
          del =1;
        }
      }
      else
      {
        if ((ptr_k->pre_ds ==NULL)&&(ptr_k->next_ds != NULL))  //erstes Element
        {
          if (ptr_k->x == x)
          {
            help = ptr_k;
            pd->datensatz = ptr_k->next_ds;
            ptr_k=ptr_k->next_ds;
            ptr_k->pre_ds = NULL;
            delete help;
            ds_info[ds_nummer]--;
            if (ds_info[ds_nummer] <=15)
              scr.anzahl--;
            del =1;
            while (ptr_k != NULL)
            {
              ptr_k->ds_nr --;
              ptr_k = ptr_k->next_ds;
            }
          }
        }
        else    // sonst.
        {
          if (ptr_k->x == x)
          {
            help = ptr_k;
            if (ptr_k->pre_ds !=NULL)
              ptr_k->pre_ds->next_ds = ptr_k->next_ds;  //Elem.aushängen
            if (ptr_k->next_ds !=NULL)
              ptr_k= ptr_k->next_ds;
            if (ptr_k->pre_ds !=NULL)
              ptr_k->pre_ds = help->pre_ds;
            
            //    delete help;
            if ((ptr_k->next_ds == NULL)&&(ptr_k->pre_ds==NULL))  // genau Element !!
            {
              delete help;
              help=NULL;
              ptr_k=NULL;
              pd->datensatz = NULL;
              
            }
            else
            {
              delete help;
              help=NULL;
            }
            
            ds_info[ds_nummer]--;
            if (ds_info[ds_nummer] <=15)
              scr.anzahl--;
            del =1;
            while (ptr_k != NULL)
            {
              ptr_k->ds_nr --;
              ptr_k = ptr_k->next_ds;
            }
          }
        }
      }
    }
    
    return del;
  }
  return 0;
}
/***************************************************/
int List::MakeWriteInfo(FILE *in)
{ // 3 Info Zeilen vor Datensatz lesen
  int i,k,j=0,leer=0;
  BOOLEAN fehler = FALSE;
  char chr=' ';
  
  if (!feof(in))
    chr = fgetc(in);
  else fehler = TRUE;
  
  if (chr==' ')
    while ((chr==' ')&&(!feof(in)) )
      chr=fgetc(in);
    if  ( (chr=='\n')&&(!feof(in)) )
      chr=fgetc(in);
    else if (feof(in))
    {
      fehler = TRUE;
      return fehler;
    }
    for (i=0;i<=2;i++)
    {
      leer=0; 
      if(i==1) //Dick 11.02.99
        while((chr!='\n') && (leer<=100) &&(!feof(in))&&chr==' ') //Dick 11.02.99
        {
          chr = fgetc(in);//Dick 11.02.99
          leer++;
        }
        while ((chr!='\n') && (j<=250) &&(!feof(in)))//Dick 11.02.99 150->250
        {
          ptr_profil->daten_info[i][j] = chr;             
          chr = fgetc(in);
          j++;
        }
        k=j-1;
        if (feof(in))
        {
          fehler = TRUE;
          return fehler;
        }
        while (ptr_profil->daten_info[i][k]==' ')  // vom Ende space überlesen
          k--;
        ptr_profil->daten_info[i][k+1]=' ';
        ptr_profil->daten_info[i][k+2]='\0';
        
        while ( (chr !='\n')&&(!feof(in)) )
          chr=fgetc(in);
        if (feof(in))
        {
          fehler = TRUE;
          return fehler;
        }
        if ((i<2)&&(!feof(in)) )
          chr=fgetc(in);
        else if (feof(in))
        {
          fehler = TRUE;
          return fehler;
        }
        
        j=0;
        
    }
    return fehler;
}
#pragma optimize ( "", off )
/***************************************************/
void List::MakeSaveInfo(FILE *out)
{
  /*schreiben der 3 Infozeilen auf die ptr_profil zeigt */
  char *str;
  char str_anzahl[5];
  
  for (int i=0;i<=2;i++)
  {
    if(ptr_profil->profiltyp==COMMENT)
    {
      int anzahl=xvt_slist_count(slist_comment);
      itoa(anzahl,str_anzahl,10);
      ptr_profil->daten_info[2][22]='\0';
      strcat(ptr_profil->daten_info[2],str_anzahl);
      strcat(ptr_profil->daten_info[2]," 17");
    }
    if((ptr_profil->profiltyp==ARMCO84)&&(i==1))
    {
      str=ptr_profil->daten_info[1];
      if ( (xvt_str_match(str,"M*",TRUE)) ||
        (xvt_str_match(str,"W*",TRUE)) )
      {
        ptr_profil->daten_info[2][25]='2';
        ptr_profil->daten_info[2][26]='0';
      }  
      if ( (xvt_str_match(str,"E*",TRUE)) ||
        (xvt_str_match(str,"SE*",TRUE)) )
      {
        ptr_profil->daten_info[2][25]='2';
        ptr_profil->daten_info[2][26]='2';
      }
      if (xvt_str_match(str,"SB*",TRUE))
      {
        ptr_profil->daten_info[2][25]='2';
        ptr_profil->daten_info[2][26]='1';
      }
    }
    if( !LWA_PROJEKT )
    {
      if((ptr_profil->profiltyp==RAUHIGKEIT || ptr_profil->profiltyp==RAUHIGKEIT_KST) && i==0)
        strcpy(ptr_profil->daten_info[i],"RAUHIGKEIT");
    };
    
    str = ptr_profil->daten_info[i];
    if( i==1 && ( ptr_profil->profiltyp == STATION || ptr_profil->profiltyp == STATION_FIX || 
      ( berechnen && editieren && strlen( str ) < 99 ) ) ) //Dick 9.02.99 //Dick 9.04.99
    {
      // 99 Leerzeilen voranstellen
      char temp[100];
      strcpy( temp," " );
      for( int j = 0; j < 98; j++ )
        strcat( temp, " " );
      fprintf( out, "%s%s\n", temp, str );
    }
    else
      fprintf( out, "%s\n", str );
  }
}
#pragma optimize ( "", off )
/***************************************************/
int List::WriteX(FILE *in,int n1)
{  int x;
double fx;
BOOLEAN fehler=FALSE;

pp=ptr_profil->datensatz;


for (int k=1;k <= n1;k++)
{
  if (!feof(in))
  {
    if ( fscanf(in,"%2d",&x ))
    {
      pp->pre_x = x;
    }
    else
      if (!fehler)
      {
        char buf[200],buf2[200];
        xvt_res_get_str(STR_PROF_ERROR,buf,sizeof(buf));
        xvt_res_get_str(STR_READ_Y_WERT,buf2,sizeof(buf2));
        xvt_dm_post_error("%s\n%s",buf,buf2);
        //xvt_dm_post_error("schwerwiegender Fehler in Profildatei:\nLesen von y-Werten /p");
        fehler=TRUE;
      }
  }
  else fehler = TRUE;
  if (!feof(in))
  {
    if ( fscanf(in,"%13lf", &fx))
      //    if ( fscanf(in,"%lf", &fx))
    {
      pp->x = fx;
    }
    else
      if (!fehler)
      {
        //xvt_dm_post_error("schwerwiegender Fehler in Profildatei:\nLesen von y-Werten");
        char buf[200],buf2[200];
        xvt_res_get_str(STR_PROF_ERROR,buf,sizeof(buf));
        xvt_res_get_str(STR_READ_Y_WERT,buf2,sizeof(buf2));
        xvt_dm_post_error("%s\n%s",buf,buf2);
        fehler=TRUE;
      }
  }
  else fehler = TRUE;
  pp = pp->next_ds;
}
return fehler;
}
/***************************************************/
int List::WriteY(FILE *in,int n1)
{
  int y;
  double fy;
  BOOLEAN fehler=FALSE;
  
  pp=p_h;
  for (int k=1;k <= n1;k++)
  {
    if (!feof(in))
    {
      if ( fscanf(in,"%2d",&y ))
      {
        pp->pre_y = y;
      }
      else
      {
        pp->pre_y = 0;
        if (!fehler)
        {
          //xvt_dm_post_error("schwerwiegender Fehler in Profildatei:\nLesen von Plottwerten");
          char buf[200],buf2[200];
          xvt_res_get_str(STR_PROF_ERROR,buf,sizeof(buf));
          xvt_res_get_str(STR_READ_PLOT_WERT,buf2,sizeof(buf2));
          xvt_dm_post_error("%s\n%s",buf,buf2);
          fehler=TRUE;
        }
      }
    }
    else fehler =TRUE;
    if (!feof(in))
    {
      if ( fscanf(in,"%13lf", &fy))
        //     if ( fscanf(in,"%lf", &fy))
      {
        pp->y = fy;
      }
      else
      {
        pp->y=BCE_NAN;
        if (!fehler)
        {
          //xvt_dm_post_error("schwerwiegender Fehler in Profildatei:\nLesen von Höhenwerten");
          char buf[200],buf2[200];
          xvt_res_get_str(STR_PROF_ERROR,buf,sizeof(buf));
          xvt_res_get_str(STR_READ_H_WERT,buf2,sizeof(buf2));
          xvt_dm_post_error("%s\n%s",buf,buf2);
          fehler=TRUE;
        }
      }
    }
    else fehler = TRUE;
    pp = pp->next_ds;
  }
  return fehler ;
}
/***************************************************/
int List::GetInfoline2(int ds_nummer,BRUECKE *br)
{
  /*Zeile 2 bei Bruckenprofil entschlüsseln*/
  char str[15];
  double ok_wert1=0.5,ok_wert2=0.8,ok_wert3=0;
  strcpy(br->typ,"STANDARD");
  str[0]='\0';
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&& (ptr_profil->ds_nummer<ds_nummer))
  {
    ptr_profil = ptr_profil->next;
  }
  
  if (typ[ds_nummer]==UK_BRUECKE)
  {
    if (ptr_profil->daten_info[1][0]!='\0')
    {
      sscanf(ptr_profil->daten_info[1],"%10s %10s %10s %10s %10s",br->sohle,br->breite,br->rauheit,br->beiwert,str);
      if( LWA_PROJEKT )
      {
        if ( atof(br->beiwert)==0.0 )
          if (strlen(str)>0)
          {
            strcpy(br->beiwert,str);
            br->rehbock = TRUE;
          }
          else br->rehbock = FALSE;
      }
      else
        br->rehbock = FALSE;
      
      return 1;
    }
    else return 0;
  }
  else  if ((typ[ds_nummer]==OK_BRUECKE)||(typ[ds_nummer]==SCHUETZ))
  {   //Überfallbeiwert lesen
    if( LWA_PROJEKT )
    {
      if (ptr_profil->daten_info[1][0]!='\0')
      {
        //sscanf(ptr_profil->daten_info[1],"%14s",br->beiwert);
        sscanf(ptr_profil->daten_info[1],"%lf%lf%s%lf",&ok_wert1,&ok_wert2,br->typ, &ok_wert3);
        sprintf(br->beiwert,"%.1lf",ok_wert1);
        sprintf(br->abflusszahl,"%.1lf",ok_wert2);
        sprintf(br->neigung,"%.1lf",ok_wert3);
      }
    };
  }
  return 0;
}
/***************************************************/
int List::GetInfoline2(int ds_nummer, int koordinate, buhne *bu)
{
  //von Bruecke Anfang
  /* char str[15], ein_buhnen_satz[100];
  double ok_wert1=0.5,ok_wert2=0.8;
  int j, zeichen, viertes_blank, ur_zeichen, stop;
  
    
      bu->hoehe_links[0]='\0';
      bu->neig_links_rueck[0]='\0';
      bu->neig_links_vorne[0]='\0';
      
        str[0]='\0';
        ptr_profil = ptr_anfang;
        while ((ptr_profil !=NULL)&& (ptr_profil->ds_nummer<ds_nummer))
        {
        ptr_profil = ptr_profil->next;
        }
        
          if (typ[ds_nummer]==BUHNE && koordinate>0)
          {
          if (ptr_profil->daten_info[1][0]!='\0')
          {
          zeichen=0;
          ur_zeichen=0;
          for(j=1; j<=koordinate; j++)
          {
          zeichen=0;
          stop=0;
          viertes_blank=0;
          ein_buhnen_satz[0]='\0';
          while(stop!=1)
          {
          while(zeichen==0 && ptr_profil->daten_info[1][ur_zeichen]==' ')
          ur_zeichen++;
          ein_buhnen_satz[zeichen]=ptr_profil->daten_info[1][ur_zeichen];
          if(ptr_profil->daten_info[1][ur_zeichen]==' ')
          viertes_blank++;
          if((viertes_blank==4)||(ptr_profil->daten_info[1][ur_zeichen]=='\0'))
          stop=1;
          zeichen++;
          ur_zeichen++;
          }
          ein_buhnen_satz[zeichen]='\0';
          if((ptr_profil->daten_info[1][ur_zeichen]=='\0')|| (ptr_profil->daten_info[1][ur_zeichen]=='\n'))
          {
          //        ein_buhnen_satz[0]='\0';
          j=koordinate;
          }
          }
          //     sscanf(ptr_profil->daten_info[1],"%2s",bu->lage);
          if(ein_buhnen_satz[0]!='\0')
          {
          bu->hoehe_links[0]='\0';
          bu->neig_links_rueck[0]='\0';
          bu->neig_links_vorne[0]='\0';
          sscanf(ein_buhnen_satz,"%2s %10s %10s %10s",bu->lage, bu->hoehe_links,bu->neig_links_rueck,bu->neig_links_vorne);
          }
          else
          {
          
            bu->hoehe_links[0]='\0';
            bu->neig_links_rueck[0]='\0';
            bu->neig_links_vorne[0]='\0';
            
              }
              return 1;
              }
              else return 0;
              }
              //von Bruecke Ende
  */ 
  int buhnen=0;
  int drei=0;
  int buhnen_aus_koord=0;
  
  while ((ptr_profil !=NULL)&& (ptr_profil->ds_nummer<ds_nummer))
  {
    ptr_profil = ptr_profil->next;
  }
  
  if (typ[ds_nummer]==BUHNE && koordinate>0)
  {
    int len=strlen(ptr_profil->daten_info[1]);
    for(int i=0; i<len; i++)
    {
      if(ptr_profil->daten_info[1][i]!=' ')
        buhnen++;
    }
    
    for(i=0; i<=koordinate; i++)
    {
      drei++;
      if(drei==3)
      {
        buhnen_aus_koord++;
        drei=0;
      }
    }
    
    for(i=1; i<=buhnen_aus_koord; i++)
      sscanf(ptr_profil->daten_info[1], "%s", bu->lage);
  }
  return 0;
  
}
/**********************************************************/
void List::SaveInfoline2(int ds_nummer,int koordinate, buhne *bu)
{
  
  ptr_profil = ptr_anfang;
  char anfang[100], ende[100];
  int blank=0;
  int ein_satz=0;
  int j=0;
  anfang[0]='\0';
  ende[0]='\0';
  int buhne=1;
  int dreier=0;
  int wievielbuhnen=0;
  int wievielbuhnen_in_ds_info=0;
  int einfuegen=0;
  
  for(int i=1; i<=koordinate; i++)
  {
    if(dreier==3)
    {
      dreier=0;
      buhne++;
    }
    dreier++;
  }
  
  while ((ptr_profil !=NULL)&& (ptr_profil->ds_nummer<ds_nummer))
  {
    ptr_profil = ptr_profil->next;
  }
  
  pp=ptr_profil->datensatz;
  while(pp!=NULL)
  {
    if(pp->y!=BCE_NAN)
      wievielbuhnen++;
    pp=pp->next_ds;
  }
  
  wievielbuhnen=wievielbuhnen/3;
  
  if (typ[ds_nummer]==BUHNE)
  {
    int len=strlen(ptr_profil->daten_info[1]);
    for(int i=0; i<len; i++)
    {
      if(ptr_profil->daten_info[1][i]!=' ')
        wievielbuhnen_in_ds_info++;
    }
    if(wievielbuhnen_in_ds_info<wievielbuhnen)
      einfuegen=1;
    else
      einfuegen=0;
    
    for (i=0; i<=len; i++)
    {
      if(ein_satz!=buhne)
      {
        anfang[i]=ptr_profil->daten_info[1][i];
        if(ptr_profil->daten_info[1][i]==' ')
        {
          blank++;
          //          if(blank==4)
          ein_satz++;
        }
      }
      if(ein_satz==buhne)
      {
        anfang[i]='\0';
        ende[j]=ptr_profil->daten_info[1][i];
        j++;
        
      }
    }
    ende[j]='\0';
    ptr_profil->daten_info[1][0]='\0';
    strcpy(ptr_profil->daten_info[1],anfang);
    if((strlen(bu->lage))==0)
      strcpy(bu->lage,"L"); 
    if(einfuegen)
    {
      strcat(ptr_profil->daten_info[1]," "); //Bley 30.1.01
      strcat(ptr_profil->daten_info[1],bu->lage);
    }
    else
    {
    /*    len=strlen(ptr_profil->daten_info[1]);
    for(int j=len; j>=0; j--)
    if(ptr_profil->daten_info[1][j]!=' ')
    ptr_profil->daten_info[1][j]=bu->lage[0];
    
      */
      len=strlen(ende);
      for (int j=0; j<len; j++)
      {
        if(ende[j]!=' ')
        {
          ende[j]=bu->lage[0];
          break;
        }
        
      }
      
    }
    
    strcat(ptr_profil->daten_info[1]," ");
    strcat(ptr_profil->daten_info[1], ende);
    
    
    
  }
  
}
/********************************************************/
void List::SaveInfoline2(int ds_nummer,BRUECKE *br)
/*      Zeile2 Infoblock:Brücke sichern  */
{
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&& (ptr_profil->ds_nummer<ds_nummer))
  {
    ptr_profil = ptr_profil->next;
  }
  
  if (typ[ds_nummer]==UK_BRUECKE)
  {
    strcpy(ptr_profil->daten_info[1],br->sohle);
    strcat(ptr_profil->daten_info[1]," ");
    strcat(ptr_profil->daten_info[1],br->breite);
    strcat(ptr_profil->daten_info[1]," ");
    strcat(ptr_profil->daten_info[1],br->rauheit);
    strcat(ptr_profil->daten_info[1]," ");
    if( LWA_PROJEKT )
    {
      if (br->rehbock)
        strcat(ptr_profil->daten_info[1],"0.0 ");
    };
    strcat(ptr_profil->daten_info[1],br->beiwert);
  }
  else  if ((typ[ds_nummer]==OK_BRUECKE)||(typ[ds_nummer]==SCHUETZ))
  {   //nur Überfallbeiwert
    if( LWA_PROJEKT )
    {
      strcpy(ptr_profil->daten_info[1],br->beiwert);
      strcat(ptr_profil->daten_info[1]," ");
      strcat(ptr_profil->daten_info[1],br->abflusszahl);
      strcat(ptr_profil->daten_info[1]," ");
      strcat(ptr_profil->daten_info[1],br->typ);
      strcat(ptr_profil->daten_info[1]," ");
      strcat(ptr_profil->daten_info[1],br->neigung);
      if(typ[ds_nummer]==SCHUETZ)
      {
        ptr_profil->datensatz->x=0;
        ptr_profil->datensatz->y=0;
      }
    };
  }
}
/***************************************************/
int  List::GetInfoline2( int ds_nummer,WEHR* wehr )
// füllt die Struktur whr mit Daten aus der list
// Parameter:
//        int ds_nummer: Nummer des Datenblocks?
//        WEHR* wehr: die zu füllende Struktur
// rückgabewert:
//          int: LWA Version: 0 kein Erfolg, 1 Erfolg
//               BCE Version: typ-Nummer
{
  /*Zeile 2 bei Wehrprofil entschlüsseln*/
  ptr_profil = ptr_anfang;
  while( ( ptr_profil ) && ( ptr_profil->ds_nummer < ds_nummer ) )
    ptr_profil = ptr_profil->next;
  
  if( !LWA_PROJEKT )
  {
    strcpy( wehr->bwert, "0.0" ); // default
    strcpy( wehr->wehrtyp, "rundkornig" ); // default Wert
    
    if ( ptr_profil && ptr_profil->daten_info[1][0] != '\0' )
    {
      char* bw = new char[100]; 
      sscanf( ptr_profil->daten_info[1], "%19s" , wehr->typ );
      char* bwTmp = strchr( ptr_profil->daten_info[1], ' ' );
      if ( bwTmp )
        strcpy( wehr->bwert, ++bwTmp );
      else
        wehr->bwert[0] = '\0';
      
      if ( strlen( wehr->bwert ) <= 1 )
        strcpy( wehr->bwert, "0.0" );
      
      int nReturn = -1;
      if (xvt_str_match(wehr->typ,"rundkronig*",FALSE))
        nReturn = 0;
      if (xvt_str_match(wehr->typ,"breitkronig*",FALSE))
        nReturn = 1;
      if (xvt_str_match(wehr->typ,"Beiwert*",FALSE))
        nReturn = 2;
      if (xvt_str_match(wehr->typ,"scharfkantig*",FALSE))
      {
        wehr->bwert[0] = '\0';
        nReturn = 3;
      };
      if ( nReturn == -1 )
        nReturn = 0; // default
      else
        strcpy( wehr->wehrtyp, wehr->typ );
      
      delete []bw;
      return nReturn;
    }
    else
      return 0; // default
  }
  else
  {
    if (ptr_profil && ptr_profil->daten_info[1][0] != '\0' )
    {
      sscanf(ptr_profil->daten_info[1],"%15s%15s%15s%15s%15s%15s",wehr->typ,
        wehr->kote,wehr->breite,wehr->beiwert,wehr->hoehe,wehr->ausuferungshoehe);
      if (xvt_str_match(wehr->typ,"STREICHWEHR*",FALSE))
        wehr->wehrtyp[0]=0;
      if (xvt_str_match(wehr->typ,"LWA-BREIT*",FALSE))
        wehr->wehrtyp[0]=1;
      if (xvt_str_match(wehr->typ,"LWA-DACH*",FALSE))
        wehr->wehrtyp[0]=2;
      if (xvt_str_match(wehr->typ,"LWA-RUND*",FALSE))
        wehr->wehrtyp[0]=3;
      if (xvt_str_match(wehr->typ,"LWA-SCHARF*",FALSE))
        wehr->wehrtyp[0]=4;
      return 1;
    }
    else 
      return 0;
  };
}; // GetInfoline2
/***************************************************/
void List::SaveInfoline2(int ds_nummer,WEHR *wehr)
/*      Zeile2 Infoblock:Wehr sichern  */
{
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&& (ptr_profil->ds_nummer<ds_nummer))
  {
    ptr_profil = ptr_profil->next;
  }
  
  if( !LWA_PROJEKT )
  {
    strcpy(ptr_profil->daten_info[1],wehr->wehrtyp);
    strcat(ptr_profil->daten_info[1]," ");
    strcat(ptr_profil->daten_info[1],wehr->bwert);
  }
  else
  {
    if (xvt_str_match(wehr->typ,"STREICHWEHR*",FALSE))
      strcpy(ptr_profil->daten_info[1],"STREICHWEHR");
    if (xvt_str_match(wehr->typ,"BREITKRONIG*",FALSE))
      strcpy(ptr_profil->daten_info[1],"LWA-BREIT");
    if (xvt_str_match(wehr->typ,"DACHFÖRMIG*",FALSE))
      strcpy(ptr_profil->daten_info[1],"LWA-DACH");
    if (xvt_str_match(wehr->typ,"RUNDKRONIG*",FALSE))
      strcpy(ptr_profil->daten_info[1],"LWA-RUND");
    if (xvt_str_match(wehr->typ,"SCHARFKANTIG*",FALSE))
      strcpy(ptr_profil->daten_info[1],"LWA-SCHARF");
    
    strcat(ptr_profil->daten_info[1]," ");
    strcat(ptr_profil->daten_info[1],wehr->kote);
    strcat(ptr_profil->daten_info[1]," ");
    strcat(ptr_profil->daten_info[1],wehr->breite);
    strcat(ptr_profil->daten_info[1]," ");
    strcat(ptr_profil->daten_info[1],wehr->beiwert);
    strcat(ptr_profil->daten_info[1]," ");
    strcat(ptr_profil->daten_info[1],wehr->hoehe);
    strcat(ptr_profil->daten_info[1]," ");
    strcat(ptr_profil->daten_info[1],wehr->ausuferungshoehe);
  };
}
/***************************************************/
double List::GetInfoline2(int ds_nummer)
{ /*Zeile 2 aus Infoblock: 1.Wert  -Gefälle erhalten*/
  double gefaelle=0.0;
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&& (ptr_profil->ds_nummer<ds_nummer))
    ptr_profil = ptr_profil->next;
  if (ptr_profil->daten_info[1][0]!='\0')
    sscanf(ptr_profil->daten_info[1],"%8lf",&gefaelle);
  return gefaelle;
}
/***************************************************/
/***************************************************/
void List::SaveInfoline2(int ds_nummer,double gefaelle)
{ /*Zeile 2 aus Infoblock: 1.Wert  -Gefälle erhalten*/
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&& (ptr_profil->ds_nummer<ds_nummer))
    ptr_profil = ptr_profil->next;
  sprintf(ptr_profil->daten_info[1],"%6.4lf",gefaelle);
}
/***************************************************/
int List::Infoline2(int ds_nummer,char *profiltyp,BOOLEAN op)
//Zeile 2 für ARMCO84 holen [op=0]/sichern [op=1]
{
  int typ=0;
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&& (ptr_profil->ds_nummer<ds_nummer))
    ptr_profil = ptr_profil->next;
  if (op) //sichern
    sprintf(ptr_profil->daten_info[1],"%s",profiltyp);
  else   //lesen
  {
    sscanf(ptr_profil->daten_info[1],"%s",profiltyp);
    
    if (xvt_str_match(profiltyp,"MA*",FALSE))
      typ=1;
    if (xvt_str_match(profiltyp,"MB*",FALSE))
      typ=2;
    if (xvt_str_match(profiltyp,"WA*",FALSE))
      typ=3;
    if (xvt_str_match(profiltyp,"WB*",FALSE))
      typ=4;
    if (xvt_str_match(profiltyp,"EA*",FALSE))
      typ=5;
    if (xvt_str_match(profiltyp,"EB*",FALSE))
      typ=6;
    if (xvt_str_match(profiltyp,"SE*",FALSE))
      typ=7;
    if (xvt_str_match(profiltyp,"SB*",FALSE))
      typ=8;
  }
  return typ;
}
/***************************************************/

/*
 * Seiteneffekt: p_h muss auf Koordinaten des zu lesenden Datensatz zeigen 
 *
 */
int List::ReadSonderProfil( FILE* in, int typ )
{
  //Datenblock aus Profildatei in verz.Liste einlesen
  int anzahl = 0;   // Anzahl der Daten
  BOOLEAN fehler = FALSE;
  BOOLEAN zeile5 = TRUE;
  
  switch( typ )
  {
  case MAUL:
  case EIPROFIL:
    {
      anzahl = 5;
      zeile5 = FALSE;
    }
    break;
  case TRAPEZ:
    {
      anzahl = 6;
      zeile5 = FALSE;
    }
    break;
  case KREIS:
    {
      anzahl = 4;
      zeile5 = FALSE;
    }
    break;
  case ARMCO84:
  case ARMCO71:
    {

      anzahl = 5;   //Anzahl 6 für ARMCO84 wird später gelesen
      zeile5 = FALSE;
    }
    break;
  case NWRINNE:
    {
      anzahl = 3;
      zeile5 = FALSE;
    }
    break;
  case FUELLHOEHE: 
    anzahl = 0;
    break;  //noch nicht implementiert
  
  case NAU_MED:
    anzahl = 0;
    break;  //noch nicht implementiert
  
  case UNKNOWN:
  default:
    break;
  }
  

  Koord* pp = p_h;
  for( int i = 1; i <= anzahl;i++ )
  {
    if( !feof( in ) )
    {
      double wert;
      if( fscanf( in,"%10lf", &wert ) )
        pp->x = wert;
      else
      {
        //xvt_dm_post_error("Fehler beim einlesen von Daten\nList::ReadSonderprofil");
        char buf[200];
        xvt_res_get_str(STR_DAT_READ_ERR,buf,sizeof(buf));
        xvt_dm_post_note("%s\nList::ReadSonderprofil",buf);
      }
    }
    else
      fehler = TRUE;
    pp = pp->next_ds;
  }
  
  if( zeile5 )
  {
    char chr=' ';
    while ( ( chr != '\n') && ( !feof( in ) ) )
      chr = fgetc( in );       // bis Zeilenende  lesen
    chr = ' ';
    while( ( chr != '\n' ) && ( !feof( in ) ) )
      chr = fgetc( in );       // 2.Zeile überlesen
  }

  if( ( typ == ARMCO84 ) || ( typ == ARMCO71 ) )
  {
    if( typ == ARMCO84 )
      anzahl++; //in 2.Zeile stehen 6 Werte !

    Koord* pp = p_h;
    for( int i = 1; i <= anzahl; i++ )
    {
      if( ( i == anzahl ) && ( typ == ARMCO84 ) )
        pp->x = BCE_NAN;
      if( !feof( in ) )
      {
        double wert;
        if( fscanf(in,"%10lf", &wert ) )
          pp->y = wert;
        else
        {
          // xvt_dm_post_error("Fehler beim Lesen von Daten:ARMCO71/84");
          char buf[200];
          xvt_res_get_str(STR_DAT_READ_ERR,buf,sizeof(buf));
          xvt_dm_post_note("%s:ARMCO71/84",buf);
        }
      }
      else 
        fehler = TRUE;
      pp = pp->next_ds;
    }
  }
  return fehler;
}
/***************************************************/
void List::Update_Scr(int pos,Scroller *pscr)
{
  Koord *pp_neu;
  Profildatei *ptr_prof;
  
  ptr_prof=ptr_anfang;
  if (ptr_prof !=NULL)
    pp_neu = ptr_prof->datensatz;
  
  // x-y-Geländekoordinaten neu einlesen
  if (pp_neu !=NULL)
    while ((pp_neu->ds_nr < pscr->scrollpos)&&(pp_neu->next_ds!=NULL))
      pp_neu = pp_neu->next_ds;
    
    for (int i=0;i<pos;i++)
      if (pp_neu!=NULL)
        pp_neu=pp_neu->next_ds;
      for (i=pos;i<=14;i++)  // (i=0;i<=14-pos;i++)
      {
        if (pp_neu!=NULL)
        {
          pscr->x[i] = pp_neu->x;
          pscr->y[i] = pp_neu->y;
          pp_neu=pp_neu->next_ds;
        }
      }
}
/******************************************************************************/
void List::GetScrollDaten( Scroller* pscr )
{
  /* Daten aus Liste:ptr_profil in Scroller *pscr einlesen  */
  int i,m,gefunden;
  Koord* pp_neu;
  Profildatei* ptr_prof;
  
  pscr->z_offset = 0;
  
  for( i = 0; i <= 14;i++ )   // INIT
  {
    pscr->x[i] = BCE_NAN;
    pscr->y[i] = BCE_NAN;
    pscr->z[i] = BCE_NAN;      // Z-Array initialisieren
  }
  
  ptr_prof = ptr_anfang;
  if( ptr_prof != NULL )
    pp_neu = ptr_prof->datensatz;
  
  // y-z-Geländekoordinaten einlesen: Datensatz GELAENDE
  double lastX = BCE_NAN; // sehr klein // für Markieren von rücksrpüngen die letzte position merken
  if( pp_neu != NULL )
  {
    while( pp_neu->next_ds && pp_neu->ds_nr < pscr->scrollpos )
    {
      if( pp_neu != NULL )
      {
        lastX = pp_neu->x;
        pp_neu = pp_neu->next_ds;

      }
      else 
      {
        //xvt_dm_post_error("Fehler beim Einlesen der Profildaten:_GetScrollDaten");
        char buf[200];
        xvt_res_get_str(STR_GETSCROLLDATEN,buf,sizeof(buf));
        xvt_dm_post_note("%s",buf);
      }
    } // while
  }

  pscr->lastX = lastX; // für das Markieren von Rücksprüngen den X-Wert vor der letzten Position merken
  
  for( i = 0; i <= 14; i++ )
  {
    if( zoom_right_x && zoom_info.level > 0 )
    {
      if( pp_neu && zoom_info.level > 0 && pp_neu->ds_nr <= zoom_right_x->ds_nr )
      {
        pscr->x[i] = pp_neu->x;
        pscr->y[i] = pp_neu->y;
        
        pscr->z[i] = BCE_NAN;//pp_neu->y;//Dick 10.08.98
        pp_neu=pp_neu->next_ds;
      }
    }
    else
    {
      if( pp_neu && zoom_info.level == 0 )
      {
        pscr->x[i] = pp_neu->x;
        pscr->y[i] = pp_neu->y;
        pscr->z[i] = BCE_NAN;//pp_neu->y;//Dick 10.08.98
        pp_neu=pp_neu->next_ds;
      }
    }
  }  //-for...
  
  // ausgewählter Datensatz 2 einlesen
  while ((ptr_prof !=NULL)&&(ptr_prof->ds_nummer < pscr->datensatz)&&(pscr->datensatz <= ds_info[0]))
    ptr_prof = ptr_prof->next;
  if ((ptr_prof!=NULL)&&(ptr_prof->datensatz !=NULL))
    pp_neu = ptr_prof->datensatz;
  else pp_neu =NULL;
  if (pp_neu!=NULL)
    switch (ptr_prof->profiltyp)
  {
    case (DURCHST_BEREICH):
      {
        pscr->z[0]= pp_neu->x;       // y-links
        pscr->z[1]= pp_neu->y;       // z-links
        if (pp_neu->next_ds !=NULL)
          pp_neu = pp_neu->next_ds;
        pscr->z[2]= pp_neu->x;       //y-rechts
        pscr->z[3]= pp_neu->y;       //z-rechts
      }
      break;
    case MAUL:
      {
        for (i=0;i<5;i++)
        {
          pscr->z[i] = pp_neu->x;
          if (pp_neu->next_ds !=NULL)
            pp_neu = pp_neu->next_ds;
        }
        //     if (pp_neu!=NULL)
        //      pp_neu->next_ds =NULL;
      }
      break;
    case EIPROFIL:
      {
        for (i=0;i<5;i++)
        {
          pscr->z[i] = pp_neu->x;
          if (pp_neu->next_ds !=NULL)
            pp_neu = pp_neu->next_ds;
        }
        //     if (pp_neu!=NULL)
        //       pp_neu->next_ds =NULL;
      }
      break;
    case TRAPEZ:
      {
        for (i=0;i<6;i++)
        {
          pscr->z[i] = pp_neu->x;
          if (pp_neu->next_ds !=NULL)
            pp_neu = pp_neu->next_ds;
        }
      }
      break;
    case KREIS:
      {
        for (i=0;i<4;i++)
        {
          pscr->z[i] = pp_neu->x;
          if (pp_neu->next_ds !=NULL)
            pp_neu = pp_neu->next_ds;
        }
        //     if (pp_neu!=NULL)
        //       pp_neu->next_ds =NULL;
      }
      break;
    case KREISSEGM:
      {
        for (i=0;i<=2;i++)
        {
          pscr->z[i] = pp_neu->x;
          pscr->z[i+3] = pp_neu->y;
          if (pp_neu->next_ds!=NULL)
            pp_neu = pp_neu->next_ds;
        }
        scr.z[6] = GetInfoline2(scr.datensatz);
      }
      break;
    case ARMCO84:
    /*
    Daten in pscr.z[0...11]: Breite,Höhe,Gefälle,Station,Geländehöhe,don't care,R1,R2,R3,W1,W2/W3,FCRED
      */
      {
        for (i=0;i<6;i++)
        {
          pscr->z[i] = pp_neu->x;
          pscr->z[i+6] = pp_neu->y;
          if (pp_neu->next_ds !=NULL)
            pp_neu = pp_neu->next_ds;
        }
        pscr->z[5]=BCE_NAN;  //init
      }
      break;
    case ARMCO71:
      {
      /*
      Daten in pscr.z[0...9]: Breite,Höhe,Gefälle,Station,Geländehöhe,RCO,R1CO,R2CO,BCO,FCRED
        */
        for (i=0;i<5;i++)
        {
          pscr->z[i] = pp_neu->x;
          pscr->z[i+5] = pp_neu->y;
          if (pp_neu->next_ds !=NULL)
            pp_neu = pp_neu->next_ds;
        }
      }
      break;
    case NWRINNE:
      {
        for (i=0;i<3;i++)
        {
          pscr->z[i] = pp_neu->x;
          if (pp_neu->next_ds !=NULL)
            pp_neu = pp_neu->next_ds;
        }
      }
      break;
    case FUELLHOEHE:break;
    case NAU_MED: break;
    case GAUSS:
      {
        for (i=0;i<3;i++)
        {
          pscr->z[i]=pp_neu->x;
          pscr->z[i+3]=pp_neu->y;
          if (pp_neu->next_ds !=NULL)
            pp_neu = pp_neu->next_ds;
        }
      }
      break;
    case TRENNFLAECHEN:
    case BORDVOLL:
      {
        if ((pp_neu->y ==1)||(pp_neu->y ==3))     //links
        {
          pscr->z[0] =pp_neu->x;
          pscr->z[2] =pp_neu->y;
        }
        else
          if((pp_neu->y ==2)||(pp_neu->y ==4))      //rechts
          {
            pscr->z[1] =pp_neu->x;
            pscr->z[3] =pp_neu->y;
          }
          else
          {
            pscr->z[0] = BCE_NAN;
            pscr->z[1] = BCE_NAN;
            pscr->z[2] = 1.0;
            pscr->z[3] = 2.0;
          }
          if (pp_neu->next_ds !=NULL)
          {
            pp_neu = pp_neu->next_ds;
            if ((pp_neu->y ==1)||(pp_neu->y ==3))     //links
            {
              pscr->z[0] =pp_neu->x;
              pscr->z[2] =pp_neu->y;
            }
            else
              if ((pp_neu->y ==2)||(pp_neu->y ==4))      //rechts
              {
                pscr->z[1] =pp_neu->x;
                pscr->z[3] =pp_neu->y;
              }
              else
              {
                pscr->z[0] = BCE_NAN;
                pscr->z[1] = BCE_NAN;
                pscr->z[2] = 1.0;
                pscr->z[3] = 2.0;
              }
          }
      }
      break;

    case MODELLGRENZEN:
      {
          pscr->z[0] =pp_neu->x;
          pscr->z[2] =pp_neu->y;
          
          if( pp_neu->next_ds )
          {
            pp_neu = pp_neu->next_ds;
            
            pscr->z[1] =pp_neu->x;
            pscr->z[3] =pp_neu->y;
          }
      }
      break;

    default:
      {
        m = 0;
        gefunden = -1;
        int correct = 0;
        int pos_gefunden = -1;

        for( i = 0; i <= 14; i++ )
          pscr->z[i] = BCE_NAN;      // Z-Array initialisieren
        
        if( pp_neu != NULL && pscr->datensatz > 1 )
        {
          int pos=14;
          int num_st,num_gelaende,pos_gelaende=0,pos_koord_neu;
          BOOLEAN sperr;
          while( pscr->x[pos] == BCE_NAN )
            --pos;
          while ((pp_neu !=NULL)&&(m <= 14))//GaussTest 
          { 
            sperr=FALSE;
            if (pp_neu->x !=BCE_NAN)
            {
              num_st = num_station_in_ds(pp_neu->x ,pscr->datensatz); //wievielte Koordinate ist Station(pp-Neu->y im Datensatz pscr-Datensatz)
              num_gelaende = num_station_in_ds(pp_neu->x ,GELAENDEHOEHE); //wievielte in Gelände
              if( num_st > 1 )  //nicht erste Koordinate
              {
                num_gelaende = num_station_in_ds(pp_neu->x ,GELAENDEHOEHE);
                if((num_gelaende>1)&&(num_st==num_gelaende))
                {
                  int num_scr=0;
                  for( int k = 0; k <= 14; k++ )
                  {
                    if(pp_neu->x==pscr->x[k])
                      num_scr++;
                  }
                  
                  if ((num_scr < num_gelaende)&&(!correct)&&(m<1))
                  {
                    for (int k=0;k<num_gelaende-num_scr;k++)
                    {
                      if (pp_neu!=NULL)
                      {
                        //pp_neu = pp_neu->next_ds; //Auskommentiert von Dick GaussTest
                        //pscr->z_offset++;
                      }
                    }
                    correct=TRUE;
                  }
                }
                else if(pp_neu->pre_ds!=NULL  && num_st > num_gelaende ) //dick 9.08.98
                  sperr=TRUE;
                //Dick 5.05.99 
                else if(num_st < num_gelaende && (typ[pscr->datensatz]==DKUK 
                  || typ[pscr->datensatz]==DKOK
                  || typ[pscr->datensatz]==BUHNE) )
                {
                  Koord *profilart=HoleDatensatz(typ[pscr->datensatz]);
                  if(profilart!=NULL)
                  {
                    if(profilart->y==0.0)
                    {
                      if(typ[pscr->datensatz]==DKUK)
                      {
                        Koord_Einf(pp_neu->x,BCE_NAN,pp_neu->ds_nr,DKUK);
                        Koord_Einf(pp_neu->x,BCE_NAN,pp_neu->ds_nr-1,DKOK);
                      }
                      if(typ[pscr->datensatz]==DKOK)
                      {
                        Koord_Einf(pp_neu->x,BCE_NAN,pp_neu->ds_nr,DKOK);
                        Koord_Einf(pp_neu->x,BCE_NAN,pp_neu->ds_nr-1,DKUK);
                      }
                      if(typ[pscr->datensatz]==BUHNE)
                      {
                        Koord_Einf(pp_neu->x,BCE_NAN,pp_neu->ds_nr,BUHNE);
                        Koord_Einf(pp_neu->x,BCE_NAN,pp_neu->ds_nr-1,BUHNE);
                      }
                      
                      pp_neu=pp_neu->pre_ds;
                      continue;
                    }
                  }
                }
                //
              } //-if(num_st>1) /* Dick*/
              pos_gelaende = Get_Station_pos_num( pp_neu->x, Get_Station_pos_allg( pp_neu->x, pp_neu->ds_nr, pscr->datensatz ), GELAENDEHOEHE );
              for( int k = m; k <= 14; k++ )
              {
                if( pp_neu != NULL && pp_neu->x == pscr->x[k] && gefunden == -1 &&
                  pos_gelaende >= pscr->scrollpos && !sperr )
                  {                      
                    if( k >= m )
                    {
                      pscr->z[k] = pp_neu->y;
                      gefunden = k;
                    }
                  }
              } //-for k ...
              
              pos_gefunden = -1;
              if( num_st <= 1 || num_st > 1 && num_gelaende >= num_st )
                pos_gefunden = ExistStationX( pp_neu->x, 1 );//Prüfen ob Station wircklich nicht da ist
              if( gefunden >= 0 || pos_gefunden >= 0 )
                pp_neu = pp_neu->next_ds;
              else
              {
                if ((pp_neu!=NULL)&&((pp_neu->x<=pscr->x[14])||(pscr->x[14]==BCE_NAN)))
                  if ((m<=pos && zoom_info.level ==0)||(m<=pos && zoom_info.level >0 && pp_neu->x>=zoom_info.station_min && pp_neu->x <= zoom_info.station_max))//Test Dick 7.08.98
                  {
                    if(pp_neu->pre_ds==NULL)                        
                      pos_koord_neu=Koord_Einf_Sorted(pp_neu->x,BCE_NAN,1);                        
                    else
                    {
                      pos_gelaende=Get_Station_pos_num(pp_neu->pre_ds->x,Get_Station_pos_allg(pp_neu->pre_ds->x,pp_neu->pre_ds->ds_nr,pscr->datensatz),GELAENDEHOEHE);
                      pos_koord_neu=Koord_Einf_Sorted(pp_neu->x,BCE_NAN,pos_gelaende);
                      if(win117_mouse_move)//Dick 30.04.99
                      {
                        if(pos_koord_neu == mmp.position)
                        {
                          win117_mouse_move_aktiv=FALSE;
                          mmp.position_mouse_down++;
                        }
                      }
                    }
                    pos_koord_neu=pos_koord_neu-pscr->scrollpos;
                    Update_Scr(m,pscr);//GaussTest weg
                    if (pos<14)    //if abfrage neu 24.5.96 Dirk
                      pos++;
                    for (int f=0;f<=14;f++)
                      if (pp_neu->x == pscr->x[f] && f<=pos_koord_neu)
                      {
                        m=f;
                        
                      }
                      if (pp_neu->x == pscr->x[m])
                      {
                        pscr->z[m] = pp_neu->y;
                        gefunden= m;
                      }
                      else
                      {
                      }
                  }
                  //else break;
                  if (pp_neu!=NULL) pp_neu = pp_neu->next_ds;
              }
              
              m=gefunden+1;                
              gefunden=-1;
        } //-if (pp_neu->x !=BCE_NAN)
        else if (pp_neu!=NULL)
          pp_neu = pp_neu->next_ds;
       }  //-while ((pp_neu !=NULL)&&(m <= 14)....
       
       /*testen, ob noch weitere Stationen folgen, die größer als GELAENDE sind*/
       if( pp_neu != NULL && m <= 14 && pp_neu->x >= pscr->x[0] && 
         pp_neu->x >= pscr->x[pos] && pp_neu->x > pmm.maxX )
       {
         while( pp_neu != NULL )   //NEU DB 6.5.96 while
         {
           if (zoom_info.level==0)
           {
             Sort_In_Ds(pp_neu->x,BCE_NAN,1,0);
             Update_Scr(m,pscr);
             GetMinMax(&pmm,scr.datensatz);
             /********neu 6.5.96 db*********************/
             pos++;
             if((pp_neu->y!=NULL) && (pp_neu->y!=BCE_NAN))
               pscr->z[pos]=pp_neu->y;
             /*****************************/
           }
           if(pp_neu!=NULL)
             pp_neu=pp_neu->next_ds;  //neu next while
         } // while
       }  //-if
      }  // - if
    }  // end default
    break;
   }  // end switch
   
 }
 //****************************************************************************
 int List::num_station_in_ds( double station, int ds )  // ungleich list.dll
   // gibt die Nummer der Koordinate mit x-Wert station im Datensatz nummer ds zurück
   // Parameter:
   //          int ds: nummer des zu durchsuchenden Datensatzes
   //          double station: zu suchende x-koordinate
   // Rückgabewert:
   //            -1, falls der Datensatz ds nicht gefunden wurde
   //           sonst nummer der gefundenen Koordinate, möglicherweise = Anzahl der koordinaten, falls nichts gefunden wurde
   //          
 {
   Profildatei* ptr_prof = ptr_anfang;
   
   while( ptr_prof && ( ptr_prof->ds_nummer < ds ) )
     ptr_prof = ptr_prof->next;
   
   if( !ptr_prof )
     return -1;
   
   int anzahl = 0;
   Koord* pp_neu = ptr_prof->datensatz;
   while( pp_neu )
   {
     if( fabs( pp_neu->x - station ) < PRECISION_KRD )
     {
       anzahl++;
       break;
     }
     else
     {
       pp_neu = pp_neu->next_ds;
       anzahl++;
     }
   }; // while pp_neu
   
   return anzahl;
 }; // num_station_in_ds
 //****************************************************************************
 int List::num_station_leer_in_ds(double station,int ds)
 {
   Koord *pp_neu;
   Profildatei *ptr_prof;
   int anzahl=0;
   
   ptr_prof=ptr_anfang;
   
   while ((ptr_prof !=NULL)&& (ptr_prof->ds_nummer < ds))
     ptr_prof = ptr_prof->next;
   
   if (ptr_prof !=NULL)
     pp_neu = ptr_prof->datensatz;
   else return -1;
   
   while ((pp_neu != NULL))//&&(pp_neu->x == station))
   {
     if(pp_neu->x == station && pp_neu->y == BCE_NAN)//Dick 3.08.98
       anzahl++;
     pp_neu = pp_neu->next_ds;
   }
   
   return anzahl;
 }
 //****************************************************************************
 int List::Get_Station_pos(double station,int pos,int offset)
   // return:Der wievielte x-Wert in Datensatz GELAENDE ist
   //        der x-Wert:'station'  an der Stelle 'pos'
 {
   Koord *pp_neu;
   Profildatei *ptr_prof;
   int anzahl=0;
   
   ptr_prof=ptr_anfang;
   
   if (ptr_prof !=NULL)
     pp_neu = ptr_prof->datensatz;
   else return -1;
   
   if (pp_neu !=NULL)
     if (offset >1)
       while ((pp_neu != NULL)&&(pp_neu->ds_nr < offset))
       {
         if (pp_neu->x == station)
           anzahl++;
         pp_neu = pp_neu->next_ds;
       }
       else
         while ((pp_neu != NULL)&&(pp_neu->x != station))
           pp_neu = pp_neu->next_ds;
         
         if (anzahl==0)
           while ((pp_neu != NULL)&&(pp_neu->x != station))
             pp_neu = pp_neu->next_ds;
           
           if ((pp_neu != NULL)&&(pp_neu->x == station))
           {
             pp_neu = pp_neu->next_ds;
             anzahl=1;
             while ((pp_neu != NULL)&&(pp_neu->x == station)&&(anzahl < pos))
             {
               anzahl++;
               pp_neu = pp_neu->next_ds;
             }
             
           }
           return anzahl;
 }
 //****************************************************************************
 //****************************************************************************
 int List::Get_Station_pos_allg(double station,int pos,int datensatz)
   // return:Der wievielte x-Wert in Datensatz datensatz ist
   //        der x-Wert:'station'  an der Stelle 'pos'
 {
   Koord *pp_neu_koord;
   Profildatei *ptr_prof_help;
   int anzahl=0;
   
   ptr_prof_help=ptr_anfang;
   while((ptr_prof_help!=NULL)&&(ptr_prof_help->ds_nummer < datensatz))
     ptr_prof_help=ptr_prof_help->next;
   if (ptr_prof_help !=NULL)
     pp_neu_koord = ptr_prof_help->datensatz;
   else return -1;
   
   while ((pp_neu_koord != NULL)&&(pp_neu_koord->ds_nr <= pos))
   {
     if(pp_neu_koord->x == station)
       anzahl++;
     pp_neu_koord = pp_neu_koord->next_ds;
   }  
   return anzahl;
 }
 //****************************************************************************
 //****************************************************************************
 int List::Get_Station_pos_num(double station,int pos,int datensatz)
   // return:Die ds_nr vom pos-ten x-Wert in Datensatz datensatz
   //        
 {
   Koord *pp_neu_koord;
   Profildatei *ptr_prof_help;
   int anzahl=0,ds_num=0;
   
   ptr_prof_help=ptr_anfang;
   while((ptr_prof_help!=NULL)&&(ptr_prof_help->ds_nummer < datensatz))
     ptr_prof_help=ptr_prof_help->next;
   if (ptr_prof_help !=NULL)
     pp_neu_koord = ptr_prof_help->datensatz;
   else return -1;
   
   while ((pp_neu_koord != NULL)&&(anzahl < pos))
   {
     if(pp_neu_koord->x == station)
     {
       anzahl++;
       ds_num=pp_neu_koord->ds_nr;
     }
     pp_neu_koord = pp_neu_koord->next_ds;
   }
   if(anzahl!=pos)
     return -1;
   else
     return ds_num;
 }
 //****************************************************************************
 
 
 void List::GetDateninfo(WINDOW lwin)
 {
   char *chr,xx[135];//Dick 9.02.99
   SLIST datenblocktypen;
   int i=0,j=0;
   BOOLEAN find=FALSE;
   
   SLIST_ELT e;
   long *data, d;
   int wsp_col=0,wsp_col_fix=0;
   
   xvt_slist_destroy(title_list);
   
   if ((title_list = xvt_slist_create())==NULL)
   {
     // Fehlerbehandlung ;
     xvt_dm_post_error(" Can't create title_list ");
   }
   if ((datenblocktypen = xvt_slist_create())==NULL)
   {
     // Fehlerbehandlung ;
     xvt_dm_post_error(" Can't create SLIST:datenblocktypen ");
   }
   
   // Datenblocktypen aus Resource:"wspdlg.rc" lesen
   read_res_datentypen(datenblocktypen,1);  //in: util.cpp
   
   for (i=1;i<=ds_info[0];i++)
   {
     xx[0]='\0';
     chr=NULL;
     if (typ[i] >= 0 )
       for (e=xvt_slist_get_first(datenblocktypen);e!=NULL;e=xvt_slist_get_next(datenblocktypen,e))
       {
         data = xvt_slist_get_data(e);
         d=*data ;
         if (typ[i]== d)
         {
           chr = xvt_slist_get(datenblocktypen,e,0L);
           find=TRUE;
           //Dick 25.08.99
           if(typ[i]==STATION)
           {
             if(++wsp_col>1)
             {
               sprintf(xx,"%d.",wsp_col);
               strcat(xx,chr);
               chr=xx;
             }
           }
           if(typ[i]==STATION_FIX)
           {
             if(++wsp_col_fix>1)
             {
               sprintf(xx,"%d.",wsp_col_fix);
               strcat(xx,chr);
               chr=xx;
             }
           }
           //Ende Neu
         }
         if (typ[i]==UNKNOWN || (typ[i]>=SOHLHOEHE_2 && typ[i]<=BOESCHUNG_RECHTS_2))
         {
           Profildatei *ptr_t;
           if (ptr_anfang != NULL)
           {
             ptr_t = ptr_anfang;
             while ((ptr_t->ds_nummer != i)&&(ptr_t->next!=NULL))
               ptr_t = ptr_t->next;
           }
           if ((ptr_t!=NULL)&&(strlen(ptr_t->daten_info[0])>0))
             strcpy(xx,ptr_t->daten_info[0]);
           else
             strcpy(xx,"unbekannter Datensatz");
           chr =xx;
           find=TRUE;
         }
         
         if( LWA_PROJEKT )
         {
           if (((typ[i]>=PUNKT_NR)&&(typ[i]<MAUL) &&(typ[i]!=STATION) &&(typ[i]!=STATION_FIX)&&(typ[i]!=BORDVOLL))||(typ[i]==TRAPEZ))//Dick 5.02.99 &&  23.08.99
           {
             strcpy(xx,"BCE-Datensatz");
             chr =xx;
           }
         };
         
       }
       if(chr !=NULL)
       {
         if (strlen(chr)>0)
           xvt_slist_add_at_elt(title_list,NULL,chr,j);
       }
       else //Dick 20.01.00
       {
         strcpy(xx,"unbekannter Datensatz");
         chr =xx;
         xvt_slist_add_at_elt(title_list,NULL,chr,j);
       }
       j++;
   }
   xvt_list_add(lwin, 0, (char *)title_list);   // Ausgabe in LISTBUTTON
   if(scr.datensatz==0 || WIN130 !=NULL_WIN)
     xvt_list_set_sel(lwin,0, TRUE)  ;// 1.Listeneintrag auswählen //Dick 2.12.98
   else
     xvt_list_set_sel(lwin,scr.datensatz-1, TRUE)  ; // ausgewählte Listeneintrag auswählen
}

/***************************************************************************/
void List::GetMinMax(MinMax* pMinMax,int datensatz ) // ungleich list_dll.cpp
// ermittelt minimale und maximale Werte des Profils
// Parameter:
//        MinMax* pMinMax: hier werden die gefundenen Werte übergeben
//        int datensatz: ? eine Datensatznummer
{
  static int anzahl_aufrufe = 0; // für Rekursion ?
  
  // zoom_left und right müssen richtig gesetzt werden
  Set_Zoom_Marker( &zoom_info );
  
  anzahl_aufrufe++;
  if (ptr_anfang !=NULL)
  {
    ptr_profil=ptr_anfang;
    pp = ptr_profil->datensatz;
    
    if ((pp!=NULL)&&(zoom_info.level>0)&&(zoom_left_x!=NULL) ) //Zoomen
    {
      while ((pp->next_ds != NULL)&&(pp->ds_nr < zoom_left_x->ds_nr))
        if (pp!=NULL)
          pp = pp->next_ds;
        else 
        {
          //xvt_dm_post_error("Fehler beim Einlesen der Profildaten:_GetMinMax");
          char buf[200];
          xvt_res_get_str(STR_GETMINMAX,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
        }
    }
    
    if (pp!=NULL)
    {
      
      pMinMax->minX = pp->x;
      pMinMax->maxX = pp->x;
      if (pp->y!=BCE_NAN)
      {
        pMinMax->minY = pp->y;
        pMinMax->posMinY = pp->ds_nr;
      }
      pMinMax->maxY = pp->y;
      pMinMax->posMinX = pp->ds_nr;
      pMinMax->posMaxX = pp->ds_nr;
      pMinMax->posMaxY = pp->ds_nr;
      
      pp = pp->next_ds;
    }
    
    while( pp && ( zoom_info.level == 0 || ( zoom_right_x && pp->ds_nr <= zoom_right_x->ds_nr ) ) )
    {
      if( pp->x != BCE_NAN )
      {
        if( pp->x < pMinMax->minX )
        {
          pMinMax->minX = pp->x;
          pMinMax->posMinX = pp->ds_nr;
        }
        if( pp->x > pMinMax->maxX )
        {
          pMinMax->maxX = pp->x;
          pMinMax->posMaxX = pp->ds_nr;
        }
        if( ( pp->y < pMinMax->minY ) && ( pp->y != BCE_NAN ) )
        {
          pMinMax->minY = pp->y;
          pMinMax->posMinY = pp->ds_nr;
        }
        if( ( pp->y > pMinMax->maxY ) && ( pp->y != BCE_NAN ) )
        {
          pMinMax->maxY = pp->y;
          pMinMax->posMaxY = pp->ds_nr;
        }
      } // if pp->x !=BCE_NAN
      pp = pp->next_ds;
    }; // while pp
    
    /*    Andere Datensätze: nur Geländehöhe durchsuchen nicht relevant für Zoomfunktionen */
    double helpminx=pMinMax->minX;
    double helpmaxx=pMinMax->maxX;
    double helpminy=pMinMax->minY;
    double helpmaxy=pMinMax->maxY;
    
    for( int i = 1; i <= 49; i++ )
    {
      if(!checkbox && !checkbox_alle)
      {
        while((ptr_profil!=NULL)&&(ptr_profil->ds_nummer < datensatz))
          ptr_profil=ptr_profil->next;
      }
      else
      {
        ptr_profil=ptr_anfang;
        while((ptr_profil!=NULL)&&(ptr_profil->ds_nummer < i))
          ptr_profil=ptr_profil->next;
      }
      
      if( ((!checkbox && !checkbox_alle) || ( (checkbox || checkbox_alle) &&(draw_liste[i]!=0) )) && (i!=1) )
      {
        if (ptr_profil!=NULL)
        {
          pp=ptr_profil->datensatz;
          switch (ptr_profil->profiltyp)
          {
          case UK_BRUECKE:
          case OK_BRUECKE:
          case OK_WEHRS:
          case OK_GELAENDE:
          case KASTEN:
          case STATION_FIX://Dick 5.02.99
            //case KREISSEGM: // wird nicht gezeichnet, hat also keine ausdehnung
          case GELAENDE2:
          case SOHLHOEHE_2:
          case BUHNE:
          case WASSERSPIEGEL:
          case WASSERSPIEGEL_2:
          case BOESCHUNG_LINKS:
          case BOESCHUNG_RECHTS:
          case BOESCHUNG_LINKS_2:
          case BOESCHUNG_RECHTS_2:
          case DKOK:   //Dick 2.11.98
          case DKUK:
          case WSP_FIXIERUNG://Dick 04.02.99
          case BVHOEHE:
          case AUSUFERUNG_LINKS: 
          case AUSUFERUNG_RECHTS:
          case ENERGIEHOEHE:     
            {
              while (pp !=NULL  )
              {
                if( pp->x != BCE_NAN )
                {
                  if( zoom_info.level == 0 || 
                    ( zoom_left_x && zoom_left_x->x <= pp->x && 
                    zoom_right_x && pp->x <= zoom_right_x->x ) )
                  {
                    if ((pp->y > pMinMax->maxY)&&(pp->y != BCE_NAN))
                      pMinMax->maxY = pp->y;
                    if ((pp->y < pMinMax->minY)&&(pp->y != BCE_NAN))
                      pMinMax->minY = pp->y;
                  }; // if zoom_info.level
                }; // if pp->dx != BCE_NAN
                pp = pp->next_ds;
              }  //-while
            }
            break;
          case STATION: // = WSP_HOEHE
            while( pp )
            {
              Koord* pp2 = pp->next_ds; // ein Wasseroberflächenstück: pp - pp2
              if( pp2 && pp->x != BCE_NAN && pp2->x != BCE_NAN && pp->y != BCE_NAN )
              {
                // falls sich die beiden Strecken (pp->x, pp2->x) und (zoom_left_x->x, zoom_right_x->x) überschneiden
                // die y Werte anpassen
                if( zoom_left_x == NULL || zoom_right_x == NULL ||
                  ( zoom_left_x->x <= pp->x && pp->x <= zoom_right_x->x ) ||
                  ( zoom_left_x->x <= pp->x && pp2->x <= zoom_right_x->x ) )
                {
                  pMinMax->minY = min( pMinMax->minY, pp->y );
                  pMinMax->maxY = max( pMinMax->maxY, pp->y );
                };
                
              }; // if pp2
              
              pp = pp->next_ds;
            }; // while pp
            break;
          case MAUL:
          case EIPROFIL:
            {
              double hp,hx;
              pp=pp->next_ds;
              if ((pp !=NULL)&&(pp->x!=BCE_NAN))
              {
                hp = pp->x;
                for (int m=1;m<=3;m++)
                {
                  if(pp!=NULL)
                    pp=pp->next_ds; //um 3 Positionen weiterhängen
                }
                if ((pp !=NULL)&&(pp->x!=BCE_NAN))
                {
                  hx = pp->x;
                  if ( (hp + hx) > pMinMax->maxY )
                  {
                    pMinMax->maxY = hp+hx;
                    pMinMax->posMaxY = -1;
                  }
                  if ( hx < pMinMax->minY )
                  {
                    pMinMax->minY = hx;
                    pMinMax->posMinY = -1;
                  }
                }
              }
            }
            break;
          case KREIS:
            {
              double d,hx;
              if ((pp !=NULL)&&(pp->x!=BCE_NAN))
              {
                //   d = 0.5*pp->x;
                d = pp->x;
                for (int m=1;m<=3;m++)
                  pp=pp->next_ds; //um  Positionen weiterhängen
                if ((pp !=NULL)&&(pp->x!=BCE_NAN))
                {
                  hx = pp->x;
                  if ( (hx+d) > pMinMax->maxY )
                  {
                    pMinMax->maxY = d+hx;
                    pMinMax->posMaxY = -1;
                  }
                  if ( (hx-d) < pMinMax->minY )
                  {
                    pMinMax->minY = hx-d;
                    pMinMax->posMinY = -1;
                  }
                }
              }
            }
            break;
          case TRAPEZ:
            {
              double d,hx;
              if ((pp !=NULL)&&(pp->x!=BCE_NAN))
              {
                //   d = 0.5*pp->x;
                d = pp->x;
                for (int m=1;m<=5;m++)
                  pp=pp->next_ds; //um  Positionen weiterhängen
                if ((pp !=NULL)&&(pp->x!=BCE_NAN))
                {
                  hx = pp->x;
                  if ( (hx+d) > pMinMax->maxY )
                  {
                    pMinMax->maxY = d+hx;
                    pMinMax->posMaxY = -1;
                  }
                  if ( (hx-d) < pMinMax->minY )
                  {
                    pMinMax->minY = hx-d;
                    pMinMax->posMinY = -1;
                  }
                }
              }
            }
            break;
          case ARMCO84: /* kein updaten, da nicht gezeichnet wird !  */
          case ARMCO71:
          case GAUSS:
          case FLAECHE:
          { /* da bei FLAECHE 2.Gelände gezeichnet wird muss ein update
            von MinMax bzgl. Gelände2 erfolgen*/
            if(anzahl_aufrufe<3)//Dick 5.08.98 
              for (int ds=1;ds<=ds_info[0];ds++)
                if (typ[ds]==GELAENDE2)
                  GetMinMax(pMinMax,ds);
          }
          break;
          default:break;
    };  //-switch
    /*******************************************/
    //neu
    if(pMinMax->maxX>helpmaxx)
      helpmaxx=pMinMax->maxX;
    else
      pMinMax->maxX=helpmaxx;
    if(pMinMax->maxY>helpmaxy)
      helpmaxy=pMinMax->maxY;
    else
      pMinMax->maxY=helpmaxy;
    if(pMinMax->minX<helpminx)
      helpminx=pMinMax->minX;
    else
      pMinMax->minX=helpminx;
    if(pMinMax->minY<helpminy)
      helpminy=pMinMax->minY;
    else
      pMinMax->minY=helpminy;
    
    //neu
    
   } //if ptr_profil!=NULL
  } // if checkbox &&
  } //for i
  pMinMax->distanceX=  pMinMax->maxX - pMinMax->minX;
  pMinMax->distanceY=  pMinMax->maxY - pMinMax->minY;
  
  // wenn alle Stationswerte auf gleicher Höhe Division durch NULL in list.Paint !!!!!!
  if (pMinMax->distanceX==0)
  {
    pMinMax->distanceX=1;
  }
  if (pMinMax->distanceY==0)
  {
    pMinMax->distanceY=1;            //Dick 22.04.99 2->1
    pMinMax->maxY =pMinMax->minY +1;
  }
  
 } //if ptr_anfang!=NULL
 anzahl_aufrufe = 0;
}; // GetMinMax


void List::DeleteStation( Rem_Station* station ) // fast identisch mit list.cpp // jetzt nicht mehr: für Mehrfeldoptimierung Änderungen, sollten auch in list.cpp stattfinden?
// unterschiede: - win_listbutton33 statt win_list2
//               - anzahl_ds existiert nicht
{
  Profildatei* ptr_prof;
  Koord* ptr_koord, *ph;
  int nummer = 1, pos;
  int rel_pos = 0;
  int h_anzahl;
  int hilfsdsnummer;
  
  if ( ptr_anfang )
  {
    ptr_prof = ptr_anfang;
    while ( ptr_prof )
    {
      //  wenn Datensatz gelöscht ist überspringen 
      while( ptr_prof && ptr_prof->datensatz == NULL )
      {
        ptr_prof = ptr_prof->next;
        nummer++;
      }; // while ptr_prof ...
      
      if( !ptr_prof )
        break;
      
      ptr_koord = ptr_prof->datensatz;
      ph        = ptr_prof->datensatz;
      
      if( ( station->anzahl > 1 ) && ph  )    //Position suchen
      {
        int k = 1;
        
        while( ph && ( fabs( station->x - ph->x ) > PRECISION_KRD ) )
          ph = ph->next_ds;
        
        // für Decken unter und Oberkanten die Koordinaten kopieren, 
        // damit sie nicht gelöscht werden
        if( ph && ( ptr_prof->profiltyp == DKUK || ptr_prof->profiltyp == DKOK ) )
        {
          while( Get_Station_Num( station->x, ptr_prof->ds_nummer ) < station->anzahl && fabs( station->x - ph->x ) < PRECISION_KRD )
            Koord_Einf( station->x, ph->y, ph->ds_nr + 1, ptr_prof->profiltyp ); // eine kopie der Station anfertigen
        }; // if ph && DKUK
        //Ende Neu
        
        if( ( ptr_prof->profiltyp != GELAENDEHOEHE ) && ph && (ptr_prof->profiltyp != SOHLHOEHE ) ) // Position sichern
          station->nummer = ph->ds_nr + rel_pos - 1;
        
        while( ph && ( ph->ds_nr < station->nummer ) )
        {
          ph = ph->next_ds;
          k++;
        }; // while ph
        if( ptr_prof->profiltyp == GELAENDEHOEHE || ptr_prof->profiltyp == SOHLHOEHE ) // Position sichern
          rel_pos = k;
      }; // if station_anzahl > 1 && ph
      
      ph = NULL;
      
      while( ( ptr_koord->x != station->x ) && ptr_koord->next_ds )
        ptr_koord = ptr_koord->next_ds;
      
      if ( station->anzahl == 1 )   //Wert in "GELAENDE" 1x vorhanden
      {
        h_anzahl = Get_Station_Num(station->x,nummer);// Station ist vorhanden

        if( h_anzahl == 0 && ptr_prof->profiltyp == LP_TEXT )
        {
          int ptNr = station->nummer;
          SLIST commentList = slist_comment; //m_profilData->slist_comment;

          // Anzahl der Blöcke rausfinden
          SLIST_ELT elt = xvt_slist_get_first( commentList );

          int dummy, count;
          if( sscanf( xvt_slist_get( commentList, elt, NULL ), "%d %d", &dummy, &count ) == 2 )
          {
            elt = xvt_slist_get_next( commentList, elt );

            for( int i = 0; i < count; i++ )
            {
              if( elt == NULL )
                break;
              
              // Anzahl der Linien pro profil lesen               
              int p1, p2, p3, p4, p5, p6;
              if( sscanf( xvt_slist_get( commentList, elt, NULL ), "%d %d %d %d %d %d", &p1, &p2, &p3, &p4, &p5, &p6 ) != 6 )
                break;
              
              // falls dies die PunktNummer ist, den Datensatz löschen
              bool bDelete = ( p5 == ptNr );
              
              // falls die Punktnummer grösser ist, um eins vermindern
              if( p5 > ptNr )
              {
                // das funktioniert nur, wenn der String sicher nicht länger wird!
                sprintf( xvt_slist_get( commentList, elt, NULL ), "%d %d %d %d %d %d", p1, p2, p3, p4, p5 - 1, p6 );
              }
              
              for( int j = 0; j < p1; j++ )
              {
                SLIST_ELT lastElt = elt;
                elt = xvt_slist_get_next( commentList, elt );

                if( bDelete )
                {
                   xvt_slist_rem( commentList, lastElt );
                   ds_info[nummer]--;
                }

                if( elt == NULL )
                  break;
              }

              SLIST_ELT lastElt = elt;

              elt = xvt_slist_get_next( commentList, elt );

              if( bDelete )
              {
                 ds_info[nummer]--;
                 xvt_slist_rem( commentList, lastElt );
              }
           } // for i
            
            
          }; // if sscanf == 2
        }
        
        if( h_anzahl == 1 )
        {
          if ( (ptr_prof->profiltyp!=DURCHST_BEREICH)&&(ptr_prof->profiltyp!=TRENNFLAECHEN)&&(ptr_prof->profiltyp!=BORDVOLL)&&(ptr_prof->profiltyp!=MODELLGRENZEN) )
          {
            if ((ptr_prof->profiltyp==ARMCO84)&&(ptr_koord->ds_nr==4))
            {
              //xvt_dm_post_error("y-Wert %4lf in ARMCO84-Profil ist ungültig.Bitte korrigieren!",station->x);
              char buf[200],buf2[200];
              xvt_res_get_str(STR_DELETE_STATION_1,buf,sizeof(buf));
              xvt_res_get_str(STR_Y_WERT,buf2,sizeof(buf2));
              xvt_dm_post_note("%s%4lf%s",buf2,station->x,buf);                               
            }
            if ((ptr_prof->profiltyp==ARMCO71)&&(ptr_koord->ds_nr==4))
            {
              //xvt_dm_post_error("y-Wert %4lf in ARMCO71-Profil ist ungültig.Bitte korrigieren!",station->x);
              char buf[200],buf2[200];
              xvt_res_get_str(STR_DELETE_STATION_2,buf,sizeof(buf));
              xvt_res_get_str(STR_Y_WERT,buf2,sizeof(buf2));
              xvt_dm_post_note("%s%4lf%s",buf2,station->x,buf);
              
            }
            else
              DeleteKoord(ptr_koord->ds_nr,nummer,station->x,ptr_koord->y);
          }
          else  //Trennfl., Durchstr. Ber., Bordvoll
          {
            char temp[25];
            
            char buf[200],buf2[200];//Dick 26.11.99
            xvt_res_get_str(1000+ptr_prof->profiltyp,temp,sizeof(temp));
            xvt_res_get_str(STR_Y_WERT_IN,buf,sizeof(buf));
            xvt_res_get_str(STR_DELETE_STATION_3,buf2,sizeof(buf2));
            strcat(buf," ");
            strcat(buf,temp);
            strcat(buf," ");
            strcat(buf,buf2);
            xvt_res_get_str(STR_WARNING,buf2,sizeof(buf2));
            MessageBox(NULL,buf,buf2,MB_ICONINFORMATION|MB_OK);
            //xvt_dm_post_note("y-Wert in %s ist ungültig geworden.\nDatensatz wird gelöscht!",temp);
            
            hilfsdsnummer=ptr_prof->ds_nummer; 
            DeleteNode(ptr_prof->ds_nummer, (int*)&ds_info, (int*)&typ );
            nummer--;
            // in list.cpp: win:list2
            xvt_list_rem(win_list2,hilfsdsnummer-1);
            
            xvt_list_resume(win_list2);
            xvt_list_set_sel(win_list2,0,TRUE);
          }
        }
        else if (h_anzahl>1)
        {
          if (ptr_prof->profiltyp == GELAENDEHOEHE)
            DeleteKoord(station->nummer,nummer,station->x,ptr_koord->y);
          else
          {
            if ( (ptr_prof->profiltyp!=DURCHST_BEREICH)&&(ptr_prof->profiltyp!=TRENNFLAECHEN)
              &&(ptr_prof->profiltyp!=BORDVOLL)&&(ptr_prof->profiltyp!=MODELLGRENZEN) )
            {
              if((ptr_prof->profiltyp!=ARMCO84)&&(ptr_prof->profiltyp!=ARMCO71))
                for (int j=1;j<=h_anzahl;j++)
                {
                  pos = Get_First_Station(station->x,nummer);
                  DeleteKoord(pos,nummer,station->x,ptr_koord->y);
                }
            }
            else  //Trennfl., Druchstr. Ber., Bordvoll
            {
              char temp[25];
              
              //xvt_dm_post_note("y-Wert in %s ist ungültig geworden.Datensatz wird gelöscht!",temp);
              char buf[200],buf2[200];//Dick 26.11.99
              xvt_res_get_str(1000+ptr_prof->profiltyp,temp,sizeof(temp));
              xvt_res_get_str(STR_Y_WERT_IN,buf,sizeof(buf));
              xvt_res_get_str(STR_DELETE_STATION_3,buf2,sizeof(buf2));
              strcat(buf," ");
              strcat(buf,temp);
              strcat(buf," ");
              strcat(buf,buf2);
              xvt_res_get_str(STR_WARNING,buf2,sizeof(buf2));
              MessageBox(NULL,buf,buf2,MB_ICONINFORMATION|MB_OK);
              
              hilfsdsnummer=ptr_prof->ds_nummer; 
              DeleteNode(ptr_prof->ds_nummer, (int*)&ds_info, (int*)&typ );
              anzahl_ds--;
              nummer--;
              xvt_list_rem(win_list2,hilfsdsnummer-1);
              
              xvt_list_resume(win_list2);
              xvt_list_set_sel(win_list2,0,TRUE);
            }
          }
        }
      }
      else   //Wert in "GELAENDE"mehrmals vorhanden
      {
        if ( (ptr_prof->profiltyp!=DURCHST_BEREICH)&&(ptr_prof->profiltyp!=TRENNFLAECHEN) )
          DeleteKoord(station->nummer,nummer,station->x,ptr_koord->y);
      }
      ptr_prof = ptr_prof->next;
      nummer++;
    }; // while ptr_prof
  }; // if ptr_anfang
}; // DeleteStation


/**************************************************************************/
int List::Get_First_Station(double station ,int nr)
{
  Profildatei *ptr_prof;
  Koord *ptr_koord;
  int position=0,i=1; //Dick 23.07.98 i 0->1 sonst wurde falsche station gelöscht wenn >1 gleich war
  
  if (ptr_anfang != NULL)
  {
    ptr_prof = ptr_anfang;
    while ((ptr_prof->ds_nummer != nr)&&(ptr_prof->next!=NULL))
      ptr_prof = ptr_prof->next;
    ptr_koord = ptr_prof->datensatz;
    while (ptr_koord != NULL)
    {
      if (ptr_koord->x == station)
      {
        position = i;
        ptr_koord=NULL;
      }
      else
      {
        ptr_koord=ptr_koord->next_ds;
        i++;
      }
    }
  }
  return position;
}
/**************************************************************************/
int List::Get_Station_Num( double station, int nr ) // identisch zu list_dll.cpp
// zählt wieviele gleiche stationen ein Datensatz hat
// Parameter:
//        double station: gescuhter Stationswert
//        int nr: Im Datensatz mit nr nr wird gesucht
// Return:
//       Anzahl der Stationen mit x-Wert station in Datensatz nr
{
  int anzahl = 0;
  
  if (ptr_anfang )
  {
    Profildatei* ptr_prof = ptr_anfang;
    
    while( ( ptr_prof->ds_nummer != nr ) && ptr_prof->next )
      ptr_prof = ptr_prof->next;
    
    Koord* ptr_koord = ptr_prof->datensatz;
    while( ptr_koord )
    {
      if ( fabs( ptr_koord->x - station ) < PRECISION_KRD )
        anzahl++;
      ptr_koord = ptr_koord->next_ds;
    }; // while ptr_koord
  }; // if ptr_anfang
  return anzahl;
}; // GetStation_Num

/**************************************************************************/
double List::Get_Num_Station(int ds_nummer,int *position,int correct)
/* aus dem Wert 'position' die zugehörige Station in [m] zurückgeben*/
{
  double last_x=BCE_NAN;
  Koord *ptr_koord;
  if (ptr_anfang != NULL)
  {
    ptr_koord = ptr_anfang->datensatz;
    
    if (zoom_info.level>0)  //ZOOM
    {
      while ((ptr_koord != NULL)&&(ptr_koord != zoom_left_x))
        ptr_koord=ptr_koord->next_ds;
      
      if(fehl_korr==FALSE) 
        if (ptr_koord != NULL)
          *position = (*position) +ptr_koord->ds_nr -1;
          /*  if (zoom_info.level>1)
          position = position+zoom_info.pos_station_min-1;
        else */
        //    if (ptr_koord != NULL)
        //       *position = (*position) +ptr_koord->ds_nr -1;
        //    else *position = 0;
        //      while ((ptr_koord != NULL)&&(ptr_koord->ds_nr < *position))
        //        ptr_koord=ptr_koord->next_ds;
    }
    //    else
    {
      while ((ptr_koord != NULL)&&(ptr_koord->ds_nr < *position))
      {
        if (correct)
          if (ptr_koord->y == BCE_NAN) /* Korrektur, falls BCE_NAN-Werte in Gelände*/
          {
            (*position)++;
          }
          if (ptr_koord->y != BCE_NAN)
          {
            last_x = ptr_koord->x;   //letzten gültigen Wert !=BCE_NAN sichern
          }
          ptr_koord=ptr_koord->next_ds;
      }
      
      /* Korrektur, falls BCE_NAN-Werte in Gelände so wird der nächste gültige genommen*/
      if(ptr_koord != NULL)
        if (ptr_koord->y == BCE_NAN)
          while ((ptr_koord != NULL)&&(ptr_koord->y == BCE_NAN))
            ptr_koord=ptr_koord->next_ds;
          if (ptr_koord !=NULL)
            *position = ptr_koord->ds_nr ; //zur Korrektur
    } //-while
  }
  if (ptr_koord !=NULL)
    return ptr_koord->x;
  else
  {
    *position = ds_info[1];//Abbruch bei Scrollen
    return last_x;   //ehemals:BCE_NAN;
  }
}
/**************************************************************************/

#define LEFT  1
#define RIGHT 2

int List::Get_Num_Station_Zoom(ZOOM *info,int direction)
/* aus dem Wert: zoom_info.pos_station_max und zoom_info.pos_station_min
die zugehörige Station in [m] zurückgeben.
RETURN: Listenende erreicht ja/nein
*/
{
  double last_x;
  int    last_pos;
  int    stop=0;
  Koord *ptr_koord;
  if (ptr_anfang != NULL)
  {
    ptr_koord = ptr_anfang->datensatz;
    while ((ptr_koord != NULL)&&(ptr_koord->ds_nr < info->pos_station_min))
    {
      if (ptr_koord->y != BCE_NAN)
      {
        last_x = ptr_koord->x;   //letzten gültigen Wert !=BCE_NAN sichern
        last_pos = ptr_koord->ds_nr;
      }
      ptr_koord=ptr_koord->next_ds;
    }
    /* Korrektur, falls BCE_NAN-Werte in Gelände so wird der nächste gültige genommen*/
    if(ptr_koord != NULL)
    {
      if (ptr_koord->y == BCE_NAN)
      {
        if (ptr_koord->pre_ds !=NULL)
          while ((ptr_koord != NULL)&&(ptr_koord->y == BCE_NAN)&& !Exist_Station_Hoehe_Allg(ptr_koord->x,ptr_koord->y,scr.datensatz))
          {
            if ((direction == RIGHT)&&( (info->pos_station_max-info->pos_station_min+1)>3  ))//Dick 7.08.98
              ptr_koord=ptr_koord->next_ds;
            else //if (direction == LEFT)
              ptr_koord=ptr_koord->pre_ds;
          }
          else
          {
            if (info->pos_station_min==1)
            {
              while ((ptr_koord != NULL)&&(ptr_koord->y == BCE_NAN))
              {
                ptr_koord=ptr_koord->next_ds;
              }
              stop =1;
            }
          }
      }
    }
    
    if(ptr_koord != NULL)
    {
      info->station_min = ptr_koord->x;
      info->pos_station_min = ptr_koord->ds_nr ;
    }
    else
    {
      info->station_min = last_x;
      info->pos_station_min = last_pos;
    }
    
    while ((ptr_koord != NULL)&&(ptr_koord->ds_nr < info->pos_station_max))
    {
      if (ptr_koord->y != BCE_NAN)
      {
        last_x = ptr_koord->x;   //letzten gültigen Wert !=BCE_NAN sichern
        last_pos = ptr_koord->ds_nr;
      }
      ptr_koord=ptr_koord->next_ds;
    }
    /* Korrektur, falls BCE_NAN-Werte in Gelände so wird der nächste gültige genommen*/
    if(ptr_koord != NULL)
    {
      if (ptr_koord->y == BCE_NAN)
      {
        while ((ptr_koord != NULL)&&(ptr_koord->y == BCE_NAN) && !Exist_Station_Hoehe_Allg(ptr_koord->x,ptr_koord->y,scr.datensatz))//Dick 7.08.98
        {
          info->pos_station_max = ptr_koord->ds_nr ; //zur Korrektur
          if (direction == RIGHT)
            ptr_koord=ptr_koord->next_ds;
          else
          {
            if ((direction == LEFT)&&( (info->pos_station_max-info->pos_station_min-1)>3  )) //Dick 7.08.98
              ptr_koord=ptr_koord->pre_ds;
            else
              ptr_koord=ptr_koord->next_ds;
          }
        }
      }
    }
    
    if(ptr_koord != NULL)
    {
      info->station_max = ptr_koord->x;
      info->pos_station_max = ptr_koord->ds_nr ;
    }
    else
    {
      info->station_max = last_x;
      info->pos_station_max = last_pos;
      stop =1;
    }
    
  }
  
  return stop;
}
/**************************************************************************/
double List::Get_Station_Hoehe(double station)
/* zum Stationswert:'station' die erste zugehörige Höhe zurückgeben*/
{
  Koord* ptr_koord;
  if( ptr_anfang != NULL )
  {
    ptr_koord = ptr_anfang->datensatz;
    while( ptr_koord != NULL && ptr_koord->x != station )
      ptr_koord=ptr_koord->next_ds;
  }
  if( ptr_koord != NULL )
    return ptr_koord->y;
  else return 0.0;
}
/**************************************************************************/

int List::Exist_Station_Hoehe(double station,double hoehe)
/* existiert in Gelaende zum Stationswert:'station' eine Höhe :hoehe*/
{
  Koord *ptr_koord;
  if (ptr_anfang != NULL)
  {
    ptr_koord = ptr_anfang->datensatz;
    while ((ptr_koord != NULL)&&(ptr_koord->x != station))
      ptr_koord=ptr_koord->next_ds;
    if ((ptr_koord != NULL)&&(ptr_koord->x==station)&&(ptr_koord->y== hoehe))
      return 1;
    else
    {
      while ((ptr_koord != NULL)&&(ptr_koord->x == station)&&(ptr_koord->y!=hoehe))
        ptr_koord=ptr_koord->next_ds;
      if ((ptr_koord != NULL)&&(ptr_koord->x==station)&&(ptr_koord->y== hoehe))
        return 1;
      else return 0;
    }
  }
  return 0;
}
/**************************************************************************/
/**************************************************************************/

int List::Exist_Station_Hoehe_Allg(double station,double hoehe,int datensatz)
/* existiert in Datensatz: 'datensatz' zum Stationswert:'station' eine Höhe :hoehe*/
{
  Koord *ptr_koord_help;
  Profildatei *profil_help;
  if (ptr_anfang != NULL)
  {  
    profil_help=ptr_anfang;
    while(profil_help->ds_nummer < datensatz && profil_help!=NULL)
      profil_help=profil_help->next;
    ptr_koord_help = profil_help->datensatz;
    while ((ptr_koord_help != NULL)&&(ptr_koord_help->x != station))
      ptr_koord_help=ptr_koord_help->next_ds;
    if(hoehe!=BCE_NAN)
    {
      if ((ptr_koord_help != NULL)&&(ptr_koord_help->x==station)&&(ptr_koord_help->y== hoehe))
        return 1;
      else
      {
        while ((ptr_koord_help != NULL)&&(ptr_koord_help->x == station)&&(ptr_koord_help->y!=hoehe))
          ptr_koord_help=ptr_koord_help->next_ds;
        if ((ptr_koord_help != NULL)&&(ptr_koord_help->x==station)&&(ptr_koord_help->y== hoehe))
          return 1;
        else return 0;
      }
    }
    else
    {
      if ((ptr_koord_help != NULL)&&(ptr_koord_help->x==station)&&(ptr_koord_help->y!=BCE_NAN))
        return 1;
      else
      {
        while ((ptr_koord_help != NULL)&&(ptr_koord_help->x == station)&&(ptr_koord_help->y==BCE_NAN))
          ptr_koord_help=ptr_koord_help->next_ds;
        if ((ptr_koord_help != NULL)&&(ptr_koord_help->x==station)&&(ptr_koord_help->y!= BCE_NAN))
          return 1;
        else return 0;
      }
    }
  }
  return 0;
}
/**************************************************************************/

void List::Change_Station(double x_new,double x_old,int anzahl,int position)
{
  int pos_nr=0;
  if (ptr_anfang != NULL)
  {
    ptr_profil = ptr_anfang;
    
    /*if (anzahl >1)
    {
    pp = ptr_profil->datensatz;
    while (pp->ds_nr <= position)
    {
    if (pp->x == x_old)
    pos_nr++;      //in pos_nr :=position, der wievielte x-Wert
    pp=pp->next_ds; //Dick 4.08.98
    }
  }*/
    
    //if (ptr_profil->next !=NULL)       //Dick 15.06.99 weg sonst wird nicht gespeichert
    //   ptr_profil = ptr_profil->next;
    while (ptr_profil !=NULL)
    {
      pp = ptr_profil->datensatz;
      
      while ((pp != NULL)&&(pp->x != x_old))   //1.Wert oder Listenende
        pp = pp->next_ds;
      
      if ((anzahl > 1)&&(pp!=NULL))    //Position suchen
      {
        //int k=1;
        while ((pp !=NULL)&&(pp->ds_nr < pos_nr))
          //{
          pp=pp->next_ds;
        //if (pp->x ==x_old)
        //  k++;
        //}
        pp->x = x_new;
      }
      if ((anzahl ==1)&&(pp!=NULL))   //Wert in "GELAENDE" 1x vorhanden
      {
        while (pp!=NULL)
        {
          if (pp->x == x_old)
            switch (ptr_profil->profiltyp)
          {
              case MAUL:
                break;
              case EIPROFIL:
                break;
              case KREIS:
                break;
              case KREISSEGM:
                break;
              case ARMCO84:
                break;
              case ARMCO71:
                break;
              case NWRINNE:
                break;
              default:
                {
                  pp->x = x_new;
                }
                break;
          } ;
          pp= pp->next_ds;
        }
      } //-if
      ptr_profil = ptr_profil->next;
    }  //-while
  }
}
/**************************************************************************/
void List::Change_Station2(double x_new,double x_old,int anzahl,int position,int ds_nummer)
{
  int pos_nr=0;
  Profildatei *profil_help;
  Koord *pp_help;
  if (ptr_anfang != NULL)
  {
    profil_help = ptr_anfang;
    if(ds_nummer>0)
      while(profil_help->ds_nummer!=ds_nummer)
        profil_help=profil_help->next;
      if (anzahl >1)
      {
        pp_help = profil_help->datensatz;
        while (pp_help->ds_nr <= position)
        {
          if (pp_help->x == x_old)
            pos_nr++;      //in pos_nr :=position, der wievielte x-Wert
          pp_help=pp_help->next_ds; //Dick 4.08.98
        }
      }
      
      //if (profil_help->next !=NULL)      sonst rutscht  er ein weiter
      //   profil_help = profil_help->next;
      //while (profil_help !=NULL)
      if(profil_help !=NULL)
      {
        pp_help = profil_help->datensatz;
        
        while ((pp_help != NULL)&&(pp_help->x != x_old))   //1.Wert oder Listenende
          pp_help = pp_help->next_ds;
        
        if ((anzahl > 1)&&(pp_help!=NULL))    //Position suchen
        {
          int k=1;
          while ((pp_help !=NULL)&&(k < pos_nr))
          {
            pp_help=pp_help->next_ds;
            if (pp_help->x ==x_old)
              k++;
          }
          if(k==pos_nr)
            pp_help->x = x_new;
        }
        if ((anzahl ==1)&&(pp_help!=NULL))   //Wert in "GELAENDE" 1x vorhanden
        {
          while (pp_help!=NULL)
          {
            if (pp_help->x == x_old)
              switch (profil_help->profiltyp)
            {
              case MAUL:
                break;
              case EIPROFIL:
                break;
              case KREIS:
                break;
              case KREISSEGM:
                break;
              case ARMCO84:
                break;
              case ARMCO71:
                break;
              case NWRINNE:
                break;
              default:
                {
                  pp_help->x = x_new;
                  return;
                }
                break;
            } ;
            pp_help= pp_help->next_ds;
          }
        } //-if
        //profil_help = profil_help->next;
      }  //-while
  }
}
/**************************************************************************/
void List::Change_Station_Allg(double x_new,double x_old,int anzahl,int position)
{ //Wenn x-Koordinate wurde geändert muß in allen Datensätzen aktualisiert werden
  int pos_nr=0;
  Profildatei *profil_help;
  Koord *pp_help;
  
  if (ptr_anfang != NULL)
  {
    profil_help = ptr_anfang;
    
    if (anzahl >1)
    {
      pp_help = profil_help->datensatz;
      while (pp_help->ds_nr <= position )
      {
        if (pp_help->x == x_old)
          pos_nr++;      //in pos_nr :=position, der wievielte x-Wert
        pp_help=pp_help->next_ds; //Dick 4.08.98
        if(pp_help==NULL)break;//Dick 6.08.98
      }
    }
    
    //if (profil_help->next !=NULL)      sonst rutscht  er ein weiter
    //   profil_help = profil_help->next;
    while (profil_help !=NULL)
      if(profil_help !=NULL)
      {
        pp_help = profil_help->datensatz;
        
        while ((pp_help != NULL)&&(pp_help->x != x_old))   //1.Wert oder Listenende
          pp_help = pp_help->next_ds;
        //Dick 29.04.99
        if ((anzahl > 1)&&(pp_help!=NULL)&&profil_help->profiltyp!=GELAENDE2&&profil_help->profiltyp!=FLAECHE)    //Position suchen
        {
          int k=0;
          while ((pp_help !=NULL)&&(k < pos_nr))
          {
            
            if (pp_help->x ==x_old)
              k++;
            
            if(k==pos_nr)
              if((typ[scr.datensatz] !=GELAENDE2 || (typ[scr.datensatz] ==GELAENDE2 && pp_help->y != BCE_NAN))) //Dick 30.04.99
                if((typ[scr.datensatz] !=FLAECHE || (typ[scr.datensatz] ==FLAECHE && pp_help->y != BCE_NAN))) //Dick 30.04.99
                  pp_help->x = x_new;
                pp_help=pp_help->next_ds;
          }
          //if(k==pos_nr)
          //pp_help->x = x_new;
        }          //Dick 29.04.99
        if ((anzahl ==1)&&(pp_help!=NULL)&&profil_help->profiltyp!=GELAENDE2&&profil_help->profiltyp!=FLAECHE)   //Wert in "GELAENDE" 1x vorhanden
        {
          while (pp_help!=NULL)
          {
            if (pp_help->x == x_old)
              if((typ[scr.datensatz] !=GELAENDE2 || (typ[scr.datensatz] ==GELAENDE2 && pp_help->y != BCE_NAN)))//Dick 30.04.99
                if((typ[scr.datensatz] !=FLAECHE || (typ[scr.datensatz] ==FLAECHE && pp_help->y != BCE_NAN))) //Dick 30.04.99
                {
                  pp_help->x = x_new;   
                  break;
                }
                pp_help= pp_help->next_ds;
          }
        } //-if
        profil_help = profil_help->next;
      }  //-while
  }
}
/**************************************************************************/

void List::SaveScrollDaten(WINDOW win,Scroller *scr,char *str,int typ)
{
/*
Daten aus scroller *scr an die entsprechenden Positionen
in ptr_profil schreiben
Werte für int 'typ':  0 = x  ,1 = y , 2 =z-Spalte
  */
  double  wert;
  int  n1,n2;
  int dy = 0;
  //  int pos=1;
  
  if (strlen(str)==0)
    wert = BCE_NAN;
  else
    wert = atof(str);
  ptr_profil = ptr_anfang;
  
  if ( typ == 2 )     //   = Z - Koordinate ,anderen Datensatz wählen
  {
    while ((ptr_profil->ds_nummer < scr->datensatz)&&(ptr_profil->next != NULL))
      ptr_profil = ptr_profil->next;
  }
  pp = ptr_profil->datensatz;
  
  if ((win>=3000)&&(win<= 3044))     // Koordinate aus ALPHA-Editor
  {
    if (typ ==1)    //2.Spalte=y-Koord
      dy=15;
    if (typ ==2)    //3.Spalte=z-KOORD.
      dy=30;
    
    n1 = win - 3000 ;   // +1;
    
    n1 = n1 + scr->scrollpos - dy;
  }
  
  else
  {
    if ((win>=2000)&&(win<= 2014))   // Koordinate aus Grafik-Editor
    {
      if (typ ==1)    //2.Spalte=y-Koord
        dy=5;
      if (typ ==2)    //3.Spalte=z-KOORD.
        dy=10;
      n1 = win -2000 ;
      n1 = n1 + scr->scrollpos - dy;
    }
    else
      xvt_dm_post_error("list.SaveScrollDaten:Error Window Id");                                                 
  }
  if (typ ==2)    //3.Spalte=z-KOORD.
  {
    n2=Get_Station_pos_num(scr->x[n1-scr->scrollpos],Get_Station_pos_allg(scr->x[n1-scr->scrollpos],n1,GELAENDEHOEHE),scr->datensatz);
    if(pp !=NULL)//Dick 10.08.98
      while ((pp->ds_nr < n2) && (pp->next_ds != NULL))
        pp = pp->next_ds;
  }
  else
  {
    if(pp !=NULL)//Dick 10.08.98
      while ((pp->ds_nr < n1) && (pp->next_ds != NULL)) 
        pp = pp->next_ds;
  }
  
  if ((typ == 0) && (wert== BCE_NAN))  //1.Spalte:x-Wert !!
  {  // KOORDINATE LÖSCHEN
    // Station in allen Datensätzen löschen ?",pp->x))
    rem_station.nummer = n1;
    rem_station.typ = scr->datensatz;
    rem_station.x = pp->x;
    rem_station.y = pp->y;
    rem_station.anzahl=Get_Station_Num(pp->x,1);
    
    DeleteStation(&rem_station);
    
    scr->anzahl = ds_info[1];
  }
  else
    switch (typ)
  {
     case 0:
       {
         
         if (pp->x ==BCE_NAN)
           pp->x = wert;
         else
         {
           if (wert != pp->x)
           {
             rem_station.nummer = n1;
             rem_station.typ = scr->datensatz;
             rem_station.x = pp->x;   //noch alter x-Wert
             rem_station.y = pp->y;
             rem_station.anzahl=Get_Station_Num(pp->x,1);
             
             if ((rem_station.anzahl==1)&&(ds_info[1]==1))   //nur 1Wert-1.Edit Feld editiert
               Change_Station(wert,rem_station.x,1,1);
             else
             {
               Change_Station_Allg(wert,rem_station.x,rem_station.anzahl,rem_station.nummer);
             }
           }
         }
       }
       break;
     case 1: pp->y = wert;
       break;
     case 2:
       {
         int anzahl_stationen_gelaende,
           anzahl_stationen_datensatz;
         int x_position_gelaende;
         int g, x_pos=0, einf_pos;
         
         anzahl_stationen_gelaende = Get_Station_Num(scr->x[n1-scr->scrollpos],GELAENDEHOEHE);
         anzahl_stationen_datensatz = Get_Station_Num(scr->x[n1-scr->scrollpos],scr->datensatz);
         if (anzahl_stationen_gelaende == 1)
         {
           if (anzahl_stationen_datensatz == 1)
           {
             if (scr->x[n1-scr->scrollpos]==pp->x)
               pp->y = wert;
             else
             {
               pp = ptr_profil->datensatz;
               while ((pp!= NULL) && (pp->x != scr->x[n1-scr->scrollpos] ))
                 pp = pp->next_ds;
               pp->y = wert;
             }
           }
           else if (anzahl_stationen_datensatz == 0 && wert!=BCE_NAN)
           {
             if(ptr_profil->profiltyp!= TRENNFLAECHEN && 
               ptr_profil->profiltyp!= DURCHST_BEREICH && 
               ptr_profil->profiltyp!= BORDVOLL &&
               ptr_profil->profiltyp!= MODELLGRENZEN)
             {
               einf_pos=PositionSuchen(scr->datensatz,n1);                            
               NewKoord(scr->datensatz,einf_pos) ;  // GaussTest //Neue Koordinate  einfügen
               Change_Station2(scr->x[n1-scr->scrollpos],BCE_NAN,1,n1-scr->scrollpos,scr->datensatz); // GaussTest //x-Koord. aktualisieren
               ChangeNumber(scr->x[n1-scr->scrollpos],wert,scr->datensatz);//GaussTest // Neue z-Koord eintragen
             }
             //pp->y = wert;//GaussTest
           }
         }  //-if (anzahl_stationen_gelaende == 1)
         else
         {
           if (anzahl_stationen_gelaende == anzahl_stationen_datensatz)
           {
             if (pp!=NULL)
               pp->y = wert;
           }
           else if(anzahl_stationen_gelaende > anzahl_stationen_datensatz)
           {
             for (g=0;g<=14;g++)
             {
               if ((scr->x[g]==scr->x[n1-scr->scrollpos])&&(g<=n1-scr->scrollpos))
                 x_pos++;
             }
             x_position_gelaende =Get_Station_pos(scr->x[n1-scr->scrollpos],x_pos,scr->scrollpos);
             if (x_position_gelaende > anzahl_stationen_datensatz)
             {
               for (int e=1;e<= x_position_gelaende - anzahl_stationen_datensatz;e++ )
               {
                 if( ( ptr_profil->profiltyp!= TRENNFLAECHEN ) && 
                   ( ptr_profil->profiltyp!= DURCHST_BEREICH ) &&
                   ( ptr_profil->profiltyp!= BORDVOLL ) && 
                   ( ptr_profil->profiltyp!= MODELLGRENZEN ) )
                 {                
                   einf_pos=PositionSuchen(scr->datensatz,n1+e-1);                                                  
                   NewKoord(scr->datensatz,einf_pos) ;  // GaussTest //Neue Koordinate  einfügen
                   Change_Station2(scr->x[n1-scr->scrollpos],BCE_NAN,1,n1-scr->scrollpos,scr->datensatz); // GaussTest //x-Koord. aktualisieren
                   ChangeNumber2(scr->x[n1-scr->scrollpos],wert,scr->datensatz,einf_pos);//GaussTest // Neue z-Koord eintragen
                 }
               }
             }
             else //genug Stationen vorhanden
               pp->y = wert;
             
           }
         }
         
       }
       break;
     default:pp->x=0.0;pp->y=0.0;break;
    }
}
/**************************************************************************/
int List::ChangeNumber(double station,double y,int ds_nr)
{
  Profildatei *pd;
  
  pd = ptr_anfang;
  
  while ((pd->ds_nummer < ds_nr)&&(pd->next != NULL))
    pd = pd->next;
  pp = pd->datensatz;
  
  while ((pp!=NULL)&&(pp->x != station))
    pp = pp->next_ds;
  if (pp->x == station)
  {
    pp->y = y;
    return 1;
  }
  else return 0;
}
/**************************************************************************/
/**************************************************************************/
int List::ChangeNumber2(double station,double y,int ds_nr,int pos)
{   //Wenn mehrere Stationen gleich sind
  Profildatei *pd;
  Koord *help_koord;
  pd = ptr_anfang;
  
  while ((pd->ds_nummer < ds_nr)&&(pd->next != NULL))
    pd = pd->next;
  help_koord = pd->datensatz;
  
  while ((help_koord!=NULL)&&(help_koord->ds_nr != pos))
    help_koord = help_koord->next_ds;
  if (help_koord->x == station)
  {
    help_koord->y = y;
    return 1;
  }
  else return 0;
}
/**************************************************************************/

void List::Sort_In_Ds(double x,double y,int ds_nr,int pre_x)
{
  Koord *ppa, *help,*new_pp;
  Profildatei *ptr_pp;
  
  ptr_pp = ptr_anfang;
  
  while ((ptr_pp !=NULL)&& (ptr_pp->ds_nummer < ds_nr))
    ptr_pp = ptr_pp->next;
  
  switch (ptr_pp->profiltyp)
  {
  case TRENNFLAECHEN:
  case BORDVOLL:
  case DURCHST_BEREICH:
  case MODELLGRENZEN:
    {
    }
    break;
  default:
    {
      ppa=ptr_pp->datensatz;
      
      new_pp = new Koord;
      new_pp->x = x;
      new_pp->y = y;
      new_pp->attr = 1;
      new_pp->pre_x = pre_x;
      new_pp->pre_y = 0;
      new_pp->status =-1;
      ds_info[ptr_pp->ds_nummer]++;
      
      while ((ppa->next_ds != NULL)&&(ppa->x<=x))
        ppa = ppa->next_ds;
      help = ppa;
      if ((ppa->next_ds==NULL)&&(ppa->x<=x))   // ListenENDE
      {
        ppa->next_ds=new_pp;
        new_pp->next_ds = NULL;
        new_pp->pre_ds=help;
        new_pp->ds_nr = ppa->ds_nr +1;
      }
      else
      {
        if (ppa->pre_ds!=NULL)
        {
          ppa->pre_ds->next_ds = new_pp;
          new_pp->next_ds = ppa;
          new_pp->pre_ds = ppa->pre_ds;
          ppa->pre_ds = new_pp;
          new_pp->ds_nr = ppa->ds_nr ;
        }
        else  // Listenanfang !
        {
          if (ptr_pp->datensatz == ppa)
          {
            ptr_pp->datensatz = new_pp;
            new_pp->next_ds = ppa;
            ppa->pre_ds = new_pp;
            new_pp->pre_ds = NULL;
            new_pp->ds_nr = ppa->ds_nr;
          }
        }
        while (ppa!=NULL)
        {
          if (ppa->next_ds ==NULL)
            ppa->ds_nr = ppa->pre_ds->ds_nr+1;
          else
            ppa->ds_nr =ppa->next_ds->ds_nr;
          ppa = ppa->next_ds;
        }
      }
      
    }
    break;
  }
  
}


/*!
 * Gibt die Rauheiten eines Profils zurück.
 * Siehe Beschreibung von Get_Rauheit für Details.
 *
 * @return Rauheits_Klassen  : eine Struktur mit den 3 Rauheitswerten plus der Typ
 */
Rauheits_Klassen List::GetRauheitsKlassen()
{
  Rauheits_Klassen rK = { BCE_NAN, BCE_NAN, BCE_NAN, UNKNOWN };

  Get_Rauheit( &rK.vorland_links, &rK.flussschlauch, &rK.vorland_rechts, &rK.typ );

  return rK;
}; // GetRauheitsKlassen


/*!
 * Setzt die Rauheiten für die vorländer und den
 * Flussschlauch neu.
 *
 * @param rK : Eine Struktur mit den neuen Werten. Sind einige der Werte BCE_NAN, wird nicht
 *              geändert
 *
 */
void List::SetRauheitsKlassen( const Rauheits_Klassen& rK )
{
  // als erstes die Begrenzung der Trennflaechen herausfinden
  Profildatei* pTrenn = HoleDatenblock( TRENNFLAECHEN );

  double trennLinks = BCE_NAN;
  double trennRechts = BCE_NAN;

  if( pTrenn && pTrenn->datensatz && pTrenn->datensatz->next_ds )
  {
    trennLinks = pTrenn->datensatz->x;
    trennRechts = pTrenn->datensatz->next_ds->x;
  }; // if pTrenn

  // jetzt die Rauheit finden
  Profildatei* pRauh = HoleDatenblock( rK.typ );
  if( !pRauh || !pRauh->datensatz )
    return;

  // der Reihe nach die Rauheiten ersetzen
  Koord* pCrd = pRauh->datensatz;

  // erst links
  if( trennLinks != BCE_NAN )
  {
    while( pCrd && pCrd->x < trennLinks )
    {
      if( rK.vorland_links != BCE_NAN )
        pCrd->y = rK.vorland_links;

      pCrd = pCrd->next_ds;
    }; // while pCrd
  }; // if trennLinks

  // die Mitte
  while( pCrd && ( trennRechts == BCE_NAN || pCrd->x < trennRechts ) )
  {
    if( rK.flussschlauch != BCE_NAN )
      pCrd->y = rK.flussschlauch;

    pCrd = pCrd->next_ds;
  }; // while pCrd

  // zuletzt rechts
  if( trennRechts != BCE_NAN && rK.vorland_rechts != BCE_NAN )
  {
    while( pCrd )
    {
      pCrd->y = rK.vorland_rechts;

      pCrd = pCrd->next_ds;
    }; // while pCrd
  }; // if trennRechts
}; // SetRauheitsKlassen

/**************************************************************************/
void List::SaveSonderprofildaten( Scroller* scr, int id )
{
  if( !ptr_anfang )
    return;
  
  Profildatei* profilPtr = ptr_anfang;
  while( profilPtr->ds_nummer < scr->datensatz && profilPtr->next )
    profilPtr = profilPtr->next;
  
  pp = profilPtr->datensatz;
  if( !pp )
    return;
  
  switch( id )
  {
  case TRENNFLAECHEN:
  case BORDVOLL:
  case MODELLGRENZEN:
    {
      Rauheits_Klassen rK = GetRauheitsKlassen(); // alte Rauheiten merken

      pp->x = scr->z[0];    //Stationswert links
      pp->y = scr->z[2];    //    Lage       "
      if( pp->next_ds )
      {
        pp = pp->next_ds;
        pp->x = scr->z[1];    //Stationswert  rechts
        pp->y = scr->z[3];     //    Lage       "
      }

      // nur wenn alle drei Fliesszonen eine feste Rauheit haben diese automatisch anpassen.
      if( id == TRENNFLAECHEN && rK.vorland_links != BCE_NAN && rK.flussschlauch != BCE_NAN && rK.vorland_rechts != BCE_NAN )
        SetRauheitsKlassen( rK ); // die Rauheiten entsprechend anpassen
    }; // case TrennFlaechen
    break;
    
  case DURCHST_BEREICH:
    {
      pp->x = scr->z[0];       // y-links
      pp->y = scr->z[1];       // z-links
      if (pp->next_ds !=NULL)
      {
        pp = pp->next_ds;
        pp->x = scr->z[2];       //y-rechts
        pp->y = scr->z[3];       //z-rechts
        
      }
    }
    break;
  case MAUL:
    {
      for( int i = 0; i < 5; i++ )
      {
        if ( pp!=NULL )
        {
          pp->x = scr->z[i];
          pp->y = 0 ;
          if (pp->next_ds !=NULL)
            pp = pp->next_ds;
        }
      }
    }
    break;
  case EIPROFIL:
    {
      for( int i = 0; i < 5; i++ )
      {
        if ( pp!=NULL )
        {
          pp->x = scr->z[i];
          pp->y = 0 ;
          if (pp->next_ds !=NULL)
            pp = pp->next_ds;
        }
      }
    }
    break;
  case TRAPEZ:
    {
      for( int i = 0; i < 6; i++ )
      {
        if ( pp!=NULL )
        {
          pp->x = scr->z[i];
          pp->y = 0 ;
          if (pp->next_ds !=NULL)
            pp = pp->next_ds;
        }
      }
    }
    break;
  case KREIS:
    {
      for( int i = 0; i < 4; i++ )
      {
        if ( pp!=NULL )
        {
          pp->x = scr->z[i];
          pp->y = 0 ;
          if (pp->next_ds !=NULL)
            pp = pp->next_ds;
        }
      }
    }
    break;
  case ARMCO84:
    /*Daten in pscr.z[0...11]: Breite,Höhe,Gefälle,Station,Geländehöhe,don'tcare,R1,R2,R3,W1,W2/W3,FCRED  */
    {
      for( int i = 0; i < 6; i++ )
      {
        pp->x = scr->z[i];
        pp->y = scr->z[i+6];      // !! pscr[5]==BCE_NAN;
        if (pp->next_ds !=NULL)
          pp = pp->next_ds;
      }
    }
    break;
  case ARMCO71:
    /*  Daten in pscr.z[0...9]: Breite,Höhe,Gefälle,Station,Geländehöhe,RCO,R1CO,R2CO,BCO,FCRED */
    {
      for (int i=0;i<5;i++)
      {
        pp->x = scr->z[i];
        pp->y = scr->z[i+5];
        if (pp->next_ds !=NULL)
          pp = pp->next_ds;
      }
    }
    break;
  case NWRINNE:
    {
      for (int i=0;i<3;i++)
      {
        pp->x = scr->z[i];
        if (pp->next_ds !=NULL)
          pp = pp->next_ds;
      }
    }
    break;
  case GAUSS:
    {
      for (int i=0;i<3;i++)
      {
        pp->x = scr->z[i];
        pp->y = scr->z[i+3];
        if (pp->next_ds !=NULL)
          pp = pp->next_ds;
      }
      
    }
    break;
  case KREISSEGM:  //wie normalen datensatz behandeln
    {
      for (int i=0;i<=2;i++)
      {
        if ( pp!=NULL )
        {
          pp->x = scr->z[i];
          pp->y = scr->z[i+3];
          pp = pp->next_ds;
        }
      }
      SaveInfoline2(scr->datensatz,scr->z[6]);  // Gefälle in 2.Zeile ablegen
    }
    break;
    
  }
} // SaveSonderprofilDaten
/**************************************************************************/
void List::GetDatenInfo3(char *str, int ds)
{
  ptr_profil=ptr_anfang;
  while ((ptr_profil->ds_nummer < ds)&&(ptr_profil !=NULL))
    ptr_profil = ptr_profil->next;
  strcpy(str,ptr_profil->daten_info[2]);
}
/**************************************************************************/
void List::SaveDatenInfo3(char *str, int ds)
{
  ptr_profil=ptr_anfang;
  while ((ptr_profil->ds_nummer < ds)&&(ptr_profil !=NULL))
    ptr_profil = ptr_profil->next;
  strcpy(ptr_profil->daten_info[2],str);
}

/*****************************************************************
* ermittelt den Datenblocktyp aus den momentan gelesenen        *
*  3  Infozeilen                                       *
*  Konstantendefinition in: typen.h                              *
*  Aufruf in :   List::MakeWriteInfo                             *
******************************************************************/
int List::GetProfilTyp()
{
/*  Rückgabe :  0  OK
1  extern IS_COMMENT =TRUE setzen
2  extern exist_plot    =TRUE  setzen
-1  nichts gefunden

  */
  
  char  *str1,*str2;
  
  str1 = ptr_profil->daten_info[0];
  str2 = ptr_profil->daten_info[1];
  ptr_profil->profiltyp = UNKNOWN;
  
  if ((xvt_str_match(str1,"GELAENDE*",FALSE))&&(xvt_str_match(str2,"HOEHE*",TRUE)))
  {
    ptr_profil->profiltyp = GELAENDEHOEHE;
    typ[ptr_profil->ds_nummer] = GELAENDEHOEHE;
    return 0;
  }
  if ((xvt_str_match(str1,"*.SOHLHOEHE*",FALSE))) //Dick 21.06.99 damit aufzählen kann 2,3...n
  {
    ptr_profil->profiltyp = SOHLHOEHE_2;
    typ[ptr_profil->ds_nummer] = SOHLHOEHE_2;
    return 0;
  }
  if ((xvt_str_match(str1,"SOHLHOEHE*",FALSE)))
  {
    ptr_profil->profiltyp = SOHLHOEHE;
    typ[ptr_profil->ds_nummer] = SOHLHOEHE;
    return 0;
  }
  if ((xvt_str_match(str1,"*.Boeschung-li*",FALSE)))//Dick 21.06.99 damit aufzählen kann 2,3...n
  {
    ptr_profil->profiltyp = BOESCHUNG_LINKS_2;
    typ[ptr_profil->ds_nummer] = BOESCHUNG_LINKS_2;
    return 0;
  }
  if ((xvt_str_match(str1,"BOESCHUNG-Li*",FALSE)))
  {
    ptr_profil->profiltyp = BOESCHUNG_LINKS;
    typ[ptr_profil->ds_nummer] = BOESCHUNG_LINKS;
    return 0;
  }
  if ((xvt_str_match(str1,"*.Boeschung-re*",FALSE)))//Dick 21.06.99 damit aufzählen kann 2,3...n
  {
    ptr_profil->profiltyp = BOESCHUNG_RECHTS_2;
    typ[ptr_profil->ds_nummer] = BOESCHUNG_RECHTS_2;
    return 0;
  }
  if ((xvt_str_match(str1,"BOESCHUNG-re*",FALSE)))
  {
    ptr_profil->profiltyp = BOESCHUNG_RECHTS;
    typ[ptr_profil->ds_nummer] = BOESCHUNG_RECHTS;
    return 0;
  }
  if ((xvt_str_match(str1,"BOESCHUNG-Li*",FALSE)))
  {
    ptr_profil->profiltyp = BOESCHUNG_LINKS;
    typ[ptr_profil->ds_nummer] = BOESCHUNG_LINKS;
    return 0;
  }
  if ((xvt_str_match(str1,"BOESCHUNG-re*",FALSE)))
  {
    ptr_profil->profiltyp = BOESCHUNG_RECHTS;
    typ[ptr_profil->ds_nummer] = BOESCHUNG_RECHTS;
    return 0;
  }
  //Dick 23.09.99 neue Datensätze
  if ((xvt_str_match(str1,"AUSUFERUNG-Li*",FALSE)))
  {
    ptr_profil->profiltyp = AUSUFERUNG_LINKS;
    typ[ptr_profil->ds_nummer] = AUSUFERUNG_LINKS;
    return 0;
  }
  if ((xvt_str_match(str1,"AUSUFERUNG-re*",FALSE)))
  {
    ptr_profil->profiltyp = AUSUFERUNG_RECHTS;
    typ[ptr_profil->ds_nummer] = AUSUFERUNG_RECHTS;
    return 0;
  }
  if ((xvt_str_match(str1,"SCHLEPPSPANNUNG*",FALSE)))
  {
    ptr_profil->profiltyp = SCHLEPPSPANNUNG;
    typ[ptr_profil->ds_nummer] = SCHLEPPSPANNUNG;
    return 0;
  }
  if ((xvt_str_match(str1,"ENERGIEHOEHE*",FALSE)))
  {
    ptr_profil->profiltyp = ENERGIEHOEHE;
    typ[ptr_profil->ds_nummer] = ENERGIEHOEHE;
    return 0;
  }
  //
  if ((xvt_str_match(str1,"Wassersp.-Br*",FALSE)))
  {
    ptr_profil->profiltyp = WSP_BREITE;
    typ[ptr_profil->ds_nummer] = WSP_BREITE;
    return 0;
  }
  if ((xvt_str_match(str1,"Text*",FALSE)))
  {
    ptr_profil->profiltyp = LP_TEXT;
    typ[ptr_profil->ds_nummer] = LP_TEXT;
    return 1;
  }
  
  if ((xvt_str_match(str1,"WSP-HOEHE*",FALSE)))
  {
    ptr_profil->profiltyp=STATION;
    typ[ptr_profil->ds_nummer] = STATION;
    return 0;
  }
  if ((xvt_str_match(str1,"WSP-Messung*",FALSE))) //Dick 5.02.99
  {
    ptr_profil->profiltyp=STATION_FIX;
    typ[ptr_profil->ds_nummer] = STATION_FIX;
    return 0;
  }
  if ((xvt_str_match(str1,"WASSERSPIEGEL*",TRUE)) && (xvt_str_match(str2,"HQ1*",TRUE)))
  {
    ptr_profil->profiltyp=WASSERSP1;
    typ[ptr_profil->ds_nummer] = WASSERSP1;
    return 0;
  }
  if ((xvt_str_match(str1,"WASSERSPIEGEL*",TRUE)) && (xvt_str_match(str2,"HQ100*",TRUE)))
  {
    ptr_profil->profiltyp=WASSERSP100;
    typ[ptr_profil->ds_nummer] = WASSERSP100;
    return 0;
  }
  if ((xvt_str_match(str1,"WASSERSPIEGEL*",TRUE)) && (xvt_str_match(str2,"HQ5*",TRUE)))
  {
    ptr_profil->profiltyp=WASSERSP5;
    typ[ptr_profil->ds_nummer] = WASSERSP5;
    return 0;
  }
  if ((xvt_str_match(str1,"Wasserspiegel *",FALSE)))
  {
    ptr_profil->profiltyp = WASSERSPIEGEL;
    typ[ptr_profil->ds_nummer] = WASSERSPIEGEL;
    return 0;
  }
  if ((xvt_str_match(str1,"Wasserspiegel*",FALSE)) && (xvt_str_match(str2,"Druckhoehe*",FALSE)))
  {
    ptr_profil->profiltyp=WASSERSPIEGEL;
    typ[ptr_profil->ds_nummer] = WASSERSPIEGEL;
    return 0;
  }
  if ((xvt_str_match(str1,"Wasserspiegelfixierung*",FALSE)))//Dick 4.02.99
  {
    ptr_profil->profiltyp = WSP_FIXIERUNG;
    typ[ptr_profil->ds_nummer] = WSP_FIXIERUNG;
    return 0;
  }
  if ((xvt_str_match(str1,"WSP-Fixierung*",FALSE)))//Dick 20.04.99
  {
    ptr_profil->profiltyp = WSP_FIXIERUNG;
    typ[ptr_profil->ds_nummer] = WSP_FIXIERUNG;
    return 0;
  }
  if ((xvt_str_match(str1,"WSP-Differenz*",FALSE)))//Bley 8.11.2000
  {
    ptr_profil->profiltyp = WSP_DIFFERENZ;
    typ[ptr_profil->ds_nummer] = WSP_DIFFERENZ;
    return 0;
  }
  if ((xvt_str_match(str1,"Laenge*",FALSE)))
  {
    ptr_profil->profiltyp = LAENGE;
    typ[ptr_profil->ds_nummer] = LAENGE;
    return 0;
  }
  if ((xvt_str_match(str1,"Abfluss*",FALSE)))
  {
    ptr_profil->profiltyp = ABFLUSS;
    typ[ptr_profil->ds_nummer] = ABFLUSS;
    return 0;
  }
  if ((xvt_str_match(str1,"Profilart*",FALSE)))
  {
    ptr_profil->profiltyp = PROFILART;
    typ[ptr_profil->ds_nummer] = PROFILART;
    return 0;
  }
  if ((xvt_str_match(str1,"Verzweigung*",FALSE)))
  {
    ptr_profil->profiltyp = VZKENNG;
    typ[ptr_profil->ds_nummer] = VZKENNG;
    return 0;
  }
  if ((xvt_str_match(str1,"Profilkennung*",FALSE)))
  {
    ptr_profil->profiltyp = PROFILKENNG;
    typ[ptr_profil->ds_nummer] = PROFILKENNG;
    return 0;
  }
  if ((xvt_str_match(str1,"Deckenunterk*",FALSE)))
  {
    ptr_profil->profiltyp = DKUK;
    typ[ptr_profil->ds_nummer] = DKUK;
    return 0;
  }
  if ((xvt_str_match(str1,"Deckenoberk*",FALSE)))
  {
    ptr_profil->profiltyp = DKOK;
    typ[ptr_profil->ds_nummer] = DKOK;
    return 0;
  }
  if (xvt_str_match(str1,"TRENNFLAECHEN*",FALSE))
  {
    ptr_profil->profiltyp = TRENNFLAECHEN;
    typ[ptr_profil->ds_nummer] = TRENNFLAECHEN;
    return 0;
  }
  if ((xvt_str_match(str1,"RAUHIGKEIT*",TRUE)) && (xvt_str_match(str2,"KST*",FALSE)))
  {
    ptr_profil->profiltyp=RAUHIGKEIT_KST;
    typ[ptr_profil->ds_nummer] = RAUHIGKEIT_KST;
    return 0;
  }
  if ((xvt_str_match(str1,"RAUHIGKEIT*",TRUE)) && (xvt_str_match(str2,"k-s*m*",TRUE)))
  {
    ptr_profil->profiltyp=RAUHIGKEIT;
    typ[ptr_profil->ds_nummer] = RAUHIGKEIT;
    return 0;
  }
  if ((xvt_str_match(str1,"RAUHIGKEIT*",TRUE)) && (xvt_str_match(str2,"KS*",FALSE)))
  {
    ptr_profil->profiltyp=RAUHIGKEIT;
    typ[ptr_profil->ds_nummer] = RAUHIGKEIT;
    return 0;
  }
  if ((xvt_str_match(str1,"RAUHIGKEIT*",TRUE)) && (xvt_str_match(str2,"k-S*",TRUE)))
  {
    ptr_profil->profiltyp=RAUHIGKEIT;
    typ[ptr_profil->ds_nummer] = RAUHIGKEIT;
    return 0;
  }
  //Dick 1.06.99
  if ((xvt_str_match(str1,"RAUHEIT*",TRUE)) && (xvt_str_match(str2,"KST*",FALSE)))
  {
    ptr_profil->profiltyp=RAUHIGKEIT_KST;
    typ[ptr_profil->ds_nummer] = RAUHIGKEIT_KST;
    return 0;
  }
  if ((xvt_str_match(str1,"RAUHEIT*",TRUE)) && (xvt_str_match(str2,"KS*",FALSE)))
  {
    ptr_profil->profiltyp=RAUHIGKEIT;
    typ[ptr_profil->ds_nummer] = RAUHIGKEIT;
    return 0;
  }
  //
  if (xvt_str_match(str1,"PUNKT-NR*",TRUE))
  {
    ptr_profil->profiltyp=PUNKT_NR;
    typ[ptr_profil->ds_nummer] = PUNKT_NR;
    return 0;
  }
  if ((xvt_str_match(str1,"STATION*",FALSE)) && (xvt_str_match(str2,"WSP-HOEHE*",FALSE)))
  {
    ptr_profil->profiltyp=STATION;
    typ[ptr_profil->ds_nummer] = STATION;
    return 0;
  }
  //  if (xvt_str_match(str1,"WSP-HOEHE*",FALSE)) //identisch mit:"STATION WSP-Hoehe" 04.03.96 für Erfurt
  //     {
  //      ptr_profil->profiltyp=STATION;
  //      typ[ptr_profil->ds_nummer] = STATION;
  //      return 0;
  //     } überflüssig
  if ((xvt_str_match(str1,"DURCHSTROEMTE*",TRUE)) && (xvt_str_match(str2,"BEREICHE*",TRUE)))
  {
    ptr_profil->profiltyp=DURCHST_BEREICH;
    typ[ptr_profil->ds_nummer] = DURCHST_BEREICH;
    return 0;
  }
  if ((xvt_str_match(str1,"OK-WEHR*",FALSE)))
  {
    ptr_profil->profiltyp=OK_WEHRS;
    typ[ptr_profil->ds_nummer] = OK_WEHRS;
    return 0;
  }
  if ((xvt_str_match(str1,"TRENNLINIE*",TRUE)) && (xvt_str_match(str2,"WEHR*",TRUE)))
  {
    ptr_profil->profiltyp=TRENN_WEHR;
    typ[ptr_profil->ds_nummer] = TRENN_WEHR;
    return 0;
  }
  if (xvt_str_match(str1,"UK-BRUECKE*",FALSE))
  {
    ptr_profil->profiltyp=UK_BRUECKE;
    typ[ptr_profil->ds_nummer] = UK_BRUECKE;
    return 0;
  }
  if (xvt_str_match(str1,"OK-BRUECKE*",FALSE))
  {
    ptr_profil->profiltyp=OK_BRUECKE;
    typ[ptr_profil->ds_nummer] = OK_BRUECKE;
    return 0;
  }
  if ((xvt_str_match(str1,"WASSERSPIEGEL*",TRUE)) && (xvt_str_match(str2,"HQ1*",TRUE)))
  {
    ptr_profil->profiltyp=WASSERSP1;
    typ[ptr_profil->ds_nummer] = WASSERSP1;
    return 0;
  }
  if ((xvt_str_match(str1,"WASSERSPIEGEL*",TRUE)) && (xvt_str_match(str2,"HQ100*",TRUE)))
  {
    ptr_profil->profiltyp=WASSERSP100;
    typ[ptr_profil->ds_nummer] = WASSERSP100;
    return 0;
  }
  if ((xvt_str_match(str1,"*.Wasserspiegel*",FALSE)))//Dick 21.06.99 damit aufzählen kann 2,3...n
  {
    ptr_profil->profiltyp = WASSERSPIEGEL_2;
    typ[ptr_profil->ds_nummer] = WASSERSPIEGEL_2;
    return 0;
  }
  if ((xvt_str_match(str1,"WASSERSPIEGEL*",TRUE)) && (xvt_str_match(str2,"HQ5*",TRUE)))
  {
    ptr_profil->profiltyp=WASSERSP5;
    typ[ptr_profil->ds_nummer] = WASSERSP5;
    return 0;
  }
  if ((xvt_str_match(str1,"BAUWERK*",FALSE)))
  {
    ptr_profil->profiltyp = BAUWERK;
    typ[ptr_profil->ds_nummer] = BAUWERK;
    return 0;
  }
  if (xvt_str_match(str1,"AX*",TRUE))
  {
    ptr_profil->profiltyp=AXM;
    typ[ptr_profil->ds_nummer] = AXM;
    return 0;
  }
  if (xvt_str_match(str1,"AY*",TRUE))
  {
    ptr_profil->profiltyp=AYM;
    typ[ptr_profil->ds_nummer] = AYM;
    return 0;
  }
  if (xvt_str_match(str1,"DP*",TRUE))
  {
    ptr_profil->profiltyp=DPM;
    typ[ptr_profil->ds_nummer] = DPM;
    return 0;
  }
  if (xvt_str_match(str1,"BORDVOLL-Hoehe*",FALSE))
  {
    ptr_profil->profiltyp = BVHOEHE;
    typ[ptr_profil->ds_nummer] = BVHOEHE;
    return 0;
  }
  else//Dick 8.06.99 weil sonst kein unterschied
    if (xvt_str_match(str1,"BORDVOLL*",FALSE))
    {
      ptr_profil->profiltyp = BORDVOLL;
      typ[ptr_profil->ds_nummer] = BORDVOLL;
      return 0;
    }
    if (xvt_str_match(str1,"BOESCHUNGSKANTE*",TRUE))
    {
      ptr_profil->profiltyp=BORDVOLL;
      typ[ptr_profil->ds_nummer] = BORDVOLL;
      return 0;
    }
    
    if (xvt_str_match(str1,"EI*",TRUE))
    {
      ptr_profil->profiltyp=EIPROFIL;
      typ[ptr_profil->ds_nummer] = EIPROFIL;
      return 0;
    }
    if (xvt_str_match(str1,"MAULPROFIL*",TRUE))
    {
      ptr_profil->profiltyp=MAUL;
      typ[ptr_profil->ds_nummer] = MAUL;
      return 0;
    }
    if (xvt_str_match(str1,"KREISSEGMENT*",TRUE))
    {
      ptr_profil->profiltyp=KREISSEGM;
      typ[ptr_profil->ds_nummer] = KREISSEGM;
      return 0;
    }
    if (xvt_str_match(str1,"KREIS*",TRUE))
    {
      ptr_profil->profiltyp=KREIS;
      typ[ptr_profil->ds_nummer] = KREIS;
      return 0;
    }
    if (xvt_str_match(str1,"TRAPEZ*",TRUE))
    {
      ptr_profil->profiltyp=TRAPEZ;
      typ[ptr_profil->ds_nummer] = TRAPEZ;
      return 0;
    }
    //  if(xvt_str_match(str1,"SONDERBAUWERK*", TRUE)) //Bley 28.2.2001
    if(xvt_str_match(str1,"BUHNEN*", TRUE)) 
    {
      ptr_profil->profiltyp=BUHNE;
      typ[ptr_profil->ds_nummer]=BUHNE;
      return 0;
    }
    
    if(xvt_str_match(str1,"MODELLGRENZEN*", TRUE))
    {
      ptr_profil->profiltyp=MODELLGRENZEN;
      typ[ptr_profil->ds_nummer]=MODELLGRENZEN;
      return 0;
    }
    
    if(xvt_str_match(str1,"SCHUETZ*", TRUE))
    {
      ptr_profil->profiltyp=SCHUETZ;
      typ[ptr_profil->ds_nummer]=SCHUETZ;
      return 0;
      
    }
    if (xvt_str_match(str1,"SVA-WERT*",TRUE))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = SVA_WERT;
      return 0;
    }
    if (xvt_str_match(str1,"OK-GELAENDE*",FALSE))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = OK_GELAENDE;
      return 0;
    }
    if (xvt_str_match(str1,"KASTENPROFIL*",TRUE))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = KASTEN;
      return 0;
    }
    if (xvt_str_match(str1,"LWA-FELDER*",FALSE))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = LWA_FELDER;
      return 0;
    }
    if ((xvt_str_match(str1,"GAUSSPROFIL*",FALSE)) && (xvt_str_match(str2,"MIT*",FALSE)))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = GAUSSRUECK;
      return 0;
    }
    if (xvt_str_match(str1,"LAGE-KOORDINATEN*",TRUE))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = GAUSS;
      return 0;
    }
    if (xvt_str_match(str1,"ARMCO84*",TRUE))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = ARMCO84;
      return 0;
    }
    if (xvt_str_match(str1,"ARMCO71*",TRUE))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = ARMCO71;
      return 0;
    }
    if (xvt_str_match(str1,"NW-RINNE*",TRUE))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = NWRINNE;
      return 0;
    }
    if (xvt_str_match(str1,"FUELLHOEHEN-PROFIL*",TRUE))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = FUELLHOEHE;
      return 0;
    }
    if (xvt_str_match(str1,"NAUDASCHER-MEDLARZ*",TRUE))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = NAU_MED;
      return 0;
    }
    if (xvt_str_match(str1,"Kommentar:*",TRUE))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = COMMENT;
      return 1;
    }
    if (xvt_str_match(str1,"datplt*",FALSE))
    {
      ptr_profil->profiltyp=DATPLOT;
      typ[ptr_profil->ds_nummer] = DATPLOT;
      ds_info[0]--;
      return 2;
    }
    
    if (xvt_str_match(str1,"2.GELAENDE*",TRUE))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = GELAENDE2;
      return 0;
    }
    if (xvt_str_match(str1,"FLAECHE*",TRUE))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = FLAECHE;
      return 0;
    }
    if (xvt_str_match(str1,"RECHTSWERT*",TRUE))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = RECHTSWERT;
      return 0;
    }
    if (xvt_str_match(str1,"HOCHWERT*",TRUE))
    {
      ptr_profil->profiltyp=typ[ptr_profil->ds_nummer] = HOCHWERT;
      return 0;
    }
    if((xvt_str_match(str1,"Gefaelle*",FALSE)))
    {
      ptr_profil->profiltyp = GEFAELLE;
      typ[ptr_profil->ds_nummer] = GEFAELLE;
      return 0;
      
    }
    if((xvt_str_match(str1,"v-*",FALSE)))
    {
      ptr_profil->profiltyp = VMITTEL;
      typ[ptr_profil->ds_nummer] = VMITTEL;
      return 0;
      
    }
    if ((xvt_str_match(str1,"Wsp-*",FALSE)))
    {
      ptr_profil->profiltyp = WASSERSPIEGEL;
      typ[ptr_profil->ds_nummer] = WASSERSPIEGEL;
      return 0;
    }
    
    if ((xvt_str_match(str1,"Q-*",FALSE)))
    {
      ptr_profil->profiltyp = ABFLUSS;
      typ[ptr_profil->ds_nummer] = ABFLUSS;
      return 0;
    }
    if (ptr_profil->profiltyp == UNKNOWN)
    {
      ptr_profil->profiltyp = typ[ptr_profil->ds_nummer] = UNKNOWN;
      Profildatei *ptr_profil_save;
      ptr_profil_save=ptr_profil;
      //xvt_dm_post_error ("Datensatz %d:\n%s\n konnte nicht identifiziert\nwerden!",ptr_profil->ds_nummer,str1);//Weg weil p_h wird in Konstruktor sonst nicht aktualisiert. Dick 17.09.98
      ptr_profil=ptr_profil_save;
      //char temp[200];
      //sprintf(temp,"Datensatz %d:\n%s\n konnte nicht identifiziert\nwerden!",ptr_profil->ds_nummer,str1);
      //MessageBox(NULL,temp,"Error",MB_OK|MB_ICONERROR);
      return 0;
    }
    return -1;
}
/**************************************************************************/

void List::SaveXY(FILE *out,int n1)  //n1 = Datensatz-Nr
{
  int k;
  
  if (ptr_anfang != NULL)
  {
    ptr_profil = ptr_anfang;
    while ((ptr_profil->ds_nummer < n1)&&(ptr_profil !=NULL))
      ptr_profil = ptr_profil->next;
    if( !ptr_profil || !ptr_profil->datensatz )
      return;
    pp=ptr_profil->datensatz;
    
    MakeSaveInfo(out);       //3 Infozeilen vor Datenblock sichern
    
    if (ptr_profil->profiltyp != COMMENT)
    {
      k=0;
      while (pp !=NULL)
      {
        if ((pp->x != BCE_NAN) && (pp->y != BCE_NAN))
        {
          k++;
          fprintf(out,"%2d ",pp->pre_x );
          fprintf(out,"%13.4lf", pp->x);
          pp = pp->next_ds;
          if ((k % 8)==0)
            fprintf(out,"\n");
        }
        else pp = pp->next_ds;
      }
      if ((k % 8)!=0)
        fprintf(out,"\n");
      pp=ptr_profil->datensatz;
      k=0;
      while (pp != NULL)
      {
        if ( (pp->x !=BCE_NAN) && (pp->y != BCE_NAN))
        {
          k++;
          fprintf(out,"%2d ",pp->pre_y );
          fprintf(out,"%13.4lf", pp->y);
          pp = pp->next_ds;
          if ((k % 8)==0)
            fprintf(out,"\n");
        }
        else pp = pp->next_ds;
      }
      if ((k % 8)!=0)
        fprintf(out,"\n");
    }
  }
}
/**************************************************************************/
// Sonderprofil: Typ-Nr >MAUL auf Platte schreiben
void List::SaveSonderProfil(FILE *out,int nr,int typ)   //nr:=Datensatznummer # typ
{
  int k;
  
  if (ptr_anfang != NULL)
  {
    ptr_profil = ptr_anfang;
    while ((ptr_profil->ds_nummer <  nr)&&(ptr_profil !=NULL))
      ptr_profil = ptr_profil->next;
    pp=ptr_profil->datensatz;
    
    MakeSaveInfo(out);       //3 Infozeilen vor Datenblock sichern
    k=0;
    switch (typ)
    {
    case MAUL:
    case EIPROFIL:
      while ((pp !=NULL)&&(pp->ds_nr<=5))
      {
        if(pp->x==BCE_NAN)
          pp->x=0; //3.12.96
        fprintf(out,"%13.4lf", pp->x);
        pp = pp->next_ds;
      }
      fprintf(out,"\n");
      break;
    case TRAPEZ:
      while ((pp !=NULL)&&(pp->ds_nr<=6))
      {
        if(pp->x==BCE_NAN)
          pp->x=0; //3.12.96
        fprintf(out,"%13.4lf", pp->x);
        pp = pp->next_ds;
      }
      fprintf(out,"\n");
      break;
    case KREIS:
      while ((pp !=NULL)&&(pp->ds_nr<=4))
      {
        if(pp->x==BCE_NAN)
          pp->x=0; //3.12.96
        fprintf(out,"%13.4lf", pp->x);
        pp = pp->next_ds;
      }
      fprintf(out,"\n");
      break;
    case NWRINNE:
      while ((pp !=NULL)&&(pp->ds_nr<=3))
      {
        if(pp->x==BCE_NAN)
          pp->x=0; //3.12.96
        fprintf(out,"%13.4lf", pp->x);
        pp = pp->next_ds;
      }
      fprintf(out,"\n");
      break;
    default:
      {
        while (pp !=NULL)
        {
          k++;
          if ((typ==ARMCO84)&&(pp->ds_nr==6))
            fprintf(out,"\n", pp->x);
          else
          { 
            if(pp->x==BCE_NAN)
              pp->x=0; //3.12.96
            
            fprintf(out,"%13.4lf", pp->x);
          }
          pp = pp->next_ds;
          if ((k % 8)==0)
            fprintf(out,"\n");
        }
        if (typ!=ARMCO84)
          fprintf(out,"\n");
        
      }
      break;
    }
    switch (typ)
    {
    case MAUL:
    case EIPROFIL:
    case KREIS:
    case TRAPEZ:
    case NWRINNE:
    case UNKNOWN:
    case FUELLHOEHE:   // noch nicht implementiert
    case NAU_MED:
      break;
      
    case KREISSEGM:
    case ARMCO84:
    case ARMCO71:
      {
        pp=ptr_profil->datensatz;
        k=0;
        while (pp !=NULL)
        {
          k++;
          if(pp->y==BCE_NAN)
            pp->y=0; //3.12.96
          
          fprintf(out,"%13.4lf", pp->y);
          pp = pp->next_ds;
          if ((k % 8)==0)
            fprintf(out,"\n");
        }
        fprintf(out,"\n");
      }
      break;
    default:break;
    }
   }
}
/**************************************************************************/

void List::Edit_Gelaende_Bereiche(int datensatz,double faktor,int typ,MMP *mmp)
{
  Koord *pp_neu;
  Profildatei *ptr_prof;
  double wert;
  
  ptr_prof = ptr_anfang;
  while((ptr_prof->ds_nummer < datensatz) && (ptr_prof!=NULL))
    ptr_prof=ptr_prof->next;
  pp_neu=ptr_prof->datensatz;
  
  while ((pp_neu->ds_nr < mmp->position_mouse_down)&&(pp_neu->next_ds !=NULL))
    pp_neu = pp_neu->next_ds;
  
  while ((pp_neu !=NULL)&&(pp_neu->ds_nr <= mmp->position_mouse_up))
  {
    switch (typ)
    {
    case OFFSET:
      {
        if(pp_neu->y !=BCE_NAN)
        {
          wert = pp_neu->y + faktor;
          if (wert >0)
            pp_neu->y = wert;
          else
            pp_neu->y = 0.0;
        }
      }
      break;
    case FAKTOR:
      {
        if(pp_neu->y !=BCE_NAN)
        {
          wert = pp_neu->y * faktor;
          if (wert >0)
            pp_neu->y = wert;
          else
            pp_neu->y = 0.0;
        }
      }
      break;
    case NEU:
      {
        if(pp_neu->y !=BCE_NAN)
          pp_neu->y = faktor;
      }
      break;
    }
    pp_neu = pp_neu->next_ds;
  }
}
/*********************************************************************/
void List::Set_Zoom_Marker( ZOOM* info )
{
  Koord* pp_neu = ptr_anfang->datensatz;
  
  while( pp_neu != NULL && pp_neu->next_ds != NULL && pp_neu->ds_nr < info->pos_station_min )
    pp_neu = pp_neu->next_ds;
  
  if( pp_neu != NULL && info->station_min == pp_neu->x )
  {
    zoom_left_x = pp_neu;
    while ((pp_neu !=NULL)&&(pp_neu->ds_nr < info->pos_station_max))
      pp_neu = pp_neu->next_ds;
    if( ( pp_neu != NULL ) && ( info->station_max == pp_neu->x ) )
      zoom_right_x = pp_neu;
    else
    {
      // evt. Fehlerbehandlung !?
      zoom_left_x=NULL;
      zoom_right_x =NULL;
    }
  }
  else
  {
    // evt. Fehlerbehandlung !?
    zoom_left_x=NULL;
    zoom_right_x =NULL;
  }
}
//****************************************************************************
int List::Get_Plot_Options(int ds,SLIST werte,int art,int *attr)
{
/*Optionen: Zeichnen/Nicht Zeichnen für einzelne Werte eines
  Datensatzes in SLIST lesen*/
  Koord *pp_neu;
  Profildatei *ptr_prof;
  char temp[25];
  int z=0;
  
  ptr_prof=ptr_anfang;
  while ((ptr_prof !=NULL)&& (ptr_prof->ds_nummer < ds))
    ptr_prof = ptr_prof->next;
  
  if (ptr_prof !=NULL)
    pp_neu = ptr_prof->datensatz;
  else return 0;
  
  z=0;
  if (pp_neu !=NULL)
  {
    if (art ==1)
      while (pp_neu != NULL)
      {
        gcvt(pp_neu->x,15,temp);
        attr[z] = pp_neu->pre_x;
        if (!xvt_slist_add_at_elt((SLIST)werte,NULL,(char*)temp,0L))
          return 0;
        pp_neu = pp_neu->next_ds;
        z++;
      }
      else if (art == 2)
        while (pp_neu != NULL)
        {
          gcvt(pp_neu->y,15,temp);
          attr[z] = pp_neu->pre_y;
          if(!xvt_slist_add_at_elt((SLIST)werte,NULL,(char*)temp,0L))
            return 0;
          pp_neu = pp_neu->next_ds;
          z++;
        }
  }
  return 1;
}
/*********************************************************************/
int List::Save_Plot_Options(int ds,SLIST werte,int art,int *attr)
{
/*Optionen: Zeichnen-Nicht Zeichnen für einzelne Werte eines
  Datensatzes aus SLIST lesen*/
  Koord *pp_neu;
  Profildatei *ptr_prof;
  char *temp;
  SLIST_ELT elt;
  long data;
  double wert;
  int z=0;
  
  ptr_prof=ptr_anfang;
  while ((ptr_prof !=NULL)&& (ptr_prof->ds_nummer < ds))
    ptr_prof = ptr_prof->next;
  
  if (ptr_prof !=NULL)
    pp_neu = ptr_prof->datensatz;
  else return 0;
  
  if (pp_neu !=NULL)
  {
    if (art==1)
    {
      elt=xvt_slist_get_first((SLIST)werte);
      while (pp_neu != NULL)
      {
        if (elt !=NULL)
        {
          temp = xvt_slist_get((SLIST)werte,elt,&data);
          wert = atof(temp);
          if (pp_neu->x == wert)
            pp_neu->pre_x =attr[z];
        }
        else
        {
          pp_neu->pre_x = 1;
          return 0;
        }
        pp_neu = pp_neu->next_ds;
        z++;
        elt=xvt_slist_get_next(werte,elt);
      }
    }
    else if (art==2)
    {
      elt=xvt_slist_get_first((SLIST)werte);
      while (pp_neu != NULL)
      {
        if (elt !=NULL)
        {
          temp = xvt_slist_get((SLIST)werte,elt,&data);
          wert = atof(temp);
          if (pp_neu->y == wert)
            pp_neu->pre_y =attr[z];
        }
        else
        {
          pp_neu->pre_y = 1;
          return 0;
        }
        pp_neu = pp_neu->next_ds;
        z++;
        elt=xvt_slist_get_next(werte,elt);
      }
    }
    
  }
  return 1;
}
/**************************************************************************/

int List::change_rauheiten(FILE *protokoll_file,char file[15],
                           double links,double mitte,double rechts,
                           int rauh_change_typ)
{
  Koord *pp_neu;
  Profildatei *ptr_prof;
  int trennflaechen_nr=0,
    rauheit_nr=0;
  double trenn_li,trenn_re;
  for (int i=1;i<=ds_info[0];i++)
  {
    switch (typ[i])
    {
    case TRENNFLAECHEN:
      trennflaechen_nr = i ;
      break;
    case RAUHIGKEIT:
      rauheit_nr = i;
      break;
    case RAUHIGKEIT_KST:
      rauheit_nr = i;
      break;
    default:
      break;
    }
  }
  
  if (trennflaechen_nr ==0)
  {
    char buf[200],buf2[200];//Dick 26.11.99
    xvt_res_get_str(STR_CHANGE_RAUH_1,buf,sizeof(buf));
    xvt_res_get_str(STR_IN_DETEI ,buf2,sizeof(buf2));
    xvt_dm_post_error("%s%s%s",buf2,file,buf); 
    //xvt_dm_post_error("In Datei %s sind keine Trennflõchen vorhanden !-nderung der Rauheiten nicht m÷glich!",file);
    buf[0]='\0';
    xvt_res_get_str(STR_CHANGE_RAUH_2,buf,sizeof(buf));
    fprintf(protokoll_file,"%s",buf);
    //fprintf(protokoll_file,"Datei enthõlt keine Trennflõchen !\n\n");
    return FALSE;
  }
  
  if (rauheit_nr ==0)
  {
    if (rauh_change_typ==0)//Dick 20.10.98
    {
      //xvt_dm_post_error("In Datei %s sind keine Rauheiten vorhanden !-nderung der Rauheiten nicht m÷glich!",file);
      //fprintf(protokoll_file,"Datei enthõlt keine Rauheiten !\n\n");
      char buf[200],buf2[200];
      xvt_res_get_str(STR_CHANGE_RAUH_3,buf,sizeof(buf));
      xvt_res_get_str(STR_IN_DETEI ,buf2,sizeof(buf2));
      xvt_dm_post_error("%s%s%s",buf2,file,buf);
      buf[0]='\0';
      xvt_res_get_str(STR_CHANGE_RAUH_4,buf,sizeof(buf));
      fprintf(protokoll_file,"%s\n\n",buf);
      return FALSE;
    }
    else //Neu Datensatz anlegen
    {
      ds_info[0]++;
      anzahl_ds=ds_info[0];
      typ[anzahl_ds]=rauh_change_typ+2;//siehe typen.h
      ds_info[anzahl_ds]=0;
      MakeNewNode(ds_info[0]);
      MakeNewKoord(ds_info[1]);
      CopyStation(anzahl_ds);  // Stationswerte in neuen Datensatz kopieren
      ds_info[anzahl_ds]=ds_info[1];
      InitKoord(anzahl_ds,0.0);  // mit 0.0 initialisieren
      WriteTypDaten(anzahl_ds,rauh_change_typ+2,NULL);
      rauheit_nr =ds_info[0];//als letzter
      char buf[200];
      xvt_res_get_str(STR_CHANGE_RAUH_5,buf,sizeof(buf));
      fprintf(protokoll_file,"%s\n\n",buf);
      //fprintf(protokoll_file,"Rauheiten wurden neu erstellt !\n\n");
    }
  }
  
  if (ptr_anfang !=NULL)
  {
    ptr_prof=ptr_anfang;  // zu Trennflõchen gehen
    while ((ptr_prof !=NULL)&& (ptr_prof->ds_nummer < trennflaechen_nr))
      ptr_prof = ptr_prof->next;
    if (ptr_prof !=NULL)
      pp_neu = ptr_prof->datensatz;
    else return FALSE;
    
    if ((pp_neu->y ==1)||(pp_neu->y ==3))     // Trennflõchen lesen
    {
      trenn_li =pp_neu->x;
      trenn_re =pp_neu->next_ds->x;
    }
    else
      if((pp_neu->y ==2)||(pp_neu->y ==4))
      {
        trenn_re =pp_neu->x;
        trenn_li =pp_neu->next_ds->x;
      }
      else return FALSE;
      
      ptr_prof=ptr_anfang;   //zu Rauhigkeit gehen
      while ((ptr_prof !=NULL)&& (ptr_prof->ds_nummer < rauheit_nr))
        ptr_prof = ptr_prof->next;
      if (ptr_prof !=NULL)
      {
        if(rauh_change_typ!=0 && rauh_change_typ+2 != ptr_prof->profiltyp )//Dick 20.10.98
        {
          ptr_prof->profiltyp=rauh_change_typ+2;//Dick 20.10.98
          WriteTypDaten(rauheit_nr,rauh_change_typ+2,NULL);//Dick 20.10.98
          char buf[200];
          xvt_res_get_str(STR_CHANGE_RAUH_6,buf,sizeof(buf));
          fprintf(protokoll_file,"%s\n\n",buf);
          //fprintf(protokoll_file,"Rauheitstyp wurde geõndert !\n\n");
        }
        pp_neu = ptr_prof->datensatz;         
      }
      else return FALSE;
      
      if (pp_neu !=NULL)
      {
        fprintf(protokoll_file,"%4.2lf\t",pp_neu->x);  //von
        
        while ((pp_neu != NULL)&&(pp_neu->x <trenn_li))  //li.Tr. zu Flu¯schlauch
        {
          if(links != BCE_NAN)
            pp_neu->y = links;
          pp_neu = pp_neu->next_ds;
        }
        if (pp_neu != NULL)
        {
          if (pp_neu->pre_ds !=NULL)
            fprintf(protokoll_file,"%4.2lf\t\t",pp_neu->pre_ds->x); //bis
          else
            fprintf(protokoll_file,"%4.2lf\t\t",pp_neu->x); // bis : jedoch 1.Stationswert
          fprintf(protokoll_file,"%4.2lf\t",pp_neu->x);           //von
        }
        while ((pp_neu != NULL)&&(pp_neu->x <trenn_re))   //re.Tr. zu rechtem Vorland
        {
          if(mitte != BCE_NAN)
            pp_neu->y = mitte;
          pp_neu = pp_neu->next_ds;
        }
        if (pp_neu != NULL)
        {
          fprintf(protokoll_file,"%4.2lf\t\t",pp_neu->pre_ds->x); //bis
          fprintf(protokoll_file,"%4.2lf\t",pp_neu->x);           //von
        }
        bool weiter=FALSE;
        while (pp_neu != NULL)
        {
          if (pp_neu->next_ds !=NULL && !weiter)//Dick 13.07.99
            weiter=TRUE;
          if (pp_neu->next_ds !=NULL || weiter)//Dick 13.07.99
          {
            if(rechts != BCE_NAN)
              pp_neu->y = rechts;
          }
          else
          {
            if(mitte != BCE_NAN)
              pp_neu->y = mitte;
          }
          if (pp_neu->next_ds ==NULL)
            fprintf(protokoll_file,"%4.2lf\n",pp_neu->x);   //bis
          pp_neu = pp_neu->next_ds;
        }
      }
      
  }
  else return FALSE;
  return TRUE;
}


/*!
 * Liest die Rauheiten des Profils aus.
 * Es werden die Rauheiten ausgelesen, aber nur jeweils ein Wert
 * für jeden Profilabschnitt (Vorländer, Flussschlauch)..
 * Falls sich dieser Wert nicht eindeutig feststellen lässt,
 * wir jeweils BCE_NAN zurückgegeben.
 *
 * @param links : hier wird der Wert fürs linke vorland zurückgegeben, BCE_NAN falls nicht möglich
 * @param mitte : hier wird der Wert fürs den Flussschlauch zurückgegeben, BCE_NAN falls nicht möglich
 * @param rechts : hier wird der Wert fürs rechte Vorland zurückgegeben, BCE_NAN falls nicht möglich
 * @param rauh_typ : was ists RAUHIET_KS oder KST, UNKNOWN falls gar keiner da
 *
 * @return bool  : obs geklappt hat der nicht
 */
bool List::Get_Rauheit( double* links, double* mitte, double* rechts, int* rauh_typ )
{
  int trennflaechen_nr = 0,
    rauheit_nr = 0;

  // mit Standards vorbelegen
  *links = *rechts = *mitte = BCE_NAN;
  *rauh_typ = UNKNOWN;

  // Datensatznummern suchen
  for( int dsID = 1; dsID <= ds_info[0]; dsID++ )
  {
    switch( typ[dsID] )
    {
    case TRENNFLAECHEN:
      trennflaechen_nr = dsID;
      break;

    case RAUHIGKEIT:
      rauheit_nr = dsID;
      *rauh_typ = RAUHIGKEIT;
      break;

    case RAUHIGKEIT_KST:
      rauheit_nr = dsID;
      *rauh_typ = RAUHIGKEIT_KST;
      break;

    default:
      break;
    } // switch typ
  } // for dsID
  
  if( rauheit_nr == 0 || !ptr_anfang )
    return false;
  
  double trenn_li = BCE_NAN;
  double trenn_re = BCE_NAN;

  // links und rechts raufinden
  if( trennflaechen_nr != 0 )
  {
    Profildatei* ptr_prof = ptr_anfang;  // zu Trennflächen gehen
    while( ptr_prof && ptr_prof->ds_nummer < trennflaechen_nr )
      ptr_prof = ptr_prof->next;

    if( !ptr_prof || !ptr_prof->datensatz )
      return false;
    
    Koord* pp_neu = ptr_prof->datensatz;
    if( pp_neu->y == 1 || pp_neu->y == 3 )     // Trennflächen lesen
    {
      trenn_li = pp_neu->x;
      trenn_re = pp_neu->next_ds->x;
    }
    else if( pp_neu->y == 2 || pp_neu->y == 4 )
    {
      trenn_re = pp_neu->x;
      trenn_li = pp_neu->next_ds->x;
    }
    else 
      trennflaechen_nr = 0;
  }

  Profildatei* ptr_prof=ptr_anfang;   //zu Rauhigkeit gehen
  while( ptr_prof && ptr_prof->ds_nummer < rauheit_nr )
    ptr_prof = ptr_prof->next;
  if( !ptr_prof || !ptr_prof->datensatz )
    return false;

  Koord* pp_neu = ptr_prof->datensatz;
  
  // zuerst die linke holen
  *links = pp_neu->y;
  if( trennflaechen_nr != 0 )
  {
    while( pp_neu && pp_neu->x < trenn_li )  //li.Tr. zu Flußschlauch
    {
      if( pp_neu->y != *links )
        *links = BCE_NAN; // nicht überall gleich -> Rauheit nicht feststellbar

      pp_neu = pp_neu->next_ds;               
    }
  }
  else
    *links = BCE_NAN;

  // Jetzt die Mitte
  // Zeiger zeigt jetzt auf Anfang von Mitte
  if( !pp_neu )
    return false;

  *mitte = pp_neu->y;
  while( pp_neu && ( pp_neu->x < trenn_re || trennflaechen_nr == 0 ) )
  {
    if( pp_neu->y != *mitte )
      *mitte = BCE_NAN;

    pp_neu = pp_neu->next_ds;
  };

  if( trennflaechen_nr == 0 )
    return true;

  if( !pp_neu )
    return false;

  // Jetzt Rechts
  *rechts = pp_neu->y;
  while( pp_neu )
  {
    if( pp_neu->y != *rechts )
      *rechts = BCE_NAN;

    pp_neu = pp_neu->next_ds;
  } // while pp_neu

  return true;
} // Get_Rauheit
//****************************************************************************
int List::check_durch_bereiche(int ds,int action)
{
/*
testet, ob in durchst. Bereichen oder Trennflächen
x-Werte ausreichend vorhanden (return -1), auf erstem
und letzten Stationswert stehen (return 0/1) und setzt
sie gegebenenfalls.

  Rückgabe  -1   *einer der Werte enthielt BCE_NAN
  0   *Stationen von durchst.Bereich stehen nicht
  auf erstem-letztem Stationswert von Gelände
  1   * OK
  
  */
  
  Koord *pp_neu;
  Profildatei *ptr_prof;
  double x1,x2, y1, y2;
  
  ptr_prof=ptr_anfang;
  if (ptr_prof !=NULL)
  {
    pp_neu = ptr_prof->datensatz;
    x1 = pp_neu->x;   //erster Stationswert in Gelände
    y1=pp_neu->y;
    while (pp_neu->next_ds !=NULL)
      pp_neu=pp_neu->next_ds;
    x2=pp_neu->x;     //letzter Stationswert
    y2=pp_neu->y;
  }
  
  ptr_prof=ptr_anfang;
  while ((ptr_prof !=NULL)&& (ptr_prof->ds_nummer < ds))
    ptr_prof = ptr_prof->next;
  if (ptr_prof !=NULL)
    pp_neu = ptr_prof->datensatz;
  /*  x1,x2 mit DURCH_BEREICH vergleichen*/
  if (action == 2)
  {
    if ((pp_neu->x == BCE_NAN)||(pp_neu->next_ds->x == BCE_NAN))
    {
      pp_neu->x = x1;
      pp_neu->y =y1;           /*neu 2.5.96 db*/
      pp_neu->next_ds->x = x2;
      pp_neu->next_ds->y =y2;
      return -1;
    }
  }
  if (action == 3)//Dick 13.07.99 für Trennfläche
  {
    if ((pp_neu->x == BCE_NAN)||(pp_neu->next_ds->x == BCE_NAN))
    {
      pp_neu->x = x1;
      pp_neu->y =1;           /*neu 2.5.96 db*/
      pp_neu->next_ds->x = x2;
      pp_neu->next_ds->y =2;
      return -1;
    }
  }
  if ((pp_neu->x != x1)||(pp_neu->next_ds->x != x2))
  {
    if (action == 1)
    {
      pp_neu->x = x1;
      pp_neu->next_ds->x = x2;    /*neu 2.5.96 db*/
      pp_neu->y =y1;
      pp_neu->next_ds->y =y2;    /* "*/
    }
    return 0;
  }
  else return 1;
}
//****************************************************************************

int List::check_sort(void)
/* testet alle Datensätze auf korrekte Sortierung der Stationswerte
Rückgabe: Datensatz-Nr in der ein Fehler vorliegt oder Null       */
{
  Koord *pp_neu;
  Profildatei *ptr_prof;
  double maximum;
  
  ptr_prof=ptr_anfang;
  
  while (ptr_prof !=NULL)
  {
    //if (typ[ptr_prof->ds_nummer] < MAUL && typ[ptr_prof->ds_nummer] != OK_BRUECKE)
    if(typ[ptr_prof->ds_nummer] == GELAENDEHOEHE && ptr_prof->datensatz!=NULL) //Dick 6.08.98
    {
      pp_neu = ptr_prof->datensatz;
      maximum= pp_neu->x;
      
      while (pp_neu !=NULL)
      {
        if(typ[scr.datensatz]==GELAENDEHOEHE || pp_neu->y != BCE_NAN)
        {
          if (pp_neu->x > maximum)
            maximum= pp_neu->x;
          
          if (pp_neu->x < maximum)
            return ptr_prof->ds_nummer;
        }
        pp_neu = pp_neu->next_ds;
        
      }
    }//-if
    ptr_prof = ptr_prof->next;
  }
  return 0;
}
/********************************************/

int List::ExistGelaende2Daten(void)
{
/* Testen, ob GELAENDE2-Datensatz vorhanden
Rückgabe: 0  := OK
>0 := Nr des Datensatzes,der GELANDE2 enthält
  */
  
  Profildatei *ptr_prof;
  
  ptr_prof=ptr_anfang;
  
  while ((ptr_prof!=NULL)&& (ptr_prof->profiltyp != GELAENDE2))
    ptr_prof = ptr_prof->next;
  
  if ((ptr_prof!=NULL)&&(ptr_prof->profiltyp == GELAENDE2))
    return ptr_prof->ds_nummer;
  else return 0;
}
/************************************************************/
int List::Check_Gel1_Gel2_Daten(List *gelaende2)
{
/*  Teste ob y- und z-Werte von erstem und letztem Profilpunkt
in Profil 1 (==> List) und Profil 2 (==>gelaende2) identisch
sind.

  Rückgabewert: 0 :=  erster und letzter Profilpunkt identisch
  1 :=  erste Profilpunkte stimmen nicht überein
  2 :=  letzte Profilpunkte stimmen nicht überein
  3 :=  sonstiger Fehler (GELAENDEHOEHE nicht gefunden)
  
  */
  Koord *pp_gel1,*pp_gel2;
  Profildatei *prof1,*prof2;
  
  prof1=ptr_anfang;
  while ((prof1 !=NULL)&& (prof1->profiltyp != GELAENDEHOEHE))
    prof1 = prof1->next;
  
  if ((prof1 !=NULL)&&(prof1->profiltyp == GELAENDEHOEHE))
    pp_gel1 = prof1->datensatz;
  else return 3;
  
  prof2=gelaende2->get_anfang();
  while ((prof2 !=NULL)&& (prof2->profiltyp != GELAENDEHOEHE))
    prof2 = prof2->next;
  
  if ((prof2 !=NULL)&&(prof2->profiltyp == GELAENDEHOEHE))
    pp_gel2 = prof2->datensatz;
  else return 3;
  
  /* teste ersten Profilpunkt*/
  if ( (pp_gel1->x != pp_gel2->x)||(pp_gel1->y != pp_gel2->y))
    return 1;
  
  /* in  beiden Listen ans Ende gehen */
  while (pp_gel1->next_ds !=NULL)
    pp_gel1 = pp_gel1->next_ds;
  while (pp_gel2->next_ds !=NULL)
    pp_gel2 = pp_gel2->next_ds;
  /*beide Zeiger auf letztem Profilpunkt*/
  if ( (pp_gel1->x != pp_gel2->x)||(pp_gel1->y != pp_gel2->y))
    return 2;
  
  return 0; // Übereinstimmung !!!
}
/************************************************************/

int List::CopyGel2ToGel1Daten(List *gelaende2)
{
  Koord *pp_gel1,*pp_gel2;
  Profildatei *prof1,*prof2;
  
  prof1=ptr_anfang;
  while ((prof1 !=NULL)&& (prof1->profiltyp != GELAENDE2))
    prof1 = prof1->next;
  
  if ((prof1 !=NULL)&&(prof1->profiltyp == GELAENDE2))
    pp_gel1 = prof1->datensatz;
  else return 0;
  
  prof2=gelaende2->get_anfang();
  while ((prof2 !=NULL)&& (prof2->profiltyp != GELAENDEHOEHE))
    prof2 = prof2->next;
  
  if ((prof2 !=NULL)&&(prof2->profiltyp == GELAENDEHOEHE))
    pp_gel2 = prof2->datensatz;
  else return 0;
  
  
  while ((pp_gel1)&&(pp_gel2))
  {
    pp_gel1->x = pp_gel2->x;
    pp_gel1->y = pp_gel2->y;
    pp_gel1 = pp_gel1->next_ds;
    pp_gel2 = pp_gel2->next_ds;
  }
  return 1;
  
}
/************************************************************/
int List::ExistFlaecheDaten(void)
{
/* Testen, ob FLAECHE-Datensatz vorhanden
Rückgabe:  0  := OK
>0 := Nr des Datensatzes,der GELANDE2 enthält
  */
  
  Profildatei *ptr_prof;
  
  ptr_prof=ptr_anfang;
  
  while ((ptr_prof!=NULL)&& (ptr_prof->profiltyp != FLAECHE))
    ptr_prof = ptr_prof->next;
  
  if ((ptr_prof!=NULL)&&(ptr_prof->profiltyp == FLAECHE))
    return ptr_prof->ds_nummer;
  else return 0;
}
/************************************************************/
int List::CopyGelDatenToLaengspro(LAENGSPROFIL *lp)
{/*lp existiert und für querprof1,querprof2 ist Speicher reserviert
 Rückgabe:  0  :=Fehler
 1  :=OK
  */
  Koord *pp_gel;
  Profildatei *profil;
  GELAENDE *tmp;
  
  if (lp==NULL) return 0;
  profil=ptr_anfang;
  pp_gel = profil->datensatz;  // auf 1.GELAENDE
  tmp = lp->querprof1;
  while (pp_gel)
  {
    if (tmp)
    {
      tmp->y     = pp_gel->x;
      tmp->z     = pp_gel->y;
      tmp->index = pp_gel->ds_nr;
      tmp = tmp->next_ds;
    }
    pp_gel=pp_gel->next_ds;
  }
  while ((profil !=NULL)&& (profil->profiltyp != GELAENDE2))
    profil = profil->next;
  if ((profil !=NULL)&&(profil->profiltyp == GELAENDE2))  //auf 2.GELAENDE
    pp_gel = profil->datensatz;
  else return 0;
  
  tmp = lp->querprof2;
  while (pp_gel)
  {
    if (tmp)
    {
      tmp->y     = pp_gel->x;
      tmp->z     = pp_gel->y;
      tmp->index = pp_gel->ds_nr;
      tmp = tmp->next_ds;
    }
    pp_gel=pp_gel->next_ds;
  }
  return 1;
}
/************************************************************/
int List::CopyFlaecheToProfil(LAENGSPROFIL *lp)
{ /* Daten aus der Flächenberechnung in Datensatz FLAECHE kopieren
  Speicher für Datensatz FLAECHE muss zuvor angelegt werden
  
    Rückgabe 1:= OK
    0:= Fehler
  */
  GELAENDE *tmp_fl;
  Koord *pp_fl;
  Profildatei *prof;
  int anzahl=0;
  
  tmp_fl = lp->flaeche;
  
  prof=ptr_anfang;
  while ((prof !=NULL)&& (prof->profiltyp != FLAECHE))
    prof = prof->next;
  
  if ((prof !=NULL)&&(prof->profiltyp == FLAECHE))
    pp_fl = prof->datensatz;             //zeigt jetzt auf Datensatz FLAECHE
  else return 0;
  
  while ((tmp_fl)&&(pp_fl))
  {
    pp_fl->x = tmp_fl->y;
    pp_fl->y = tmp_fl->z;
    pp_fl->ds_nr = anzahl++;
    tmp_fl = tmp_fl->next_ds;
    pp_fl = pp_fl->next_ds;
  }
  if ((tmp_fl!=NULL)||(pp_fl!=NULL))  //Listenlängen stimmen nicht überein ->Fehler
    return 0;
  return 1;
}
/************************************************************/
int List::SaveSummeToFlaeche(char *summe)
{
/* Summenzeile (Summe,Summe-Auftrag,Summe-Abtrag) in 2.Zeile von
Datensatz FLAECHE schreiben
  */
  // GELAENDE *tmp_fl;
  Profildatei *prof;
  
  
  prof=ptr_anfang;
  while ((prof !=NULL)&& (prof->profiltyp != FLAECHE))
    prof = prof->next;
  
  if ((prof !=NULL)&&(prof->profiltyp == FLAECHE))
  {
    strcpy(prof->daten_info[1],summe);
    return 1;
  }
  else return 0;
}
/************************************************************/
void List::GetInfolineFlaeche(S_FLAECHE *fl,int ds_nummer)
{ /*Fläche: Zeile 2: Summen aus Infoblock holen*/
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&& (ptr_profil->ds_nummer<ds_nummer))
    ptr_profil = ptr_profil->next;
  if (ptr_profil->daten_info[1][0]!='\0')
    sscanf(ptr_profil->daten_info[1],"%s %s %s",fl->s,fl->s_auf,fl->s_ab);
}
/************************************************************/
void List::GetInfolineSecGel(char *zeile,int datensatz)
{ /* 2.Gelände: Zeile 2: Zustand + Referenzdateiname lesen */
  char zustand[20],dateiname[20];
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&& (ptr_profil->ds_nummer<datensatz))
    ptr_profil = ptr_profil->next;
  if ((ptr_profil->daten_info[1][0]!='\0')&&(ptr_profil->daten_info[1][1]!='\0'))
  {
    sscanf(ptr_profil->daten_info[1],"%s %s",zustand,dateiname);
    strcpy(zeile,zustand);
    strcat(zeile," ");
    strcat(zeile,dateiname);
  }
  else zeile[0]='\0';
}
/************************************************************/
int List::CopyGelDatenToGelaende(GELAENDE *ptr_gel,int typ,int anzahl)
{/*
 Daten aus Datensatz 'typ' in ptr_gel kopieren
 
   ptr_gel existiert und Speicher mit Länge 'anzahl' ist reserviert
   typ    : gibt den Datensatz an, der kopiert werden soll (GELAENDE,TRENNFLAECHEN..)
   anzahl : Anzahl der alloziierten Tupel(Länge) von ptr_gel(Zur Kontrolle->Fehler 3 )
   
     Rückgabe:  0  :=OK
     1  :=Fehler
     2  :=Datensatz nicht vorhanden
     3  :=Datensatz im Profil ist länger als 'ptr_gel(anzahl)'
     
  */
  Koord *pp_gel;
  Profildatei *profil;
  GELAENDE *tmp;
  int zaehler=0;
  
  if (ptr_gel==NULL) return 1;
  profil=ptr_anfang;
  while ((profil !=NULL)&& (profil->profiltyp != typ))
    profil = profil->next;
  if ((profil !=NULL)&&(profil->profiltyp == typ))  // Datensatz wirklich gefunden
    pp_gel = profil->datensatz;
  else return 2;    //Datensatz 'typ' nicht vorhanden
  
  tmp = ptr_gel;
  while ((pp_gel!=NULL)&&(zaehler<anzahl))
  {
    if (tmp)
    {
      tmp->y     = pp_gel->x;
      tmp->z     = pp_gel->y;
      tmp->index = pp_gel->ds_nr;
      tmp = tmp->next_ds;
      zaehler++;
    }
    pp_gel=pp_gel->next_ds;
  }
  if ((pp_gel!=NULL)&&(zaehler==anzahl))  // Länge von ptr_gel zu klein
    return 3;
  return 0;
}
/************************************************************/
int List::ExistDatensatzTyp(int typ)
{
/* Testen, ob im Profil Datensatz vom Typ:'typ' vorhanden ist
Rückgabe: 0  := OK
  >0 := Nr des Datensatzes,der GELANDE2 enthält */
  
  Profildatei *ptr_prof;
  ptr_prof=ptr_anfang;
  while ((ptr_prof!=NULL)&& (ptr_prof->profiltyp != typ))
    ptr_prof = ptr_prof->next;
  if ((ptr_prof!=NULL)&&(ptr_prof->profiltyp == typ))
    return ptr_prof->ds_nummer;
  else return 0;
}
/************************************************************/
void List::DelDummySList()
{
  if (slist_dummy!=NULL)
  {
    DeleteWspSList((_WSP_SLIST*)slist_dummy);
    slist_dummy=NULL;
    dummy_typ=0;
  }
}
/************************************************************/
int List::AddDummySList(char *line)
{/* für Längsprofil eine Zeile in SLIST "Bauwerk" anhängen*/
  slist_dummy =(WSP_SLIST*) AppendStringToWspSList((_WSP_SLIST*)slist_dummy,line);
  if (slist_dummy !=NULL) return 1;else return 0;
}
/***********************************************************************/
int List::WriteDummySList(FILE *out,int typ /* = -1 */ )
{
  int  i,anzahl;
  char *tmp;
  
  if( out && ( typ == -1 || typ == dummy_typ ) )
  {
    tmp = new char[100];
    anzahl = WspSListCountElem((_WSP_SLIST*)slist_dummy);
    for (i=0;i<anzahl;i++)
    {
      GetStringFromWspSList((_WSP_SLIST*)slist_dummy, i,tmp);
      fprintf(out,"%s\n",tmp);
    }
    return 1; }
  return 0; }
/***********************************************************************/
int List::ReadDummySList(FILE *in,int anzahl,int typ)
{
  int  i;
  char *tmp,*a;
  
  if (in !=NULL)
  {
    if (slist_dummy!=NULL)
    {
      DeleteWspSList((_WSP_SLIST*)slist_dummy);
      slist_dummy=NULL;
      dummy_typ=0;
    }
    
    tmp = new char[100];
    for (i=0;i<anzahl;i++)
    {
      fgets(tmp,99,in);
      if (a=strrchr(tmp,'\n')) a[0]='\0';
      AddDummySList(tmp);
    }
    dummy_typ = typ;
    return 1;
  }
  return 0;
}
/***********************************************************************/
/*******************************************
ListWspLpData(int pos,double *x,double *y)
für Längsprofil:
Datensatz: Wasserspiegel mNN suchen
und Werte an der Position pos zurückliefern

  Rückgabe: 1  = Wert gefunden
  0  = Wert nicht vorhanden 
  -1 = Datensatz nicht vorhanden 
  
    x = Station
    y = Wsp-Höhe
    
*******************************************/
int List::ListWspLpData(int pos,double *x,double *y,int *n_vzk,double *wsp_fix)
{
  Koord *ptr_koord;
  Profildatei *pd;
  int ok=0;
  BOOLEAN gefunden=FALSE;
  
  *wsp_fix=BCE_NAN;
  
  if( LWA_PROJEKT )
  {
    //Neu Dick 10.09.98 vzk ermitteln
    pd = ptr_anfang;
    
    while ((pd != NULL)&&(pd->profiltyp != VZKENNG))
      pd = pd->next;
    
    if (pd == NULL)  return -1;  // Datensatz nicht vorhanden
    
    if (pd != NULL)
    {
      ptr_koord = pd->datensatz;
      while ((ptr_koord != NULL)&&(ptr_koord->ds_nr < pos))
        ptr_koord=ptr_koord->next_ds;
      if (ptr_koord == NULL)
        return 0;
      else
      {
        
        *n_vzk =(int)ptr_koord->y;
        
      }
    }
    //Ende Neu
  }
  else
    *n_vzk =0;
  
  pd = ptr_anfang;
  
  while ((pd != NULL)&&(pd->profiltyp != WASSERSPIEGEL))
    pd = pd->next;
  
  if (pd == NULL)  return -1;  // Datensatz nicht vorhanden
  
  if (pd != NULL)
  {
    ptr_koord = pd->datensatz;
    while ((ptr_koord != NULL)&&(ptr_koord->ds_nr < pos))
      ptr_koord=ptr_koord->next_ds;
    if (ptr_koord == NULL)
      return 0;
    else
    {
      *x = ptr_koord->x;
      *y = ptr_koord->y;
      //return 1;
      ok=1;
    }
  }
  //Neu Dick 5.02.99 Wasserspiegelfixirungen
  pd = ptr_anfang;
  
  while ((pd != NULL)&&(pd->profiltyp != WSP_FIXIERUNG))
    pd = pd->next;
  
  if (pd != NULL)
  {
    ptr_koord = pd->datensatz;
    while ((ptr_koord != NULL)&&!gefunden)
    {
      if(*x==ptr_koord->x)
      {
        *wsp_fix=ptr_koord->y;
        gefunden=TRUE;
      }
      ptr_koord=ptr_koord->next_ds;
    }       
  }
  if(ok==1)
    return 1;
  else
    //ende neu
    return 0;
} 

int List::InsertQpWspInLp(int nr, double wsp, char* name, char* BerVariante, int datensatz )
// Wasserspiegel wsp (aus Längsprofil) als neuen Datensatz:"STATION" in Querprofil einfügen
// Dabei kann char*BerVariante in der 2.Zeile gespeichert werden
// Parameter:
// Rückgabewert:
//          int: Anzahl der erzeugten Koordinaten
{
  double x1,x2,y1,y2;
  double wx1,wx2,wy1,wy2;
  double sx1,sy1,sx2,sy2;
  Profildatei *pd;
  Koord *ptr_koord;
  char *temp=NULL;
  BOOL left=FALSE,right=FALSE;
  int len=strlen(name)+strlen(BerVariante);
  int schnittpkt=0;
  int linienpkt=0;
  typedef struct _schnitt   //Dick 12.01.99
  {
    double x;
    double y;
    int steigung;//1-nach unten 2-nach oben
  }schnitt,*lpschnitt;
  schnitt schnitt_koord[100];
  Koord *durchst_ber;    //Dick 12.01.99
  Koord *new_koord;    //Dick 12.01.99
  double d_ber_x1,d_ber_x2;//,sx_temp;
  int i=0;
  
  temp = new char[len+2+100];//Dick 9.02.99
  strcpy(temp," ");//Dick 9.02.99
  for(i=0;i<99;i++) //Dick 9.02.99
    strcat(temp," ");//Dick 9.02.99
  strcpy(temp,BerVariante);
  strcat(temp,"@");
  strcat(temp,name);
  
  MakeNewNode(nr);
  WriteTypDaten(nr,datensatz,temp); //Neu Dick 5.02.99
  delete[] temp;
  
  durchst_ber=HoleDatensatz(DURCHST_BEREICH);//Dick 12.01.99
  
  ptr_koord = ptr_anfang->datensatz;
  if (ptr_koord == NULL) 
    return 0;
  
  x1 = ptr_koord->x; // erster Stationswert
  y1 = ptr_koord->y;
  while (ptr_koord->next_ds !=NULL)
    ptr_koord = ptr_koord->next_ds;
  x2 = ptr_koord->x;   // letzter Stationswert
  y2 = ptr_koord->y;
  
  if(durchst_ber!=NULL )
  {
    d_ber_x1=durchst_ber->x;
    if ( durchst_ber->next_ds )
      d_ber_x2=durchst_ber->next_ds->x;
    else
      d_ber_x2=x2;
  }
  else
  {
    d_ber_x1=x1;
    d_ber_x2=x2;
  }
  
  if (wsp >y1)//Dick 9.02.99 = weg  
  {
    wx1=x1; wy1=wsp;
    left = TRUE;
  }
  if (wsp >y2)//Dick 9.02.99 = weg
  {
    wx2=x2; wy2=wsp;
    right=TRUE;
  }
  if(left == TRUE)
  {
    schnitt_koord[linienpkt].x=wx1; 
    schnitt_koord[linienpkt].y=wy1;
    schnitt_koord[linienpkt].steigung=1;
    linienpkt++;
  }
  
  double TOL=1.0e-05; // Toleranz
  double X,Y;
  int letztesteigung=0;
  ptr_koord = ptr_anfang->datensatz;
  if (ptr_koord == NULL) 
    return 0;
  while (ptr_koord->next_ds !=NULL)
  {      // --> Leibo.cpp :main_dll
    int ret=SchnittPunktzweierStrecken(TOL,
      ptr_koord->x,ptr_koord->y,
      ptr_koord->next_ds->x,ptr_koord->next_ds->y,
      ptr_koord->x-1,wsp,
      ptr_koord->next_ds->x+1,wsp,
      &X,&Y);
    if (ret >=10000)
    {
      schnitt_koord[linienpkt].x=X; 
      schnitt_koord[linienpkt].y=Y;
      if((ptr_koord->y - ptr_koord->next_ds->y)>=0.0)
        schnitt_koord[linienpkt].steigung=1;
      else
        schnitt_koord[linienpkt].steigung=2;
      
      if (schnittpkt==0)
      {sx1 = X;
      sy1 = Y;}
      if (schnittpkt>=1)
      {sx2 = X;
      sy2 = Y;}
      schnittpkt++;
      if(ret!=10001 || 
        (linienpkt==0 && schnitt_koord[linienpkt].steigung!=2) || //Dick 24.08.99
        (schnitt_koord[linienpkt].steigung !=letztesteigung && letztesteigung!=0) )
      {
        letztesteigung=schnitt_koord[linienpkt].steigung;
        linienpkt++;
      }
    }
    ptr_koord = ptr_koord->next_ds;
  }
  
  if (schnittpkt==0) //Dick 25.01.99 von oben verschoben
  { //Wasserspiegel geht über volle Geländebreite
    pd = ptr_anfang;
    while ((pd != NULL)&&(pd->ds_nummer < nr)) 
      pd = pd->next;
    if ( pd==NULL ) 
      return 0;
    
    ptr_koord = pd->datensatz;
    ptr_koord->x = x1;
    ptr_koord->y = wsp;
    ptr_koord->next_ds->x = x2;
    ptr_koord->next_ds->y = wsp;
    ptr_koord->next_ds->next_ds = NULL;
    return 2; // es gibt genau zwei Coordinaten
  }
  
  if(right == TRUE)
  {
    schnitt_koord[linienpkt].x=wx2; 
    schnitt_koord[linienpkt].y=wy2;
    schnitt_koord[linienpkt].steigung=2;
    linienpkt++;
  }
  
  pd = ptr_anfang;
  while ((pd != NULL)&&(pd->ds_nummer < nr))
    pd = pd->next;
  if ( pd==NULL ) 
    return 0;
  
  ptr_koord = pd->datensatz;
  int linienanz=0;
  int anzSchnitt = 2;
  for(i=0;i<linienpkt;i+=2)
  {
    if((schnitt_koord[i].x >= d_ber_x1 && schnitt_koord[i].x < d_ber_x2) || 
      (schnitt_koord[i+1].x <= d_ber_x2 && schnitt_koord[i+1].x > d_ber_x1) ||
      (schnitt_koord[i].x < d_ber_x1 && schnitt_koord[i+1].x > d_ber_x2))
    {              
      linienanz++;
      if(linienanz>1)
      {
        for(int j=1;j<3;j++)
        {
          new_koord=new Koord;
          new_koord->ds_nr = i+j;
          new_koord->x=schnitt_koord[i+j-1].x;
          new_koord->y=schnitt_koord[i+j-1].y;
          new_koord->pre_x=0;
          new_koord->pre_y=0;
          new_koord->attr=0;
          new_koord->status =1;
          ptr_koord->next_ds->next_ds=new_koord;
          ptr_koord->next_ds->next_ds->pre_ds=ptr_koord->next_ds;
          ptr_koord=ptr_koord->next_ds;
          ptr_koord->next_ds->next_ds=NULL;
        }
        anzSchnitt += 2;
      }
      else
      {
        ptr_koord->x = schnitt_koord[i].x;
        ptr_koord->y = schnitt_koord[i].y;
        ptr_koord->next_ds->x = schnitt_koord[i+1].x;
        ptr_koord->next_ds->y = schnitt_koord[i+1].y;
      }
    }
  }
  
  ptr_koord->next_ds->next_ds = NULL;
  return anzSchnitt;
}

/***********************************************************************/
int List::ExtractStationsString(int nr,char *str,int datensatz)
{
/* 2.Zeile von Datensatz 'STATION' zerlegen in:
"WSP-HOEHE NN+m" +  " aus Ber.variante:" + REST

  Return: 0 : Fehler / REST nicht vorhanden
  1 : OK,  REST  in str
  d.h. kommt nur "WSP-HOEHE NN+m" vor wird 0 zurückgegeben         
  */
  Profildatei *pd;
  char *pt1;
  char *tmp = new char[251];//Dick 9.02.99 151->251
  int i=0;
  
  pd = ptr_anfang;
  while ((pd != NULL)&&(pd->ds_nummer < nr))
    pd = pd->next;
  //if (( pd != NULL ) && (pd->profiltyp == STATION))
  if (( pd != NULL ) && (pd->profiltyp == datensatz))
  {
    strcpy(tmp,pd->daten_info[1]);
    if(tmp!=NULL)
    {
      while(tmp[i]==' ') //Dick 9.02.99
        i++;
      strcpy(str,&tmp[i]);
      delete[] tmp;
      return 1;
    }
    else
    {
      delete[] tmp;
      return 0;
    }
    if (xvt_str_match(tmp,"WSP-HOEHE NN+m*",FALSE) || xvt_str_match(tmp,"WSP-Messung NN+m*",FALSE))
      if(xvt_str_match(tmp,"WSP-HOEHE NN+m*aus Ber.variante:*",FALSE)|| xvt_str_match(tmp,"WSP-Messung NN+m*aus Ber.variante:*",FALSE))
      {
        pt1 = strchr(tmp,'@');
        if (pt1) pt1[0]='\0'; // Dateiname
        pt1 = strchr(tmp,':');
        pt1++;
        strcpy(str,pt1);
        delete[] tmp;
        return 1;
      }
  }
  else
  {
    delete[] tmp;
    return 0;
  }
  return 0;
}
/***********************************************************************/
int List::ExtractInfo2String(int nr,char *str)
{
/* 2.Zeile von Datensatz einlesen

  Return: 0 : Fehler keine Info
  1 : OK,  Info  in str
  
  */
  Profildatei *pd;
  char *pt1;
  char *tmp = new char[251];//Dick 26.11.98 151->251
  
  pd = ptr_anfang;
  while ((pd != NULL)&&(pd->ds_nummer < nr))
    pd = pd->next;
  if (pd != NULL)
  {
    strcpy(tmp,pd->daten_info[1]);
    if(tmp!=NULL)
    {
      pt1=tmp;
      while(isspace(*tmp))
        tmp=_tcsinc(tmp);
      strcpy(str,tmp);
      tmp=pt1;
      delete[] tmp;
      return 1;
    }
    else
    {
      delete[] tmp;
      return 0;
    }
    
  }
  else
  {
    delete[] tmp;
    return 0;
  }
  return 0;
}

/***********************************************************************/
int List::FindWspQpData(char *filename,char *BerVariante)
{
  Profildatei *ptr_prof;
  char *p1,tmp[255],t1[255],f1[255];
  int j=0;
  ptr_prof=ptr_anfang;
  while (ptr_prof!=NULL)
  { 
    if (ptr_prof->profiltyp == STATION)
    {
      //         if(xvt_str_match(ptr_prof->daten_info[1],"WSP-HOEHE NN+m*aus Ber.variante:*",FALSE))
      if(xvt_str_match(ptr_prof->daten_info[0],"WSP-HOEHE NN+m*",FALSE))
      {
        strcpy(tmp,ptr_prof->daten_info[1]);
        p1  = strrchr(tmp,'@');
        if (p1)
        {
          p1++;
          strcpy(t1,p1); //in t1 steht jetzt der Dateiname
          strcpy(f1,filename);
          strcat(f1,"*");
          if(xvt_str_match(t1,f1,FALSE))
            //           if(xvt_str_match(t1,filename,FALSE))
          {
          /*       p1  = strrchr(tmp,'@');
          p1[0]='\0';
          p1 = strchr(tmp,':');
          if (p1)
          {
          p1++;
          strcpy(t1,p1); //in t1 steht die Ber.variante
          
            */
            for(unsigned int i=0; i<=(strlen(tmp)); i++)
            {
              //if(tmp[i]!=' ' && tmp[i]!='@')//Dick 10.02.99
              if(tmp[i]!='@')//Dick 12.02.99
                t1[j++]=tmp[i];
              if (tmp[i]=='@')
              {
                t1[j++]='\0';
                break;
              }
              
            }
            
            //}
            if(xvt_str_match(t1,BerVariante,FALSE))
              return ptr_prof->ds_nummer;      
            
          }
        }
      }
      //      return 0;      
    }
    ptr_prof=ptr_prof->next;
  }
  return 0;
}

/***********************************************************************/
/***********************************************************************/
/***********************************************************************/

int List::ProfilVerl(WSP_PROFIL_LISTE *pWPL,int ver)
{  /* Profilverlängerung:
   in aktuelles Profil Daten aus List *prof - Datensatz Gelände 
   links oder rechts (ver = 1 oder 2) einfügen.
   
     Rückgabe: Anzahl der eingefügten Werte oder NULL
  */
  
#define  LINKS_VERL  1
#define  RECHTS_VERL 2
  
  Koord *pn,*pm,*po;
  int nr,k=0;
  double x1,xL,xR,dX;
  
  pn = ptr_anfang->datensatz;
  pm = pWPL->PList->ptr_anfang->datensatz;
  
  if (ver == RECHTS_VERL)
  {
    while (pn->next_ds !=NULL)
      pn = pn->next_ds;
    
    for (int i=0;i<pWPL->data->ds_info[1]-1;i++)
      NewKoord(GELAENDEHOEHE) ;
    
    pn = pn->next_ds;
    
    /*
    erster Punkt von Profilverlängerung gleich letzter Punkt von aktuel.Profil 
    also um eine Position weiterhängen
    */
    pm = pm->next_ds;
    
    while ((pm)&&(pn))
    {
      pn->x = pn->pre_ds->x + pm->x - pm->pre_ds->x;
      pn->y = pm->y;
      pn ->pre_x = pm->pre_x;
      pn ->pre_y = pm->pre_y;
      pm = pm->next_ds;
      pn = pn->next_ds;
      k++;
    }
    
  }
  else if (ver == LINKS_VERL)
  {
  /*
  zuerst mit NewKoord() neue Koordinaten hinten anfügen und 
  anschließend alles nach vorne hängen
    */
    x1 = pn->x;  // ersten Wert von Gelände merken
    while (pn->next_ds !=NULL)
      pn = pn->next_ds;
    
    if (pWPL->data->ds_info[1]>0)
    {
      for (int i=0;i<pWPL->data->ds_info[1]-1;i++)
        NewKoord(GELAENDEHOEHE) ;
      
      // nach vorne hängen
      po = pn->next_ds;
      pm = pn;
      while (pm->next_ds !=NULL)
        pm = pm->next_ds;
      pn->next_ds = NULL;
      po->pre_ds = NULL;
      pm->next_ds = ptr_anfang->datensatz;
      pm->next_ds->pre_ds = pm;
      ptr_anfang->datensatz = po;
      
      pn = pWPL->PList->ptr_anfang->datensatz; // pm zeigt auf 2.Profil
      xL=pn->x;      //erster Wert aus 2.Profil
      while (pn->next_ds !=NULL)
        pn = pn->next_ds;
      xR = pn->x;    //letzter Wert aus 2.Profil
      pn = pn->pre_ds; 
      
      /*pm zeigt noch auf letztes Element der vorne eingefügten Kette*/
      while ((pm!=NULL)&&(pn!=NULL))
      {
        dX=  pn->next_ds->x - pn->x;
        pm->x = x1-dX;
        x1 = pm->x;
        pm->y = pn->y;
        pm ->pre_x = pn->pre_x;
        pm ->pre_y = pn->pre_y;
        k++;
        pm = pm->pre_ds;
        pn = pn->pre_ds;
      }
    }
  }
  // jetzt noch ds_nr aktualisieren
  nr =1;
  pn = ptr_anfang->datensatz;
  while (pn)
  {
    pn->ds_nr = nr;
    nr++;
    pn=pn->next_ds;
  }
  return k;
}
/***********************************************************************/
/***********************************************************************/
/***********************************************************************/
/***********************************************************************/
int List::check_nwrinne(int ds)
{
  Koord *pp_neu;
  Profildatei *ptr_prof;
  double x1,x2;
  
  ptr_prof=ptr_anfang;
  while((ptr_prof->ds_nummer<ds) && (ptr_prof->next!=NULL))
    ptr_prof=ptr_prof->next;
  if (ptr_prof !=NULL)
  {
    pp_neu = ptr_prof->datensatz;
    x1 = pp_neu->x;   //erster Stationswert in Gelände
    if(x1==BCE_NAN)
      return 1;
    while (pp_neu->next_ds !=NULL)
    {
      pp_neu=pp_neu->next_ds;
      x2=pp_neu->x;     //letzter Stationswert
      if(x2==BCE_NAN)
        return 1;  
    }
  }
  else
    return 1;
  return 0;
}

/****************************************************/
int List::GetBuhnenKoord(int editwindow, Scroller *pscr)
{
  int koordinate=0, buhne=0;
  int i, dreier=0, gefunden=0;
  Koord *pp_neu;
  Profildatei *ptr_prof;
  
  ptr_prof=ptr_anfang;
  while((ptr_prof->ds_nummer<pscr->datensatz) && (ptr_prof->next!=NULL))
    ptr_prof=ptr_prof->next;
  if (ptr_prof !=NULL)
  {
    pp_neu = ptr_prof->datensatz;
    if(pp_neu!=NULL)
    {
      if(pp_neu->y!=BCE_NAN)
        buhne=1;
      
      pp_neu = ptr_prof->datensatz;
      while(pp_neu!=NULL)
      {
        
        for(i=0; i<=editwindow; i++)
        {
          if(pscr->x[i]==pp_neu->x)
          {
            gefunden=buhne;
            break;
          }
        }
        if(!gefunden)
        {
          pp_neu=pp_neu->next_ds;
          //              dreier++;
          //                if (dreier==3)
          //              {
          //                dreier=0;
          buhne++;
          //              }
        }
        else
          break;
      } //while
    }//if pp!=NULL
  } //if ptr_prof!=NULL
  
  //return koordinate;  // "
  return gefunden;
}

/***************************************************/

double List::hole_station(int editwindow,Scroller *pscr)
{
  double station;
  station=pscr->x[editwindow];
  return station;
}

/***********************************************************************/
void List::schluessel_holen(int position,char *typ, char *vzkt, char *pkt)
{
/*
ptr_profil = ptr_anfang;
ptr_anfang = ptr_anfang->next;
p_h = ptr_profil->datensatz;
while (p_h)
{
pp=p_h;
p_h = p_h->next_ds;

  */
  //char helptext[30];
  
  ptr_profil=ptr_anfang;
  pp=ptr_profil->datensatz;
  strcpy(typ,"offenes Profil");
  strcpy(vzkt,"0");
  strcpy(pkt,"0");
  
  while(ptr_profil)
  {
    pp=ptr_profil->datensatz;
    for(int i=1;i<=position;i++)
    {
      //pp=ptr_profil->datensatz;
      if((pp!=NULL) && (pp->next_ds!=NULL))
        pp=pp->next_ds;
    }
    
    if(ptr_profil->profiltyp==108) //profilart
    {
      if((pp->y==10) || (pp->y==20) || (pp->y==30) || (pp->y==40))
        strcpy(typ,"Kreisprofil");
      if((pp->y==11) || (pp->y==21) || (pp->y==31) || (pp->y==41))
        strcpy(typ,"geschlossenes Profil");
      if((pp->y==12) || (pp->y==22) || (pp->y==32) || (pp->y==42))
        strcpy(typ,"Kreissegment");
      if((pp->y==13) || (pp->y==23) || (pp->y==33) || (pp->y==43))
        strcpy(typ,"MAUl DIN 4263");
      if((pp->y==14) || (pp->y==24) || (pp->y==34) || (pp->y==44))
        strcpy(typ,"MAUl ARMCO 71");
      if((pp->y==15) || (pp->y==25) || (pp->y==35) || (pp->y==45))
        strcpy(typ,"Wertetabelle");
      if((pp->y==16) || (pp->y==26) || (pp->y==36) || (pp->y==46))
        strcpy(typ,"Eiprofil");
      if((pp->y==17) || (pp->y==27) || (pp->y==37) || (pp->y==47))
        strcpy(typ,"Maul Armco 84");
      if((pp->y==18) || (pp->y==28) || (pp->y==38) || (pp->y==48))
        strcpy(typ,"Ellipse Armco 84");
      if((pp->y==19) || (pp->y==29) || (pp->y==39) || (pp->y==49))
        strcpy(typ,"Super-Span Armco 84");
      if((pp->y>=70) && (pp->y<=77))
        strcpy(typ,"Wehr");
      if((pp->y>=80) && (pp->y<=83))
        strcpy(typ,"Brücke");
      
      if((pp->y>=30) && (pp->y<=49))
        strcat(typ," ü.fl.\0");
      
    }
    if(ptr_profil->profiltyp==109) //vzk
    {
      if(pp->y>0)
      {
        //gcvt(pp->y,10,vzkt);
        sprintf(vzkt,"%lf",pp->y);
      }
    }
    if(ptr_profil->profiltyp==110) //pk
    {
      if(pp->y!=0)
      {
        strcpy(pkt,"Mehrf.br. ");
        
        if(pp->y==2)
          strcat(pkt,"FF");
        if(pp->y==1)
          strcat(pkt,"LL");
        if(pp->y==3)
          strcat(pkt,"RR");
        
      }
    }
    /*    Vorsicht Anzahl Punkte ist nicht Anzahl Geländehöhen
    if(ptr_profil->profiltyp==111) //DKUK
    {
    if(pp->y>0)
    strcat(typ," überfl.");
    }
    */
    
    ptr_profil=ptr_profil->next;
  }
  
}

/***********************************************************************/
void List::BauwerkansEnde( int nummer, int* ds_info, int* typ )
//für jeden Datensatz geeignet
{
  Profildatei* p_help = NULL;
  
  ptr_profil = ptr_anfang;
  
  while ((ptr_profil->ds_nummer < nummer-1)&&(ptr_profil !=NULL))
    ptr_profil = ptr_profil->next;
  
  if ((ptr_profil == ptr_anfang)&&(nummer==1)&&(ptr_profil->profiltyp==1))  //erster Datensatz:Gelaende
  {
    //xvt_dm_post_note("Dieser Datensatz kann nicht umgesetzt werden!");
    char buf[200];//Dick 26.11.99
    xvt_res_get_str(STR_BAUANSENDE,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf);
  }
  else
  {
    if (ptr_profil->next != NULL)
      p_help = ptr_profil->next;
    if (p_help->next != NULL)
    {
      ptr_profil->next = p_help->next;
      while (ptr_profil->next !=NULL)
      {
        ptr_profil = ptr_profil->next;
        ptr_profil->ds_nummer--;
      }
      int help=ds_info[nummer]; 
      
      for (int i=nummer;i<=ds_info[0];i++)
      {
        ds_info[i]=ds_info[i+1];
        typ[i]=typ[i+1];
        if(i==ds_info[0])
        {
          ds_info[i]=help;
          typ[i]=p_help->profiltyp;
        }
      }
      if(ptr_profil->next==NULL)
      {
        ptr_profil->next=p_help;
        p_help->next=NULL;
      }
    }
  }
  ptr_profil=ptr_anfang;
  int zaehler=1;
  ptr_profil->ds_nummer=zaehler;
  while(ptr_profil->next!=NULL)  //Dick 22.10.98
  {
    ptr_profil=ptr_profil->next;
    zaehler++;
    ptr_profil->ds_nummer=zaehler;
  }
}
/***************************************************/

void List::Koord_Einf( double x, double y, int Pos, int datensatz )
// Koordinate (x,y) vor der Position Pos einfügen
{
  ptr_profil = ptr_anfang;
  
  while( ptr_profil )
  {
    if( ( ( ( ptr_profil->profiltyp < OK_WEHRS ) ||
      ( ptr_profil->profiltyp == UNKNOWN ) ||
      ( ptr_profil->profiltyp == BUHNE ) )    && datensatz == -1 ) ||
      ( datensatz != -1 && ptr_profil->profiltyp == datensatz ) )
    {
      switch ( ptr_profil->profiltyp )
      {
      case TRENNFLAECHEN:
      case BORDVOLL:
      case DURCHST_BEREICH:
      case MODELLGRENZEN:
        {
        }
        break;      
        
      default:
        { 
          Koord* help;
          int Pos_help = Pos;
          Koord* new_pp = new Koord;
          
          pp = ptr_profil->datensatz;
          
          if ( ptr_profil->profiltyp > 1 && datensatz == -1 )   // nicht GELAENDE
            y = BCE_NAN;
          new_pp->x = x;
          new_pp->y = y;
          new_pp->pre_x = 0;
          new_pp->pre_y = 0;
          ds_info[ptr_profil->ds_nummer]++;
          
          if( pp == NULL ) // Wenn erstes Element
          {
            ptr_profil->datensatz = new_pp;
            new_pp->next_ds = NULL;
            new_pp->pre_ds = NULL;
            new_pp->ds_nr = 1;
            break;
          }; // if pp == NULL
          
          if( ( ptr_profil->profiltyp == UK_BRUECKE || ptr_profil->profiltyp == OK_BRUECKE ||
            ptr_profil->profiltyp == OK_WEHRS   || ptr_profil->profiltyp == OK_GELAENDE ||
            ptr_profil->profiltyp == BUHNE ) && datensatz == -1 )
            Pos_help = PositionSuchen( ptr_profil->ds_nummer, Pos );
          
          while( ( pp->next_ds ) && ( pp->ds_nr != Pos_help ) )
            pp = pp->next_ds;
          help = pp;
          if( ( pp->next_ds == NULL ) && ( pp->ds_nr != Pos_help ) )   // ListenENDE
          {
            pp->next_ds = new_pp;
            new_pp->next_ds = NULL;
            new_pp->pre_ds = help;
            new_pp->ds_nr = pp->ds_nr + 1;
          }
          else
          {
            if ( pp->pre_ds )
            {
              pp->pre_ds->next_ds = new_pp;
              new_pp->next_ds = pp;
              new_pp->pre_ds = pp->pre_ds;
              pp->pre_ds = new_pp;
              new_pp->ds_nr = pp->ds_nr;
            }
            else  // Listenanfang !
            {
              if( ptr_profil->datensatz == pp )
              {
                ptr_profil->datensatz = new_pp;
                new_pp->next_ds = pp;
                pp->pre_ds = new_pp;
                new_pp->pre_ds = NULL;
                new_pp->ds_nr = pp->ds_nr;
              }; // if datenstatz == pp
            }; // if pp->pre_ds
            
            while ( pp )
            {
              if( pp->next_ds == NULL )
                pp->ds_nr = pp->pre_ds->ds_nr + 1;
              else
                pp->ds_nr = pp->next_ds->ds_nr;
              pp = pp->next_ds;
            }; // while pp
          }; // if pp->next_ds == NULL ...
          
        }; // default
        break;
      }; // switch ptr_profil->profiltyp
    }; // if ( .... )
    ptr_profil = ptr_profil->next;
  }; // while ptr_profil
  
  if( x < pmm.minX )
  {
    GetMinMax( &pmm,scr.datensatz );
    Update_Scr( 0, &scr );
  }; // if x < pmm.minX
  
}; // Koord_Einf
/***************************************************/
int List::Koord_Einf_Sorted(double x,double y,int Pos)
{  //Koordinate (x,y) ab der Position Pos Sortiert einfügen
  Koord *help,*new_pp;
  int einfpos=14;
  ptr_profil = ptr_anfang;
  
  while ((ptr_profil !=NULL))//&& ((ptr_profil->profiltyp < OK_WEHRS)||(ptr_profil->profiltyp ==UNKNOWN))    )
  {
    if((ptr_profil->profiltyp < OK_WEHRS)||(ptr_profil->profiltyp ==UNKNOWN) || 
      (ptr_profil->profiltyp==BUHNE)||
      ((ptr_profil->profiltyp >= SOHLHOEHE)&&(ptr_profil->profiltyp < LP_TEXT)) ||
      ((ptr_profil->profiltyp >= GEFAELLE)&&(ptr_profil->profiltyp < WSP_FIXIERUNG)) || //Dick 23.09.99 neue Datensätze 
      ((ptr_profil->profiltyp >= SCHLEPPSPANNUNG)&&(ptr_profil->profiltyp < SOHLHOEHE_2)))//Dick 23.09.99 neue Datensätze
    {
      
      switch (ptr_profil->profiltyp)
      {
      case TRENNFLAECHEN:
      case BORDVOLL:
      case MODELLGRENZEN:
      case DURCHST_BEREICH:
      case UK_BRUECKE:
      case OK_BRUECKE:
      case BUHNE:
        {
        }
        break;      
      default:
        {
          new_pp = new Koord;
          
          pp=ptr_profil->datensatz;
          
          if (ptr_profil->profiltyp > 1)   // nicht GELAENDE
            y=BCE_NAN;
          new_pp->x = x;
          new_pp->y = y;
          new_pp->pre_x = 0;
          new_pp->pre_y = 0;
          ds_info[ptr_profil->ds_nummer]++;
          
          
          while ((pp->next_ds != NULL)&&((pp->ds_nr < Pos)||pp->x < x))//Dick 3.04.2000 = weg bei Pos
            pp = pp->next_ds;
          help = pp;
          if ((pp->next_ds==NULL)&&(pp->x < x)/*9.05.2000 pp->x < x*/)   // ListenENDE  Dick 3.04.2000 weg &&(pp->ds_nr<Pos)
          {
            pp->next_ds=new_pp;
            new_pp->next_ds = NULL;
            new_pp->pre_ds=help;
            new_pp->ds_nr = pp->ds_nr +1;
          }
          else
          {
            if (pp->pre_ds!=NULL)
            {
              pp->pre_ds->next_ds = new_pp;
              new_pp->next_ds = pp;
              new_pp->pre_ds = pp->pre_ds;
              pp->pre_ds = new_pp;
              new_pp->ds_nr = pp->ds_nr ;
            }
            else  // Listenanfang !
            {
              if (ptr_profil->datensatz == pp)
              {
                ptr_profil->datensatz = new_pp;
                new_pp->next_ds = pp;
                pp->pre_ds = new_pp;
                new_pp->pre_ds = NULL;
                new_pp->ds_nr = pp->ds_nr;
              }
            }
            while (pp!=NULL)
            {
              if (pp->next_ds ==NULL)
                pp->ds_nr = pp->pre_ds->ds_nr+1;
              else
                pp->ds_nr =pp->next_ds->ds_nr;
              pp = pp->next_ds;
            }
          }
          
        }
        break;
      };
    }
    if(ptr_profil->profiltyp==GELAENDEHOEHE)
      einfpos=new_pp->ds_nr;
    ptr_profil = ptr_profil->next;
  }
  
  
  
  if (x < pmm.minX)
  {
    GetMinMax(&pmm,scr.datensatz);
    Update_Scr(0,&scr);
  }
  return einfpos;
}
/****************************************************************************
* Funktion zur Ermitlung relative Position für Einfügen in ein Datensatz   *
* von der Position in der Geländehöhe                                      *
* z.B.Brücke:                                                              *
* Geländehöhe X: 0  5  5  3  7  18  21  21                                 *
* UK_BRUCKE   X: 5  5  21  21                                              *
* 18 muß in der UK_BRUCKE                                                  *
* input: pos=6,ds_num=UK_BRUCKE                                            *
* output = 3 (zwieschen  zweite 5 und erste 21)                            *
*                                                                          *
****************************************************************************/
int List::PositionSuchen(int ds_num,int pos)
{
  BOOLEAN gefunden;
  Koord *G_Koord,*NG_Koord;
  Profildatei *help_prof;
  int ende;
  
  help_prof=ptr_anfang;
  
  
  while((help_prof!=NULL)&&(help_prof->ds_nummer < ds_num))
    help_prof=help_prof->next;
  
  NG_Koord=help_prof->datensatz;
  if(NG_Koord==NULL)return 1;
  while(NG_Koord!=NULL)
  {
    G_Koord=ptr_anfang->datensatz;
    gefunden=FALSE;
    
    while(G_Koord != NULL && G_Koord->ds_nr <= pos  )
    {
      if(NG_Koord->x == G_Koord->x)
        gefunden=TRUE;
      if(G_Koord->ds_nr == pos && NG_Koord->x == G_Koord->x && 
        Get_Station_pos_allg(G_Koord->x,pos,1)<=
        Get_Station_pos_allg(NG_Koord->x,NG_Koord->ds_nr,ds_num)     )
        return NG_Koord->ds_nr;
      G_Koord=G_Koord->next_ds;
    }
    if(gefunden)
    {
      ende=NG_Koord->ds_nr; 
      NG_Koord=NG_Koord->next_ds;                
    }
    else
      return NG_Koord->ds_nr;
  }
  return ende+1;
}

/***************************************************/
void List::Koord_Update(int grafik)
{  //Koordinaten mit y==BCE_NAN beim Datensatzwechsel löschen
  Profildatei *ptr_profil_help;
  Koord *pp_save;
  int num_station,num_gelaende,num_station_leer,pos_station,pos_gelaende;
  ptr_profil_help = ptr_anfang;
  
  while ((ptr_profil_help !=NULL))//&& ((ptr_profil_help->profiltyp < OK_WEHRS)||(ptr_profil_help->profiltyp ==UNKNOWN))    )
  {
    if((ptr_profil_help->profiltyp < OK_WEHRS)||
      (ptr_profil_help->profiltyp ==UNKNOWN)||
      (ptr_profil_help->profiltyp==BUHNE) ||
      (ptr_profil_help->profiltyp==RECHTSWERT) || //Bley 12.02.2001
      (ptr_profil_help->profiltyp==HOCHWERT) ||
      (ptr_profil_help->profiltyp ==BORDVOLL) ||
      ((ptr_profil_help->profiltyp >DATPLOT/*Längsschnitt*/)&&
      (ptr_profil_help->profiltyp < LP_TEXT)) ||
      ((ptr_profil_help->profiltyp >BAUWERK/*Längsschnitt BCE*/)&&
      (ptr_profil_help->profiltyp < SOHLHOEHE_2))||
      (ptr_profil_help->profiltyp == WSP_FIXIERUNG))
    {
      
      switch (ptr_profil_help->profiltyp)
      {
        
      case LP_TEXT:
      case BAUWERK:
        {
        }
        break;
      case SCHUETZ:
        {
          pp=ptr_profil_help->datensatz;
          pp->x=0;
          pp->y=0;
          
        }
      case TRENNFLAECHEN:
      case BORDVOLL:
      case DURCHST_BEREICH:
      case MODELLGRENZEN:
        {
          if(grafik==0 )
          {
            pp=ptr_profil_help->datensatz;
            while (pp!=NULL)
            {
              if(ExistStationX(pp->x,GELAENDEHOEHE)<1)
              {
                char temp[25];
                char buf[200],buf2[200];//Dick 26.11.99
                xvt_res_get_str(1000+ptr_profil_help->profiltyp,temp,sizeof(temp));
                xvt_res_get_str(STR_Y_WERT_IN,buf,sizeof(buf));
                xvt_res_get_str(STR_DELETE_STATION_3,buf2,sizeof(buf2));
                strcat(buf," ");
                strcat(buf,temp);
                strcat(buf," ");
                strcat(buf,buf2);
                xvt_res_get_str(STR_WARNING,buf2,sizeof(buf2));
                MessageBox(NULL,buf,buf2,MB_ICONINFORMATION|MB_OK);
                //               xvt_dm_post_note (y-Wert %buf2 ist ungültig geworden)
                
                int hilfsdsnummer=ptr_profil_help->ds_nummer; 
                DeleteNode( ptr_profil_help->ds_nummer, (int*)&ds_info, (int*)&typ );
                anzahl_ds--;
                
                xvt_list_rem(win_list2,hilfsdsnummer-1);               
                xvt_list_resume(win_list2);
                xvt_list_set_sel(win_list2,scr.datensatz-1,TRUE);
                break;
              }
              pp=pp->next_ds;
            }
          }
        }
        break;
      default:
        {                   
          pp=ptr_profil_help->datensatz;
          
          while (pp!=NULL)
          {
            pp_save=pp->next_ds;//Dick, Wenn gelöscht wird nächste element retten
            num_station = num_station_in_ds(pp->x ,ptr_profil_help->ds_nummer);
            num_gelaende = num_station_in_ds(pp->x ,GELAENDEHOEHE);
            num_station_leer=num_station_leer_in_ds(pp->x ,ptr_profil_help->ds_nummer);
            pos_gelaende=Get_Station_pos_allg(pp->x,pp->ds_nr,1);
            pos_station=Get_Station_pos_allg(pp->x,pp->ds_nr,ptr_profil_help->ds_nummer);
            if(num_station_leer<0)
              num_station_leer=0;
            if(((pp->y==BCE_NAN)&&(ptr_profil_help->profiltyp!=AXM &&
              ptr_profil_help->profiltyp!=AYM 
              && ptr_profil_help->profiltyp!=DPM &&
              ptr_profil_help->profiltyp!=RAUHIGKEIT && 
              ptr_profil_help->profiltyp!=RAUHIGKEIT_KST)) 
              ||
              (((num_gelaende < (num_station-num_station_leer)&&pos_station>pos_gelaende)||(num_gelaende !=num_station &&(pp->y==BCE_NAN)&&num_station_leer!=0))
              &&(ptr_profil_help->profiltyp==AXM ||
              ptr_profil_help->profiltyp==AYM ||
              ptr_profil_help->profiltyp==DPM ||
              ptr_profil_help->profiltyp==RAUHIGKEIT || ptr_profil_help->profiltyp==RAUHIGKEIT_KST))
              )
              DeleteKoord(pp->ds_nr,ptr_profil_help->ds_nummer,pp->x,pp->y);
            pp = pp_save;//Dick
          }       
        }
        break;
      };
    }
    ptr_profil_help = ptr_profil_help->next;
   }
   
}
/************************************************************/
Koord* List::HoleDatensatz(int typ) // identisch mit list_dll.cpp
// im Profil Datensatz vom Typ:'typ' zurückgeben
// Rückgabe: NULL  := fehler
//           sonst : der Datensatzes
{
  Profildatei* ptr_prof = ptr_anfang;
  
  while( ( ptr_prof ) && ( ptr_prof->profiltyp != typ ) )
    ptr_prof = ptr_prof->next;
  if( ( ptr_prof ) && ( ptr_prof->profiltyp == typ ) )
    return ptr_prof->datensatz;
  else 
    return NULL;
} // HoleDatensatz

Profildatei* List::HoleDatenblock( int typ )
// gibt den ersten Datenblock eines bestimmten Typs zurück
// Rückgabewert: NULL, kein solcher Datenblock vorhanden
{
  Profildatei* pDB = get_anfang();
  while( pDB != NULL && pDB->profiltyp != typ )
    pDB = pDB->next;
  
  return pDB;
} // HoleDatenblock


/************************************************************/
/**************************************************************/
int List::ConcatDatensatz(WSP_PROFIL_LISTE *pWPL)
{
  char *tmp;
  int *lp1C,*lp2C;
  int *lp1Typ,*lp2Typ;
  Profildatei *ptr_profil1,*ptr_profil2,*ptrTmp,*ptrTmp2;
  int nummer=2,i;//Dick 21.06.99
  
  
  if (pWPL!=NULL)
  {
    tmp = new char[170];
    
    lp1C= pWPL->data->ds_info;
    lp2C= pWPL->PListNext->data->ds_info;
    lp1Typ=pWPL->data->typ;
    lp2Typ=pWPL->PListNext->data->typ;
    
    /*  10.07.97 alte Datensätze nicht mehr löschen bzw. 
    es ist nun möglich mehrere DAtensätze anzulegen
    
      if (DeleteDatensaetze2(pWPL,SOHLHOEHE_2,BOESCHUNG_RECHTS_2))
      {
      int num,c=0;
      if (pWPL->window != NULL)
      {
      num = xvt_list_count_all(pWPL->window);
      if (num>0)
      for (int i=0;i<num;i++)
      {
      xvt_list_get_elt(pWPL->window,i-c,tmp,160);
      if( ( xvt_str_match(tmp,"2.Sohlh*",FALSE))||
      ( xvt_str_match(tmp,"2.Wasserspiegel*",FALSE))||
      ( xvt_str_match(tmp,"2.Böschung li.*",FALSE))||
      ( xvt_str_match(tmp,"2.Böschung re.*",FALSE)))
      {
      xvt_list_rem(pWPL->window,i-c);
      c++;
      }
      }
      }
      }
    */
    ptr_profil1 = pWPL->PList->ptr_anfang;
    while (ptr_profil1->next !=NULL)
      ptr_profil1 = ptr_profil1->next;
    ptrTmp = ptr_profil1;
    
    ptr_profil2=pWPL->PListNext->PList->ptr_anfang;
    ptrTmp2    =ptr_profil2;
    
    while(ptr_profil2!=NULL)
    {
      nummer=2; 
      if ((ptr_profil2->profiltyp==SOHLHOEHE)
        ||(ptr_profil2->profiltyp==WASSERSPIEGEL)
        ||(ptr_profil2->profiltyp==BOESCHUNG_LINKS)
        ||(ptr_profil2->profiltyp==BOESCHUNG_RECHTS))
      {
        MakeNewNode(lp1C[0]+1);
        MakeNewKoord(lp2C[ptr_profil2->ds_nummer]);
        ptrTmp=ptrTmp->next;
        ptrTmp->profiltyp = ptr_profil2->profiltyp;
        CopyDatenL(ptrTmp->datensatz,ptr_profil2->datensatz);
        
        if (pWPL->window != NULL)
        {
          char typname[25];//,res_name[25];
          
          switch (ptr_profil2->profiltyp)
          {
          case SOHLHOEHE:
            for(i=1;i<=lp1C[0];i++)
            {
              if(lp1Typ[i]==SOHLHOEHE_2)
                nummer++;
            }
            //xvt_res_get_str(SOHLHOEHE_2+1000,res_name,sizeof(res_name));
            //strcpy(typname,res_name);
            //       strcpy(typname,"2.Sohlhoehe");
            sprintf(typname,"%d.Sohlhoehe",nummer);
            break;
          case WASSERSPIEGEL:
            for(i=1;i<=lp1C[0];i++)
            {
              if(lp1Typ[i]==WASSERSPIEGEL_2)
                nummer++;
            }
            //strcpy(typname,"2.Wasserspiegel");
            sprintf(typname,"%d.Wasserspiegel",nummer);
            break;
          case BOESCHUNG_LINKS:
            for(i=1;i<=lp1C[0];i++)
            {
              if(lp1Typ[i]==BOESCHUNG_LINKS_2)
                nummer++;
            }
            //strcpy(typname,"2.Böschung li.");
            sprintf(typname,"%d.Böschung li.",nummer);
            break;
          case BOESCHUNG_RECHTS:
            for(i=1;i<=lp1C[0];i++)
            {
              if(lp1Typ[i]==BOESCHUNG_RECHTS_2)
                nummer++;
            }
            //strcpy(typname,"2.Böschung re.");
            sprintf(typname,"%d.Böschung re.",nummer);
            break;
          default:typname[0]='\0';
          };
          xvt_list_add(pWPL->window,-1,(char*)typname);
        }
        
        // ds_info[] + typ[] aktualisieren
        (lp1C[0])++;
        lp1C[lp1C[0]]=lp2C[ptr_profil2->ds_nummer];
        lp1Typ[lp1C[0]]=ptr_profil2->profiltyp+30;
        ptrTmp->ds_nummer = lp1C[0];
        
        // umbenennen
        ptrTmp->profiltyp = ptrTmp->profiltyp+30; //=2.-xxx..Sohlhöhe etc.
        //strcpy(tmp,"2.");
        sprintf(tmp,"%d.",nummer);
        strcat(tmp,ptr_profil2->daten_info[0]);
        strcpy(ptrTmp->daten_info[0],tmp);
        strcpy(ptrTmp->daten_info[1],ptr_profil2->daten_info[1]);
        strcpy(ptrTmp->daten_info[2],ptr_profil2->daten_info[2]);
        
        ptr_profil2=ptr_profil2->next;
      }
      else
        ptr_profil2=ptr_profil2->next;
    }
 }
 else return FALSE;
 delete[] tmp;
 return TRUE;
 
}
/****************************************************************/
/****************************************************************/
int List::CopyDatenL(Koord *ptr1,Koord *ptr2)
{
  while ((ptr1 !=NULL)&&(ptr2 !=NULL))
  {
    ptr1->ds_nr = ptr2->ds_nr;
    ptr1->x = ptr2->x;
    ptr1->y = ptr2->y;
    ptr1->pre_x = ptr2->pre_x;
    ptr1->pre_y = ptr2->pre_y;
    ptr1=ptr1->next_ds;
    ptr2=ptr2->next_ds;
  }
  return TRUE;
}
/******************************************************************************/
/****************Plotterdatenbankroutinen************************/
void List::GetPlotDatenInfo3(int ds)
{
  char *temp;
  char temp_ini[LENLINE+1];
  char *str;
  DWORD dw;
  /*ptr_profil=ptr_anfang;
  while ((ptr_profil->ds_nummer < ds)&&(ptr_profil !=NULL))
  ptr_profil = ptr_profil->next;*/
  if(ptr_profil !=NULL)
  {
    switch (ptr_profil->profiltyp)
    {
    case GELAENDEHOEHE:
      str="GELAENDEHOEHE";
      break;
    case TRENNFLAECHEN:
      str="TRENNFLAECHEN";
      break;
    case RAUHIGKEIT:
      str="RAUHIGKEIT";
      break;
    case RAUHIGKEIT_KST:
      str="RAUHIGKEIT_KST";
      break;
    case DURCHST_BEREICH:
      str="DURCHST_BEREICH";
      break;
    case UK_BRUECKE:
      str="UK-BRUECKE";
      break;
    case OK_BRUECKE:
      str="OK_BRUECKE";
      break;
    case AXM:
      str="AXM"; 
      break;
    case AYM:
      str="AYM";
      break;
    case DPM:
      str="DPM"; 
      break;
    case OK_WEHRS:
      str="OK_WEHRS"; 
      break;
      /* nur LWA*/
    case KREISSEGM:
      str="KREISSEGM";
      break;
    case SVA_WERT:
      str="SVA_WERT";
      break;
    case OK_GELAENDE:
      str="OK_GELAENDE";
      break;
    case KASTEN:
      str="KASTEN"; 
      break;
    case LWA_FELDER:
      str="LWA_FELDER"; 
      break;                             
    case STATION:                 
      str="WSP-HOEHE";              
      break; 
    case STATION_FIX:                 
      str="WSP-MESSUNG";              
      break;    
    case MAUL:
      str="MAUL"; 
      break;
    case EIPROFIL:
      str="EIPROFIL";
      break;
    case KREIS:
      str="KREIS";
      break;
    case ARMCO84:
      str="ARMCO84";
      break;
    case ARMCO71:
      str="ARMCO71";
      break;
    case NWRINNE:
      str="NWRINNE";
      break;         
    case GELAENDE2:
      str = "GELAENDE2";              
      break;
    case FLAECHE:
      str = "FLAECHE";
      break;                        
    case UNKNOWN:
      str="UNKNOWN";
      break;
    case RECHTSWERT:
      str="RECHTSWERT";
      break;
    case HOCHWERT:
      str="HOCHWERT"; 
      break;
    default:
      str="Keine";
      break;
    } 
    dw=GetPrivateProfileString("DXF",str," ",temp_ini,LENLINE,Plot_Ini_Datei);
    if(dw>0)
    {
      //strcpy(temp,"  ");
      //strcat(temp,temp_ini);
      if(temp_ini[0]=='$') 
        temp=&temp_ini[1];
      else
        temp=&temp_ini[0];
      strcpy(ptr_profil->daten_info[2],temp);
    }
     }
     
}



int List::WritePlotDatenInfo3(int ds)
{
  
  char *str;
  char temp[251];
  BOOL dw;
  ptr_profil=ptr_anfang;
  while ((ptr_profil->ds_nummer < ds)&&(ptr_profil !=NULL))
    ptr_profil = ptr_profil->next;
  if(ptr_profil !=NULL)
  {
    switch (ptr_profil->profiltyp)
    {
    case GELAENDEHOEHE:
      str="GELAENDEHOEHE";
      break;
    case TRENNFLAECHEN:
      str="TRENNFLAECHEN";
      break;
    case RAUHIGKEIT:
      str="RAUHIGKEIT";
      break;
    case RAUHIGKEIT_KST:
      str="RAUHIGKEIT_KST";
      break;
    case DURCHST_BEREICH:
      str="DURCHST_BEREICH";
      break;
    case UK_BRUECKE:
      str="UK-BRUECKE";
      break;
    case OK_BRUECKE:
      str="OK_BRUECKE";
      break;
    case AXM:
      str="AXM"; 
      break;
    case AYM:
      str="AYM";
      break;
    case DPM:
      str="DPM"; 
      break;
    case OK_WEHRS:
      str="OK_WEHRS"; 
      break;
      
    case KREISSEGM:
      str="KREISSEGM";
      break;
    case SVA_WERT:
      str="SVA_WERT";
      break;
    case OK_GELAENDE:
      str="OK_GELAENDE";
      break;
    case KASTEN:
      str="KASTEN"; 
      break;
    case LWA_FELDER:
      str="LWA_FELDER"; 
      break;                             
    case STATION:                 
      str="WSP-HOEHE";              
      break;
    case STATION_FIX:                 
      str="WSP-MESSUNG";              
      break;    
    case MAUL:
      str="MAUL"; 
      break;
    case EIPROFIL:
      str="EIPROFIL";
      break;
    case KREIS:
      str="KREIS";
      break;
    case ARMCO84:
      str="ARMCO84";
      break;
    case ARMCO71:
      str="ARMCO71";
      break;
    case NWRINNE:
      str="NWRINNE";
      break;         
    case GELAENDE2:
      str = "GELAENDE2";              
      break;
    case FLAECHE:
      str = "FLAECHE";
      break;                        
    case UNKNOWN:
      str="UNKNOWN";
      break;
    case RECHTSWERT:
      str="RECHTSWERT";
      break;
    case HOCHWERT:
      str="HOCHWERT"; 
      break;
    default:
      str="Keine";
      break;
    }
    strcpy(temp,"$");
    strcat(temp,ptr_profil->daten_info[2]);
    dw=WritePrivateProfileString("DXF",str,temp,Plot_Ini_Datei);
    if(!dw)
    {
      return 0;
    }
     }
     return 1;
}


void List::ChangePlotDatenInfo3(int ds)
{
  char *temp;
  char temp_ini[LENLINE+1];
  char *str;
  DWORD dw;
  ptr_profil=ptr_anfang;
  while ((ptr_profil->ds_nummer < ds)&&(ptr_profil !=NULL))
    ptr_profil = ptr_profil->next;
  if(ptr_profil !=NULL)
  {
    switch (ptr_profil->profiltyp)
    {
    case GELAENDEHOEHE:
      str="GELAENDEHOEHE";
      break;
    case TRENNFLAECHEN:
      str="TRENNFLAECHEN";
      break;
    case RAUHIGKEIT:
      str="RAUHIGKEIT";
      break;
    case RAUHIGKEIT_KST:
      str="RAUHIGKEIT_KST";
      break;
    case DURCHST_BEREICH:
      str="DURCHST_BEREICH";
      break;
    case UK_BRUECKE:
      str="UK-BRUECKE";
      break;
    case OK_BRUECKE:
      str="OK_BRUECKE";
      break;
    case AXM:
      str="AXM"; 
      break;
    case AYM:
      str="AYM";
      break;
    case DPM:
      str="DPM"; 
      break;
    case OK_WEHRS:
      str="OK_WEHRS"; 
      break;
      /* nur LWA*/
    case KREISSEGM:
      str="KREISSEGM";
      break;
    case SVA_WERT:
      str="SVA_WERT";
      break;
    case OK_GELAENDE:
      str="OK_GELAENDE";
      break;
    case KASTEN:
      str="KASTEN"; 
      break;
    case LWA_FELDER:
      str="LWA_FELDER"; 
      break;                             
    case STATION:                 
      str="WSP-HOEHE";              
      break;
    case STATION_FIX:                 
      str="WSP-MESSUNG";              
      break;    
    case MAUL:
      str="MAUL"; 
      break;
    case EIPROFIL:
      str="EIPROFIL";
      break;
    case KREIS:
      str="KREIS";
      break;
    case ARMCO84:
      str="ARMCO84";
      break;
    case ARMCO71:
      str="ARMCO71";
      break;
    case NWRINNE:
      str="NWRINNE";
      break;         
    case GELAENDE2:
      str = "GELAENDE2";              
      break;
    case FLAECHE:
      str = "FLAECHE";
      break;                        
    case UNKNOWN:
      str="UNKNOWN";
      break;
    case RECHTSWERT:
      str="RECHTSWERT";
      break;
    case HOCHWERT:
      str="HOCHWERT"; 
      break;
    default:
      str="Keine";
      break;
    } 
    dw=GetPrivateProfileString("DXF",str," ",temp_ini,LENLINE,Plot_Ini_Datei);
    if(dw>0)
    {
      //strcpy(temp,"  ");
      //strcat(temp,temp_ini);
      if(temp_ini[0]=='$') 
        temp=&temp_ini[1];
      else
        temp=&temp_ini[0];
      strcpy(ptr_profil->daten_info[2],temp);
    }
     }
     
}



double List::Koord_Interpolieren(double x,int pos)
{
  double x_vorher,x_nachher,y_vorher,y_nachher;  
  double result=BCE_NAN;
  pp=ptr_anfang->datensatz;
  
  while(pp->next_ds!=NULL && pp->ds_nr!=pos)
    pp=pp->next_ds;
  
  if(pp->pre_ds!=NULL && pp!=NULL)
  {
    x_vorher=pp->pre_ds->x;
    y_vorher=pp->pre_ds->y;
    x_nachher=pp->x;
    y_nachher=pp->y;
    if(x_vorher != x_nachher && x <= x_nachher && x >= x_vorher)
    {
      if(y_nachher>=y_vorher)
        result=y_nachher-(fabs(x_nachher-x)/fabs(x_nachher-x_vorher))*fabs(y_nachher-y_vorher);
      else
        result=y_nachher+(fabs(x_nachher-x)/fabs(x_nachher-x_vorher))*fabs(y_nachher-y_vorher);
    }
    
  }
  return result;
}

void List::Mehrfeldoptimierung() // identisch mit list_dll.cpp
{
  Koord *gaelende = NULL, *profiltyp = NULL, *verzw = NULL;
  
  bool ist_next = FALSE;
  
  if( ptr_anfang )
  {
    gaelende = ptr_anfang->datensatz;
    profiltyp = HoleDatensatz( PROFILKENNG );
    verzw = HoleDatensatz( VZKENNG );
  }; // if ptr_anfang
  
  while( profiltyp && gaelende && verzw )
  {
    if(profiltyp->pre_ds ==NULL && gaelende->pre_ds==NULL && verzw->pre_ds==NULL )
    {
      gaelende = gaelende->next_ds;
      profiltyp = profiltyp->next_ds;
      verzw = verzw->next_ds;
      continue;
    }
    else
    {
      if( gaelende->pre_ds->x == gaelende->x && verzw->pre_ds->y == verzw->y )
      {
        if( ( profiltyp->pre_ds->y == 2 && ( profiltyp->y == 1 || profiltyp->y == 3 ) ) ||
          ( profiltyp->pre_ds->y == 2 && profiltyp->y == 2 && gaelende->pre_ds->y <= gaelende->y ) ||
          ( ( profiltyp->pre_ds->y == 1 || profiltyp->pre_ds->y == 3) && ( profiltyp->y == 1 || profiltyp->y == 3 ) && gaelende->pre_ds->y <= gaelende->y ) )
        {
          rem_station.nummer = profiltyp->ds_nr;
          rem_station.typ = 1;
          rem_station.x = gaelende->x;
          rem_station.y = gaelende->y;
          rem_station.anzahl = Get_Station_Num( gaelende->x, 1 );
          gaelende = gaelende->next_ds;
          profiltyp = profiltyp->next_ds;
          verzw = verzw->next_ds;
          ist_next = TRUE;
          DeleteStation( &rem_station );
        }
        else
        {
          rem_station.nummer = profiltyp->pre_ds->ds_nr;
          rem_station.typ = 1;
          rem_station.x = gaelende->pre_ds->x;
          rem_station.y = gaelende->pre_ds->y;
          rem_station.anzahl = Get_Station_Num( gaelende->pre_ds->x, 1 );
          DeleteStation( &rem_station );
        }
      }; // if ...->pre_ds->x == ...
    }; // if ...->pres_ds == NULL
    if( ist_next )
      ist_next = FALSE;
    else
    {
      gaelende = gaelende->next_ds;
      profiltyp = profiltyp->next_ds;
      verzw = verzw->next_ds;
    }; // if ist_next
  }; // while gaelaende && verzweig && profiltyp
}; // Mehrfeldoptimierung

int List::GetUGWsp(int nr,double wsp,double *ugx1,double *ugx2)
{
  double x1,x2,y1,y2;
  double wx1,wx2,wy1,wy2;
  double sx1,sy1,sx2,sy2;
  
  Koord *ptr_koord;
  
  BOOL left=FALSE,right=FALSE;
  
  int schnittpkt=0;
  int linienpkt=0;
  typedef struct _schnitt   //Dick 12.01.99
  {
    double x;
    double y;
    int steigung;//1-nach unten 2-nach oben
  }schnitt,*lpschnitt;
  schnitt schnitt_koord[100];
  Koord *durchst_ber;    //Dick 12.01.99
  
  double d_ber_x1,d_ber_x2;//,sx_temp;
  int i=0;
  
  durchst_ber=HoleDatensatz(DURCHST_BEREICH);//Dick 12.01.99
  
  ptr_koord = ptr_anfang->datensatz;
  if (ptr_koord == NULL) return 0;
  
  x1 = ptr_koord->x; // erster Stationswert
  y1 = ptr_koord->y;
  while (ptr_koord->next_ds !=NULL)
    ptr_koord = ptr_koord->next_ds;
  x2 = ptr_koord->x;   // letzter Stationswert
  y2 = ptr_koord->y;
  
  if(durchst_ber!=NULL)
  {
    d_ber_x1=durchst_ber->x;
    d_ber_x2=durchst_ber->next_ds->x;
  }
  else
  {
    d_ber_x1=x1;
    d_ber_x2=x2;
  }
  
  if (wsp >y1)//Dick 9.02.99 = weg  
  {
    wx1=x1; wy1=wsp;
    left = TRUE;
  }
  if (wsp >y2)//Dick 9.02.99 = weg
  {
    wx2=x2; wy2=wsp;
    right=TRUE;
  }
  if(left == TRUE)
  {
    schnitt_koord[linienpkt].x=wx1; 
    schnitt_koord[linienpkt].y=wy1;
    schnitt_koord[linienpkt].steigung=1;
    linienpkt++;
  }
  
  
  /*  --> leibo.cpp 
  int SchnittPunktzweierStrecken(double TOL,double x1,double y1,double x2,double y2,
  double x3,double y3,double x4,double y4,
  double *X,double *Y)
  */
  
  double TOL=1.0e-05; // Toleranz
  double X,Y;
  int letztesteigung=0;
  ptr_koord = ptr_anfang->datensatz;
  if (ptr_koord == NULL) return 0;
  while (ptr_koord->next_ds !=NULL)
  {      // --> Leibo.cpp :main_dll
    int ret=SchnittPunktzweierStrecken(TOL,
      ptr_koord->x,ptr_koord->y,
      ptr_koord->next_ds->x,ptr_koord->next_ds->y,
      ptr_koord->x-1,wsp,
      ptr_koord->next_ds->x+1,wsp,
      &X,&Y);
    if (ret >=10000)
    {
      // Dick 12.01.99   
      schnitt_koord[linienpkt].x=X; 
      schnitt_koord[linienpkt].y=Y;
      if((ptr_koord->y - ptr_koord->next_ds->y)>=0.0)
        schnitt_koord[linienpkt].steigung=1;
      else
        schnitt_koord[linienpkt].steigung=2;
      // ende
      
      if (schnittpkt==0)
      {sx1 = X;
      sy1 = Y;}
      if (schnittpkt>=1)
      {sx2 = X;
      sy2 = Y;}
      schnittpkt++;
      if(ret!=10001 || 
        (linienpkt==0 && schnitt_koord[linienpkt].steigung!=2) || //Dick 24.08.99
        (schnitt_koord[linienpkt].steigung !=letztesteigung && letztesteigung!=0) )
      {
        letztesteigung=schnitt_koord[linienpkt].steigung;
        linienpkt++;
      }
    }
    ptr_koord = ptr_koord->next_ds;
  }
  
  if (schnittpkt==0) //Dick 25.01.99 von oben verschoben
  { //Wasserspiegel geht über volle Geländebreite
    
    *ugx1=*ugx2=BCE_NAN;
    return 1;
  }
  
  if(right == TRUE)
  {
    schnitt_koord[linienpkt].x=wx2; 
    schnitt_koord[linienpkt].y=wy2;
    schnitt_koord[linienpkt].steigung=2;
    linienpkt++;
  }
  
  
  int linienanz=0;
  for(i=0;i<linienpkt;i+=2)
  {
    if((schnitt_koord[i].x >= d_ber_x1 && schnitt_koord[i].x < d_ber_x2) || 
      (schnitt_koord[i+1].x <= d_ber_x2 && schnitt_koord[i+1].x > d_ber_x1) ||
      (schnitt_koord[i].x < d_ber_x1 && schnitt_koord[i+1].x > d_ber_x2))
    {              
      linienanz++;
      
      *ugx2=schnitt_koord[i+1].x;
      
      *ugx1 = schnitt_koord[i].x;
      
    }
  }
  if(right == TRUE)
  {
    *ugx2=-BCE_NAN;
  }
  if(left == TRUE)
  {
    *ugx1=BCE_NAN;
  }
  
  
  return 1;
}

int List::GetInterpKoord(double x,double *y,int datensatz)
{
  double x_vorher,x_nachher,y_vorher,y_nachher;  
  
  ptr_profil=ptr_anfang;
  int result=0;
  while ((ptr_profil->profiltyp != datensatz)&&(ptr_profil !=NULL))
    ptr_profil = ptr_profil->next;
  if(ptr_profil !=NULL)
  {
    pp=ptr_profil->datensatz;
    if(pp==NULL)
      return 0;
    while(pp->next_ds!=NULL && pp->x < x)
      pp=pp->next_ds;
    
    if(pp->pre_ds!=NULL && pp->next_ds!=NULL)
    {
      x_vorher=pp->pre_ds->x;
      y_vorher=pp->pre_ds->y;
      x_nachher=pp->x;
      y_nachher=pp->y;
      if(x_vorher != x_nachher && x <= x_nachher && x >= x_vorher)
      {
        if(y_nachher>=y_vorher)
          *y=y_nachher-(fabs(x_nachher-x)/fabs(x_nachher-x_vorher))*fabs(y_nachher-y_vorher);
        else
          *y=y_nachher+(fabs(x_nachher-x)/fabs(x_nachher-x_vorher))*fabs(y_nachher-y_vorher);
      }
      else
        *y=x_vorher;
      
    }
    else if(pp->pre_ds!=NULL || pp->next_ds!=NULL)
    {
      *y=pp->y;           
    }
    else
      return 0;
    result=1;
  }
  return result;
}
/***********************************************************************/
int List::BuildBauwerk(int anzahl)
{
  Profildatei *profil;
  Koord *dkuk,*dkok,*gelaende,*gel_tmp;
  int i,j,lines=0,dkuk_ds_num=0,grenze=0;
  char *tmp;
  double daten[3]={0.0,0.0,0.0};
  
  if (anzahl==0) return 0;
  
  dkok=dkuk=NULL;
  tmp = new char[100];
  
  profil = ptr_anfang;
  while (profil!=NULL)
  {
    if (profil->profiltyp == SOHLHOEHE) gelaende = gel_tmp = profil->datensatz;
    if (profil->profiltyp == DKUK)
    {
      dkuk     = profil->datensatz;
      dkuk_ds_num=profil->ds_nummer;
    }
    if (profil->profiltyp == DKOK)      dkok     = profil->datensatz;
    profil = profil->next;
  }
  
  // 1.Zeile füllen
  sprintf(tmp,"1  %i",anzahl);// 1 = Nummer des Datensatzes,auf die sich die Symbole beziehen
  
  
  if (dummy_typ==0)
  {
    AddDummySList(tmp);
    dummy_typ=BAUWERK;
    lines++;
  }
  else
  {  // Slist_dummy wird schon verwendet
    delete[] tmp;
    return 0;
  }
  
  // Folgezeilen in Slist lp->slist_bauwerk sichern
  for (j=0;j<anzahl;j++)
  {
    if ((dkuk!=NULL)&&(dkok!=NULL))
    {
      gel_tmp = gelaende;
      i=1;
      grenze=Get_Station_pos_num(dkuk->x,
        Get_Station_pos_allg(dkuk->x,dkuk->ds_nr,dkuk_ds_num),1/*SOHLHOEHE*/);//Dick 11.02.2000 Gleiche Stationen richtig numerieren
      // !=profilart->ds_nr
      while ((gel_tmp !=NULL)&&(gel_tmp->x < dkuk->x || ((gel_tmp->x == dkuk->x)&&(gel_tmp->ds_nr<grenze)) ))
      {
        gel_tmp = gel_tmp->next_ds;
        i++;
      }
      //gelaende zeigt jetzt auf gleichen Stationswert wie DKUK
      daten[0]= fabs(dkok->y - dkuk->y);
      daten[1]= 0.0;  // Breite
      //  daten[2]= -(dkuk->x - gel_tmp->x);
      if(gel_tmp!=NULL)  //16.3.2001
        daten[2]= -(dkuk->y - gel_tmp->y);
      sprintf(tmp,"%i %lf %lf %lf",i,daten[0],daten[1],daten[2]);
      AddDummySList(tmp);
      lines++;
    }
    dkuk=dkuk->next_ds; //neu
    dkok=dkok->next_ds; //neu
  } //for
  delete[] tmp;
  return lines;
}
/*****************************************************/
//Bley 20.10.2000 Anfang
void List::spiegelbild(void)
{
  ptr_anfang = get_anfang();
  ptr_profil = ptr_anfang;
  
  while (ptr_profil!=NULL)
  {
    if(ptr_profil!=NULL)
      pp=ptr_profil->datensatz;
    else
      pp=NULL;
    
    while (pp!=NULL)
    {
      if(pp!=NULL)
        pp->x=pp->x*(-1);
      pp=pp->next_ds;
    }
    
    ptr_profil=ptr_profil->next;
  }
}

/****************Bley 9.11.2000***********************************/
int  List::ListSucheStation(double old_station,double new_station, int anzahl_koord, int anzahl_straenge)
{
  
  Koord *ptr_koord;
  Profildatei *pd;
  int ok=0, i_stat;
  BOOLEAN gefunden=FALSE;
  double abstand;
  
  pd = ptr_anfang;
  
  while(pd!=NULL)
  {
    ptr_koord = pd->datensatz;
    
    
    abstand=abstand*-1000;
    i_stat=anzahl_straenge-anzahl_koord+2;
    for (ok=1;ok<i_stat; ok++)
    {
      if(ptr_koord->next_ds!=NULL)  
        ptr_koord=ptr_koord->next_ds;
    }
    
    if((ptr_koord!=NULL)&&(ptr_koord->pre_ds!=NULL))
      ptr_koord->pre_ds->x=ptr_koord->x+abstand;
    
    abstand=abstand/-1000;
    pd=pd->next;
  }
  return 0;
}
/***************************************************************/
void List::LeseWerteInPtr(FILE* start_file,int anzahl_punkte)
{
  ptr_profil=ptr_anfang;
  pp=ptr_profil->datensatz;
  
  for(int i=1; i<=anzahl_punkte;i++)
  {
    if(!feof(start_file))
    {
      fscanf(start_file,"%lf",&pp->x);
      fscanf(start_file,"%lf",&pp->y);
      pp=pp->next_ds;
    }
  }
}  

/***************************************************************/
int List::Errechne_Buhnen_Neigung(int ds_nummer, int koordinate, buhne *bu)
{
  //von Bruecke Anfang
  double neigungr, neigungv, differenzr, differenzv, hight, xabstand;
  int k=0, werte=0, welche_buhne, welcher_dp_in_buhne;
  
  //koordinate ist Datenpunkt in ptr
  //welcher dp in buhne=1,2oder 3
  
  bu->hoehe_links[0]='\0';
  bu->neig_links_rueck[0]='\0';
  bu->neig_links_vorne[0]='\0';
  
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&& (ptr_profil->ds_nummer<ds_nummer))
  {
    ptr_profil = ptr_profil->next;
  }
  
  pp=ptr_profil->datensatz;
  werte=koordinate*3;
  welche_buhne=1;
  welcher_dp_in_buhne=1;
  
  if (typ[ds_nummer]==BUHNE && koordinate>0)
  {
    if(koordinate!=1)
    {
      welcher_dp_in_buhne=0;
      while(pp!=NULL)
      {
        if(welcher_dp_in_buhne==3)
        {
          welche_buhne++;
          welcher_dp_in_buhne=0;
        }
        
        if(pp->y!=BCE_NAN)
        {
          k++;
          welcher_dp_in_buhne++;
        }
        //         if(k==werte-1)
        if(k==koordinate)
          break;
        else
        {
          if(pp->next_ds!=NULL)
            pp=pp->next_ds;
        }
        
      }
    }
    for(int ja=1; ja<=welche_buhne; ja++)
      sscanf(ptr_profil->daten_info[1], "%s", bu->lage);
    
    if(welcher_dp_in_buhne==1)
      if(pp!=NULL)
        pp=pp->next_ds;
      if(welcher_dp_in_buhne==3)
        if(pp!=NULL)
          pp=pp->pre_ds;
        if(pp!=NULL &&pp->pre_ds!=NULL)
        {
          differenzr=(pp->pre_ds->y-pp->y);
          xabstand=pp->pre_ds->x-pp->x;
          neigungr=xabstand/differenzr;   
          //    neigungr=((differenzr /pp->y)*100);
          gcvt(neigungr,15,bu->neig_links_rueck);
        }
        if(pp!=NULL &&pp->next_ds!=NULL)
        {
          differenzv=(pp->next_ds->y-pp->y);
          xabstand=(pp->next_ds->x-pp->x);
          neigungv=xabstand/differenzv;
          gcvt(neigungv,15,bu->neig_links_vorne);
        }
        
        if(pp!=NULL)
        {
          hight=pp->y*1;
          char hilfsstring[20];
          gcvt(hight,15,hilfsstring);
          strcpy(bu->hoehe_links,hilfsstring);
        }
  }
  
  //von Bruecke Ende
  return 0;
}

/******************************************************************************************/
void List::GaussKruegerInterpolieren( Scroller *pscr )
// interpoliert GaussKrüger koordinaten
// Die Interpolation erfolgt immer zwischen zwei gültigen Werten ( d.h ungleich BCE_NAN bzw. 0.0 )
// stehen am Anfang bzw. Ende ungültige Koordinaten, werden diese aus den beiden 
// folgenden / vorhergehenden interpoliert
{
  // zuerst die beiden Datensätzte holen: Geländehöhe der zu interpolierende DS
  Profildatei* db_gelaende = ptr_anfang;
  Profildatei* db_gauss = ptr_anfang;
  while( db_gauss && db_gauss->ds_nummer < pscr->datensatz )
    db_gauss = db_gauss->next;
  
  if( !db_gauss )
    return; // falls der Datenblock nicht gefunden wurde, zurück
  
  // jetzt zuerst sichern, dass db_gauss mindestens soviele Koordinaten hat wie db_gelaende
  Koord* pp_gelaende = db_gelaende->datensatz;
  while( pp_gelaende )
  {
    // suchen, ob diese Gauss-Krüger Koordinate vorhanden ist
    int count = 0;
    bool bFound = false;
    Koord* pp_gauss = db_gauss->datensatz;
    while( pp_gauss )
    {
      if( pp_gelaende->x > pp_gauss->x )
        count++;
      
      if( fabs( pp_gelaende->x - pp_gauss->x ) < 0.00001 )
      {
        bFound = true;
        break;
      }; // if pp_gelaende->x == pp_gauss->x
      
      pp_gauss = pp_gauss->next_ds;
    }; // while pp_gaus
    
    // falls die Koordinate nicht gefunden wurde, an der richtigen Stelle einfügen
    if( !bFound )
      Koord_Einf( pp_gelaende->x, BCE_NAN, count + 1, db_gauss->profiltyp );
    
    pp_gelaende = pp_gelaende->next_ds;
  }; // while pp_gelaende
  
  
  // jetzt, interpolieren
  Koord* pp_gauss = db_gauss->datensatz;
  Koord* pp_first = pp_gauss;
  // die erste gültige Koordinate suchen
  while( pp_first && ( pp_first->y == BCE_NAN || fabs( pp_first->y ) < 0.00001 ) )
    pp_first = pp_first->next_ds;
  
  TCHAR message[1000];
  xvt_res_get_str( STR_GAUSS_INTERPOL_ERROR, message, sizeof(message) );
  
  if( !pp_first )
  {
    xvt_dm_post_note( message );
    return;
  };
  
  bool bInterpol = false; // Flag, ob mindestens einmal interpoliert wurde
  double faktor; // global, weil damit noch der Rest interpoliert wird
  // jetzt stets von pp_gaus bis zur ersten gültigen Koordinate nach pp_first interpolieren
  while( pp_gauss )
  {
    Koord* pp_second = pp_first->next_ds;
    while( pp_second && ( pp_second->y == BCE_NAN || fabs( pp_second->y ) < 0.00001 ) )
      pp_second = pp_second->next_ds;
    
    if( pp_second )
      faktor = ( pp_second->y - pp_first->y ) / ( pp_second->x - pp_first->x );
    else if( !bInterpol )
    {
      xvt_dm_post_note( message );
      return;
    };
    
    bInterpol = true;
    
    // hier wird endlich interpoliert, auch wenn p_second gleich NULL, wird zumindest noch bis zum 
    // Schluss mit dem letzten Faktor interpoliert
    while( pp_gauss != pp_second )
    {
      pp_gauss->y = pp_first->y + ( pp_gauss->x - pp_first->x ) * faktor;  
      
      pp_gauss = pp_gauss->next_ds;
    }; // while pp_gaus != pp_second
    
    // jetzt pp_first, pp_gauss und pp_second neu setzen
    pp_gauss = pp_first = pp_second; // die nächste interpolation erfolgt direkt von pp_second aus
  }; // while pp_gelaende
  
}; // GraussKruegerInterpolieren

//////////////////////////////////////////////
// aus der List_dll übernommmene Funktionen //
//////////////////////////////////////////////

void  List::MakeSaveInfoDLL(FILE *out)
{ /*schreiben der 3 Infozeilen auf die ptr_profil zeigt */
  char *str;
  int j,i;
  for (i=0;i<=2;i++)
  {
    str= ptr_profil->daten_info[i];
    if(i==1)//Dick 12.02.99
    {
      char temp[100];
      strcpy(temp," ");
      for(j=0;j<98;j++)
        strcat(temp," ");
      fprintf(out,"%s%s\n",temp,str);
    }
    else      
      fprintf(out,"%s\n",str);
  }
}

void  List::SetPtrAnfang(Profildatei *prf)
{
  ptr_anfang = prf;
}

void List::WSP_Differenz() // identisch zu list.cpp
// berechnet die Wsp-Differenz
// d.h zu jedem Knoten des DB-WspDifferenz werden die entsprechenden Knoten der Datenblöcke
// Wasserpiegel und Wsp-Differenz gesucht und dann die Differenz neu gesetzt
{
  Koord* ptr_wsp = NULL;
  Koord* ptr_wspfix = NULL;
  Koord* ptr_wspdiff = NULL;
  
  // Wasserspiegel holen
  Profildatei* pd = ptr_anfang;
  while( pd  && ( pd->profiltyp != WASSERSPIEGEL ) )
    pd = pd->next;
  if( pd )
    ptr_wsp = pd->datensatz;
  
  // Wasserpiegelfixierung holen
  pd = ptr_anfang;
  while( pd && ( pd->profiltyp != WSP_FIXIERUNG ) )
    pd = pd->next;
  if( pd )
    ptr_wspfix = pd->datensatz;
  
  // Wassserpiegeldifferenz holen
  pd = ptr_anfang;
  while( pd  && ( pd->profiltyp != WSP_DIFFERENZ ) )
    pd = pd->next;
  if( pd )
    ptr_wspdiff = pd->datensatz;
  
  // zu jeder Wasserspiegelfixierung eine differenz ausrechnen
  while( ptr_wspfix )
  {
    Koord* ptr_wsp_tmp = ptr_wsp;
    Koord* ptr_wspdiff_tmp = ptr_wspdiff;

    // entsprechende wsp_diff Koordinate suchen
    while( ptr_wspdiff_tmp && fabs( ptr_wspdiff_tmp->x - ptr_wspfix->x ) > PRECISION_KRD )
      ptr_wspdiff_tmp = ptr_wspdiff_tmp->next_ds;
    
    // entsprechende wsp koordinate suchen
    while( ptr_wsp_tmp && fabs( ptr_wsp_tmp->x - ptr_wspfix->x ) > PRECISION_KRD )
      ptr_wsp_tmp = ptr_wsp_tmp->next_ds;
    
    if( ptr_wsp_tmp && ptr_wspdiff_tmp && ptr_wspfix->y != BCE_NAN && ptr_wsp_tmp->y != BCE_NAN )
      ptr_wspdiff_tmp->y = ptr_wsp_tmp->y - ptr_wspfix->y;
    
    ptr_wspfix = ptr_wspfix->next_ds;
  }; // while ptr_wspfix
}; // WSP_Differenz

int  List::vgl_ds_nr(double station)
{
  Koord *tmp;
  int zaehler;
  int zurueck=0;//Dick 4.02.99
  double help=station*(-1);
  
  ptr_profil=ptr_anfang;
  if(ptr_profil!=NULL)
  {
    tmp=ptr_profil->datensatz;
    zaehler=1;
    
    while(tmp!=NULL)
    {
      //     if(tmp->x==station)
      if(tmp->x==help)
        zurueck=zaehler;
      zaehler++;
      tmp=tmp->next_ds;
    }
  }
  return zurueck;
}

void  List::GetLPlotInfo(MinMax *pMinMax,double *abstand_hoehe,double *abstand_breite)
{ 
  int plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9;
  
  if (ptr_anfang !=NULL)
  {
    ptr_profil=ptr_anfang;
    pp = ptr_profil->datensatz;
    
    sscanf(ptr_profil->daten_info[2],"%d%d%d%d%d%d%d%d%d",
      &plot1,&plot2,&plot3,&plot4,&plot5,&plot6,&plot7,&plot8,&plot9);
    switch(plot5)
    {
    case 1:
    case 3:
      *abstand_hoehe+=15.;
      break;      
    case 2:
    case 4:
      *abstand_hoehe+=7.5;
      break;    
    }
    
    pMinMax->minX = pp->x;
    pMinMax->maxX = pp->x;
    if (pp->y!=BCE_NAN)
    {
      pMinMax->minY = pp->y;
      pMinMax->posMinY = pp->ds_nr;
    }
    pMinMax->maxY = pp->y;
    pMinMax->posMinX = pp->ds_nr;
    pMinMax->posMaxX = pp->ds_nr;
    pMinMax->posMaxY = pp->ds_nr;
    
    pp = pp->next_ds;
    
    
    while (pp != NULL)
    {
      if(pp->x !=BCE_NAN)
      {
        if (pp->x < pMinMax->minX)
        {
          pMinMax->minX = pp->x;
          pMinMax->posMinX = pp->ds_nr;
        }
        if ( pp->x > pMinMax->maxX)
        {
          pMinMax->maxX = pp->x;
          pMinMax->posMaxX = pp->ds_nr;
        }
        if ((pp->y < pMinMax->minY)&&(pp->y !=BCE_NAN))
        {
          pMinMax->minY = pp->y;
          pMinMax->posMinY = pp->ds_nr;
        }
        if ((pp->y > pMinMax->maxY)&&(pp->y !=BCE_NAN))
        {
          pMinMax->maxY = pp->y;
          pMinMax->posMaxY = pp->ds_nr;
        }
      } //- pp->x !=BCE_NAN
      pp = pp->next_ds;
    }  // -while
    
    
    
       /*    Andere Datensätze: nur Geländehöhe durchsuchen
    nicht relevant für Zoomfunktionen                */
    while((ptr_profil!=NULL)&&(ptr_profil->ds_nummer < 2))
      ptr_profil=ptr_profil->next;
    while (ptr_profil!=NULL)
    {
      pp=ptr_profil->datensatz;
      sscanf(ptr_profil->daten_info[2],"%d%d%d%d%d%d%d%d%d",
        &plot1,&plot2,&plot3,&plot4,&plot5,&plot6,&plot7,&plot8,&plot9);
      switch(plot5)
      {
      case 1:
      case 3:
        *abstand_hoehe+=15.;
        break;      
      case 2:
      case 4:
        *abstand_hoehe+=7.5;
        break;    
      }
      if(plot1!=0 && plot9==0)
        while (pp !=NULL)
        {
          if(pp->x !=BCE_NAN)
          {
            if ((pp->y > pMinMax->maxY)&&(pp->y != BCE_NAN))
            {
              pMinMax->maxY = pp->y;
              pMinMax->posMaxY = -1;
            }
            if ((pp->y < pMinMax->minY)&&(pp->y != BCE_NAN))
            {
              pMinMax->minY = pp->y;
              pMinMax->posMinY = -1;
            }
          }
          pp = pp->next_ds;
        }  //-while
        ptr_profil=ptr_profil->next;
    }
    
    /*******************************************/
    pMinMax->distanceX=  pMinMax->maxX - pMinMax->minX;
    pMinMax->distanceY=  pMinMax->maxY - pMinMax->minY;
    
    // wenn alle Stationswerte auf gleicher Höhe Division durch NULL in list.Paint !!!!!!
    if (pMinMax->distanceX==0)
    {
      pMinMax->distanceX=1;
    }
    if (pMinMax->distanceY==0)
    {
      pMinMax->distanceY=2;
      pMinMax->maxY =pMinMax->minY +1;
    }  
 } 
}

int List::ds_check_daten( int* typ,int* ds_info, WSP_SLIST* slist_comment )
//  int ds_info[] updaten und neu ermitteln / finden von BCE_NAN -Werten
//
//  Rückgabewert:
//  
//    change = -1   ->keine Änderung an ds_info vorgenommen
//    change = 0    ->ds_info geändert jedoch kein Wert in ds_info==0
//    change > 0    ->change =der zuletzt ermittelte Datensatz mit Anzahl x-y Tupel==0
{
  int anzahl, change = -1, datensatz = 0, zaehler = 0, resp_stat = -1;
  
  double wert = 0.0;
  char str[25];
  
  if( ptr_anfang )
  {
    ptr_profil = ptr_anfang;
    
    while( ptr_profil )
    {
      anzahl = 0;
      datensatz++;
      if( ( typ[datensatz] < MAUL ) || ( typ[datensatz] == UNKNOWN ) )
        if( typ[datensatz] != RAUHIGKEIT && typ[datensatz] != RAUHIGKEIT_KST &&
          typ[datensatz] != AXM && typ[datensatz] != AYM && typ[datensatz] != DPM )
        {
          pp = ptr_profil->datensatz;
          while( pp )
          {
            if( pp->x != BCE_NAN && pp->y != BCE_NAN )
              anzahl++;
            pp = pp->next_ds;
          }
          if( ds_info[datensatz] > anzahl )
          {
            ds_info[datensatz] = anzahl;
            if( anzahl > 0 )
            {
              if( change < 0 )
                change = 0;
            }
            else //anzahl==0
              change = datensatz;
          };
        }  //-if ((typ[datensatz]!=RAUHIGKEIT).....
        else
        {
          if( typ[datensatz] == COMMENT )
          {
            anzahl = WspSListCountElem( slist_comment );
            ds_info[datensatz] = anzahl;
            change = 0;
          }
          
          if( typ[datensatz] == RAUHIGKEIT || typ[datensatz] == RAUHIGKEIT_KST ||
            typ[datensatz] == AXM || typ[datensatz] == AYM || typ[datensatz] == DPM || typ[datensatz] == FLAECHE )
          { 
            // sorgt, dafür, dass in diesen Datensätzen immer genau die gleichen Koordinaten
            // vorhanden sind wie im Gelände
            if( !GetSonderProfilTyp( &str[0] ) || LWA_PROJEKT ) // kein Sonderprofil enthalten
            { //   für alle Stationswerte fehlende Rauhigkeiten ergänzen
              Koord *help_pp;
              BOOLEAN gefunden;
              
              // zuerst überall 'attr' auf NULL setzen
              pp = ptr_profil->datensatz;
              while( pp != NULL )
              {
                pp->attr=0;
                pp = pp->next_ds;
              }

              // den Geländedatenstz durchgehen, und schauen, ob alle Koordinaten mit Rauhigkeiten ausgestatet sind
              help_pp = ptr_anfang->datensatz; //zeigt auf Gelände
              while( help_pp != NULL )
              {
                pp = ptr_profil->datensatz;
                gefunden = FALSE;
                
                // ungültige Koordinaten überspringen
                while( pp != NULL && pp->x == BCE_NAN )//da Vergleich...siehe unten
                  pp=pp->next_ds;
                
//                while( pp != NULL && pp->x <= help_pp->x )
                while( pp != NULL && !gefunden )
                {
                  if( fabs( pp->x - help_pp->x ) < 0.00001 && !gefunden && !pp->attr )
                  {
                    if( help_pp->y == BCE_NAN )
                      pp->y = BCE_NAN;
                    if( help_pp->y == BCE_NAN && pp->y == BCE_NAN )
                    {
                      ds_info[datensatz]--; //Tupel wird später nicht geschrieben
                      pp->x = BCE_NAN;   //x-Wert mit BCE_NAN wird nicht geschrieben
                    }
                    else
                      pp->attr = 1;
                    gefunden=TRUE;
                  }
                  pp=pp->next_ds;
                }
                if( !gefunden && help_pp->y != BCE_NAN ) //sortiere
                  Sort_In_Ds( help_pp->x, BCE_NAN, datensatz, 1 );
                
                help_pp = help_pp->next_ds;
              } // while help_pp != NULL
              
              if (ds_info[1]==ds_info[datensatz])
              {  //einsetzen von fehlenden KS-Werten...
                pp=ptr_profil->datensatz;
                if (pp->y==BCE_NAN)   //1.Wert !
                {
                  while ((pp->y ==BCE_NAN)&&(pp->next_ds !=NULL))
                  {
                    pp = pp->next_ds;
                    zaehler++;
                  }
                  if ((pp->next_ds ==NULL)&&(pp->y == BCE_NAN))
                    wert=0.0;
                  else
                    wert = pp->y;
                  pp=ptr_profil->datensatz;  //wieder zum Anfang
                  while ((pp->y ==BCE_NAN)&&(pp->next_ds !=NULL))
                  {   //  wert einsetzen
                    pp->y=wert;
                    pp = pp->next_ds;
                  }
                }  //-if (pp->y==BCE_NAN)
                //else
                while (pp != NULL)
                {
                  while ((pp->y !=BCE_NAN)&&(pp->next_ds !=NULL))
                    pp = pp->next_ds;
                  if (pp->y ==BCE_NAN)
                  {
                    wert = pp->pre_ds->y;
                    while ((pp !=NULL)&&(pp->y ==BCE_NAN))
                    {   //  wert einsetzen
                      if (pp->attr==1)
                        pp->y=wert;
                      pp = pp->next_ds;
                    }
                  }
                  if (pp!=NULL)
                    pp=pp->next_ds;
                }
              }  // -if (RAUHIGKEIT...)
              
              //Korrektur von ds_info[]
              pp=ptr_profil->datensatz;
              zaehler =0;
              while (pp !=NULL)
              {
                if ( (pp->x !=BCE_NAN)&&(pp->y!=BCE_NAN) )
                  zaehler++;
                pp = pp->next_ds;
              }
              ds_info[datensatz]=zaehler;
              if (ds_info[datensatz]==0)
                DeleteNode(datensatz, (int*)&ds_info, (int*)&typ); //Datensatz mit Länge 0 löschen
            }
            else // Rauhigkeit && Sonderprofil enthalten
            {
              for (int k=1;k<=ds_info[0];k++)
                if ((typ[k]==KREIS)||(typ[k]==MAUL)||(typ[k]==EIPROFIL)||
                  (typ[k]==ARMCO84)||(typ[k]==ARMCO71)||(typ[k]==KREISSEGM))
                {
                  pp=ptr_profil->datensatz;
                  while (pp !=NULL)
                  {
                    if ( (pp->x !=BCE_NAN)&&(pp->y!=BCE_NAN) )
                      anzahl++;
                    pp = pp->next_ds;
                  }
                  
                  if ((anzahl<=1)&&(ds_info[datensatz] != anzahl ))
                  {  // Korrektur von ds_info[]  !!!!
                    ds_info[datensatz] = anzahl;
                    change=0;
                    if (anzahl==0)
                      DeleteNode(datensatz, (int*)&ds_info, (int*)&typ); //Datensatz mit Länge 0 löschen
                  }
                  
                  if ((anzahl>1)&&(resp_stat==-1))
                    switch (xvt_dm_post_ask("Ja","Nein",NULL,"Profildatei enthält ein Sonderprofil und mehrere Rauheitswerte.Soll nur der erste Rauheitswert gespeichert werden"))
                  {
              case RESP_DEFAULT:
                resp_stat = 1;
                break;
              case RESP_2:
                resp_stat = 0;
                break;
              case RESP_3:
                break;
                  }//-switch
                  if (anzahl>1)
                    switch (resp_stat)
                  {
               case 0:
               /*Die folgenden 3 Programmzeilen sind nur enthalten, weil nach xvt_dm_post_ask
               ptr_profil verbogen war und hiermit sichergestellt wird, daß ptr_profil
                 auf den richtigen Datensatz zeigt       */
                 ptr_profil = ptr_anfang;
                 while ((ptr_profil !=NULL)&& (ptr_profil->ds_nummer<datensatz))
                   ptr_profil = ptr_profil->next;
                 
                 pp=ptr_profil->datensatz;
                 anzahl =0;
                 while (pp !=NULL)
                 {
                   if ( (pp->x !=BCE_NAN)&&(pp->y!=BCE_NAN) )
                     anzahl++;
                   pp = pp->next_ds;
                 }
                 if (ds_info[datensatz] != anzahl )
                 {
                   ds_info[datensatz] = anzahl;
                   if (anzahl>0)
                   {
                     if (change<0)
                       change = 0;
                   }
                   else //anzahl==0
                     change =datensatz;
                 }
                 break;
               case 1:
               /*Die folgenden 3 Programmzeilen sind nur enthalten, weil nach xvt_dm_post_ask
               ptr_profil verbogen war und hiermit sichergestellt wird, daß ptr_profil
                 auf den richtigen Datensatz zeigt       */
                 ptr_profil = ptr_anfang;
                 while ((ptr_profil !=NULL)&& (ptr_profil->ds_nummer<datensatz))
                   ptr_profil = ptr_profil->next;
                 pp=ptr_profil->datensatz;
                 anzahl =0;
                 while (pp !=NULL)
                 {
                   if (anzahl>0)
                   {
                     pp->x =BCE_NAN;
                     pp->y =BCE_NAN;
                   }
                   else
                     if ( (pp->x !=BCE_NAN)&&(pp->y!=BCE_NAN) )
                       anzahl++;
                     pp = pp->next_ds;
                 }
                 ds_info[datensatz]=1;
                 break;
               default:break;
                  } //-switch
                }
            }//-else: Rauhigkeit && Sonderprofil enthalten
        } // if typ == RAUHIGKEIT ...
       } //-else
       if(ptr_profil!=NULL)
         ptr_profil = ptr_profil->next;
     } // while ptr_profil
   } // if ptr_anfang
   return change;
}; // ds_check_daten

double List::GetSohltiefe()
// gibt die Tiefe des tiefsten Punktes der Geländehöhe zurück
// 0.0 bei Fehler
{
  Koord* krd = HoleDatensatz( GELAENDEHOEHE );
  
  double tiefe = 1e36;
  while( krd )
  {
    if( krd->y < tiefe )
      tiefe = krd->y;
    
    krd = krd->next_ds;
  }; // while krd
  
  // falls nichts gefunden wurde, den standardwert 0.0 zurückgeben
  if( tiefe > 1e35 )
    tiefe = 0.0;
  
  return tiefe;
} // GetSohltiefe