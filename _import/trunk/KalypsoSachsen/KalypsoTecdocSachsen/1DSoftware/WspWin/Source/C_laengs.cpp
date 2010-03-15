/**********************************************************************

	 MODUL:  Konvertieren von LWA-LängsProfil --> BCE LängsProfil

	 CLASS   LaengsProfil
				11.09.96 Andresen


**********************************************************************/
#include <windows.h>
#include "xvt.h"

#include "global_types.h"

#include "l_typen.h"  // Längsprofiltypen
#include "typen.h"    // Querprofiltypen
#include "slist.h"
#include "list.h"

#include "c_laengs.h"


LaengsProfil::LaengsProfil()
{
 ptr_profil = ptr_ende = ptr_anfang;
 ptr_anfang->ds_nummer=1;
 ptr_anfang->datensatz->ds_nr=1;
 ptr_anfang->datensatz->pre_x=0;
 ptr_anfang->datensatz->pre_y=0;
}
LaengsProfil::~LaengsProfil(void)
{
}

void LaengsProfil::SetPtrEnde(Profildatei *anfang)
{
 if(anfang==NULL)
     return;
 ptr_ende =anfang;
 while(ptr_ende->next != NULL)
     ptr_ende=ptr_ende->next;
}

/***************************************************/
void LaengsProfil::WriteLPTypDaten(int nummer,int typ,char* info)
{
  char *s1, *s2 ;
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
  case SOHLHOEHE:
		  s1 = "SOHLHOEHE NN+m"; s2=info;//s2="\0";
      ptr_profil->daten_info[2][2]  ='1';
      ptr_profil->daten_info[2][11] ='2';
      ptr_profil->daten_info[2][14] ='1';
      ptr_profil->daten_info[2][26] ='0';
      break;
  case LAENGE:
		  s1="Laenge m";        s2=info;//s2="\0";
      
      ptr_profil->daten_info[2][8]  ='1';
      ptr_profil->daten_info[2][11] ='2';
      ptr_profil->daten_info[2][14] ='1';
      ptr_profil->daten_info[2][26] ='0';
      break;
  case ABFLUSS:
		  s1="Abfluss m**3/s";         s2=info;//s2="\0";
      ptr_profil->daten_info[2][8]  ='1';
      ptr_profil->daten_info[2][11] ='2';
      ptr_profil->daten_info[2][14] ='1';
      ptr_profil->daten_info[2][26] ='0';
      break;
  case WASSERSPIEGEL:
		  s1="Wasserspiegel NN+m";		  s2=info;//s2="\0";
      ptr_profil->daten_info[2][2]  ='2';
      ptr_profil->daten_info[2][8]  ='1';
      ptr_profil->daten_info[2][11] ='2';
      ptr_profil->daten_info[2][14] ='1';
      ptr_profil->daten_info[2][26] ='0';
      break;
  case WSP_BREITE:
		  s1="Wassersp.-Br. m";		  s2=info;//s2="\0";
      ptr_profil->daten_info[2][8]  ='1';
      ptr_profil->daten_info[2][11] ='2';
      ptr_profil->daten_info[2][14] ='1';
      ptr_profil->daten_info[2][26] ='0';
      break;
  case BOESCHUNG_LINKS:
		  s1="Boeschung-li. NN+m";		  s2=info;//s2="\0";
      ptr_profil->daten_info[2][2]  ='8';
      ptr_profil->daten_info[2][8]  ='1';
      ptr_profil->daten_info[2][11] ='2';
      ptr_profil->daten_info[2][14] ='1';
      ptr_profil->daten_info[2][26] ='0';
      break;
  case BOESCHUNG_RECHTS:
		  s1="Boeschung-re. NN+m";		  s2=info;//s2="\0";
      ptr_profil->daten_info[2][2]  ='9';
      ptr_profil->daten_info[2][8]  ='1';
      ptr_profil->daten_info[2][11] ='2';
      ptr_profil->daten_info[2][14] ='1';
      ptr_profil->daten_info[2][26] ='0';
      break;
  case PROFILART:
		  s1="Profilart";		  s2=info;//s2="\0";
      ptr_profil->daten_info[2][26] ='0';
      break;
  case VZKENNG:
		  s1="Verzweigungskennung";		  s2=info;//s2="\0";
      ptr_profil->daten_info[2][26] ='0';
      break;
  case PROFILKENNG:
		  s1="Profilkennung";		  s2=info;//s2="\0";
      ptr_profil->daten_info[2][26] ='0';
      break;
  case DKUK:
		  s1="Deckenunterk.";		  s2=info;//s2="\0";
      ptr_profil->daten_info[2][8]  ='1';
      ptr_profil->daten_info[2][11] ='2';
      ptr_profil->daten_info[2][14] ='1';
      ptr_profil->daten_info[2][26] ='0';
      break;
  case DKOK:
		  s1="Deckenoberk.";		  s2=info;//s2="\0";
      ptr_profil->daten_info[2][8]  ='1';
      ptr_profil->daten_info[2][11] ='2';
      ptr_profil->daten_info[2][14] ='1';
      ptr_profil->daten_info[2][26] ='0';
      break;
  case LP_TEXT:
		  s1="TEXT";		  s2=info;//s2="\0";
      ptr_profil->daten_info[2][25] ='1';
      ptr_profil->daten_info[2][26] ='2';
      break;
      
  case BAUWERK:
		  s1 ="BAUWERK";  	  s2=info;//s2="\0";
      ptr_profil->daten_info[2][26] ='4';
      break;
  case COMMENT:
		  s1="Kommentar:";
      s2="[-]";
      ptr_profil->daten_info[2][26]='0';
      break;
  case WSP_FIXIERUNG:
		  s1="WSP-Fixierung NN+m";		  s2=info;//s2="\0";   //Dick 20.04.99
      ptr_profil->daten_info[2][2]  ='2';
      ptr_profil->daten_info[2][8]  ='1';
      ptr_profil->daten_info[2][11] ='2';
      ptr_profil->daten_info[2][14] ='1';
      ptr_profil->daten_info[2][26] ='0';
      break;
      //Neu Dick 23.09.99 neue Datensätze
  case SCHLEPPSPANNUNG:
		  s1="Schleppspannung N/m²";         s2=info;//s2="\0";
      ptr_profil->daten_info[2][8]  ='1';
      ptr_profil->daten_info[2][11] ='2';
      ptr_profil->daten_info[2][14] ='1';
      ptr_profil->daten_info[2][26] ='0';
      break;
  case AUSUFERUNG_LINKS:
		  s1="Ausuferung-li. NN+m";	s2=info;	 //s2="\0"; 
      ptr_profil->daten_info[2][2]  ='8';
      ptr_profil->daten_info[2][8]  ='1';
      ptr_profil->daten_info[2][11] ='2';
      ptr_profil->daten_info[2][14] ='1';
      ptr_profil->daten_info[2][26] ='0';
      break;
  case AUSUFERUNG_RECHTS:
		  s1="Ausuferung-re. NN+m";	s2=info; //s2="\0";
      ptr_profil->daten_info[2][2]  ='9';
      ptr_profil->daten_info[2][8]  ='1';
      ptr_profil->daten_info[2][11] ='2';
      ptr_profil->daten_info[2][14] ='1';
      ptr_profil->daten_info[2][26] ='0';
      break;
  case ENERGIEHOEHE:
		  s1 = "ENERGIEHOEHE NN+m"; s2=info; //s2="\0";
      ptr_profil->daten_info[2][2]  ='1';
      ptr_profil->daten_info[2][11] ='2';
      ptr_profil->daten_info[2][14] ='1';
      ptr_profil->daten_info[2][26] ='0';
      break;
      //Ende Neu
  case WSP_DIFFERENZ:
		  s1="WSP-Differenz m";
      s2=info;//s2="\0";   //Bley 9.11.2000
      ptr_profil->daten_info[2][2]  ='2';
      ptr_profil->daten_info[2][8]  ='1';
      ptr_profil->daten_info[2][11] ='2';
      ptr_profil->daten_info[2][14] ='1';
      ptr_profil->daten_info[2][26] ='0';
      break;

  case WASSERSPIEGEL_2:
    s1="2.Wasserspiegel NN+m";
    s2="";
    break;

  case UNKNOWN:
  default:
		  s1="unbekannt";
      s2=info;//s2="\0";
      ptr_profil->daten_info[2][26] ='0';
      break;
  }
  strcpy(ptr_profil->daten_info[0],s1);
  strcpy(ptr_profil->daten_info[1],s2);
}; // WriteLPTypDaten


/***************************************************/
void LaengsProfil::SetBceLPTypDaten(int typ,char* info)
{
 char *s1, *s2 ;
 ptr_profil = ptr_anfang;
 while ((ptr_profil !=NULL)&&(ptr_profil->profiltyp != typ))
		  ptr_profil = ptr_profil->next;

 if(ptr_profil ==NULL) return;
     
 for (int i=0;i<150;i++)
		ptr_profil->daten_info[2][i]=' ';
 ptr_profil->daten_info[2][150]='\0';

 for (i=2;i<26;i=i+3)                  // Zeile 3: 8 beliebige Integerzahlen ,
		ptr_profil->daten_info[2][i]='0';  // 9.Ziffer:Datenblockabhängig,bei GELAENDE=0

 ptr_profil->daten_info[0][0]=ptr_profil->daten_info[1][0]='\0';

 switch (typ)
  {
	case SOHLHOEHE:
		  s1 = "SOHLHOEHE NN+m"; s2=info;//s2="\0";
		  ptr_profil->daten_info[2][2]  ='1';
		  ptr_profil->daten_info[2][11] ='2';
		  ptr_profil->daten_info[2][14] ='1';
		  ptr_profil->daten_info[2][26] ='0';
		  break;
	case LAENGE:
		  s1="Laenge m";        s2=info;//s2="\0";

		  ptr_profil->daten_info[2][8]  ='1';
		  ptr_profil->daten_info[2][11] ='2';
		  ptr_profil->daten_info[2][14] ='1';
		  ptr_profil->daten_info[2][26] ='0';
		  break;
	case ABFLUSS:
		  s1="Abfluss m**3/s";         s2=info;//s2="\0";
		  ptr_profil->daten_info[2][8]  ='1';
		  ptr_profil->daten_info[2][11] ='2';
		  ptr_profil->daten_info[2][14] ='1';
		  ptr_profil->daten_info[2][26] ='0';
		  break;
	case WASSERSPIEGEL:
		  s1="Wasserspiegel NN+m";		  s2=info;//s2="\0";
		  ptr_profil->daten_info[2][2]  ='2';
		  ptr_profil->daten_info[2][8]  ='1';
		  ptr_profil->daten_info[2][11] ='2';
		  ptr_profil->daten_info[2][14] ='1';
		  ptr_profil->daten_info[2][26] ='0';
		  break;
	case WSP_BREITE:
		  s1="Wassersp.-Br. m";		  s2=info;//s2="\0";
		  ptr_profil->daten_info[2][8]  ='1';
		  ptr_profil->daten_info[2][11] ='2';
		  ptr_profil->daten_info[2][14] ='1';
		  ptr_profil->daten_info[2][26] ='0';
		  break;
	case BOESCHUNG_LINKS:
		  s1="Boeschung-li. NN+m";		  s2=info;//s2="\0";
		  ptr_profil->daten_info[2][2]  ='8';
		  ptr_profil->daten_info[2][8]  ='1';
		  ptr_profil->daten_info[2][11] ='2';
		  ptr_profil->daten_info[2][14] ='1';
		  ptr_profil->daten_info[2][26] ='0';
		  break;
	case BOESCHUNG_RECHTS:
		  s1="Boeschung-re. NN+m";		  s2=info;//s2="\0";
		  ptr_profil->daten_info[2][2]  ='9';
		  ptr_profil->daten_info[2][8]  ='1';
		  ptr_profil->daten_info[2][11] ='2';
		  ptr_profil->daten_info[2][14] ='1';
		  ptr_profil->daten_info[2][26] ='0';
		  break;
	case PROFILART:
		  s1="Profilart";		  s2=info;//s2="\0";
		  ptr_profil->daten_info[2][26] ='0';
		  break;
	case VZKENNG:
		  s1="Verzweigungskennung";		  s2=info;//s2="\0";
		  ptr_profil->daten_info[2][26] ='0';
		  break;
	case PROFILKENNG:
		  s1="Profilkennung";		  s2=info;//s2="\0";
		  ptr_profil->daten_info[2][26] ='0';
		  break;
	case DKUK:
		  s1="Deckenunterk.";		  s2=info;//s2="\0";
		  ptr_profil->daten_info[2][8]  ='1';
		  ptr_profil->daten_info[2][11] ='2';
		  ptr_profil->daten_info[2][14] ='1';
		  ptr_profil->daten_info[2][26] ='0';
		  break;
	case DKOK:
		  s1="Deckenoberk.";		  s2=info;//s2="\0";
		  ptr_profil->daten_info[2][8]  ='1';
		  ptr_profil->daten_info[2][11] ='2';
		  ptr_profil->daten_info[2][14] ='1';
		  ptr_profil->daten_info[2][26] ='0';
		  break;
	case LP_TEXT:
		  s1="TEXT";		  s2=info;//s2="\0";
		  ptr_profil->daten_info[2][25] ='1';
		  ptr_profil->daten_info[2][26] ='2';
		  break;

	case BAUWERK:
		  s1 ="BAUWERK";  	  s2=info;//s2="\0";
		  ptr_profil->daten_info[2][26] ='4';
		  break;
	case COMMENT:
		  s1="Kommentar:";
		  s2="[-]";
		  ptr_profil->daten_info[2][26]='0';
		  break;
    case WSP_FIXIERUNG:
		  s1="WSP-Fixierung NN+m";		  s2=info;//s2="\0";   //Dick 20.04.99
		  ptr_profil->daten_info[2][2]  ='2';
		  ptr_profil->daten_info[2][8]  ='1';
		  ptr_profil->daten_info[2][11] ='2';
		  ptr_profil->daten_info[2][14] ='1';
		  ptr_profil->daten_info[2][26] ='0';
		  break;
	case UNKNOWN:
		  s1="unbekannt";    s2=info;//s2="\0";
		  ptr_profil->daten_info[2][26] ='0';
		  break;
	default:break;
  }
  strcpy(ptr_profil->daten_info[0],s1);
  strcpy(ptr_profil->daten_info[1],s2);
}


/***************************************************/
int LaengsProfil::CopyKoordToList(int typ,int nummer,double x,double y)
/* 
  Parameter:
        int typ: welcher Datenblock
        int nummer: Nummer der Koodinate?
        double x, y: Koordinaten

*/
{
  Koord *tmp;
  int datensatz;
  
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->profiltyp != typ))
    ptr_profil = ptr_profil->next;
  if(ptr_profil==NULL)//Dick 22.09.99
    return FALSE;
  datensatz = ptr_profil->ds_nummer;
  if (ptr_profil !=NULL)
  {
    tmp= ptr_profil->datensatz;
    if( (tmp) &&
      ((typ==DKOK) || (typ==DKUK)) &&
      (tmp->next_ds==NULL) &&
      ((tmp->x==BCE_NAN) &&(tmp->y==BCE_NAN)) )
    {
      tmp= ptr_profil->datensatz;
      if (tmp)
      {
        while ((tmp->next_ds !=NULL)&&(tmp->ds_nr<=nummer))
          tmp= tmp->next_ds;
        if (tmp)
        {
          tmp->x = x;
          tmp->y = y;
        }
        return TRUE;
      } //-if (tmp)
      else
        return FALSE;
    }
    else
    {
      /***********/
      if (nummer==1)
      {
        tmp= ptr_profil->datensatz;
        if (tmp)
        {
          while ((tmp->next_ds !=NULL)&&(tmp->ds_nr<nummer))//Dick 4.02.99 = weg
            tmp= tmp->next_ds;
          if (tmp)
          {
            tmp->x = x;
            tmp->y = y;
          }
          return TRUE;
        } //-if (tmp)
        else
          return FALSE;
      } //if nummer==1
      /***********/
      if (nummer>1)
      {
        char sort_type[MAX_PATH];                          
        GetPrivateProfileString("WSPWIN","LAENGSVERZWEIGSORT","STATION",sort_type,MAX_PATH,"WSPWIN.INI");
        if(strcmp(sort_type,"STATION")==0)
          Sort_In_Ds(x,y,datensatz,0);
        else
          Koord_Einf(x,y,1,typ);
        return TRUE;
      }
      return TRUE;//nur wg. compiler
    } //else nicht dKOK oder dkuk
  } //-if (ptr_profil !=NULL)
  else
    return FALSE;
}
/***********************************************************************/
int LaengsProfil::BuildDsInfo(WSP_PROFIL_LISTE *wpl)
{
 int nr, ds = 1;

 wpl->data->typ[0] = 0;  //immer
 ptr_profil = ptr_anfang;
 while (ptr_profil )
 {
   if((ptr_profil->profiltyp!=COMMENT) &&
     (ptr_profil->profiltyp!=LP_TEXT) &&
     (ptr_profil->profiltyp!=BAUWERK)) //neu
	 {
	  pp = ptr_profil->datensatz;
	  wpl->data->typ[ds]=ptr_profil->profiltyp;
	  nr =0;
	  while (pp)
		{
		 if((pp->x!=BCE_NAN) &&(pp->y!=BCE_NAN))
		 nr++;
		 pp = pp->next_ds;
		}
	  wpl->data->ds_info[ds]=nr;
	 }

	if(ptr_profil->profiltyp==LP_TEXT)
	  wpl->data->ds_info[ds]=WspSListCountElem(wpl->data->slist_comment);

	ds++;
	ptr_profil = ptr_profil->next;
  }
 wpl->data->ds_info[0]=ds-1;
 return 1;
}
/***********************************************************************/
