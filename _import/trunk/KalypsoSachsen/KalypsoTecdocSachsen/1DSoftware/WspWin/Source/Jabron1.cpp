/**********************************************************************

	 MODUL:  Konvertieren von BCE-Profil --> JABRON Format
   
     allg. Funtionen
     siehe auch:     CLASS  jabron.cpp
**********************************************************************/
#include <windows.h>
#include "xvt.h"

#include "global_types.h"
#include "global_vars.h"

#include "resource.h"

#include "typen.h"

#include "readprof.h"
#include "wsplist.h"
#include "list.h"
#include "jabron.h"
#include "slist.h"

#include "jabron1.h"

#define T_LINKS  1
#define T_RECHTS 2


#define KOMPLETT     11 //= (X_KOO + Y_KOO)

#define MAX_FILES    200


/* Jabron Typen*/
#define SONST        -1   // Datenzeile ist Folgezeile (weitere Zahlen)
#define UNBEKANNT     0   // Datenzeile nicht zu klassifizieren
#define FLUSSNAME     1
#define DIENSTSTELLE  2
#define PROFIL        3
#define KILOMETER     4
#define X_KOO         5
#define Y_KOO         6
#define LINKS         7
#define RECHTS        8
#define SOHLE         9
#define L_BEGRENZT    10
#define R_BEGRENZT    11
#define KSG           12
#define KSLU          13
#define KSL           14
#define KS            15
#define KSOHLE        16
#define KSRU          17
#define KSR           18
#define VERMESSUNG    19
#define GEFAELLE      20
#define FLUSSPUNKT    21
#define GERINNE       22
#define EINLAUFVERLUST 23
#define KL_BEWUCHS     24
#define KR_BEWUCHS     25
#define BORDA         26
#define GESCHLOSSEN   27
#define KENNZAHL      28
#define LAENGE	       29
#define STRASSENHOEHE 30
#define AUSLAUFVERLUST 31


/*************************************************  
Prototypen aus jabron1.cpp:   
******************** *****************************/
char* strblanktab(char *string); // jabron1.cpp

int JabReadJabFile(FILE *in,WSP_PROFIL_LISTE *pWPL,char *gewaesser); // Jabron1.cpp
void JabWriteHeader(FILE *file,char *fluss,char *netzdatei); // Jabron1.cpp
void JabWriteEnd(FILE *file); // Jabron1.cpp
int JabFindGewaesserName(char *filename,char *gewaesser); // Jabron1.cpp
void JabGenProfilName(char *gewaesser,int nr,char * name); // Jabron1.cpp
int JabronOpenFileBox(char *filename); // Jabron1.cpp

int JabGetStringTyp(char *str); //Jabron1.cpp
int JabTestDataLine(char *str); // Jabron1.cpp


/*************************************************************************/

void JabWriteHeader(FILE *file,char *fluss,char *netzdatei)
{
  char *temp,*p;
  char *datum=NULL;
  
  datum = new char[40];
  temp = new char[150];
  
  GetDateString(datum); //-> bce_allg.cpp
  
  fprintf(file,"$*********************************************************************\n");
  fprintf(file,"$*               JABRON-PROFIL-DATEI                                 *\n");
  fprintf(file,"$*                  erzeugt mit                                      *\n");
  fprintf(file,"$*     W S P W I N ,Björnsen Beratende Ingenieure                    *\n");
  
  memset(temp,' ',150);
  strcpy(temp,"$*     Datum:            ");
  strcat(temp,datum);
  p=strrchr(temp,'\0');
  if (p) p[0]=' ';
  temp[69]='*';temp[70]='\n';temp[71]='\0';
  fprintf(file,"%s",temp);
  
  memset(temp,' ',150);
  strcpy(temp,"$*     Vernetzungsdatei: ");
  strcat(temp,netzdatei);
  p=strrchr(temp,'\0');
  if (p) p[0]=' ';
  temp[69]='*';temp[70]='\n';temp[71]='\0';
  fprintf(file,"%s",temp);
  
  fprintf(file,"$*                                                                   *\n");
  fprintf(file,"$*********************************************************************\n");
  fprintf(file,"$\n");
  fprintf(file,"FLUSSNAME %s\n",fluss);
  fprintf(file,"$\n");
  
  delete[] datum;
  delete[] temp;
}

/*************************************************************************/
void JabWriteEnd(FILE *file)
{
  fprintf(file,"$\n");
  fprintf(file,"$---------------------------------------------------------------------\n");
  fprintf(file,"ENDE-PROFIL-DATEI\n");
}
/*************************************************************************/
int JabConvertProfilToJabron(WSP_PROFIL_LISTE *pWPL,STR_DATA *str_data)
{
/*

	 Rückgabe : 0  - Fehler
   1 - OK
   
  */
  
  // als erstes Abfragen ob die Brücken inteproliert werden sollen
  bool bInterpolBruecke = false; // Standardmässig nein

  CharBuffer stringBuf( 1024 );
  CharBuffer jaBuf( 1024 );
  CharBuffer neinBuf( 1024 );
  CharBuffer abbrBuf( 1024 );

  xvt_res_get_str( STR_JABRON_INTERPOL, stringBuf, 1024 );
  xvt_res_get_str( STR_JA, jaBuf, 1024 );
  xvt_res_get_str( STR_NEIN, neinBuf, 1024 );
  xvt_res_get_str( STR_ABBRECHEN, abbrBuf, 1024 );
  switch( xvt_dm_post_ask( jaBuf, neinBuf, abbrBuf, "%s", stringBuf ) )
  {
  case RESP_DEFAULT:
    bInterpolBruecke = true;
    break;

  case RESP_2:
    bInterpolBruecke = false;
    break;

  default:
    return 0;
  };


  int nummer = 0;
  double station;
  char *ptr,
    prof_name[15],
    gewaesser[10];
  
  FILE *jab;
  WSP_PROFIL_LISTE *tmpWPL;
  WSP_SLIST *tmp_slist;
  
  CharBuffer name( 1024 );
  CharBuffer temp( 1024 );
  
  xvt_fsys_convert_dir_to_str(&str_data->file.dir,name,255);
  xvt_fsys_set_dir(&str_data->file.dir);
  
  strcat(name,"\\");

  CharBuffer dirName( 256 );
  strncpy( dirName, name, 256 );

  // die Jabron.ele Datei öffnen
  char* eleName = new char[1000];
  strncpy( eleName, name, 1000 );
  strncat( eleName, "jabron.ele", strlen( "jabron.ele" ) );

  FILE* eleFile = fopen( eleName, "w" );
  delete[] eleName;
  eleName = NULL;

  if( eleFile == NULL )
    return 0;

  strcat(name,"jabron.pro");
  if((jab = fopen(name,"w"))==NULL)
  {
    fclose( eleFile );
    return 0;
  }

  char buf[200],buf2[200];
  xvt_res_get_str(STR_JABRON_DLL_NOTE_5,buf,sizeof(buf));
  xvt_res_get_str(STR_JABRON_DLL_NOTE_6,buf2,sizeof(buf2));
  xvt_dm_post_note("%s%s%s",buf,dirName,buf2);  //xvt_dm_post_note("Konvertierte Daten werden nach:\n%s\ngeschrieben",name);
  
  JabWriteHeader(jab,str_data->str_gewaesser,str_data->file.name);

  tmp_slist=str_data->prof_datei;
  while( tmp_slist !=NULL )
  {
    pWPL=Init_Profil_Liste(pWPL);//neues WPL-Element anhängen
    tmpWPL = pWPL->PListNext;
    
    if(tmp_slist->string!=NULL)
      strcpy(temp,tmp_slist->string);
    if (temp!=NULL)
    {
        // jetzt das Profil in die Jabron.pro schreiben

      sscanf(temp,"%s%lf",gewaesser,&station);
      ptr = &temp[44];  //ab Position 44 steht der Dateiname
      strncpy(prof_name,ptr,12);
      prof_name[12]='\0';
      strcpy(tmpWPL->data->file.name,prof_name);
      xvt_fsys_convert_dir_to_str(&str_data->file.dir,name,255);
      xvt_fsys_convert_str_to_dir(name,&tmpWPL->data->file.dir);
      
      xvt_scr_set_busy_cursor();
      
      if( read_profildatei( tmpWPL, &tmpWPL->data->file.dir, tmpWPL->data->file.name, FALSE ) )
      { //im Fehlerfall gibt read_profildatei TRUE zurück
        Delete_Profil_Liste( tmpWPL );
        pWPL->PListNext=NULL;
        fclose( jab );
        delete[] temp;
        delete[] name;
        //delete jabron;
        return 0; // Fehler
      }

      Jabron* jabron = new Jabron();
      jabron->SetPtrAnfang( tmpWPL->PList->get_anfang() );

      int profCount = jabron->JabWriteJab( jab, nummer, prof_name, tmpWPL->data, station, 0.01, bInterpolBruecke );
      
      // für jedes Profil ein Element und ein Gerinne in die Jabron.ele schreiben
      for( int i = 0; i < profCount; i++ )
      {
        if( nummer > 0 )
        {
          fprintf( eleFile, "ELEMENT %d\n", nummer );
          fprintf( eleFile, "GERINNE %d %d\n\n", nummer, nummer - 1 );
        }; // if nummer > 0
        nummer++;
      }; // for nr

      Delete_Profil_Liste(tmpWPL);
      tmpWPL = NULL;
      pWPL->PListNext=NULL;

      delete jabron;
    }
    tmp_slist = tmp_slist->next;
	 }
  
  JabWriteEnd(jab); //Endevermerk als letzter Eintrag

  // Endekennung in Jabron.ele schreiben
  fprintf( eleFile, "ENDE-ELEMENT-DATEI\n" );
  fclose( eleFile );
  
  
  if( tmpWPL !=NULL )
    Delete_Profil_Liste( tmpWPL );
  tmpWPL = NULL;
  pWPL->PListNext=NULL;

  //delete jabron;
  fclose(jab);
  return 1; // kein Fehler
}
/*************************************************************************
************************************************************************/
// Jabron --> BCE

/*************************************************************************/
/*************************************************************************/
int JabConvertJabronToProfil(WSP_PROFIL_LISTE *pWPL)
/* 	JABRON - Profildatei nach WSPWIN Format konvertieren
*/
{
  char *temp,
    *gewaesser_name;
  FILE_SPEC jab_file;
  FILE *jabron_in;
  // WSP_SLIST *liste=NULL;
  int fehler=0;
  
  temp           = new char[256];
  gewaesser_name = new char[100];
  
  strcpy(jab_file.type,"*.pro");
  jab_file.name[0]='\0';
  
  
  if (!JabronOpenFileBox(temp))
  {
    delete[] gewaesser_name;
    delete[] temp;
    return 0;
  }
  
  if ((!fehler)&&(! JabFindGewaesserName(temp,gewaesser_name)))  //Eintrag "FLUSSName" suchen
  {
    char buf[200]; //Dick 26.11.99
    xvt_res_get_str(STR_JABRON_DLL_NOTE_8,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf); 
    if(!xvt_dm_post_string_prompt(buf,gewaesser_name,10))
      fehler=1;
  }
  
  if (!fehler)
  {
    if ((jabron_in=fopen(temp,"r"))!=NULL)
    {
      
      JabReadJabFile(jabron_in,pWPL,gewaesser_name);
      char buf[200]; //Dick 26.11.99
      xvt_res_get_str(STR_JABRON_DLL_NOTE_9,buf,sizeof(buf));
      xvt_dm_post_note("%s",buf);
      //xvt_dm_post_note("Konvertierung beendet");
      fclose(jabron_in);
    }
    else
    {
      delete[] gewaesser_name;
      delete[] temp;
      return 0;
    }
  }
  
  delete[] gewaesser_name;
  delete[] temp;
  return 1;
}
/*************************************************************************/
int JabReadJabFile(FILE *in,WSP_PROFIL_LISTE *pWPL,char *gewaesser)
{
  char *string,*ptr;
  char *profil_name,
    *temp,
    *loc_gewaesser;
  short inc_ds=0,
    header=0;
  int typ,
    ret,
    sort,
    new_prof,
    last_ds =0,
    anzahl_profile=0,
    datensatz=1,
    l_sohle,
    r_sohle,
    exist_comment=0,
    exist_global_comment=0;
  double station=0.0;
  WSP_PROFIL_LISTE *tmpWPL;
  WSP_SLIST *global_comment;  // globaler Kommentar
  BYTE  gelaendezustand=0,
    profil_typ=0;
  
  string = new char[100];
  profil_name = new char[100];
  loc_gewaesser = new char[81];
  temp = new char[100];
  
  global_comment =NULL;
  new_prof = 0;
  
  pWPL=Init_Profil_Liste(pWPL);//neues WPL-Element anhängen
  tmpWPL = pWPL->PListNext;

  Jabron* jabron = new Jabron();
  
  /* da 'tmpWPL->PList->datensatz' nicht benötigt wird bzw. durch SetPtrAnfang Referenz
	 verloren geht,kann   Koord:'tmpWPL->PList->datensatz' gelöscht werden
  */
  if (tmpWPL->PList!=NULL)
    tmpWPL->PList->DeleteKoord(0);
  
  tmpWPL->PList->SetPtrAnfang(jabron->get_anfang());
  
  for (int i=0;i<15;i++) //  15 Zeilen Header anlegen
    tmpWPL->data->slist_header=AppendStringToWspSList(tmpWPL->data->slist_header," ");
  
  
  JabGenProfilName(gewaesser,anzahl_profile,profil_name);
  
  xvt_scr_set_busy_cursor();
  
  
  if (in !=NULL)
  {
    while (!feof(in))
    {
      //	  Yield;
      fgets(string,99,in);
      ptr = strrchr(string,'\n'); //Zeilentrenner wegwerfen
      if (ptr)	ptr[0]='\0';
      if ((string[0]=='$')&&(!header))  // Kommentar wird global gesichert,
      {											//falls er vor dem ersten Profil kommt,in Jabron: 1.Zeichen ='$'
        //und in allen Profilen abgelegt
        global_comment = AppendStringToWspSList(global_comment,string);
        exist_global_comment=1;
      }
      else // Zeile kein Kommentar
      {
        header=1;
        typ = JabGetStringTyp(string);
        
        if (typ == SONST)  // = Folgezeile
          typ = last_ds;
        
        if ((typ!=last_ds)&&(gelaendezustand ==KOMPLETT)&&(profil_typ==GESCHLOSSEN))
        { // bei Geschlossenem Profil  können x-,y-Werte in unsortierter Reihenfolge vorliegen
          // daher Kontrolle und umkopieren in UK_BRUECKE
          ret  = jabron->ExistDatensatzTyp(GELAENDEHOEHE);
          sort = jabron->check_sort();
          if (sort == ret)  //Datensatz GELAENDE ist unsortiert
            if( jabron->DoGeschlProfil(datensatz))
            {
              tmpWPL->data->typ[datensatz]= UK_BRUECKE;
              datensatz++;
            }
            gelaendezustand =0;
        }
        
        
        switch (typ)
        {
        case FLUSSNAME:
          ptr = strblanktab(string);//erstes Blank oder Tab finden
          if (!ptr)
          { last_ds=UNBEKANNT; break; }
          
          strcpy (loc_gewaesser,ptr);
          ChangeStringInWspSList(tmpWPL->data->slist_header,loc_gewaesser,6);
          break;
        case DIENSTSTELLE  :
          ChangeStringInWspSList(tmpWPL->data->slist_header,string,2);
          // als Kommentar anhängen
          tmpWPL->data->slist_comment = AppendStringToWspSList(tmpWPL->data->slist_comment,string);
          last_ds=UNBEKANNT;
          exist_comment=1;
          break;
        case PROFIL:
          if (anzahl_profile)
          { // letztes Profil sichern und neues anlegen
            jabron->BuildDsInfo(tmpWPL);
            strcpy(tmpWPL->data->file.name,profil_name);
            tmpWPL->data->file.dir = STR_SPEC.dir;
            //						xvt_fsys_convert_str_to_dir("c:\test\prof",&tmpWPL->data->file.dir);
            save_profildatei( tmpWPL, FALSE );
            
            jabron->DeleteList();
            delete jabron;
            jabron = new Jabron();
            tmpWPL->PList->SetPtrAnfang(jabron->get_anfang());
            
            JabGenProfilName(gewaesser,anzahl_profile,profil_name);
            
            DeleteWspSList(tmpWPL->data->slist_header);
            tmpWPL->data->slist_header=NULL;
            
            DeleteWspSList(tmpWPL->data->slist_comment);
            tmpWPL->data->slist_comment=NULL;
            
            for (int i=0;i<15;i++) //  15 Zeilen Header anlegen
              tmpWPL->data->slist_header=AppendStringToWspSList(tmpWPL->data->slist_header," ");
            datensatz =1;
          }
          
          strcpy(temp,"JABRON-");
          strcat(temp,string);
          ChangeStringInWspSList(tmpWPL->data->slist_header,temp,1);
          profil_typ=0;
          gelaendezustand =0;
          anzahl_profile++;
          break;
        case KILOMETER:
          ptr = strblanktab(string);//erstes Blank oder Tab finden
          if (!ptr)
          { last_ds=UNBEKANNT; break; }
          strcpy(temp,"STATION KM ");
          strcat(temp,ptr);
          ChangeStringInWspSList(tmpWPL->data->slist_header,temp,8);
          station =atof(ptr);
          break;
        case X_KOO:
          ptr = strblanktab(string);//erstes Blank oder Tab finden
          if (!ptr)
          { last_ds=UNBEKANNT; break; }
          
          ret = jabron->ExistDatensatzTyp(GELAENDEHOEHE);
          
          if (!ret)
          {
            if (datensatz>1)
            {
              jabron->MakeNewNode(datensatz);
            }
            inc_ds=1;
            tmpWPL->data->typ[datensatz]= GELAENDEHOEHE;
            jabron->WriteJabXKoord(ptr,datensatz);
            jabron->WriteTypDaten(datensatz,GELAENDEHOEHE,NULL);
          }
          else
            jabron->WriteJabXKoord(ptr,ret);
          gelaendezustand=gelaendezustand+X_KOO;
          last_ds=X_KOO;
          break;
        case Y_KOO:
          ptr = strblanktab(string);//erstes Blank oder Tab finden
          if (!ptr)
          { last_ds=UNBEKANNT; break; }
          
          ret = jabron->ExistDatensatzTyp(GELAENDEHOEHE);
          if (!ret)
          {
            if (datensatz>1)
            {
              jabron->MakeNewNode(datensatz);
            }
            inc_ds=1;
            tmpWPL->data->typ[datensatz]= GELAENDEHOEHE;
            jabron->WriteJabYKoord(ptr,datensatz);
            jabron->WriteTypDaten(datensatz,GELAENDEHOEHE,NULL);
          }
          else
            jabron->WriteJabYKoord(ptr,ret);
          gelaendezustand=gelaendezustand+Y_KOO;
          last_ds=Y_KOO;
          break;
        case LINKS:
          ptr = strblanktab(string);//erstes Blank oder Tab finden
          if (!ptr)
          { last_ds=UNBEKANNT; break; }
          ret = jabron->ExistDatensatzTyp(TRENNFLAECHEN);
          if (!ret)
          {
            if (datensatz>1)
            {
              jabron->MakeNewNode(datensatz);
              jabron->MakeNewKoord(1);
              jabron->WriteTypDaten(datensatz,TRENNFLAECHEN,NULL);
            }
            inc_ds=1;
            tmpWPL->data->typ[datensatz]= TRENNFLAECHEN;
            jabron->WriteTrennflaechen(ptr,datensatz,T_LINKS);
          }
          else
            jabron->WriteTrennflaechen(ptr,ret,T_LINKS);
          last_ds=LINKS;
          break;
        case RECHTS:
          ptr = strblanktab(string);//erstes Blank oder Tab finden
          if (!ptr)
          { last_ds=UNBEKANNT; break; }
          ret = jabron->ExistDatensatzTyp(TRENNFLAECHEN);
          if (!ret)
          {
            if (datensatz>1)
            {
              jabron->MakeNewNode(datensatz);
              jabron->MakeNewKoord(1);
              jabron->WriteTypDaten(datensatz,TRENNFLAECHEN,NULL);
            }
            inc_ds=1;
            tmpWPL->data->typ[datensatz]= TRENNFLAECHEN;
            jabron->WriteTrennflaechen(ptr,datensatz,T_RECHTS);
          }
          else
            jabron->WriteTrennflaechen(ptr,ret,T_RECHTS);
          last_ds=RECHTS;
          break;
        case SOHLE :  //-> als Kommentar sichern
          ptr = strblanktab(string);//erstes Blank oder Tab finden
          if (! (sscanf(ptr,"%i %i",&l_sohle,&r_sohle))) //Werte temp. sichern
            l_sohle=r_sohle=-1;
          
          tmpWPL->data->slist_comment =	AppendStringToWspSList(tmpWPL->data->slist_comment,string);
          last_ds=SOHLE;
          exist_comment=1;
          break;
        case L_BEGRENZT :
          ptr = strblanktab(string);
          if (!ptr)
          { last_ds=UNBEKANNT; break; }
          ret = jabron->ExistDatensatzTyp(DURCHST_BEREICH) ;
          if (!ret)
          {
            if (datensatz>1)
            {
              jabron->MakeNewNode(datensatz);
              jabron->MakeNewKoord(1);
              jabron->WriteTypDaten(datensatz,DURCHST_BEREICH,NULL);
            }
            inc_ds=1;
            tmpWPL->data->typ[datensatz]= DURCHST_BEREICH;
            jabron->WriteDurchstBer(ptr,datensatz,T_LINKS);
          }
          else
            jabron->WriteDurchstBer(ptr,ret,T_LINKS);
          last_ds=L_BEGRENZT;
          break;
        case R_BEGRENZT :
          ptr = strblanktab(string);
          if (!ptr)
          { last_ds=UNBEKANNT; break; }
          ret = jabron->ExistDatensatzTyp(DURCHST_BEREICH) ;
          if (!ret)
          {
            if (datensatz>1)
            {
              jabron->MakeNewNode(datensatz);
              jabron->MakeNewKoord(1);
              jabron->WriteTypDaten(datensatz,DURCHST_BEREICH,NULL);
            }
            inc_ds=1;
            tmpWPL->data->typ[datensatz]= DURCHST_BEREICH;
            jabron->WriteDurchstBer(ptr,datensatz,T_RECHTS);
          }
          else
            jabron->WriteDurchstBer(ptr,ret,T_RECHTS);
          last_ds=R_BEGRENZT;
          break;
        case KSG :
          ptr = strblanktab(string);
          if (!ptr)
          { last_ds=UNBEKANNT; break; }
          ret = jabron->ExistDatensatzTyp(RAUHIGKEIT);
          
          if (!ret)
          {
            if (datensatz>1)
            {
              jabron->MakeNewNode(datensatz);
            }
            inc_ds=1;
            tmpWPL->data->typ[datensatz]= RAUHIGKEIT;
            jabron->WriteJabKSG(ptr,datensatz);
            jabron->WriteTypDaten(datensatz,RAUHIGKEIT,NULL);
          }
          last_ds=KSG;
          break;
        case KSLU:
          ptr = strblanktab(string);
          if (!ptr)
          { last_ds=UNBEKANNT; break; }
          ret = jabron->ExistDatensatzTyp(RAUHIGKEIT);
          
          if (!ret)
          {
            if (datensatz>1)
            {
              int gel = jabron->ExistDatensatzTyp(GELAENDEHOEHE);
              jabron->MakeNewNode(datensatz);
              jabron->MakeNewKoord(tmpWPL->data->ds_info[gel]);
              if (gel)
                jabron->CopyStation(datensatz);
            }
            inc_ds=1;
            tmpWPL->data->typ[datensatz]= RAUHIGKEIT;
            jabron->WriteTypDaten(datensatz,RAUHIGKEIT,NULL);
            jabron->WriteJabKSLU(ptr,datensatz,l_sohle);
          }
          else
            jabron->WriteJabKSLU(ptr,ret,l_sohle);
          
          last_ds=KSLU;
          break;
        case KSL:
          ptr = strblanktab(string);
          if (!ptr)
          { last_ds=UNBEKANNT; break; }
          ret = jabron->ExistDatensatzTyp(RAUHIGKEIT);
          
          if (!ret)
          {
            if (datensatz>1)
            {
              int gel = jabron->ExistDatensatzTyp(GELAENDEHOEHE);
              jabron->MakeNewNode(datensatz);
              jabron->MakeNewKoord(tmpWPL->data->ds_info[gel]);
              if (gel)
                jabron->CopyStation(datensatz);
            }
            inc_ds=1;
            tmpWPL->data->typ[datensatz]= RAUHIGKEIT;
            jabron->WriteTypDaten(datensatz,RAUHIGKEIT,NULL);
            jabron->WriteJabKSL(ptr,datensatz);
          }
          else
            jabron->WriteJabKSL(ptr,ret);
          last_ds=KSL;
          break;
        case KS:
          ptr = strblanktab(string);
          if (!ptr)
          { last_ds=UNBEKANNT; break; }
          ret = jabron->ExistDatensatzTyp(RAUHIGKEIT);
          
          if (!ret)
          {
            if (datensatz>1)
            {
              jabron->MakeNewNode(datensatz);
            }
            inc_ds=1;
            tmpWPL->data->typ[datensatz]= RAUHIGKEIT;
            jabron->WriteJabKS(ptr,datensatz,1);
            jabron->WriteTypDaten(datensatz,RAUHIGKEIT,NULL);
          }
          else 
            jabron->WriteJabKS(ptr,ret);
          last_ds=KS;
          break;
        case KSOHLE:
          ptr = strblanktab(string);
          if (!ptr)
          { last_ds=UNBEKANNT; break; }
          ret = jabron->ExistDatensatzTyp(RAUHIGKEIT);
          
          if (!ret)
          {
            if (datensatz>1)
            {
              int gel = jabron->ExistDatensatzTyp(GELAENDEHOEHE);
              jabron->MakeNewNode(datensatz);
              jabron->MakeNewKoord(tmpWPL->data->ds_info[gel]);
              if (gel)
                jabron->CopyStation(datensatz);
            }
            inc_ds=1;
            tmpWPL->data->typ[datensatz]= RAUHIGKEIT;
            jabron->WriteTypDaten(datensatz,RAUHIGKEIT,NULL);
            jabron->WriteJabKS_Sohle(ptr,datensatz,l_sohle,r_sohle);
          }
          else
            jabron->WriteJabKS_Sohle(ptr,ret,l_sohle,r_sohle);
          last_ds=KSOHLE;
          break;
        case KSRU          :
          ptr = strblanktab(string);
          if (!ptr)
          { last_ds=UNBEKANNT; break; }
          ret = jabron->ExistDatensatzTyp(RAUHIGKEIT);
          
          if (!ret)
          {
            if (datensatz>1)
            {
              int gel = jabron->ExistDatensatzTyp(GELAENDEHOEHE);
              jabron->MakeNewNode(datensatz);
              jabron->MakeNewKoord(tmpWPL->data->ds_info[gel]);
              if (gel)
                jabron->CopyStation(datensatz);
            }
            inc_ds=1;
            tmpWPL->data->typ[datensatz]= RAUHIGKEIT;
            jabron->WriteTypDaten(datensatz,RAUHIGKEIT,NULL);
            jabron->WriteJabKSRU(ptr,datensatz,r_sohle);
          }
          else
            jabron->WriteJabKSRU(ptr,ret,r_sohle);
          last_ds=KSRU;
          break;
        case KSR:
          ptr = strblanktab(string);
          if (!ptr)
          { last_ds=UNBEKANNT; break; }
          ret = jabron->ExistDatensatzTyp(RAUHIGKEIT);
          
          if (!ret)
          {
            if (datensatz>1)
            {
              int gel = jabron->ExistDatensatzTyp(GELAENDEHOEHE);
              jabron->MakeNewNode(datensatz);
              jabron->MakeNewKoord(tmpWPL->data->ds_info[gel]);
              if (gel)
                jabron->CopyStation(datensatz);
            }
            inc_ds=1;
            tmpWPL->data->typ[datensatz]= RAUHIGKEIT;
            jabron->WriteTypDaten(datensatz,RAUHIGKEIT,NULL);
            jabron->WriteJabKSR(ptr,datensatz);
          }
          else
            jabron->WriteJabKSR(ptr,ret);
          last_ds=KSR;
          break;
        case GERINNE :
          profil_typ=GERINNE;
          tmpWPL->data->slist_comment = AppendStringToWspSList(tmpWPL->data->slist_comment,string);
          exist_comment=1;
          break;
          
        case GESCHLOSSEN:
          profil_typ=GESCHLOSSEN;
          tmpWPL->data->slist_comment = AppendStringToWspSList(tmpWPL->data->slist_comment,string);
          exist_comment=1;
          break;
          
        case VERMESSUNG :
        case GEFAELLE :
        case FLUSSPUNKT:
        case EINLAUFVERLUST:
        case KL_BEWUCHS:
        case KR_BEWUCHS:
        case BORDA:
        case KENNZAHL:
        case LAENGE:
        case STRASSENHOEHE:
        case AUSLAUFVERLUST:
          tmpWPL->data->slist_comment = AppendStringToWspSList(tmpWPL->data->slist_comment,string);
          last_ds=UNBEKANNT;
          exist_comment=1;
          break;
          
        case SONST:
          last_ds=SONST;
          break;
        case UNBEKANNT:
          last_ds=UNBEKANNT;
          break;
        default: break;
       }; //-switch
       if(exist_comment)header=0;
       ret =0;
       if (inc_ds)
       {
         datensatz++;
         inc_ds=FALSE;
       }
     }  //-else
   }
   
   jabron->BuildDsInfo(tmpWPL);
   
   strcpy(tmpWPL->data->file.name,profil_name);
   save_profildatei( tmpWPL, FALSE );
  }
  
  DeleteWspSList(tmpWPL->data->slist_comment);
  tmpWPL->data->slist_comment=NULL;
  
  tmpWPL->PList=NULL;
  Delete_Profil_Liste(tmpWPL);
  pWPL->PListNext =NULL;
  DeleteWspSList(global_comment);
  
  delete[] string;
  delete[] profil_name;
  delete[] loc_gewaesser;
  delete[] temp;
  
  return 0;
}
/*************************************************************************/
int JabGetStringTyp(char *str)
{
/*  Aus 'str' versuchen ein Steuerwort zu ermitteln
Achtung im Steuerwort einer JABRON-Datei sind nur die
		ersten 6 Buchstaben signifikant  */
  if (xvt_str_match(str,"FLUSSN*",FALSE))
    return FLUSSNAME;
  if (xvt_str_match(str,"DIENST*",FALSE))
    return DIENSTSTELLE;
  if (xvt_str_match(str,"PROFIL*",FALSE))
    return PROFIL;
  if (xvt_str_match(str,"KILOME*",FALSE))
    return KILOMETER;
  if (xvt_str_match(str,"X-KOO*",FALSE))
    return X_KOO;
  if (xvt_str_match(str,"Y-KOO*",FALSE))
    return Y_KOO;
  if (xvt_str_match(str,"LINKS*",FALSE))
    return LINKS;
  if (xvt_str_match(str,"RECHTS*",FALSE))
    return RECHTS;
  if (xvt_str_match(str,"SOHLE*",FALSE))
    return SOHLE;
  if (xvt_str_match(str,"L-BEGR*",FALSE))
    return L_BEGRENZT;
  if (xvt_str_match(str,"R-BEGR*",FALSE))
    return R_BEGRENZT;
  if (xvt_str_match(str,"KSG *",FALSE))
    return KSG;
  if (xvt_str_match(str,"KSLU*",FALSE))
    return KSLU;
  if (xvt_str_match(str,"KSL *",FALSE))
    return KSL;
  if (xvt_str_match(str,"KS *",FALSE))
    return KS;
  if (xvt_str_match(str,"KSOHLE*",FALSE))
    return KSOHLE;
  if (xvt_str_match(str,"KSRU*",FALSE))
    return KSRU;
  if (xvt_str_match(str,"KSR*",FALSE))
    return KSR;
  if (xvt_str_match(str,"VERMES*",FALSE))
    return VERMESSUNG;
  if (xvt_str_match(str,"GEFAEL*",FALSE))
    return GEFAELLE;
  if (xvt_str_match(str,"FLUSSP*",FALSE))
    return FLUSSPUNKT;
  if (xvt_str_match(str,"EINLAU*",FALSE))
    return EINLAUFVERLUST;
  if (xvt_str_match(str,"KL-BEW*",FALSE))
    return KL_BEWUCHS;
  if (xvt_str_match(str,"KR-BEW*",FALSE))
    return KR_BEWUCHS;
  if (xvt_str_match(str,"BORDA*",FALSE))
    return BORDA;
  if (xvt_str_match(str,"GERINN*",FALSE))
    return GERINNE;
  if (xvt_str_match(str,"GESCHL*",FALSE))
    return GESCHLOSSEN;
  
  if (xvt_str_match(str,"KENNZA*",FALSE))
    return KENNZAHL;
  if (xvt_str_match(str,"STRASS*",FALSE))
    return STRASSENHOEHE;
  if (xvt_str_match(str,"AUSLAU*",FALSE))
    return AUSLAUFVERLUST;
  if (xvt_str_match(str,"LAENGE*",FALSE))
    return LAENGE;
  
  /* testen, ob String Datenzeile ist*/
  if (JabTestDataLine(str))
    return SONST;
  
  if ( ((str[0]>=65)&&(str[0]<=90))||((str[0]>=97)&&(str[0]<=122)) )
    return UNBEKANNT;// erstes Zeichen = sonst.Buchstabe --> Zeile ignorieren
  return UNBEKANNT;  // default
}
/*************************************************************************/
int JabTestDataLine(char *str)
/* testen, ob String Datenzeile ist, d.h. Daten des vorhergehenden Datenblocks
enthält
getestet wird, ob erstes Zeichen( != BLANK) eine Ziffer, '-','+','.' ist
Rückgabe: 1 = erstes Zeichen ist Ziffer oder "+ - ."
0 = sonst
*/
{
  char *ptr;
  ptr = str;
  
  if (ptr[0]==' ')
    while (ptr[0]==' ')
      ptr++;
    if ( ((ptr[0]>='0')&&(ptr[0]<='9'))||(ptr[0]=='-')||(ptr[0]=='+')||(ptr[0]=='.'  ) )
      return 1;
    else return 0;
}
/*************************************************************************/
int JabFindGewaesserName(char *filename,char *gewaesser)
{ /* Öffnet die Datei:filename und sucht nach dem ersten Eintrag
  "FLUSSName" und gibt diesen in gewaesser zurück
  RETURN :   1   gefunden
  0   nicht gefunden                              */
  
  FILE *in;
  char *string,*ptr;
  int ret=0,found=0;
  string = new char[100];
  
  gewaesser[0]='\0';   //init
  if ((in=fopen(filename,"r"))!=NULL)
  {
    while (!feof(in))
    {
      fgets(string,99,in);
      if (xvt_str_match(string,"FLUSSN*",FALSE))
      {
        ptr = strchr(string,' ');
        if ((ptr)&&(!found))
        {
          ptr++;
          strcpy(gewaesser,ptr);
          ptr=strrchr(gewaesser,'\n');
          if (ptr) ptr[0]='\0';
          found=1;
          ret= 1;
        }
        else ret =0;
      }
    }
    fclose(in);
  }
  else ret =0;
  
  delete[] string;
  return ret;
}
/*************************************************************************/
void JabGenProfilName(char *gewaesser,int nr,char * name)
{
  int len,i=0,n=0;
  char num[10];
  
  name[0] = gewaesser[0];
  name[1] = gewaesser[1];
  for (i=2;i<=7;i++)
    name[i]='0';
  itoa(nr,num,10);
  len = strlen(num);
  for (i=8-len ;i<=7;i++)
  {
    name[i] = num[n];
    n++;
  }
  name[8] = '.';
  name[9] = 'p';
  name[10] = 'r';
  name[11] = 'f';
  name[12] = '\0';
}

/*************************************************************************/
/*************************************************************************/
int JabronOpenFileBox(char *filename)
{
  OPENFILENAME ofn;
  char *szFilter;
  char szText[]= "JABRON-Datei (*.jab)|*.jab|JABRON-Datei (*.pro)|*.pro|alle Dateien (*.*)|*.*||";
  char szFiles[MAX_FILES];
  
  UINT i;
  
  szFilter = new char[256];
  
  memset(&ofn,0,sizeof(OPENFILENAME));
  for(i=0; szText[i]!= '\0'; ++i)
    szFilter[i] = szText[i] == '|' ? '\0' : szText[i];
  
  szFiles[0]='\0'; //init
  ofn.lpstrTitle ="Auswahl  JABRON - Datei";
  ofn.lStructSize = sizeof(OPENFILENAME);
  ofn.lpstrFilter = szFilter;
  ofn.nMaxFile = MAX_FILES;
  ofn.nMaxFileTitle = _MAX_FNAME + _MAX_EXT;
  ofn.lpstrFileTitle = NULL;
  ofn.lpstrDefExt = "jab" ;
  ofn.lpstrInitialDir=NULL;
  ofn.Flags = OFN_FILEMUSTEXIST | OFN_NOVALIDATE | OFN_HIDEREADONLY;
  
  ofn.lpstrFile=szFiles;
  
  if(! GetOpenFileName(&ofn) )
  {
    if (CommDlgExtendedError()==(DWORD)FNERR_BUFFERTOOSMALL)
    {
      char buf[200];
      xvt_res_get_str(STR_JABRON_DLL_NOTE_7,buf,sizeof(buf));
      xvt_dm_post_note("%s",buf);
      
      //xvt_dm_post_note("Zu viele Dateien gewählt oder Pfad zu lang!");
    }
    ofn.lpstrFile[0]='\0';
    delete[] szFilter;
    filename=NULL;
    return FALSE;
  }
  
  else
	 {
    strcpy(filename,ofn.lpstrFile);
    delete[] szFilter;
    return TRUE;
	 }
}

/**************************************************************************/
char *strblanktab(char *string)
{ /*erstes BLANK oder TAB-Zeichen im string zurückgeben (vergl.'strchr')*/
  int len;
  char *ptr=NULL;
  
  len = strlen(string);
  if(len>0)
  {
    ptr = string;
    while (*ptr!='\0')
    {
      if ((ptr[0]==' ')||(ptr[0]=='\t'))
        return ptr;
      else ptr++;
    }
    return NULL;
  }
  return NULL;
}
