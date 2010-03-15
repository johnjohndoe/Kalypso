#include <windows.h>
#include "xvt.h"

#include "global_types.h"
#include "global_vars.h"
#include "typen.h"
#include "resource.h"

#include "wspwin.h"

#include "strang.h"
#include "list.h"
#include "wsplist.h"
#include "readprof.h"
#include "error1.h"
#include "volume2.h"
#include "leibo.h"
#include "wspalloc.h"
#include "slist.h"

#include "flaeche.h"

// globale Variablen //

extern WSP_PROFIL_LISTE *pWPL;

extern WINDOW dlg_136;
extern char str_zustand[16], str_gewaesser[16];
extern BOOLEAN var_dlg135;
extern BOOLEAN Ber_edit_sperr; 

// Vorwärtsdeklarationen
int Connect_Prof_2Gel2(double,STR_DATA*,STR_DATA*);



long XVT_CALLCONV1 DLG_165_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
{
  return 0L;
}; // DLG_165_eh

/*****************************************************************************/
void InitStrData(STR_DATA *data)
{
  if (data !=NULL)
  {
    data->anzahl_profil_dat_entries=0;
    data->anzahl_strang_entries=0;
    data->str_zustand=NULL;
    data->str_gewaesser=NULL;
    data->ptr_anfang=NULL;
    data->prof_datei=NULL;
  }
}
/*****************************************************************************/
void DeleteStrData(STR_DATA *data)
{
  if (data)
  {
    if (data->str_zustand)
    {
      delete data->str_zustand;
      data->str_zustand=NULL;
    }
    if (data->str_gewaesser)
    {
      delete data->str_gewaesser;
      data->str_gewaesser=NULL;
    }
    if (data->prof_datei)
    {
      DeleteWspSList(data->prof_datei);
      data->prof_datei=NULL;
    }
    delete data;
    data =NULL;
  }
}
/*****************************************************************************/
int Save_Global_StrData(STR_DATA *str_data)
{  /*Globale Daten sichern*/
  char *temp;
  SLIST_ELT e;
  
  if (str_data==NULL)
    str_data=new STR_DATA;
  if (str_data->str_zustand==NULL)
    str_data->str_zustand   = new char[20];
  if (str_data->str_gewaesser==NULL)
    str_data->str_gewaesser = new char[20];
  
  str_data->ptr_anfang = strang_anfang;
  // hier evt. noch : strang_anfang =NULL setzen
  
  if (str_data->prof_datei!=NULL)  // alte Daten löschen
  {
    DeleteWspSList(str_data->prof_datei);
    str_data->prof_datei=NULL;
  }
  if (prof_datei!=NULL)
    for (e=xvt_slist_get_first(prof_datei);e!=NULL;
    e=xvt_slist_get_next(prof_datei,e) )
    { /*temp darf an dieser Stelle nur Zeiger sein - kein "new char[]"*/
      temp = xvt_slist_get(prof_datei,e,0L);
      str_data->prof_datei=AppendStringToWspSList(str_data->prof_datei,temp);
    }
    
    if ((str_data->str_zustand!=NULL)&&(str_data->str_gewaesser!=NULL))
    {
      strcpy(str_data->str_zustand,str_zustand);
      strcpy(str_data->str_gewaesser,str_gewaesser);
      
      str_data->anzahl_profil_dat_entries= anzahl_profil_dat_entries;
      str_data->anzahl_strang_entries    = anzahl_strang_entries;
      
      temp = new char[256];
      strcpy(str_data->file.name,STR_SPEC.name);
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,temp,255);
      xvt_fsys_convert_str_to_dir(temp,&str_data->file.dir);
      delete[] temp;
      return 1;
    }
    return 0;
}

/*****************************************************************************/
int Get_Global_StrData(STR_DATA *str_data)
{ /*Gesicherte globale Daten zurückschreiben*/
  char *temp ;
  WSP_SLIST *tmp_slist;
  
  strcpy(str_zustand,str_data->str_zustand);
  strcpy(str_gewaesser,str_data->str_gewaesser);
  
  strang_anfang = str_data->ptr_anfang;
  
  if (xvt_slist_is_valid(prof_datei))
  {
    xvt_slist_destroy(prof_datei);
  }
  prof_datei=xvt_slist_create();   //globale Variable
  
  if ((prof_datei!=NULL)&&(str_data->prof_datei))
  {
    temp = new char[256];
    tmp_slist=str_data->prof_datei;
    while (tmp_slist !=NULL)
    {
      if(tmp_slist->string!=NULL)
        strcpy(temp,tmp_slist->string);
      xvt_slist_add_at_elt(prof_datei,NULL,temp,0L);
      tmp_slist = tmp_slist->next;
    }
  }
  else return 0;
  
  anzahl_profil_dat_entries =str_data->anzahl_profil_dat_entries;
  anzahl_strang_entries     =str_data->anzahl_strang_entries;
  
  strcpy(STR_SPEC.name,str_data->file.name);
  
  xvt_fsys_convert_dir_to_str(&str_data->file.dir,temp,255);
  xvt_fsys_convert_str_to_dir(temp,&STR_SPEC.dir);
  
  delete[] temp;
  
  return 1;
}
/*********************************************************************************/
int Test_2STR_Data(STR_DATA *data1,STR_DATA *data2)
{
/*   In data1 stehen die (globalen) Daten der 1. str-Datei,
data2 enthält Daten der 2. str-Datei .

		Die Funktion Test_2STR_Data() dient nur dem Test der Profile,
    eine Änderung an den Profilen wird nicht vorgenommen.
    Falls GELAENDE2 existiert muß dieses an anderer Stelle
    gelöscht werden.
    Die Funktion wird beim ersten Auftreten eines Fehlers beendet.
    
      Rückgabewert:   0  OK
      1  Fehler: es gibt 2 Profile bei denen der erste und der
      letzte Profilpunkt nicht identisch sind
      2  Fehler: Gewässernamen der *.str-Datei nicht identisch
      3  Fehler: Eine der Strangtabellen enthält keine Profile
      4  Fehler: Eines der Profile enthält GELAENDE2 Daten
      5  sonstiger Fehler
  */
  int zustand;
  char *temp,
    *ptr,
    gewaesser[10],
    prof_name1[15],
    prof_name2[15];
  double station1,
		  station2;
  WSP_PROFIL_LISTE *tmpWPL;
  WSP_SLIST *tmp_slist1,
    *tmp_slist2;
  
  temp = new char[256];
  
  xvt_scr_set_busy_cursor();
  
  if ( xvt_str_match(data1->str_gewaesser,data2->str_gewaesser,FALSE) )
  {
    if ((data1->anzahl_profil_dat_entries>0)&&(data2->anzahl_profil_dat_entries>0))
    {
      tmp_slist1=data1->prof_datei;
      while (tmp_slist1 !=NULL)
      {
        if(tmp_slist1->string!=NULL)
          strcpy(temp,tmp_slist1->string);
        if (temp!=NULL)
        {
          sscanf(temp,"%s%lf",gewaesser,&station1);
          ptr = &temp[44];   //ab Position 44 steht der Dateiname
          strncpy(prof_name1,ptr,12);
          prof_name1[12]='\0';
        }
        tmp_slist2=data2->prof_datei;
        while (tmp_slist2 !=NULL)
        {
          if(tmp_slist2->string!=NULL)
            strcpy(temp,tmp_slist2->string);
          if (temp!=NULL)
          {
            sscanf(temp,"%s%lf",gewaesser,&station2);
            if (station1 == station2)
            {
              ptr = &temp[44];
              strncpy(prof_name2,ptr,12);
              prof_name2[12]='\0';
              /* 2 Profile mit gleicher Stationierung gefunden*/
              
              
              pWPL=Init_Profil_Liste(pWPL);
              pWPL=Init_Profil_Liste(pWPL);
              tmpWPL = pWPL->PListNext;
              
              strcpy(file_spec.name,prof_name1); // obsolet?
              read_profildatei( tmpWPL, &STR_SPEC.dir, prof_name1 );
              SaveGlobalData(tmpWPL->data);
              
              strcpy(file_spec.name,prof_name2); // obsolet?
              read_profildatei( tmpWPL->PListNext, &STR_SPEC.dir, prof_name2 );
              SaveGlobalData(tmpWPL->PListNext->data);
              
              GetGlobalData(tmpWPL->data);
              /*2 Profile sind jetzt in tmpWPL bzw. tmpWPL->PListNext */
              
              
              /*Testen, ob in einem der beiden Profile der
              Datensatz "GELAENDE2" bereits enthalten ist*/
              if( tmpWPL->PList->ExistGelaende2Daten() )
              {
                Delete_Profil_Liste(tmpWPL);
                pWPL->PListNext=NULL;
                delete[] temp;
                return 4; //Eines der Profile enthält GELAENDE2 Daten
              }
              /* Testen,ob erster und letzter Profilpunkt identisch sind*/
              zustand =tmpWPL->PList->Check_Gel1_Gel2_Daten(tmpWPL->PListNext->PList);
              switch (zustand)
              {
              case 0:  // erster und letzter Profilpunkt identisch
                //Bley 19.2.2001		 break;
              case 1:  // erste Profilpunkte stimmen nicht überein
              case 2:  // letzte Profilpunkte stimmen nicht überein
                /*Bley 19.2.2001*/		break;
              case 3:  // sonstiger Fehler (GELAENDEHOEHE nicht gefunden)
                Display_Errorbox1(prof_name1,prof_name2,station1,zustand);
                Delete_Profil_Liste(tmpWPL);
                pWPL->PListNext=NULL;
                delete[] temp;
                return 1;
              };
              Delete_Profil_Liste(tmpWPL);
              pWPL->PListNext=NULL;
            }
          }
          tmp_slist2 = tmp_slist2 ->next;
        }  //-while (tmp_slist2 !=NULL)
        tmp_slist1 = tmp_slist1 ->next;
      }
    }
    else
    {
      delete[] temp;
      return 3;   //keine Profile
    }
  }
  else
  {
    delete[] temp;
    return 2;  //Gewässernamen nicht identisch
  }
  delete[] temp;
  return 0;
}
/*************************************************************************/
int Connect_2STR_Data(STR_DATA *data1,STR_DATA *data2,int test_gel2)
{
/*  In data1 stehen die (globalen) Daten der 1. str-Datei,
data2 enthält Daten der 2. str-Datei
Daten werden kopiert und Profildatei auf Platte gesichert

  Rückgabewert:   0  OK
  1  Fehler: es gibt 2 Profile bei denen der erste und der
  letzte Profilpunkt nicht identisch sind
  2  F0ehler: Gewässernamen der *.str-Datei nicht identisch
  3  Fehler: Eine der Strangtabellen enthält keine Profile
  4  Fehler: Eines der Profile enthält GELAENDE2 Daten
  5  sonstiger Fehler
  */
  
  int  ds,zustand;
  char *temp,
    *ptr,
    gewaesser[10],
    prof_name1[15],
    prof_name2[15];
  double station1,
		  station2;
  WSP_PROFIL_LISTE *tmpWPL;
  WSP_SLIST        *tmp_slist1,
    *tmp_slist2;
  
  xvt_scr_set_busy_cursor();
  
  temp = new char[256];
  if ( xvt_str_match(data1->str_gewaesser,data2->str_gewaesser,FALSE) )
  {
    if ((data1->anzahl_profil_dat_entries>0)&&(data2->anzahl_profil_dat_entries>0))
    {
      tmp_slist1=data1->prof_datei;
      while (tmp_slist1 !=NULL)
      {
        if(tmp_slist1->string!=NULL)
          strcpy(temp,tmp_slist1->string);
        if (temp!=NULL)
        {
          sscanf(temp,"%s%lf",gewaesser,&station1);
          ptr = &temp[44];  //ab Position 44 steht der Dateiname
          strncpy(prof_name1,ptr,12);
          prof_name1[12]='\0';
        }
        tmp_slist2=data2->prof_datei;
        while (tmp_slist2 !=NULL)
        {
          if(tmp_slist2->string!=NULL)
            strcpy(temp,tmp_slist2->string);
          if (temp!=NULL)
          {
            sscanf(temp,"%s%lf",gewaesser,&station2);
            if (station1 == station2)
            {
              ptr = &temp[44];
              strncpy(prof_name2,ptr,12);
              prof_name2[12]='\0';
              
              /* 2 Profile mit gleicher Stationierung gefunden*/
              
              /*2 neue WPL-Elemente anhängen  */
              pWPL=Init_Profil_Liste(pWPL);
              pWPL=Init_Profil_Liste(pWPL);
              tmpWPL = pWPL->PListNext;
              
              strcpy(file_spec.name,prof_name2); // obsolet?
              if (read_profildatei( tmpWPL->PListNext, &STR_SPEC.dir, prof_name2 ))  // 2.list
                return 5; //im Fehlerfall gibt read_profildatei TRUE zurück
              SaveGlobalData(tmpWPL->PListNext->data);
              
              //Dick 12.10.99 list 2 zuerst damit  Kommentar im list1 nicht gelöscht wird
              strcpy(file_spec.name,prof_name1); // obsolet?
              if( read_profildatei( tmpWPL, &STR_SPEC.dir, prof_name1 ) )   // 1.list
                return 5;  //im Fehlerfall gibt read_profildatei TRUE zurück
              SaveGlobalData(tmpWPL->data);
              
              GetGlobalData(tmpWPL->data);
              
              /* Testen,ob erster und letzter Profilpunkt identisch sind*/
              zustand =tmpWPL->PList->Check_Gel1_Gel2_Daten(tmpWPL->PListNext->PList);
              switch (zustand)
              {
              case 0:  // erster und letzter Profilpunkt identisch
                //Bley 19.2.2001						 break;
              case 1:  // erste Profilpunkte stimmen nicht überein
              case 2:  // letzte Profilpunkte stimmen nicht überein
                /*Bley 19.2.2001*/      break;
              case 3:  // sonstiger Fehler (GELAENDEHOEHE nicht gefunden)
                Display_Errorbox1(prof_name1,prof_name2,station1,zustand);
                Delete_Profil_Liste(tmpWPL);
                pWPL->PListNext=NULL;
                delete[] temp;
                return 1;
              };
              ds = tmpWPL->PList->ExistGelaende2Daten();
              
              if (ds>0) //2.Gelände vorhanden
              {
                if (test_gel2) //Meldung/löschen ausgeben !!
                {
                  char buf[200],buf2[200],buf3[200],buf4[200];//Dick 26.11.99
                  
                  xvt_res_get_str(STR_JA,buf,sizeof(buf));
                  xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
                  xvt_res_get_str(STR_PROFIL,buf3,sizeof(buf3));
                  xvt_res_get_str(STR_GEL2_DEL_ASK,buf4,sizeof(buf4));
                  switch(xvt_dm_post_ask(buf,buf2,NULL,"%s%s%s",buf3,prof_name1,buf4))
                  {
                  case RESP_DEFAULT:
                    {
                      tmpWPL->PList->DeleteNode(ds, (int*)&ds_info, (int*)&typ );
                      anzahl_ds=ds_info[0];
                      ds = tmpWPL->PList->ExistGelaende2Daten(); // nochmal testen
                    }
                    break;
                  case RESP_2:
                    return 4;
                  case RESP_3:
                    break;
                  }
                } //-if(test_gel2)
                else // ohne Rückfrage löschen
                {
                  tmpWPL->PList->DeleteNode(ds, (int*)&ds_info, (int*)typ );
                  anzahl_ds = ds_info[0];
                  ds = tmpWPL->PList->ExistGelaende2Daten(); // nochmal testen
                }
              } //if (ds>0)
              
              if(ds==0)
              {
                anzahl_ds++;
                typ[anzahl_ds]=GELAENDE2;
                ds_info[0]++;
                ds_info[anzahl_ds]=tmpWPL->PListNext->data->ds_info[1];
                
                tmpWPL->data->anzahl_ds++;
                tmpWPL->data->typ[tmpWPL->data->anzahl_ds]= GELAENDE2;
                tmpWPL->data->ds_info[0]++;
                tmpWPL->data->ds_info[tmpWPL->data->anzahl_ds]=
                  tmpWPL->PListNext->data->ds_info[1];
                tmpWPL->PList->MakeNewNode(tmpWPL->data->anzahl_ds);
                
                // 2.Zeile Infoblock für 2.Gelände
                strcpy(temp,data2->str_zustand);  // Referenzzustand
                strcat(temp," ");
                strcat(temp,data2->file.name); //Referenzdateiname
                tmpWPL->PList->WriteTypDaten(anzahl_ds,GELAENDE2,temp);
                
                tmpWPL->PList->MakeNewKoord(tmpWPL->PListNext->data->ds_info[1]);
                
                /*Geländedaten von 2.list nach 1.list kopieren*/
                tmpWPL->PList->CopyGel2ToGel1Daten(tmpWPL->PListNext->PList);
                
                save_profildatei(tmpWPL);
                
                Delete_Profil_Liste(tmpWPL);
                pWPL->PListNext=NULL;
              }
         } //-if (station1 == station2)
        }  //-	 if (temp!=NULL)
        tmp_slist2 = tmp_slist2 ->next;
      } //-	while (tmp_slist2 !=NULL)
      tmp_slist1 = tmp_slist1 ->next;
    } //-	 while (tmp_slist1 !=NULL)
  }
  else
  {
    //xvt_dm_post_note("Eine der Zustandsdateien enthält keine Profile");
    char buf[200];//Dick 26.11.99
    xvt_res_get_str(STR_FLAECHE_NOTE_1,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf);
    delete[] temp;
    return 3;
  }
 }
 else
	{
   delete[] temp;
   return 2;
	}
 delete[] temp;
 return 0;
}
/*********************************************************************************/
int DeleteGel2InStrData(STR_DATA *data1)
/*
alle Profildateien einer Vernetzungsdatei nach Datensatz Typ:GELAENDE2
durchsuchen und diesen löschen und anschl. Profildatei wieder sichern

  Rückgabe:  0:Fehler ,  1: OK
  */
{
  int       ds;
  char      *temp,
    *ptr,
    prof_name[15];
  WSP_SLIST *tmp_slist;
  
	 temp = new char[256];
   tmp_slist=data1->prof_datei;
   while (tmp_slist !=NULL)
	  {
     if(tmp_slist->string!=NULL)
       strcpy(temp,tmp_slist->string);
     if (temp!=NULL)
     {
       ptr = &temp[44];  //ab Position 44 steht der Dateiname
       strncpy(prof_name,ptr,12);
       prof_name[12]='\0';
       strcpy(file_spec.name,prof_name); // obsolet?
       
       if(read_profildatei( pWPL, &STR_SPEC.dir, prof_name ) )   // 1.list
         return 0;
       ds = pWPL->PList->ExistGelaende2Daten();
       if (ds)
       {
         pWPL->PList->DeleteNode( ds, (int*)&ds_info, (int*)&typ );
         anzahl_ds = ds_info[0];
         
         save_profildatei(pWPL);
       }
     } //-if (temp!=NULL)
     tmp_slist=tmp_slist->next;
	  }
   delete[] temp;
   //xvt_dm_post_note("Geländeverknüpfung löschen beendet!");
   char buf[200];//Dick 26.11.99
   xvt_res_get_str(STR_FLAECHE_NOTE_2,buf,sizeof(buf));
   xvt_dm_post_note("%s",buf); 
   return 1;
}
/*********************************************************************************/
int DeleteFlaecheInStrData(STR_DATA *data1)
/*
alle Profildateien einer Vernetzungsdatei nach Datensatz Typ: FLAECHE
durchsuchen und diesen löschen und anschl. Profildatei wieder sichern

  Rückgabe:  0:Fehler ,  1: OK
  */
  
{
  int       ds;
  char      *temp,
    *ptr,
    prof_name[15];
  WSP_SLIST *tmp_slist;
  
	 temp = new char[256];
   tmp_slist=data1->prof_datei;
   while (tmp_slist !=NULL)
	  {
     if(tmp_slist->string!=NULL)
       strcpy(temp,tmp_slist->string);
     if (temp!=NULL)
     {
       ptr = &temp[44];  //ab Position 44 steht der Dateiname
       strncpy(prof_name,ptr,12);
       prof_name[12]='\0';
       strcpy(file_spec.name,prof_name); // obsolet?
       
       if( read_profildatei( pWPL, &STR_SPEC.dir, prof_name ) )   // 1.list
         return 0;
       
       ds = pWPL->PList->ExistFlaecheDaten();
       if (ds)
       {
         pWPL->PList->DeleteNode( ds, (int*)&ds_info, (int*)&typ );
         anzahl_ds=ds_info[0];
         
         save_profildatei(pWPL);
       }
     } //-if (temp!=NULL)
     tmp_slist=tmp_slist->next;
	  }
   delete[] temp;
   //xvt_dm_post_note("Flächendaten löschen beendet!");
   char buf[200];//Dick 26.11.99
   xvt_res_get_str(STR_FLAECHE_NOTE_3,buf,sizeof(buf));
   xvt_dm_post_note("%s",buf); 
   return 1;
}
/*********************************************************************************/
int Test_Flaechen_Data(STR_DATA *data)
{  /*
		 In data stehen die (globalen) Daten der Vernetzungsdatei.
     Test_Flaechen_Data() testet, ob in einem der Profile ein
     Datensatz: "2.GELAENDE" oder "FLAECHE" enthalten ist.
     Diese können gelöscht werden.
     Rückgabe:	0  :  alles OK (kein Abbruch bei löschen)
     1  :  Abbruch bei löschen Fläche
     2  :  Abbruch bei löschen 2.Gelände
     3  :  Vernetzungsdatei enthält keine Profile
     4  :  Gelände2 Daten behalten
  */
  int   		flaeche=0,
				gelaende=0,
        xFlaeche=0,
        xGelaende=0;
  char       *temp,
				*ptr,
        prof_name[15];
  WSP_SLIST  *tmp_slist;
  
  temp = new char[256];
  
  xvt_scr_set_busy_cursor();
  
  if (data->anzahl_profil_dat_entries>0)
  {
    tmp_slist=data->prof_datei;
    while (tmp_slist !=NULL)   // alle Profile lesen und "Gelaende2" und "Flaeche" testen
    {
      if(tmp_slist->string!=NULL)
        strcpy(temp,tmp_slist->string);
      if (temp!=NULL)
      {
        ptr = &temp[44];  //ab Position 44 steht der Dateiname
        strncpy(prof_name,ptr,12);
        prof_name[12]='\0';
        strcpy(file_spec.name,prof_name); // obsolet?
        read_profildatei( pWPL, &STR_SPEC.dir, prof_name );   // Profil einlesen
        
        if ( pWPL->PList->ExistFlaecheDaten() )
          flaeche++;
        if ( pWPL->PList->ExistGelaende2Daten() )
          gelaende++;
      }
      tmp_slist=tmp_slist->next;
    } //-while (tmp_slist !=NULL)
    
    if (flaeche >0)
    {
      char buf[200], buf2[200], buf3[200];
      
      xvt_res_get_str(STR_ABBRECHEN,buf,sizeof(buf));
      xvt_res_get_str(STR_LOESCHEN,buf2,sizeof(buf2));
      xvt_res_get_str(STR_FLAECHENBERECHNUNG,buf3,sizeof(buf3));
      
      switch(xvt_dm_post_ask(buf2,buf,NULL,"%s",buf3))
      {
      case RESP_DEFAULT:  // löschen
        xFlaeche=1;
        break;
      case RESP_2:       //cancel
        xFlaeche=2;
        break;
      };
    }
    
    if (gelaende>0)
    {
      char buf[200], buf2[200], buf3[200], buf4[200];
      
      xvt_res_get_str(STR_ABBRECHEN,buf,sizeof(buf));
      xvt_res_get_str(STR_LOESCHEN,buf2,sizeof(buf2));
      xvt_res_get_str(STR_UEBERNEHMEN, buf4,sizeof(buf4));
      xvt_res_get_str(STR_GELAENDEBERECHNUNG,buf3,sizeof(buf3));
      
      switch(xvt_dm_post_ask(buf2,buf,buf4,"%s",buf3))
      {
      case RESP_DEFAULT:  //löschen
        xGelaende=1;
        break;
      case RESP_2:       //cancel
        xGelaende=3;
        break;
      case RESP_3:       //übernehmen
        xGelaende=2;
        break;

      };
    }
    
    /* Abbruch testen */
    if (xFlaeche ==2)
    {
      delete[] temp;
      return 1;
    }
    if (xGelaende==3)   //Abbruch
    {
      delete[] temp;
      return 2;
    }
    
    if (xFlaeche ==1)
      DeleteFlaecheInStrData(data);
    if (xGelaende==1)
      DeleteGel2InStrData(data);
    if (xGelaende==2)  // alte Gelände2 Daten übernehmen
    {
      delete[] temp;
      return 4;
    }
    delete[] temp;
    return 0;
    
  } //-if (data->anzahl_profil_dat_entries>0)
  delete[] temp;
  return 3;
}
/*********************************************************************************/
int Make_FlaechenBerechnung(STR_DATA *data)
{ /*
  Flächenberechnung für komplette Vernetzungsdatei durchführen
  In data stehen die (globalen) Daten der Vernetzungsdatei.
  Über 'ProfilFlaechenBerechnung()' wird Modul Flächenberechnung Dr.Leibo
  aufgerufen und Profil anschließend auf Platte gesichert
  Rückgabe:	0  :  Fehler
  1  :  alles OK (kein Abbruch bei löschen)
  2  :  Fehler (kein Gelaende2) in ProfilFlaechenBerechnung
  
  */
  double       station;
  char         *temp,
    *ptr,
    gewaesser[10],
    prof_name[15];
  WSP_SLIST    *tmp_slist;
  int          ret=1;
  
  temp = new char[256];
  
  xvt_scr_set_busy_cursor();
  
  if ((data)&&(data->anzahl_profil_dat_entries>0))
  {
    tmp_slist=data->prof_datei;
    while (tmp_slist !=NULL)   // alle Profile lesen und "Gelaende2" und "Flaeche" testen
    {
      if(tmp_slist->string!=NULL)
        strcpy(temp,tmp_slist->string);
      if (pWPL==NULL)
        pWPL = Init_Profil_Liste(pWPL);
      if ((temp!=NULL)&&(pWPL!=NULL))
      {
        sscanf(temp,"%s%lf",gewaesser,&station );
        ptr = &temp[44];  //ab Position 44 steht der Dateiname
        strncpy(prof_name,ptr,12);
        prof_name[12]='\0';
        
        if(read_profildatei( pWPL, &STR_SPEC.dir, prof_name, TRUE ) )   // Profil einlesen
          return 0;  // sofort abbrechen
        
        switch (ProfilFlaechenBerechnung(pWPL->PList,station))
        {
        case 0:
          ret= 0;// Fehler oder keine Berechnung in Modul Dr.Leibo
          break;
        case 1:
          ret=1;
          save_profildatei( pWPL, TRUE ); // Profildatei auf Platte sichern
          break;
        case 2:
          ret =2;
          break;
        };
      }
      Delete_Profil_Liste(pWPL);
      pWPL=NULL;
      tmp_slist=tmp_slist->next;
    } //-while (tmp_slist !=NULL)
  }
  pWPL = Init_Profil_Liste(pWPL); //pWPL wieder alloziieren
  return ret;
}
/*********************************************************************************/
int Make_GelaendeVerknuepfung(int action,double station)
{
/*
action :=  1 komplette str-Datei (station = don't care)
2 nur für übergebene Station
station := Station km (für action =2)

	 Rückgabe: 1 =OK   , 0 =Fehler
  */
  int test=0;
  BOOLEAN error=FALSE;
  STR_DATA *str_data,*new_data;
  WINDOW dialog165;
  
  dialog_ende = 1;
  str_data=new  STR_DATA;
  new_data=new  STR_DATA;
  
  InitStrData(str_data);
  InitStrData(new_data);
  
  if(Save_Global_StrData(str_data))  //globale Daten sichern
  {
    if (action ==1)
      xvt_vobj_set_visible(dlg_136 ,FALSE); // Dialog 136 in Hintergrund
    
    dialog165 = xvt_dlg_create_res( WD_MODELESS, DLG_165,EM_ALL, DLG_165_eh, 0L );
    
    var_dlg135=TRUE;
    Ber_edit_sperr=TRUE;//Dick 28.09.99 damit title nicht ändert
    if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
			 {
      xvt_dm_post_error("Can't open dialog 135");
      Get_Global_StrData(str_data);
      error = TRUE;
			 }
    Ber_edit_sperr=FALSE;//Dick 28.09.99
    if (dialog_ende==0)  // Abbruch in Dlg135
			 {
      Get_Global_StrData(str_data);
      error =TRUE;
			 }
    var_dlg135=FALSE;
    
    xvt_vobj_destroy(dialog165);
    
    if (action ==1)
      xvt_vobj_set_visible(dlg_136 ,TRUE); // Dialog136 wieder in Vordergrund
    
                                           /* da Dialog 135=MODAL ist, wird erst weitergearbeitet,
                                           wenn ein xvt_destroy() erfolgte, also steht ab jetzt
                                           der neue *.str-Name in STR_SPEC.name,sonstige Daten
                                           werden in DLG135 nicht geändert.
    */
    
    xvt_scr_set_busy_cursor();
    /*Testen, ob Vernetzungsdateien identisch*/
    if (!error)
      if ( xvt_str_match(str_data->file.name,STR_SPEC.name,FALSE) )
      {
        //xvt_dm_post_note("Vernetzungsdateien sind identisch.\n\nBitte andere Wahl!");
        char buf[200];//Dick 26.11.99
        xvt_res_get_str(STR_MASSE_NOTE_1,buf,sizeof(buf));
        xvt_dm_post_note("%s",buf);
        if(!Get_Global_StrData(str_data))
        {
          //xvt_dm_post_error("Fehler beim Rückladen der Vernetzungsdatei! #1");
          xvt_res_get_str(STR_MASSE_NOTE_2,buf,sizeof(buf));
          xvt_dm_post_note("%s #1",buf);
        }
        error=TRUE;
      }
      
      if ((prof_datei)&&(!error))
      {
        xvt_slist_destroy(prof_datei);
        prof_datei=NULL;
      }
      
      if (!error)
      {
        strang_anfang=NULL; //Adr. ist noch in str_data gespeichert
        if (strang_anfang!=NULL)
          destroy_strang_tabelle();
        MakeNewStrang(STRANGANZAHL);
        read_profil_dat(strang_anfang);  // Strangtabelle 2.Vernetzungsdatei lesen
      }
      
      if (!error)
      {
        if ((action==1)&&(Save_Global_StrData(new_data)) )// für komplette str-Datei
        {
          /* Testen,ob erster und letzter Profilpunkt in allen Profilen identisch sind*/
          test = Test_2STR_Data(str_data,new_data);
          switch (test)
          {
          case 0: // Ok
            /*Bley 19.2.2001*/ case 1:
            {
              if (Connect_2STR_Data(str_data,new_data,0)>0) //2.Gelände einfügen
                error=TRUE;  //im OK -Fall ist return=0
              else // alles OK
              {
                //xvt_dm_post_note("Geländeverknüpfung erfolgreich ausgeführt.");
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_FLAECHE_NOTE_4,buf,sizeof(buf));
                xvt_dm_post_note("%s",buf);
              }
            }
            break;
            /*Bley 19.2.2001    case 1:   //erster/letzter Profilpunkt nicht identisch
            //					  xvt_dm_post_note("Es sind 2 Profile vorhanden, bei denen der erste bzw. letzte Profilpunkt nicht identisch sind!");
            { 
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_FLAECHE_NOTE_7,buf,sizeof(buf));
            xvt_dm_post_note("%s",buf);
            error =  TRUE;
            }
            break;
            */				  case 2:   //Fehler: Gewässernamen der *.str-Datei nicht identisch
            {
              //xvt_dm_post_error("Gewässernamen stimmen nicht überein!");
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_FLAECHE_NOTE_5,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
              error =  TRUE;
            }
            break;
          case 3:   //keine Profile
            {
              //xvt_dm_post_error("Eine der Zustandsdateien enthält keine Profile.",
              //					  "Erzeugen Sie zuerst Profile bzw. nehmen Sie welche auf !");
              char buf[200],buf2[200];//Dick 26.11.99
              xvt_res_get_str(STR_FLAECHE_NOTE_1,buf,sizeof(buf));
              xvt_res_get_str(STR_FLAECHE_NOTE_6,buf2,sizeof(buf2));
              xvt_dm_post_note("%s%s",buf,buf2);
            }
            error =  TRUE;
            break;
          case 4:   //eines der Profile enthält GELAENDE2-Daten
            //					  xvt_dm_post_error("Eines der Profile enthält bereits einen Datensatz:\n2.GELAENDE\nBitte zuerst Verknüpfung entfernen!");
            {
              char buf[200],buf2[200],buf3[200],buf4[200];//Dick 26.11.99
              
              xvt_res_get_str(STR_JA,buf,sizeof(buf));
              xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
              xvt_res_get_str(STR_ABBRECHEN,buf3,sizeof(buf3));
              xvt_res_get_str(STR_GEL2_DEL_ASK2,buf4,sizeof(buf4));
              switch(xvt_dm_post_ask(buf,buf2,buf3,buf4))
              {
              case RESP_DEFAULT:
                DeleteGel2InStrData(str_data);
              case RESP_2:
                if (Connect_2STR_Data(str_data,new_data,1)>0) //2.Gelände einfügen
                  error=TRUE;  //im OK -Fall ist return=0
                else // alles OK
                {
                  //xvt_dm_post_note("Geländeverknüpfung erfolgreich ausgeführt.");
                  char buf[200];//Dick 26.11.99
                  xvt_res_get_str(STR_FLAECHE_NOTE_4,buf,sizeof(buf));
                  xvt_dm_post_note("%s",buf);
                }
                break;
              case RESP_3:
                error =  TRUE;
                break;
              }//-switch
            }
            break;
          case 5:   //eines der Profile enthält GELAENDE2-Daten
            //xvt_dm_post_error("Profilverknüpfung kann nicht erzeugt werden!");
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_FLAECHE_NOTE_8,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
              error =  TRUE;
            }
            break;
          };
          
          Get_Global_StrData(new_data);
          destroy_strang_tabelle(); //von: new_data
          
          if(!Get_Global_StrData(str_data))
          {
            //xvt_dm_post_error("Fehler beim Rückladen der Vernetzungsdatei! #2");
            char buf[200];
            xvt_res_get_str(STR_MASSE_NOTE_2,buf,sizeof(buf));
            xvt_dm_post_note("%s #2",buf);
          }
        }
        else if (action==2) // nur für eine Station
        {
          if(Save_Global_StrData(new_data))
          {
            switch(Connect_Prof_2Gel2(station,str_data,new_data))
            {
            case 0:   // Ok
              /*Bley 19.2.2001*/	  case 1:
              {
                //xvt_dm_post_note("Geländeverknüpfung erfolgreich ausgeführt.");
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_FLAECHE_NOTE_4,buf,sizeof(buf));
                xvt_dm_post_note("%s",buf);
              }
              break;
              /*Bley 19.2.2001	  case 1:   //erster/letzter Profilpunkt nicht identisch
              {
              //						 xvt_dm_post_note("Es sind 2 Profile vorhanden, bei denen der erste bzw. letzte Profilpunkt nicht identisch sind!");
              
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_FLAECHE_NOTE_7,buf,sizeof(buf));
                xvt_dm_post_note("%s",buf);
                error =  TRUE;
                }
                break;
              */					  case 2:   //Fehler: Gewässernamen der *.str-Datei nicht identisch
              {
                //xvt_dm_post_error("Gewässernamen stimmen nicht überein!");
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_FLAECHE_NOTE_5,buf,sizeof(buf));
                xvt_dm_post_note("%s",buf);
                error =  TRUE;
              }
              break;
            case 3:   //keine Profile
              {
                //xvt_dm_post_error("Eine der Zustandsdateien enthält keine Profile.",
                //					  "Erzeugen Sie zuerst Profile bzw. nehmen Sie welche auf !");
                char buf[200],buf2[200];//Dick 26.11.99
                xvt_res_get_str(STR_FLAECHE_NOTE_1,buf,sizeof(buf));
                xvt_res_get_str(STR_FLAECHE_NOTE_6,buf2,sizeof(buf2));
                xvt_dm_post_note("%s%s",buf,buf2);
                error =  TRUE;
              }
              break;
            case 4:   //eines der Profile enthält GELAENDE2-Daten
              
              //xvt_dm_post_error("Eines der Profile enthält einen Datensatz:\n2.GELAENDE\nBitte erst entfernen!");
              { 
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_FLAECHE_NOTE_9,buf,sizeof(buf));
                xvt_dm_post_note("%s",buf);
              }
              break;
            case 5:   //eines der Profile enthält GELAENDE2-Daten
              {
                //xvt_dm_post_error("Profilverknüpfung kann nicht erzeugt werden!");
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_FLAECHE_NOTE_8,buf,sizeof(buf));
                xvt_dm_post_note("%s",buf);
              }
              break;
            }; //-switch
          }
          if(!Get_Global_StrData(str_data))
          {
            //xvt_dm_post_error("Fehler beim Rückladen der Vernetzungsdatei! #2");
            char buf[200];
            xvt_res_get_str(STR_MASSE_NOTE_2,buf,sizeof(buf));
            xvt_dm_post_note("%s #2",buf);
          }
        }//-else if (action==2)
       }
      }
      DeleteStrData(str_data);
      DeleteStrData(new_data);
      if (error) return 0;
      return 1;
}
/*********************************************************************************/
int Connect_Prof_2Gel2(double station1,STR_DATA *data1,STR_DATA *data2)
{
/*  für Alpha-/Grafik-Editor

  In data1 stehen die (globalen) Daten der 1. str-Datei,
  In data2 stehen die (globalen) Daten der 2. str-Datei,
  
    
      Rückgabewert:   0  OK
      1  Fehler: es gibt 2 Profile bei denen der erste und der
      letzte Profilpunkt nicht identisch sind
      2  Fehler: Gewässernamen der *.str-Datei nicht identisch
      3  Fehler: Eine der Strangtabellen enthält keine Profile
      4  Fehler: Eines der Profile enthält GELAENDE2 Daten
      5  sonstiger Fehler, Benutzerabbruch, read_profildatei() fehlgeschlagen
  */
  
  int              ds,zustand;
  char             *temp,
    *ptr,
    gewaesser[10],
    prof_name1[15],
    prof_name2[15];
  double           station2;
  WSP_PROFIL_LISTE *tmpWPL;
  WSP_SLIST  	   *tmp_slist2;
  
  
  temp = new char[256];
  if ( xvt_str_match(data1->str_gewaesser,data2->str_gewaesser,FALSE) )
  {
    if (data2->anzahl_profil_dat_entries>0)
    {
      strcpy(prof_name1,file_spec.name);
      tmp_slist2=data2->prof_datei;
      while (tmp_slist2 !=NULL)
      {
        if(tmp_slist2->string!=NULL)
          strcpy(temp,tmp_slist2->string);
        if (temp!=NULL)
        {
          sscanf(temp,"%s%lf",gewaesser,&station2);
          if (station1 == station2)
          {
            ptr = &temp[44];
            strncpy(prof_name2,ptr,12);
            prof_name2[12]='\0';
            
            /* 2 Profile mit gleicher Stationierung gefunden*/
            
            /*2 neue WPL-Elemente anhängen  */
            pWPL=Init_Profil_Liste(pWPL);
            tmpWPL = pWPL->PListNext;
            
            /*aktuelle Daten sichern*/
            SaveGlobalData(pWPL->data);
            
            strcpy(file_spec.name,prof_name2); // obsolet?
            if( read_profildatei( tmpWPL, &STR_SPEC.dir, prof_name2 ) )  // 2.list
              return 5;
            SaveGlobalData(tmpWPL->data);
            
            GetGlobalData(pWPL->data);
            
            /* Testen,ob erster und letzter Profilpunkt identisch sind*/
            zustand =pWPL->PList->Check_Gel1_Gel2_Daten(tmpWPL->PList);
            switch (zustand)
            {
            case 0:  // erster und letzter Profilpunkt identisch
              //Bley 19.2.2001		 break;
            case 1:  // erste Profilpunkte stimmen nicht überein
            case 2:  // letzte Profilpunkte stimmen nicht überein
              /*Bley 19.2.2001*/		break;
            case 3:  // sonstiger Fehler (GELAENDEHOEHE nicht gefunden)
              Display_Errorbox1(prof_name1,prof_name2,station1,zustand);
              Delete_Profil_Liste(tmpWPL);
              pWPL->PListNext=NULL;
              delete[] temp;
              return 1;
            };
            ds = pWPL->PList->ExistGelaende2Daten();
            
            if (ds>0) //2.Gelände vorhanden  Meldung/löschen!!
            {
              
              //bley 1.12.99
              char buf[200],buf2[200],buf3[200],buf4[200];//Dick 26.11.99
              
              xvt_res_get_str(STR_JA,buf,sizeof(buf));
              xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
              xvt_res_get_str(STR_PROFIL,buf3,sizeof(buf3));
              xvt_res_get_str(STR_GEL2_DEL_ASK2,buf4,sizeof(buf4));
              switch(xvt_dm_post_ask(buf,buf2,NULL,"%s %s %s",buf3,prof_name1,buf4))
                //					  switch(xvt_dm_post_ask("Ja","Nein",NULL,"Profil %s enthält Datensatz GELAENDE2.Soll dieser gelöscht werden?",prof_name1))
              {
              case RESP_DEFAULT:
                {
                  pWPL->PList->DeleteNode( ds, (int*)&ds_info, (int*)&typ );
                  anzahl_ds=ds_info[0];
                  SaveGlobalData(pWPL->data); //Daten in pWPL updaten
                  
                  /*Listbutton in Alpha/Grafik aktualisieren*/
                  if(pWPL->window !=NULL)
                  {
                    xvt_list_rem(pWPL->window,ds-1);
                    xvt_list_set_sel(pWPL->window,0,TRUE);
                  }
                  ds = pWPL->PList->ExistGelaende2Daten();
                }
                break;
              case RESP_2:
                return 4;  // Funktion beenden
              case RESP_3:
                break;
              }//-switch
            }//- if (ds>0) 2.GELAENDE
            
            if(ds==0)
            {
              anzahl_ds++;
              typ[anzahl_ds]=GELAENDE2;
              ds_info[0]++;
              ds_info[anzahl_ds]=tmpWPL->data->ds_info[1];
              
              pWPL->data->anzahl_ds++;
              pWPL->data->typ[pWPL->data->anzahl_ds]= GELAENDE2;
              pWPL->data->ds_info[0]++;
              pWPL->data->ds_info[pWPL->data->anzahl_ds]=
                tmpWPL->data->ds_info[1];
              pWPL->PList->MakeNewNode(pWPL->data->anzahl_ds);
              
              // 2.Zeile Infoblock für 2.Gelände
              strcpy(temp,data2->str_zustand);  // Referenzzustand
              strcat(temp," ");
              strcat(temp,data2->file.name); //Referenzdateiname
              pWPL->PList->WriteTypDaten(anzahl_ds,GELAENDE2,temp);
              
              pWPL->PList->MakeNewKoord(pWPL->data->ds_info[pWPL->data->anzahl_ds]);
              
              /*Geländedaten von 2.list nach 1.list kopieren*/
              pWPL->PList->CopyGel2ToGel1Daten(tmpWPL->PList);
              
              //GetGlobalData(pWPL->data);
              strcpy(file_spec.name,prof_name1);
              
              SaveProfilFile =TRUE;
              
              Delete_Profil_Liste(tmpWPL);
              pWPL->PListNext=NULL;
              
              /*jetzt noch Listbutton in Alpha/Grafik aktualisieren*/
              if(pWPL->window !=NULL)
              {
                if((xvt_res_get_str(1000+GELAENDE2,temp,255))==NULL)
                  xvt_dm_post_error("Can't read string resource\nin modul:Connect_Prof_2Gel2");
                xvt_list_add(pWPL->window,-1,(char*)temp );
                xvt_list_set_sel(pWPL->window,0,TRUE);
              }
            }
         } //-if (station1 == station2)
        }  //-	 if (temp!=NULL)
        tmp_slist2 = tmp_slist2 ->next;
      } //-	while (tmp_slist2 !=NULL)
  }
  else
  {
    //	 xvt_dm_post_note("Eine der Zustandsdateien enthält keine Profile");
    char buf[200];
    xvt_res_get_str(STR_FLAECHE_NOTE_1,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf);
    delete[] temp;
    return 3;
  }
 }
 else
	{
   delete[] temp;
   return 2;
	}
 delete[] temp;
 return 0;
}
/*********************************************************************************/
int ProfilFlaechenBerechnung( List *ProfList, double station )
{
/*
Daten aus pWPL->PList in Struktur LAENGSPROFIL kopieren,
Flächenberechnung  (Dr.Leibo) durchführen,
Datensatz FLAECHE in pWPL->PList anlegen und Flächendaten aus
Berechnung dort speichern +  SaveProfilFile = TRUE setzen; ,
anschließend noch Listbutton 'Auswahl Datensatz' in Alpha/Grafik
aktualisieren,falls vorhanden.

  Rückgabe:  1 : keine Fehler
  0 : Fehler oder Berechnung fehlgeschlagen
  2 : kein Gelaende2 vorhanden
  */
  
  LAENGSPROFIL *ptrLP=NULL;
  int ds,
    fl,
    anzahl=0;
  char *temp,
    summe[25];
  GELAENDE *tmp_fl;
  
  
  ds = pWPL->PList->ExistGelaende2Daten() ;
  
  if (ds>0)
  {
    if ((fl=pWPL->PList->ExistFlaecheDaten()))  // alte Flächendaten löschen
    {
      char buf[200],buf2[200],buf3[200];//Dick 26.11.99
      
      xvt_res_get_str(STR_JA,buf,sizeof(buf));
      xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
      xvt_res_get_str(STR_FLAECHE_DEL_ASK,buf3,sizeof(buf3));
      switch(xvt_dm_post_ask(buf,buf2,NULL,buf3))
      {
      case RESP_DEFAULT:
        {
          pWPL->PList->DeleteNode( fl, (int*)&ds_info, (int*)&typ );
          anzahl_ds=ds_info[0];
          SaveGlobalData(pWPL->data); //Daten in pWPL updaten
          
          /*Listbutton in Alpha/Grafik aktualisieren*/
          if(pWPL->window !=NULL)
          {
            xvt_list_rem(pWPL->window,fl-1);
            xvt_list_set_sel(pWPL->window,0,TRUE);
          }
          ds = pWPL->PList->ExistGelaende2Daten() ; // ds aktualisieren
        }
        break;
      case RESP_2:
        return 0;  // Funktion beenden
      case RESP_3:
        break;
      }//-switch
    }
    ptrLP = MakeLNode(ptrLP); // Speicher für GELAENDE anlegen
    ptrLP->querprof1=MakeLKoord(ptrLP->querprof1,ds_info[1]);  // GELAENDE (normal)
    ptrLP->querprof2=MakeLKoord(ptrLP->querprof2,ds_info[ds]); // 2.GELAENDE
    
    ptrLP->station = station;
    
    //Geländedaten aus pWPL->PList nach LAENGSPROFIL kopieren
    ProfList->CopyGelDatenToLaengspro(ptrLP);
    
    ptrLP->querprof1=DeleteBCE_NAN(ptrLP->querprof1);  // GELAENDE (normal)
    ptrLP->querprof2=DeleteBCE_NAN(ptrLP->querprof2); // 2.GELAENDE
    
    xvt_scr_set_busy_cursor();
    /*---- Hier Aufruf: FlaechenImLaengsProfilBerechnen()  Dr.Lejbo	------*/
    if (! FlaechenImLaengsprofilBerechnen(ptrLP))
    {
      DeleteLNode(ptrLP);
      return 0;
    }
    /*---- ENDE Aufruf: FlaechenImLaengsProfilBerechnen()  Dr.Lejbo	------*/
    
    tmp_fl = ptrLP->flaeche;
    
    while (tmp_fl)  //Anzahl der Tupel: 'FLAECHE' zählen
    {
      anzahl++;
      tmp_fl = tmp_fl->next_ds;
    }
    if (anzahl>0)
    {
      tmp_fl = ptrLP->flaeche;
      
      anzahl_ds++;            // globale Variablen aktualisieren
      typ[anzahl_ds]=FLAECHE;
      ds_info[0]++;
      ds_info[anzahl_ds]=anzahl;
      
      pWPL->data->anzahl_ds++;   // lokale Variablen aktualisieren
      pWPL->data->typ[pWPL->data->anzahl_ds]= FLAECHE;
      pWPL->data->ds_info[0]++;
      pWPL->data->ds_info[pWPL->data->anzahl_ds]=anzahl;
      
      /*Datensatz FLAECHE anlegen*/
      pWPL->PList->MakeNewNode(anzahl_ds);
      pWPL->PList->WriteTypDaten(anzahl_ds,FLAECHE,NULL);
      pWPL->PList->MakeNewKoord(anzahl);
      
      /* Flächendaten in Profil schreiben*/
      ProfList->CopyFlaecheToProfil(ptrLP);
      
      temp = new char[100];
      gcvt(ptrLP->summe,10,summe);   // Summe
      strcpy(temp,summe);
      strcat(temp," ");
      gcvt(ptrLP->sum_auftrag,10,summe); // Summe Auftrag
      strcat(temp,summe);
      strcat(temp," ");
      ptrLP->sum_abtrag = ptrLP->sum_abtrag * -1;
      gcvt(ptrLP->sum_abtrag,10,summe);  // Summe Abtrag
      strcat(temp,summe);
      pWPL->PList->SaveSummeToFlaeche(temp); // in 2.Zeile schreiben
      
      // file_spec.name aktualisieren
      GetGlobalData(pWPL->data);//alternativ:  strcpy(file_spec.name,pWPL->data->file.name);
      
      //  Profildatei muß gesichert werden
      SaveProfilFile = TRUE;
      
      /*jetzt noch Listbutton in Alpha/Grafik aktualisieren*/
      if(pWPL->window !=NULL)
      {
        if((xvt_res_get_str(1000+FLAECHE,temp,99))==NULL)
          xvt_dm_post_error("Can't read string resource");
        xvt_list_add(pWPL->window,-1,(char*)temp );
        xvt_list_set_sel(pWPL->window,0,TRUE);
      }
      delete[] temp;
    }
    else  // anzahl==0
    {
      DeleteLNode(ptrLP);
      return 0;
    }
    DeleteLNode(ptrLP);
  }//-if (ds>0)
  else return 2;
  return 1;
}

/*********************************************************************************/
