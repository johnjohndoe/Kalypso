#include <windows.h>
#include "xvt.h"

#include <dos.h>
#include "resource.h"
#include "typen.h"

#include "global_types.h"
#include "global_vars.h"

#include "slist.h"
#include "error1.h"
#include "flaeche.h"
#include "wspwin.h"
#include "list.h"
#include "readprof.h"
#include "strang.h"
#include "wsplist.h"
#include "wspalloc.h"
#include "bce_allg.h"
#include "volume2.h"

#include "volume1.h"



#define LINKS  0
#define FLUSS  1
#define RECHTS 2


extern BOOLEAN      var_dlg135;
extern int          dlg135ref;
extern FILE_SPEC    //STR_SPEC,
file_spec;         // 2x
extern STRANG       *strang_anfang;
extern BOOLEAN      Ber_edit_sperr;  //Dick 21.07.99 dient für 2 Sachen

#define DLG_164 164
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_164_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_164_eh XVT_CALLCONV2 (xdWindow, xdEvent)
//WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
  return 0L;
}

int MakeVolumenBerechnung(WSP_PROFIL_LISTE *pWPL)
{
  BOOLEAN error=FALSE;
  STR_DATA *str_data,*new_data,*save_data;
  char buf[200];//Dick 26.11.99
  var_dlg135 = TRUE;   // 2 Buttons disabled setzen
  str_data=new  STR_DATA;
  new_data=new  STR_DATA;
  save_data=new  STR_DATA;//Dick 21.07.99
  
  InitStrData(str_data);
  InitStrData(new_data);
  //Dick 21.07.99
  InitStrData(save_data);
  
  if (strang_anfang!=NULL)//Dick 21.07.99 darf in diese Finktion nur 1 Mall aufgerufen 
    destroy_strang_tabelle();
  strang_anfang=NULL;
  MakeNewStrang(STRANGANZAHL);
  read_profil_dat(strang_anfang);  // Strangtabelle 1.Vernetzungsdatei lesen			  
  Save_Global_StrData(save_data);
  {
    long dialoge = xvt_dlg_create_res( WD_MODELESS, DLG_164, EM_ALL, DLG_164_eh, 0L );
    Ber_edit_sperr=TRUE;//Dick 20.07.99 damit Title nicht geändert wird
    if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
    {
      xvt_dm_post_error("Can't open dialog 135");
      error = TRUE;
    }
    Ber_edit_sperr=FALSE;//Dick 20.07.99
    var_dlg135=FALSE;
    xvt_vobj_destroy(dialoge);
    if (!dlg135ref) 
    {
      Get_Global_StrData(save_data);
      delete str_data;
      delete new_data;
      delete save_data;
      return 1;//Dick 3.12.98 für Abbruch
    }
  }
  if (!error) // 1.STR-Datei in jedem Fall neu lesen
  {
    strang_anfang=NULL;
    MakeNewStrang(STRANGANZAHL);
    read_profil_dat(strang_anfang);  // Strangtabelle 1.Vernetzungsdatei lesen
  }
  /* ab hier sollte erste Vernetzungsdatei vorhanden sein*/
  
  // jetzt zweite STR lesen
  if(Save_Global_StrData(str_data))  //globale Daten  1.STR sichern
  {
    var_dlg135 = TRUE;   // 2 Buttons disabled setzen
    
    long dialoge = xvt_dlg_create_res( WD_MODELESS, DLG_165, EM_ALL, DLG_165_eh, 0L );
    
    Ber_edit_sperr=TRUE;//Dick 20.07.99
    if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
    {
      xvt_dm_post_error("Can't open dialog 135");
      error = TRUE;
    }
    Ber_edit_sperr=FALSE;//Dick 20.07.99
    xvt_vobj_destroy(dialoge);
    var_dlg135=FALSE;
    
    if (!dlg135ref) 
    {
      Get_Global_StrData(save_data);
      delete str_data;
      delete new_data;
      delete save_data;
      return 1;//Dick 3.12.98 für Abbruch
    }
    
    if (!error)
			 {              
      strang_anfang=NULL;   // Adresse von 1.STR ist in str_data
      MakeNewStrang(STRANGANZAHL);
      read_profil_dat(strang_anfang);  // Strangtabelle 2.Vernetzungsdatei lesen
			 }
    if(! Save_Global_StrData(new_data))  //globale Daten  2.STR sichern
				{
      ;// Fehlerbehandlung
				}
    if (!error)
      if ( xvt_str_match(str_data->file.name,new_data->file.name,FALSE) )
      {
        //Dick 26.11.99
        xvt_res_get_str(STR_MASSE_NOTE_1,buf,sizeof(buf));
        xvt_dm_post_note("%s",buf); 
        //xvt_dm_post_note("Vernetzungsdateien sind identisch.\n\nBitte andere Wahl!");
        if(!Get_Global_StrData(str_data))
        {
          //xvt_dm_post_error("Fehler beim Rückladen der Vernetzungsdatei! #1");
          xvt_res_get_str(STR_MASSE_NOTE_2,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
        }
        error=TRUE;
      }
      Get_Global_StrData(str_data); //globale Daten von 1.STR zurückschreiben
      if (!error)
      {
        switch (VolReadGelaendeWerte(pWPL,str_data,new_data))
        {
        case 0:break; //OK
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
        case 6:
          xvt_res_get_str(STR_MASSE_NOTE_6,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
          //xvt_dm_post_note("Massenberechnung kann nicht durchgeführt werden.");
          break;
        };
      }
  }
  //Dick 21.07.99
  Get_Global_StrData(save_data);
  delete str_data;
  delete new_data;
  delete save_data;
  //
  return 1;
}
/*-------------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------------*/
int GetProfilName(WSP_SLIST *liste,double station,char *profil_name)
{ /*  In liste (:=Profiltabelle)  zu station den Profildateinamen suchen */
  WSP_SLIST *tmp;
  char gewaesser[15],
    *ptr;
  double station1;
  
  tmp = liste;
  if (tmp)
  {
    while (tmp !=NULL)
    {
      if(tmp->string!=NULL)
      {
        sscanf(tmp->string,"%s%lf",gewaesser,&station1);
        if (station == station1)
        {
          ptr = &tmp->string[44];  //ab Position 44 steht der Dateiname
          strncpy(profil_name,ptr,12);
          profil_name[12]='\0';
          return 1;
        }
      }
      tmp = tmp->next;
    }
  }
  else return 0;
  return 0;
}
/*-------------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------------*/
int VolReadGelaendeWerte(WSP_PROFIL_LISTE *pWPL, STR_DATA *data1,STR_DATA *data2)
/* Daten aus 1. und 2. Vernetzungsdatei für Volumenberechnung
in gelaende einlesen

  data1 		 : 1. Vernetzungsdatei
  data2 		 : 2. Vernetzungsdatei
  
    *vol         : Ablegen der Daten
    
      Rückgabe:  0 : OK
				  1 : Fehler:Protokolldatei läßt sich nicht öffnen
          2 : Vernetzungsdaten nicht vorhanden
          3 : Fehler beim Lesen einer Profildatei
          4 : Fehler in einer Profildatei: erster/letzter Punkt
          5 :
          6 : sonst.Fehler
          */
{
  int  zustand,counter;
  double abstand;
  char *temp,
    *ptr,
    //	gewaesser[10],
    prof_name1[15],
    prof_name2[15];
  double station1,
		  station2,
      gesamt_masse,
      gesamt_auftragsmasse,
      gesamt_abtragsmasse;
  FILE *output_file;
  
  WSP_PROFIL_LISTE *tmpWPL;
  // WSP_SLIST        *tmp_slist1;
  VOLUMEN          *volumen;
  STRANG			   *strang_ptr,*pre_ptr;
  
  xvt_scr_set_busy_cursor();
  
  temp = new char[256];
  gesamt_masse =0.0;
  gesamt_auftragsmasse=gesamt_abtragsmasse=0.0;
  
  
  xvt_fsys_convert_dir_to_str(&data1->file.dir,temp,255);
  ptr = strrchr(temp,'\\');
  strcpy(++ptr,"dath");
  strcat(temp,"\\masse.txt");
  
  
  output_file = fopen(temp,"w+");
  if (output_file==NULL)
	 {
    char buf[200],buf2[200];//Dick 26.11.99
    xvt_res_get_str(STR_PROTOKOLLDATEI,buf,sizeof(buf));
    xvt_res_get_str(STR_PLOT_NOTE_5,buf2,sizeof(buf2));
    xvt_dm_post_note("%s %s",buf,buf2);
    //xvt_dm_post_note("Protokolldatei läßt sich nicht öffnen");
    delete[] temp;
    return 1;
	 }
  if (data1->ptr_anfang !=NULL)
  {
    strang_ptr = pre_ptr = data1->ptr_anfang;
    counter =1;
  }
  
  VolWriteHeader(output_file,data1,data2);  //Kopf der Protokolldatei schreiben
  
  
  if ((data1==NULL)||(data2==NULL))
  {
    fclose(output_file);
    delete[] temp;
    return 2;
  }
  if ( xvt_str_match(data1->str_gewaesser,data2->str_gewaesser,FALSE) )
  {
    if ((data1->anzahl_strang_entries>0)&&(data2->anzahl_strang_entries>0))
    {
      for (int i=0;i <=  data1->anzahl_strang_entries;i++)   // korrektur +1
      {
        if (i< data1->anzahl_strang_entries)
          station1 = strang_ptr->anfang;
        else
          station1 = station2;
        
        if (GetProfilName(data1->prof_datei,station1,prof_name1))
          if (GetProfilName(data2->prof_datei,station1,prof_name2))
          {
            /* 2 Profile mit gleicher Stationierung gefunden*/
            
            pWPL=Init_Profil_Liste(pWPL);//2 neue WPL-Elemente anhängen
            pWPL=Init_Profil_Liste(pWPL);
            tmpWPL = pWPL->PListNext;
            
            strcpy(file_spec.name,prof_name1); // obsolet?
            if (read_profildatei(tmpWPL, &STR_SPEC.dir, prof_name1 ))   // 1.list
            {
              Delete_Profil_Liste(tmpWPL);
              pWPL->PListNext=NULL;
              fclose(output_file);
              delete[] temp;
              return 3;  //im Fehlerfall gibt read_profildatei TRUE zurück
            }
            SaveGlobalData(tmpWPL->data);
            
            strcpy(file_spec.name,prof_name2); // obsolet?
            if( read_profildatei( tmpWPL->PListNext, &STR_SPEC.dir, prof_name2 ) )  // 2.list
            {
              Delete_Profil_Liste(tmpWPL);
              pWPL->PListNext=NULL;
              fclose(output_file);
              delete[] temp;
              return 3; //im Fehlerfall gibt read_profildatei TRUE zurück
            }
            SaveGlobalData(tmpWPL->PListNext->data);
            
            GetGlobalData(tmpWPL->data);
            
            /* Testen,ob erster und letzter Profilpunkt identisch sind*/
            zustand =tmpWPL->PList->Check_Gel1_Gel2_Daten(tmpWPL->PListNext->PList);
            switch (zustand)
            {
            case 0:  // erster und letzter Profilpunkt identisch
              break;
            case 1:  // erste Profilpunkte stimmen nicht überein
            case 2:  // letzte Profilpunkte stimmen nicht überein
            case 3:  // sonstiger Fehler (GELAENDEHOEHE nicht gefunden)
              Display_Errorbox1(prof_name1,prof_name2,station1,zustand);
              Delete_Profil_Liste(tmpWPL);
              pWPL->PListNext=NULL;
              delete[] temp;
              fclose(output_file);
              return 4;
            };
            
            volumen = new VOLUMEN;
            InitVolumen(volumen);
            if (volumen == NULL)     // Fehler
            {
              Delete_Profil_Liste(tmpWPL);
              pWPL->PListNext=NULL;
              delete[] temp;
              fclose(output_file);
              return 6;
            }
            switch ( VolMakeGelDaten( tmpWPL,volumen))
            {  // Datensatz nicht vorhanden oder allg.Fehler
            case 0: break;  //OK
            case 1:
              {
                //xvt_dm_post_note("In einem der Profile ist kein Bordvollpunkt bzw. keine",
                //					 "Trennfläche vorhanden !");
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_MASSE_NOTE_3,buf,sizeof(buf));
                xvt_dm_post_note("%s",buf);
                
              }
            case 2:
              {
                //xvt_dm_post_error("Fehler:\nBitte Bordvoll oder Trennflächen\nin allen Profilen definieren!");
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_MASSE_NOTE_4,buf,sizeof(buf));
                xvt_dm_post_note("%s",buf);
              }
              Delete_Profil_Liste(tmpWPL);
              pWPL->PListNext=NULL;
              delete[] temp;
              fclose(output_file);
              return 5;
            }/*jetzt sind alle Daten der beiden Profile in 'vol' eingelesen*/
            
            volumen->lpdaten->station = station1;
            
            if (MakeVolumeDatenLinks(volumen))
            {
              /*Ausgabe der Daten in Datei*/
              if (counter==1)
                abstand = atof(strang_ptr->abstand_links)*1000/2 ;
              else
              {
                if (counter ==data1->anzahl_strang_entries+1)
                  abstand = atof(pre_ptr->abstand_links)*1000/2 ;
                else
                  abstand = (  atof(pre_ptr->abstand_links)+ atof(strang_ptr->abstand_links) )*1000/2;
              }
              VolPrintProtokoll(output_file,volumen,abstand,station1,LINKS);
              gesamt_masse = gesamt_masse + (volumen->lpdaten->summe * abstand) ;
              gesamt_auftragsmasse=gesamt_auftragsmasse + (volumen->lpdaten->sum_auftrag * abstand);
              gesamt_abtragsmasse=gesamt_abtragsmasse + (volumen->lpdaten->sum_abtrag * abstand);
            }
            else fprintf(output_file,"*****    Station:  %4.4lf \tFehler in der Profildatei *****\n",station1);
            
            volumen =VolDeleteLPDaten(volumen);
            volumen = VolInitLPDaten(volumen);
            volumen->lpdaten->station = station1;
            //---------------------------------------
            if (MakeVolumeDatenFluss(volumen))
            {
              if (counter==1)
                abstand = atof(strang_ptr->abstand_fluss)*1000/2 ;
              else
              {
                if (counter ==data1->anzahl_strang_entries+1)
                  abstand = atof(pre_ptr->abstand_fluss)*1000/2 ;
                else
                  abstand = (  atof(pre_ptr->abstand_fluss)+ atof(strang_ptr->abstand_fluss) )*1000/2;
              }
              VolPrintProtokoll(output_file,volumen,abstand,station1,FLUSS);
              gesamt_masse = gesamt_masse + (volumen->lpdaten->summe * abstand) ;
              gesamt_auftragsmasse=gesamt_auftragsmasse + (volumen->lpdaten->sum_auftrag * abstand);
              gesamt_abtragsmasse=gesamt_abtragsmasse + (volumen->lpdaten->sum_abtrag * abstand);
              
            }
            else fprintf(output_file,"*****    Station:  %4.4lf \tFehler in der Profildatei *****\n",station1);
            
            volumen =VolDeleteLPDaten(volumen);
            volumen = VolInitLPDaten(volumen);
            volumen->lpdaten->station = station1;
            //---------------------------------------
            if (MakeVolumeDatenRechts(volumen))
            {
              if (counter==1)
                abstand = atof(strang_ptr->abstand_rechts)*1000/2 ;
              else
              {
                if (counter ==data1->anzahl_strang_entries+1)
                  abstand = atof(pre_ptr->abstand_rechts)*1000/2 ;
                else
                  abstand = (  atof(pre_ptr->abstand_rechts)+ atof(strang_ptr->abstand_rechts) )*1000/2;
              }
              VolPrintProtokoll(output_file,volumen,abstand,station1,RECHTS);
              gesamt_masse = gesamt_masse + (volumen->lpdaten->summe * abstand) ;
              gesamt_auftragsmasse=gesamt_auftragsmasse + (volumen->lpdaten->sum_auftrag * abstand);
              gesamt_abtragsmasse=gesamt_abtragsmasse + (volumen->lpdaten->sum_abtrag * abstand);
              
            }
            else fprintf(output_file,"*****    Station:  %4.4lf \tFehler in der Profildatei *****\n",station1);
            
            counter++;
            pre_ptr = strang_ptr;  // pre_ptr zeigt auf alten strang_ptr
            if (strang_ptr->next !=NULL)
              strang_ptr = strang_ptr->next;
            
            volumen = DeleteVolumen(volumen);
            delete volumen;
            volumen = NULL;
            Delete_Profil_Liste(tmpWPL);
            pWPL->PListNext=NULL;
         }
         
         if (i == data1->anzahl_strang_entries-2)
           station2 = strang_ptr->ende;
         
    } // -for...
   }
  }
  
  /*Abschlusszeile: Gesamtsumme schreiben und Datei schliessen*/
  for (int i=0;i<20;i++)
    fprintf(output_file," ");
  for (i=0;i<96;i++)
    fprintf(output_file,"_");
  fprintf(output_file,"\n");
  
  for (i=0;i<20;i++)
    fprintf(output_file," ");
  fprintf(output_file,"Gesamtmassen [m³]:         %15.4lf               %15.4lf       %15.4lf\n\n",gesamt_auftragsmasse,gesamt_abtragsmasse,gesamt_masse,gesamt_auftragsmasse,gesamt_abtragsmasse);
  
  for (i=0;i<116;i++)
    fprintf(output_file,"_");
  fprintf(output_file,"\n");
  fclose(output_file);
  delete[] temp;
  
  //xvt_dm_post_note("Massenberechnung beendet.\nErgebnisdaten wurden in der Datei\nMASSE.TXT abgelegt.");
  char buf[200];//Dick 26.11.99
  xvt_res_get_str(STR_MASSE_NOTE_5,buf,sizeof(buf));
  xvt_dm_post_note("%s",buf);
  return 0;
}
/*-------------------------------------------------------------------------------*/
int VolMakeGelDaten(WSP_PROFIL_LISTE *tmpWPL,VOLUMEN *vol)
{
/*  ProfilDaten  (GELAENDE, TRENNFLAECHEN)aus tmpWPL(1.Profil)
und tmpWPL->PListNext(2.Profil) in Struct VOLUMEN kopieren
RETURN : 0 OK
1 Fehler in tmpWPL
2 Fehler kein BORDVOLL/TRENNFLAECHE
  */
  int ds,error=1;
  GELAENDE *tmp;
  
  if ((tmpWPL ==NULL)||(tmpWPL->PListNext==NULL)
    ||(tmpWPL->PList==NULL)||(tmpWPL->PListNext->PList==NULL) ) return 1;
  
  if (  (ds=tmpWPL->PList->ExistDatensatzTyp(BORDVOLL))==0  )
    ds = tmpWPL->PList->ExistDatensatzTyp(TRENNFLAECHEN);
  if (ds)
	 {
    vol->tr1=MakeLKoord(vol->tr1,tmpWPL->data->ds_info[ds]); // 1. Trennfläche
    error=tmpWPL->PList->CopyGelDatenToGelaende(vol->tr1,TRENNFLAECHEN,
      tmpWPL->data->ds_info[ds]);
    if (error)	  return 1;
	 }
  else return 2;
  
  if (  (ds=tmpWPL->PListNext->PList->ExistDatensatzTyp(BORDVOLL))==0  )
    ds = tmpWPL->PListNext->PList->ExistDatensatzTyp(TRENNFLAECHEN);
  if (ds)
	 {
    vol->tr2=MakeLKoord(vol->tr2,tmpWPL->PListNext->data->ds_info[ds]); // 2. Trennfläche
    error=tmpWPL->PListNext->PList->CopyGelDatenToGelaende(vol->tr2,TRENNFLAECHEN,
      tmpWPL->PListNext->data->ds_info[ds]);
    if (error) return 1;
	 }
  else
	 {
    DeleteLKoord(vol->tr1); //vol->tr1 löschen
    return 2;
	 }
  vol->gel1=MakeLKoord(vol->gel1,tmpWPL->data->ds_info[1]);   // Gelände 1.Profil
  error=tmpWPL->PList->CopyGelDatenToGelaende(vol->gel1,GELAENDEHOEHE,
    tmpWPL->data->ds_info[1]);
  if (error)
	 {
    return 1;
	 }
  vol->gel2=MakeLKoord(vol->gel2,tmpWPL->PListNext->data->ds_info[1]);  // Gelände 2.Profil
  error=tmpWPL->PListNext->PList->CopyGelDatenToGelaende(vol->gel2,GELAENDEHOEHE,
    tmpWPL->PListNext->data->ds_info[1]);
    /* Da in Trennflächen als z-Komponente nur links/rechts (=1/2) steht muss
    vol->tr1 und vol->tr2 korrigiert werden und Höhenwerte aus Gelände geholt
  werden    */
  tmp = vol->gel1;
  while ( tmp && tmp->y != vol->tr1->y )  // links
    tmp = tmp->next_ds;
  if (tmp->y == vol->tr1->y)
    (vol->tr1->z = tmp->z);
  else vol->tr1->z = BCE_NAN;
  tmp = vol->gel1;
  while ((tmp)&&(tmp->y != vol->tr1->next_ds->y)) //rechts
    tmp = tmp->next_ds;
  if (tmp->y == vol->tr1->next_ds->y)
    (vol->tr1->next_ds->z = tmp->z);
  else vol->tr1->next_ds->z = BCE_NAN;
  
  
  tmp = vol->gel2;
  while ((tmp)&&(tmp->y != vol->tr2->y))  // links
    tmp = tmp->next_ds;
  if (tmp->y == vol->tr2->y)
    (vol->tr2->z = tmp->z);
  else vol->tr2->z = BCE_NAN;
  tmp = vol->gel2;
  while ((tmp)&&(tmp->y != vol->tr2->next_ds->y)) //rechts
    tmp = tmp->next_ds;
  if (tmp->y == vol->tr2->next_ds->y)
    (vol->tr2->next_ds->z = tmp->z);
  else vol->tr2->next_ds->z = BCE_NAN;
  /*  Ende Korrektur*/
  
  if (error) return 1;
  return 0;
}
/*-------------------------------------------------------------------------------*/
//NEU:  vormals  in DLL
/*-------------------------------------------------------------------------------*/
void  VolWriteHeader(FILE *outfile,STR_DATA *data,STR_DATA *ref)
{
  char str_datum[40],tmp[100],*ptr;
  FILE *out;
  out = outfile;
  
  if (out==NULL) return;
  for (int i=0;i<116;i++)	 fprintf(out,"_"); 
  fprintf(out,"\n\n");
  
  fprintf(out,"               Protokoll zur Massenberechnung:");
  
  GetDateString(str_datum);
  
  fprintf(out,"\t%s\n",str_datum);
  
  for (i=0;i<116;i++)	 fprintf(out,"_"); fprintf(out,"\n\n");
  
  
  ptr = &tmp[40];
  for (i=0;i<80;i++) tmp[i]=' ';  //init
  strcpy(tmp,"Referenzzustand:   ");strcat(tmp,data->file.name);tmp[strlen(tmp)]=' ';
  strcpy(ptr,"Zustand: ");strcat(ptr,data->str_zustand);
  fprintf(out,"%s\n",tmp);
  
  ptr = &tmp[40];
  for (i=0;i<80;i++) tmp[i]=' ';  //init
  strcpy(tmp,"Vergleichszustand: ");strcat(tmp,ref->file.name);tmp[strlen(tmp)]=' ';
  strcpy(ptr,"Zustand: ");strcat(ptr,ref->str_zustand);
  fprintf(out,"%s\n\n",tmp);
  
  fprintf(out,"                Station: Strecke: Abtragsfläche: Abtragsmasse: Auftragsfläche: Auftragsmasse:  Summe:     Masse:\n");
  fprintf(out,"                 [km]      [m]        [m²]           [m³]            [m²]           [m³]        [m²]       [m³] \n");
  
  
  for (i=0;i<116;i++) 	fprintf(out,"_");fprintf(out,"\n\n");
}
/*-------------------------------------------------------------------------------*/
int VolPrintProtokoll(FILE *out,VOLUMEN *vol,double abstand,double station,int action)
{
  static double abschnittsvolumen;
  static double auftragsmasse,abtragsmasse;
  switch (action)
  {
  case LINKS:
    abschnittsvolumen=0.0;
    auftragsmasse=abtragsmasse=0.0;
    fprintf(out,"linkes Vorland :");
    fprintf(out,"%8.4lf",station);
    break;
  case FLUSS:
    fprintf(out,"Fluss          :");
    fprintf(out,"        ");
    break;
  case RECHTS:
    fprintf(out,"rechtes Vorland:");
    fprintf(out,"        ");
    break;
  default:break;
  };
  fprintf(out," %8.4lf",abstand);
  fprintf(out,"%15.4lf",vol->lpdaten->sum_auftrag);
  fprintf(out,"%15.4lf",(vol->lpdaten->sum_auftrag* abstand));//Dick 15.09.99
  fprintf(out,"%15.4lf",vol->lpdaten->sum_abtrag);
  fprintf(out,"%15.4lf",(vol->lpdaten->sum_abtrag* abstand));//Dick 15.09.99
  fprintf(out,"%12.4lf",vol->lpdaten->summe);
  fprintf(out," %9.4lf\n",(vol->lpdaten->summe * abstand) ); //Masse
  
  abschnittsvolumen = abschnittsvolumen + (vol->lpdaten->summe * abstand);
  auftragsmasse=auftragsmasse + (vol->lpdaten->sum_auftrag* abstand);
  abtragsmasse=abtragsmasse + (vol->lpdaten->sum_abtrag* abstand);;
  if (action ==RECHTS)
  {
    for (int i=0;i<20;i++) fprintf(out," ");
    fprintf(out,"Massen des Abschnitts:         %12.4lf                  %12.4lf          %12.4lf\n\n",auftragsmasse,abtragsmasse,abschnittsvolumen);
    abschnittsvolumen=0.0;
    auftragsmasse=abtragsmasse=0.0;
  }
  return 0;
}
/*-------------------------------------------------------------------------------*/
