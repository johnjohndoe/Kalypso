#include <windows.h>
#include <math.h>
#include "xvt.h"

#include "global_types.h"
#include "global_vars.h"

#include "l_typen.h"

#include "readprof.h"
#include "wsplist.h"
#include "c_laengs.h"
#include "slist.h"
#include "strang.h"
#include "..\..\wspdlg\Include\export.h"

#include "laengs1.h"

// Vorwaertsdeklarationen
int BuildLPHeader( WSP_PROFIL_LISTE *wpl);
int BuildLP_Line15( WSP_PROFIL_LISTE *tmpWPL);
int BuildLPBauwerk( LaengsProfil *lp,WSP_PROFIL_LISTE *tmpWPL);
int BuildLP_WSF(LaengsProfil *lp,WSP_PROFIL_LISTE *tmpWPL,FILE *wsf_datei,char *abfluss_var,char *lp_str_info);
int LwaData1ToWSP(WSP_PROFIL_LISTE *pWPL,LWA_DATA1 *data,int line,LaengsProfil *laengsprofil);
int LwaData2ToWSP(WSP_PROFIL_LISTE *pWPL,LWA_DATA2 *data,int line,LaengsProfil *laengsprofil);
int LwaData3ToWSP(WSP_PROFIL_LISTE *pWPL,LWA_DATA1 *data,int line, LaengsProfil *laengsprofil);
int StringToLwaData1(char *,LWA_DATA1*); // nur hier
int StringToLwaData2(WSP_PROFIL_LISTE *pWPL,char *,LWA_DATA2*); // nur hier
int StringToLwaData3(char *,LWA_DATA1*); // nur hier


// Implementation

int BuildLPComment( WSP_PROFIL_LISTE* pWPL, LaengsProfil* lp )
// liest aus den zugehörigen Querprofilen die Kommentare aus und fügt sie als Datensatz LP_TEXT
// zum Längschnitt hinzu
// Parameter:
//          LaengsProfil* lp: zeigt auf den Längschnitt
//
// Rückgabewert:
//          int: Fehlercode:
//                  0 kein Fehler
//                  1 Fehler beim erzeugen einer neuen Profilliste
//                  2 Fehler in den Längsschnittdaten
// Bemerkung:
//        es darf noch kein entsprechender Datensatz vorhanden sein, 
//        ebenso muss pWPL->data->slist_commen leer sein
{
  WSP_SLIST* kommentarList = NULL;
  int kommentCount = 0;


  // die Strangdatei mal wieder auslesen, falls noch nicht geschehen, damit prof_datei initialisiert ist
  if( strang_anfang )
    destroy_strang_tabelle();
  strang_anfang = NULL;
  MakeNewStrang( STRANGANZAHL );
  read_profil_dat( strang_anfang );

  // jetzt alles Profile nach kommentaren durchsuchen
  // die Stationen werden durch kilometer und vzk identifiziert
  int pos = 0; // position innerhalb des Längsschnittes
  Koord* pVZK = pWPL->PList->HoleDatensatz( VZKENNG );
  Koord* pSohl = pWPL->PList->HoleDatensatz( SOHLHOEHE );
  while( pSohl )
  {
    pos++;

    if( !pVZK || fabs( pVZK->x - pSohl->x ) > PRECISION_KRD ) 
      return 2; // es muss für jede Sohlhöhe genau eine VZ-Kennung geben

    // jetzt den entsprechende Strang suchen
    double station = (double)( pSohl->x / -1000.0 ); // wieder in stationskilometer umrechnen

    for( SLIST_ELT e = xvt_slist_get_first( prof_datei ); e; e = xvt_slist_get_next( prof_datei, e ) )
    {
      LPSTR profilStr = xvt_slist_get( prof_datei, e, NULL );
      if( !profilStr || strlen( profilStr ) < 55 )
        continue;

      // den ProfilStr parsen:
      double profilStation = atof( &profilStr[9] );
      int profilVzk = atoi( &profilStr[29] );
      char profilFile[MAX_PATH];
      strncpy( profilFile, &profilStr[44], MAX_PATH );

      if( fabs( station - profilStation ) < PRECISION_KRD && (int)pVZK->y == profilVzk )
      {
        // die ProfilDatei öffnen, den Kommentar auslesen und den Kommentar an unseren anhängen
        pWPL = Init_Profil_Liste( pWPL );//neues WPL-Element anhängen
        if( !pWPL->PListNext )
          return 1;

        WSP_PROFIL_LISTE* qpWPL = pWPL->PListNext;
        if( read_profildatei( qpWPL, &STR_SPEC.dir, profilFile, false ) == 0 )  // Querprofil einlesen
        { // Daten nach Querprofil
          // Kommentar auslesen

          int count = WspSListCountElem( qpWPL->data->slist_comment );
          if( count > 0 )
          {
            // die erste Zeile des Textblocks erzeugen
            char data[MAX_PATH];
            sprintf( data, "%d 4 2 2 %d 2", count, pos );
            kommentarList = AppendStringToWspSList( kommentarList, data ); 

            // un den Kommentar anhängen
            WSP_SLIST* sList = qpWPL->data->slist_comment;
            while( sList )
            {
              kommentarList = AppendStringToWspSList( kommentarList, sList->string );
              sList = sList->next;
            };

            kommentCount++;
          }; // if count > 0
        }; // if read_profildatei

          // und Querprofil wieder löschen
          Delete_Profil_Liste( pWPL->PListNext );
          pWPL->PListNext = NULL;

          break;
      }; // falls station gefunden
    }; // while pStrang

    pSohl = pSohl->next_ds;
    pVZK = pVZK->next_ds;
  }; // while pSohl


  // zuletzt den erzeugten Kommentar dem Längschnitt hinzufügen
  if( kommentCount > 0 )
  {
    pWPL->data->exist_comment = TRUE; // es gibt einen Kommentar

    // erste Zeil eerzeugen
    char data[MAX_PATH];
    sprintf( data, "1 %d 5.0", kommentCount );
    pWPL->data->slist_comment = AppendStringToWspSList( NULL, data );

    // den Rest dranhängen
    pWPL->data->slist_comment->next = kommentarList;

    // und noch einen Knoten 'TEXT' im Längsprofil erzeugen
    int anzahlDs = pWPL->data->ds_info[0];
    anzahlDs++;
    lp->MakeNewNode( anzahlDs );
    lp->WriteLPTypDaten( anzahlDs, LP_TEXT );
    pWPL->data->ds_info[0]++;
    pWPL->data->typ[anzahlDs] = LP_TEXT;
    pWPL->data->ds_info[anzahlDs] = WspSListCountElem( kommentarList );
  }
  else
    DeleteWspSList( kommentarList );

  return 0;
}; // BuildLPComment



int ConvertLaengsprofilLwaToBce(char* file_lwa,char* file_bce,WSP_PROFIL_LISTE *pWPL,char* file_str,char* ber_var,char* abfluss_var,int enable_wsf)
//    LWA - Längsprofil in das BCE - WSPWIN Format konvertieren
//    
//      Parameter:
//      char* file_lwa  : LWA-Eingabedatei incl. kompl. Pfadangabe
//      char* file_bce  : BCE-Ausgabedatei incl. kompl. Pfadangabe
//      WSP_PROFIL_LISTE *pWPL: Zeiger aus Hauptprogramm
//      char* file_str  : Kompletter Pfad der Strangdatei
//      char* ber_var   : Name der Berechnungsvariante
//      char* abfluss_var: Name des Abflusses
//      int enable_wsf  :???
//      
//        Rückgabe:  0:  OK , keine Fehler
//        1:  fopen -Fehler oder Dateiname ungültig
//        2:
{
  WSP_PROFIL_LISTE *tmpWPL;
  FILE *lwa_in;
  FILE *lwa_str;
  
  LWA_DATA1 *lwa_data1;
  LWA_DATA2 *lwa_data2;
  
  
  
  BYTE second_line, db2;
  BYTE exist_line2,existDKUK;
  char *temp,*str;
  double station2;
  int datensatz,line,anzahl;
  int dummy1,dummy2;
  char lp_str_info[200];
  char gewaesser_name[20],zustand[20];
  int anzahl_datensaetze = 14;
  
  if (file_lwa==NULL)
	   return 1;
  if ((lwa_in = fopen(file_lwa,"r+"))==NULL)	
    return 1;
  
  if (file_str==NULL)
	   return 1;
  if ((lwa_str = fopen(file_str,"r+"))==NULL)	
    return 1;
  
  fscanf(lwa_str,"%d%d%s%s",&dummy1,&dummy2,gewaesser_name,zustand);
  fclose(lwa_str);
  strcpy(lp_str_info," ");
  for(int i=0;i<99;i++)
    strcat(lp_str_info," ");
  strcat(lp_str_info,ber_var);
  strcat(lp_str_info,"@");
  strcat(lp_str_info,gewaesser_name);
  strcat(lp_str_info,"@");
  strcat(lp_str_info,zustand);
  
  lwa_data1    = new LWA_DATA1;
  lwa_data2    = new LWA_DATA2;
  
  temp = new char[121];//Dick 23.09.99 100->121
  str  = new char[25];
  
  
  pWPL=Init_Profil_Liste(pWPL);//neues WPL-Element anhängen
  tmpWPL = pWPL->PListNext;

  LaengsProfil* laengsprofil = new LaengsProfil();

  // den Dateinamen setzen
  char tmpPath[MAX_PATH];
  strcpy( tmpPath, file_bce );
  char* tmpFileName = strrchr( tmpPath, '\\' );
  if( tmpFileName )
  {
    strcpy( tmpWPL->data->file.name, tmpFileName + 1 );
    tmpFileName[0] = '\0'; // kürzt tmpPath
    xvt_fsys_convert_str_to_dir( tmpPath, &tmpWPL->data->file.dir );
  }
  else
    return 1; // fehler
  
  if (tmpWPL->PList!=NULL)
    tmpWPL->PList->DeleteKoord(0);
  
  tmpWPL->PList->SetPtrAnfang(laengsprofil->get_anfang());
  tmpWPL->dummy = (LAENGSPROFIL*)laengsprofil;
  
  BuildLPHeader(tmpWPL);  // 15 Kopfzeilen generieren
  
  
  // ** 9(10) Datenblöcke erzeugen **
  /* Datensatz 1 wurde durch "new LaengsProfil"  erzeugt !  */
  for ( datensatz = 2; datensatz <= anzahl_datensaetze; datensatz++ )
    laengsprofil->MakeNewNode(datensatz);
  
  laengsprofil->WriteLPTypDaten(1,SOHLHOEHE,lp_str_info);
  laengsprofil->WriteLPTypDaten(2,BOESCHUNG_LINKS,lp_str_info);
  laengsprofil->WriteLPTypDaten(3,BOESCHUNG_RECHTS,lp_str_info);
  laengsprofil->WriteLPTypDaten(4,WASSERSPIEGEL,lp_str_info);
  laengsprofil->WriteLPTypDaten(5,ABFLUSS);
  laengsprofil->WriteLPTypDaten(6,LAENGE);
  laengsprofil->WriteLPTypDaten(7,PROFILART);
  laengsprofil->WriteLPTypDaten(8,VZKENNG);
  laengsprofil->WriteLPTypDaten(9,WSP_BREITE);
  laengsprofil->WriteLPTypDaten(10,PROFILKENNG);
  laengsprofil->WriteLPTypDaten(11,SCHLEPPSPANNUNG);
  laengsprofil->WriteLPTypDaten(12,AUSUFERUNG_LINKS);
  laengsprofil->WriteLPTypDaten(13,AUSUFERUNG_RECHTS);
  laengsprofil->WriteLPTypDaten(14,ENERGIEHOEHE);
  
  tmpWPL->data->typ[0]=0;
  tmpWPL->data->typ[1]=  SOHLHOEHE;
  tmpWPL->data->typ[2]=  BOESCHUNG_LINKS;
  tmpWPL->data->typ[3]=  BOESCHUNG_RECHTS;
  tmpWPL->data->typ[4]=  WASSERSPIEGEL;
  tmpWPL->data->typ[5]=  ABFLUSS;
  tmpWPL->data->typ[6]=  LAENGE;
  tmpWPL->data->typ[7]=  PROFILART;
  tmpWPL->data->typ[8]=  VZKENNG;
  tmpWPL->data->typ[9]=  WSP_BREITE;
  tmpWPL->data->typ[10]= PROFILKENNG;
  tmpWPL->data->typ[11]=  SCHLEPPSPANNUNG;
  tmpWPL->data->typ[12]=  AUSUFERUNG_LINKS;
  tmpWPL->data->typ[13]=  AUSUFERUNG_RECHTS;
  tmpWPL->data->typ[14]= ENERGIEHOEHE;
  
  /** ende -15 Datenblöcke erzeugen **/
  
  datensatz   = 1;
  line        = 0;
  second_line = FALSE;
  db2         = FALSE;
  
  exist_line2 = FALSE;    // 2.Datenzeile
  
  int anzahlLinienDatensatz2 = 0; // Anzahl der gelesenen Zeilen des 2. Datenblocks
  LWA_DATA1* pLwa_Data1 = NULL;  // Zeigt auf die gelesenen Daten des 2. Datenblockes
  bool alteBerechnung = false; // ob Berechnungsergebnisse im alten oder neuen Format
  
  while (!feof(lwa_in))
  {
    if(fgets(temp,120,lwa_in))//Dick 23.09.99 99->120
    {
      line ++;
      if( line == 1 && strlen(temp) < 100 ) //wenn noch alte Berechnungsprogramm
      {
        laengsprofil->DeleteNode( 14, tmpWPL->data->ds_info, tmpWPL->data->typ );
        laengsprofil->DeleteNode( 13, tmpWPL->data->ds_info, tmpWPL->data->typ );
        laengsprofil->DeleteNode( 12, tmpWPL->data->ds_info, tmpWPL->data->typ );
        laengsprofil->DeleteNode( 11, tmpWPL->data->ds_info, tmpWPL->data->typ );
        anzahl_datensaetze = 10;
        alteBerechnung = true;
      }; // if line == 1
      
      if( !alteBerechnung && temp[27] != ' ' )   // die nächste Zeile ist vom Typ: Datenblock 2
        db2 = TRUE;  // :=Datenblock 2
      
      if (!second_line)
      {
        if (db2)  // Datenblock 2 lesen
        {
          anzahlLinienDatensatz2++;
          pLwa_Data1 = 
            (LWA_DATA1*)realloc( pLwa_Data1, anzahlLinienDatensatz2 * sizeof(LWA_DATA1) );
          StringToLwaData3( temp, &pLwa_Data1[anzahlLinienDatensatz2 - 1] );
          
          db2 =FALSE;
        }
        else // Datenblock 1
        {
          StringToLwaData1(temp,lwa_data1);
          if (lwa_data1->idp >0)  //Sonderprofil -es gibt eine zweite Zeile
            second_line = TRUE;
          else second_line = FALSE;
          LwaData1ToWSP(tmpWPL,lwa_data1,line, laengsprofil);
          
        }
      }
      else  // Zeile ist vom Typ: 2.Zeile
      {
        for (int i=0;i<=9;i++)  str[i] = temp[i];
        str[10]='\0';
        station2 = atof(str);
        station2=station2*(-1);
        if( station2 == lwa_data1->station )
        {
          if (!exist_line2) // Datenblock für Zeile 2 neu anlegen
          {
            anzahl_datensaetze++;
            laengsprofil->MakeNewNode(anzahl_datensaetze);
            laengsprofil->WriteLPTypDaten(anzahl_datensaetze,DKUK);
            tmpWPL->data->typ[anzahl_datensaetze]= DKUK;
            
            anzahl_datensaetze++; 
            laengsprofil->MakeNewNode(anzahl_datensaetze);
            laengsprofil->WriteLPTypDaten(anzahl_datensaetze,DKOK);
            tmpWPL->data->typ[anzahl_datensaetze]= DKOK;
            
            existDKUK=TRUE;
            
            /*
            anzahl_datensaetze++;
            laengsprofil->MakeNewNode(anzahl_datensaetze);
            laengsprofil->WriteLPTypDaten(anzahl_datensaetze,LP_TEXT);
            tmpWPL->data->typ[anzahl_datensaetze]= LP_TEXT;
            tmpWPL->data->slist_comment=AppendStringToWspSList(tmpWPL->data->slist_comment,"0 0 0");
            */
            exist_line2 = TRUE;
          }
          StringToLwaData2(tmpWPL,temp,lwa_data2);
          
          LwaData2ToWSP(tmpWPL,lwa_data2,line,laengsprofil);
          
          second_line = FALSE;
        }
        else // ätsch -in bin doch eine normale Zeile
        {
          if (db2)  // Datenblock 2 lesen
          {
            anzahlLinienDatensatz2++;
            pLwa_Data1 = 
              (LWA_DATA1*)realloc( pLwa_Data1, anzahlLinienDatensatz2 * sizeof(LWA_DATA1) );
            StringToLwaData3( temp, &pLwa_Data1[anzahlLinienDatensatz2 - 1] );
            
            db2 =FALSE;
          }
          else
          {
            StringToLwaData1(temp,lwa_data1);
            if (lwa_data1->idp >0)  //Sonderprofil -es gibt eine zweite Zeile
              second_line = TRUE;
            else second_line = FALSE;
            LwaData1ToWSP(tmpWPL,lwa_data1,line,laengsprofil);
          }
        }
        
      }
    }
  } //- while (!feof(lwa_in))
  
  // jetzt evtl 2. Wasserpiegel aus 2. Datenblock hinzufügen
  if ( anzahlLinienDatensatz2 > 0 )
  {
    anzahl_datensaetze++;
    laengsprofil->MakeNewNode( anzahl_datensaetze );
    laengsprofil->WriteLPTypDaten( anzahl_datensaetze, WASSERSPIEGEL_2 );
    tmpWPL->data->typ[anzahl_datensaetze]= WASSERSPIEGEL_2;
    
    for ( int i = 0; i < anzahlLinienDatensatz2; i++ )
      LwaData3ToWSP( tmpWPL, &pLwa_Data1[i], line + i, laengsprofil );
    
    free( pLwa_Data1 );
  }; // if anzahlLinien > 0
  
  
  // jetzt Profil sichern:
  laengsprofil->BuildDsInfo(tmpWPL);
  BuildLP_Line15(tmpWPL);  // Zeile 15 nach Bauwerk erneut aktualisieren

  if( LWA_PROJEKT )
    laengsprofil->Mehrfeldoptimierung();
  
  laengsprofil->BuildDsInfo(tmpWPL);
  BuildLP_Line15(tmpWPL);  // Zeile 15 nach Bauwerk erneut aktualisieren
  
  //  B A U W E R K E
  if (existDKUK)
    anzahl = BuildLPBauwerk(laengsprofil,tmpWPL);
  
  laengsprofil->BuildDsInfo(tmpWPL);
  BuildLP_Line15(tmpWPL);  // Zeile 15 nach Bauwerk erneut aktualisieren
  
  //Wasserspiegelfixirungen
  if(enable_wsf)
  {
    //aus *.str *.wsf Datei bauen
    char wsf[200];
    FILE *wsf_datei;
    int ok=0;
    if(file_str!=NULL && abfluss_var!=NULL)
      if(file_str[0]!='\0' && abfluss_var[0]!='\0')
      {
        strcpy(wsf,file_str);
        wsf[strlen(wsf)-3]='\0';
        strcat(wsf,"wsf");
        if ((wsf_datei = fopen(wsf,"r"))!=NULL)
        {
          ok=BuildLP_WSF(laengsprofil,tmpWPL,wsf_datei,abfluss_var,lp_str_info);
          fclose(wsf_datei);
        }
      }
      if(ok==1)
      {
        laengsprofil->BuildDsInfo(tmpWPL);
        BuildLP_Line15(tmpWPL);  // Zeile 15 nach Bauwerk erneut aktualisieren
      }
  }

  // Querprofil kommentare als Text einfügen
  BuildLPComment( tmpWPL, laengsprofil );
  laengsprofil->BuildDsInfo( tmpWPL );
  BuildLP_Line15( tmpWPL );

  save_profildatei( tmpWPL, FALSE );
  
  if (tmpWPL !=NULL)
    Delete_Profil_Liste(tmpWPL);
  tmpWPL = NULL;
  pWPL->PListNext=NULL;
  
  fclose(lwa_in);
  
  delete lwa_data1;
  delete lwa_data2;
  delete[] temp;
  delete[] str;
  return 1;
}

/*********************************************************************************
int ConvertLaengsprofilBceToBce(char* file_lwa char * file_bce,WSP_PROFIL_LISTE *pWPL)
LWA - Längsprofil in das BCE - WSPWIN Format konvertieren
char* file_lwa  : LWA-Eingabedatei incl. kompl. Pfadangabe
char* file_bce  : BCE-Ausgabedatei incl. kompl. Pfadangabe
WSP_PROFIL_LISTE *pWPL: Zeiger aus Hauptprogramm
Rückgabe:  0:  OK , keine Fehler
1:  fopen -Fehler oder Dateiname ungültig
2:
*********************************************************************************/
int ConvertLaengsprofilBceToBce(char* file_bce,WSP_PROFIL_LISTE *pWPL,char* file_str,char* ber_var,char* abfluss_var,int enable_wsf)
{
  WSP_PROFIL_LISTE *tmpWPL;
  
  FILE *lwa_str;
  
  LWA_DATA1 *lwa_data1;
  LWA_DATA2 *lwa_data2;
  
  BYTE existDKUK=FALSE;
  char *temp,*str;
  
  int anzahl;
  int dummy1,dummy2;
  char lp_str_info[200];
  char gewaesser_name[20],zustand[20];
  
  if (file_str==NULL)	   return 1;
  if ((lwa_str = fopen(file_str,"r+"))==NULL)	return 1;
  fscanf(lwa_str,"%d%d%s%s",&dummy1,&dummy2,gewaesser_name,zustand);
  fclose(lwa_str);
  strcpy(lp_str_info," ");
  for(int i=0;i<99;i++)
    strcat(lp_str_info," ");
  strcat(lp_str_info,ber_var);
  strcat(lp_str_info,"@");
  strcat(lp_str_info,gewaesser_name);
  strcat(lp_str_info,"@");
  strcat(lp_str_info,zustand);
  
  lwa_data1    = new LWA_DATA1;
  lwa_data2    = new LWA_DATA2;
  
  
  temp = new char[100];
  str  = new char[25];
  char *lpStr,*tmp;
  
  
  pWPL=Init_Profil_Liste(pWPL);//neues WPL-Element anhängen
  tmpWPL = pWPL->PListNext;

  LaengsProfil* laengsprofil = new LaengsProfil();
  
  
  if (tmpWPL->PList!=NULL)
    tmpWPL->PList->DeleteKoord(0);
  
  tmpWPL->PList->SetPtrAnfang(laengsprofil->get_anfang());
  tmpWPL->dummy = (LAENGSPROFIL*)laengsprofil;
  
  tmp= new char[strlen(file_bce)+1];
  strcpy(tmp,file_bce);
  lpStr = strrchr(tmp,'\\');
  if (lpStr!=NULL)
  {
    lpStr[0]='\0'; //jetzt zeigt lpStr auf den Dateinamen
    lpStr++;
    strcpy(tmpWPL->data->file.name,lpStr);
    xvt_fsys_convert_str_to_dir(tmp,&tmpWPL->data->file.dir);
  }
  else   //Dick 6.08.98 sonst Absturz
  {
    delete tmp;
    return 0;
  } 
  delete tmp;
  
//  Lread_profildatei(tmpWPL);
  read_profildatei( tmpWPL, &tmpWPL->data->file.dir, tmpWPL->data->file.name, FALSE );
  // jetzt Profil sichern:
  laengsprofil->SetPtrAnfang(tmpWPL->PList->get_anfang());
  laengsprofil->SetPtrEnde(tmpWPL->PList->get_anfang());
  
  laengsprofil->BuildDsInfo(tmpWPL);
  BuildLP_Line15(tmpWPL);  // Zeile 15 nach Bauwerk erneut aktualisieren
  laengsprofil->SetBceLPTypDaten(SOHLHOEHE,lp_str_info);
  laengsprofil->SetBceLPTypDaten(BOESCHUNG_LINKS,lp_str_info);
  laengsprofil->SetBceLPTypDaten(BOESCHUNG_RECHTS,lp_str_info);
  laengsprofil->SetBceLPTypDaten(WASSERSPIEGEL,lp_str_info);
  
  //  B A U W E R K E
  if (existDKUK)
    anzahl = BuildLPBauwerk(laengsprofil,tmpWPL);
  
  laengsprofil->BuildDsInfo(tmpWPL);
  BuildLP_Line15(tmpWPL);  // Zeile 15 nach Bauwerk erneut aktualisieren
  
  //Neu Dick 4.02.99 Wasserspiegelfixirungen
  if(enable_wsf)
  {
    //aus *.str *.wsf Datei bauen
    char wsf[200];
    FILE *wsf_datei;
    int ok=0;
    if(file_str!=NULL && abfluss_var!=NULL)
      if(file_str[0]!='\0' && abfluss_var[0]!='\0')
      {
        strcpy(wsf,file_str);
        wsf[strlen(wsf)-3]='\0';
        strcat(wsf,"wsf");
        if ((wsf_datei = fopen(wsf,"r"))!=NULL)
        {
          ok=BuildLP_WSF(laengsprofil,tmpWPL,wsf_datei,abfluss_var,lp_str_info);
          fclose(wsf_datei);
        }
      }
      if(ok==1)
      {
        laengsprofil->BuildDsInfo(tmpWPL);
        BuildLP_Line15(tmpWPL);  // Zeile 15 nach Bauwerk erneut aktualisieren
      }
  }
  //Ende neu


  // Querprofil kommentare als Text einfügen
  BuildLPComment( tmpWPL, laengsprofil );
  laengsprofil->BuildDsInfo( tmpWPL );
  BuildLP_Line15( tmpWPL );



  //strcpy(tmpWPL->data->file.name,file_bce);
  save_profildatei( tmpWPL, FALSE );
  
  
  if (tmpWPL !=NULL)
    Delete_Profil_Liste(tmpWPL);
  tmpWPL = NULL;
  pWPL->PListNext=NULL;
  
  delete lwa_data1;
  delete lwa_data2;
  delete[] temp;
  delete[] str;
  return 1;
}


/******************************************************************************/
int StringToLwaData1(char *line,LWA_DATA1 *lwa_data)
{
/* Datenzeile aus LWA Ergebnisdateien Längsprofil (kein Sonderprofil)
in Datenstruktur LWA_DATA1 schreiben

	 Rückgabe: 1:=OK   , 0:=Fehler
  */
  char tmp[25];
  int  i;
  
  if ( (strlen(line)>0) &&(lwa_data))
  {
    for (i=0;i<=9;i++)
      tmp[i] = line[i];
    tmp[10]= '\0';
    lwa_data->station = atof(tmp);
    
    lwa_data->station=lwa_data->station*(-1);
    
    for (i=10;i<=17;i++)
      tmp[i-10] = line[i];
    tmp[8]= '\0';
    lwa_data->sohlhoehe = atof(tmp);
    
    for (i=18;i<=25;i++)
      tmp[i-18] = line[i];
    tmp[8]= '\0';
    lwa_data->ufer_links = atof(tmp);
    
    for (i=26;i<=33;i++)
      tmp[i-26] = line[i];
    tmp[8]= '\0';
    lwa_data->ufer_rechts = atof(tmp);
    
    for (i=34;i<=41;i++)
      tmp[i-34] = line[i];
    tmp[8]= '\0';
    lwa_data->wsp = atof(tmp);
    
    for (i=42;i<=49;i++)
      tmp[i-42] = line[i];
    tmp[8]= '\0';
    lwa_data->abfluss = atof(tmp);
    
    for (i=50;i<=57;i++)
      tmp[i-50] = line[i];
    tmp[8]= '\0';
    lwa_data->abstand = atof(tmp);
    
    tmp[0] = line[58];tmp[1] = line[59];tmp[2]= '\0';
    if((tmp[0]!=' ')||(tmp[1]!=' '))
      lwa_data->ibk = atoi(tmp);
    else
      lwa_data->ibk=(INT)BCE_NAN;
    
    tmp[0] = line[60];tmp[1] = line[61];tmp[2]= '\0';
    if((tmp[0]!=' ')||(tmp[1]!=' '))
      lwa_data->idp = atoi(tmp);
    else
      lwa_data->idp=(INT)BCE_NAN;
    
    tmp[0] = line[62];tmp[1] = line[63];tmp[2]= '\0';
    if((tmp[0]!=' ')||(tmp[1]!=' '))
      lwa_data->iva = atoi(tmp);
    else
      lwa_data->iva=(INT)BCE_NAN;
    
    tmp[0] = line[64];tmp[1] = line[65];
    tmp[2] = line[66];tmp[3] = line[67];
    tmp[4]= '\0';
    if((tmp[0]!=' ')||(tmp[1]!=' ') || (tmp[2]!=' ') || (tmp[3]!=' '))
      lwa_data->ivz = atoi(tmp);
    else
      lwa_data->ivz=(INT)BCE_NAN;
    
    lwa_data->mfb[0] = line[68];lwa_data->mfb[1] = line[69];lwa_data->mfb[2] = '\0';
    
    if(lwa_data->mfb[0]=='L')
      lwa_data->mfbint=1;
    else
      if(lwa_data->mfb[0]=='F')
        lwa_data->mfbint=2;
      else
        if (lwa_data->mfb[0]=='R')
          lwa_data->mfbint=3;
        else
          lwa_data->mfbint=0;
        
        for (i=70;i<=77;i++)
          tmp[i-70] = line[i];
        tmp[8]= '\0';
        lwa_data->wsp_breite = atof(tmp);
        //Dick 22.09.99  Neu
        if(strlen(line)>100) //Wenn neu Datensätze 
        {
          for (i=78;i<=85;i++)
            tmp[i-78] = line[i];
          tmp[8]= '\0';
          lwa_data->schlepp_span = atof(tmp);
          
          for (i=86;i<=93;i++)
            tmp[i-86] = line[i];
          tmp[8]= '\0';
          lwa_data->ausufer_links = atof(tmp);
          
          for (i=94;i<=101;i++)
            tmp[i-94] = line[i];
          tmp[8]= '\0';
          lwa_data->ausufer_rechts = atof(tmp);
          
          for (i=102;i<=109;i++)
            tmp[i-102] = line[i];
          tmp[8]= '\0';
          lwa_data->energiehoehe = atof(tmp);
        }
        return TRUE;
  }
  else return FALSE;
}
/******************************************************************************/
int StringToLwaData2(WSP_PROFIL_LISTE *pWPL,char *line,LWA_DATA2 *lwa_data2)
//Datenzeile aus LWA Ergebnisdateien Längsprofil mit Sonderprofil
//in Datenstruktur LWA_DATA2 schreiben
//
//Rückgabe: 1:=OK   , 0:=Fehler
{
  char *p,tmp[50], stationsstring[50];
  int  i;
  
  if ( (strlen(line)>0) &&(lwa_data2))
  {
    for (i=0;i<=9;i++)
      stationsstring[i] = line[i];
    stationsstring[10]= '\0';
    lwa_data2->station = atof(stationsstring);
    
    lwa_data2->station=lwa_data2->station*(-1);
    
    for (i=10;i<=17;i++)
      tmp[i-10] = line[i];
    tmp[8]= '\0';
    lwa_data2->dkuk = atof(tmp);
    
    for (i=18;i<=25;i++)
      tmp[i-18] = line[i];
    tmp[8]= '\0';
    lwa_data2->dkok = atof(tmp);
    
    p = &line[28];
    lwa_data2->text[0]='\0';
    for (i=0;i<=39;i++)	 lwa_data2->text[i] = p[i];
    lwa_data2->text[40]='\0';
    /*
    pWPL->data->slist_comment=AppendStringToWspSList(pWPL->data->slist_comment,stationsstring);
    pWPL->data->slist_comment=AppendStringToWspSList(pWPL->data->slist_comment,lwa_data2->text);
    */
    
    p = &line[74];
    for (i=0;i<=3;i++)	 lwa_data2->kenng[i] = p[i];
    lwa_data2->kenng[4]='\0';
    
    return TRUE;
  }
  else return FALSE;
}
/******************************************************************************/
int StringToLwaData3(char *line,LWA_DATA1 *lwa_data)
{
/* Datenzeile aus LWA Ergebnisdateien Längsprofil (kein Sonderprofil)
jedoch Datentyp 'DATENBLOCK 2'  in Datenstruktur LWA_DATA1 schreiben

	 verwendet werden: station,wsp,abfluss,iva,wsp_breite
   
     Rückgabe: 1:=OK   , 0:=Fehler
  */
  char tmp[25];
  int  i;
  
  if ( (strlen(line)>0) &&(lwa_data))
  {
    for (i=0;i<=9;i++)
      tmp[i] = line[i];
    tmp[10]= '\0';
    lwa_data->station = atof(tmp);
    
    lwa_data->station=lwa_data->station*(-1);
    
    for (i=10;i<=17;i++)
      tmp[i-10] = line[i];
    tmp[8]= '\0';
    lwa_data->wsp = atof(tmp);
    
    for (i=18;i<=25;i++)
      tmp[i-18] = line[i];
    tmp[8]= '\0';
    lwa_data->abfluss = atof(tmp);
    
    tmp[0] = line[26];tmp[1] = line[27];tmp[2]= '\0';
    lwa_data->iva = atoi(tmp);
    
    for (i=28;i<=35;i++)
      tmp[i-28] = line[i];
    tmp[8]= '\0';
    lwa_data->wsp_breite = atof(tmp);
    
    // Restliche Variablen werden nicht benötigt
    lwa_data->sohlhoehe   = lwa_data-> ufer_links =
      lwa_data->ufer_rechts = lwa_data-> abstand    = 0.0;
    
    lwa_data->ibk = lwa_data->idp = lwa_data->ivz = 0;
    lwa_data->mfb[0] = '\0';                                      
    //Dick 23.09.99 neue Datensätze
    lwa_data->schlepp_span   = lwa_data-> ausufer_links =
      lwa_data->ausufer_rechts = lwa_data-> energiehoehe    = 0.0;
    return TRUE;
  }
  else return FALSE;
}
/******************************************************************************/
int BuildLPHeader( WSP_PROFIL_LISTE *tmpWPL)
{  /* 15 Kopfzeilen für Längsprofil erzeugen	 */
  
  for (int i=0;i<15;i++) //  15 Zeilen Header anlegen (leer)
    tmpWPL->data->slist_header=AppendStringToWspSList(tmpWPL->data->slist_header," ");
  
  
  ChangeStringInWspSList(tmpWPL->data->slist_header,"SPIEGELLINIEN-LAENGSSCHNITT",6);
  ChangeStringInWspSList(tmpWPL->data->slist_header,"B-           2   0 0 0 0 0",11);
  
  // vorerst
  ChangeStringInWspSList(tmpWPL->data->slist_header,"0.00  0.00  155.00 1000.00 100.00 29.70  0",14);
  //später :::BuildLP_Line15();
  return 1;
}

/*********************************************************************************/
int BuildLP_Line15( WSP_PROFIL_LISTE *tmpWPL)
{
  double z,mx,my;
  char *line,tmp[25],tmp2[26];
  MinMax *lpMinMax;
  long  mmx,mmy;
  UINT i,j ;
  double abstand_hoehe=0.0,abstand_breite=0.0;
  line  = new char[150] ;
  lpMinMax = new MinMax;
  
  tmpWPL->PList->GetLPlotInfo(lpMinMax,&abstand_hoehe,&abstand_breite);
  
  //Muster:  "0.00  0.00  155.00 1000.00 100.00 29.70  0"
  
  strcpy(line,"0.00  0.00  ");
  
  z= floor(lpMinMax->minY-1); //kleinster z-Wert
  gcvt(z,6,tmp);   strcat(line,tmp); strcat(line,"  ");
  
  mx = ( lpMinMax->maxX - lpMinMax->minX ) /0.15;
  // Wert auf nächsten ganzzahligen vollen Hunderter,Tausender aufrunden
  mmx = (long)mx;
  ltoa(mmx,tmp,10);
  i = strlen(tmp);
  for (j=1;j<i;j++)
    tmp[j]='0';
  if (tmp[0]!='9')
	 {
    tmp[0]=tmp[0]+1;
    strcat(line,tmp);
	 }
  else
	 {
    tmp[0]='0';
    tmp2[0]='1';tmp2[1]='\0';
    strcat(tmp2,tmp);
    strcat(line,tmp2);
	 }
  strcat(line,"  ");
  
  my = (( lpMinMax->maxY - z ) *1000.)/(297-35-15-abstand_hoehe);//Dick 16.04.99 
  
  // Wert auf nächsten ganzzahligen vollen Hunderter,Tausender aufrunden
  mmy = (long)my;
  ltoa(mmy,tmp,10);
  i = strlen(tmp);
  //Dick 16.04.99 // Wert auf nur bis nächsten ganzzahligen vollen Hunderter
  for (j=0;j<2 && i-j-1>0;j++)
    tmp[i-j-1]='0';
  if (tmp[i-j-1]!='9')
  {
    tmp[i-j-1]=tmp[i-j-1]+1;
    strcat(line,tmp);
  }
  else
	 {
    tmp[i-j-1]='0';
    if(i-j-2 <=0)
    {
      tmp2[0]='1';tmp2[1]='\0';
      strcat(tmp2,tmp);
      strcat(line,tmp2);
    }
    else
    {
      tmp[i-j-2]=tmp[i-j-2]+1;
      strcat(line,tmp);
    }
	 }
  //Ende neu
  strcat(line,"  ");
  strcat(line,"29.70  0");
  
  ChangeStringInWspSList(tmpWPL->data->slist_header,line,14);
  
  delete lpMinMax;
  delete[] line;
  return 0;
}
/******************************************************************************/
int BuildLPBauwerk( LaengsProfil *lp,WSP_PROFIL_LISTE *tmpWPL)
{/* Datensatz "BAUWERK"  anhand der Daten von DKUK-DKOK erzeugen  */
  int dkuk=0,dkok=0,anzahl=0;
  
  dkuk = lp->ExistDatensatzTyp(DKUK);
  dkok = lp->ExistDatensatzTyp(DKOK);
  
  if ((dkuk)&&(dkok))
  {
    lp->MakeNewNode(tmpWPL->data->ds_info[0]+1);  // =Datensatz 11
    lp->WriteLPTypDaten(tmpWPL->data->ds_info[0]+1,BAUWERK);
    
    anzahl = max(tmpWPL->data->ds_info[dkok],tmpWPL->data->ds_info[dkuk]);
    
    if (anzahl>0)
    {
      anzahl = tmpWPL->PList->BuildBauwerk(anzahl);
      tmpWPL->data->ds_info[0]++;
      tmpWPL->data->ds_info[tmpWPL->data->ds_info[0]]=anzahl;
      tmpWPL->data->typ[tmpWPL->data->ds_info[0]]=BAUWERK;
    }
  }
  return anzahl;
}

int BuildLP_WSF( LaengsProfil *lp,WSP_PROFIL_LISTE *tmpWPL,FILE* wsf_datei,char *abfluss_var,char* lp_str_info)
{
  double x[STRANGANZAHL],y[STRANGANZAHL];
  char temp[256];
  BOOLEAN gefunden=FALSE;
  int i,anzahl=0,end=0,leer_stat=0;
  for(i=0;i<STRANGANZAHL;i++)//Dick 14.06.99 200-> muß identisch sein
  {
    x[i]=y[i]=0.0;
  }
  while ((!feof(wsf_datei)) && (!gefunden))
  {
    end=fscanf(wsf_datei,"%s",temp);
    if(end!=0&&end!=EOF)
      end=fscanf(wsf_datei,"%d",&anzahl);
    for(i=0;i<anzahl && end!=0 && end!=EOF;i++)
    {
      end=fscanf(wsf_datei,"%lf",&x[i]);
      end=fscanf(wsf_datei,"%lf",&y[i]);
    }
    gefunden=(strcmp(temp,abfluss_var)==0)?TRUE:FALSE;
  }
  for(i=0;i<anzahl;i++)
  {
    if(lp->ExistStation(x[i]*-1000.,1)==0)
    {
      x[i]=BCE_NAN;
      leer_stat++;
    }
  }
  if( gefunden && anzahl-leer_stat != 0 )
  {
    lp->MakeNewNode( tmpWPL->data->ds_info[0] + 1 );
    lp->WriteLPTypDaten( tmpWPL->data->ds_info[0] + 1, WSP_FIXIERUNG, lp_str_info );
    tmpWPL->data->ds_info[0]++;
    tmpWPL->data->ds_info[tmpWPL->data->ds_info[0]] = anzahl - leer_stat;
    tmpWPL->data->typ[tmpWPL->data->ds_info[0]] = WSP_FIXIERUNG;
    for( int i = 0;i < anzahl; i++ )
    {
      if( x[i] != BCE_NAN ) 
      {
        if ( !lp->CopyKoordToList( WSP_FIXIERUNG, i + 1, x[i] * -1000.0, y[i] ) )
          return 0; // Fehler
      }
    }
    /******Bley 8.11.2000**************/
    lp->MakeNewNode( tmpWPL->data->ds_info[0] + 1 );
    lp->WriteLPTypDaten( tmpWPL->data->ds_info[0] + 1, WSP_DIFFERENZ, lp_str_info );
    tmpWPL->data->ds_info[0]++;
    tmpWPL->data->ds_info[tmpWPL->data->ds_info[0]] = anzahl - leer_stat;
    tmpWPL->data->typ[tmpWPL->data->ds_info[0]] = WSP_DIFFERENZ;
    for( i = 0;i < anzahl; i++ )
    {
      if( x[i] != BCE_NAN ) 
      {
        if( !lp->CopyKoordToList( WSP_DIFFERENZ, i + 1, x[i] * -1000.0, y[i] ) )
          return 0; // Fehler
      }
    }
	   lp->WSP_Differenz(); //Bley 8.11.2000
     
     /*******Bley 8.11.2000 ***************/
     
     
  }
  return 1;
}

/******************************************************************************/
int LwaData1ToWSP(WSP_PROFIL_LISTE *pWPL,LWA_DATA1 *data,int line, LaengsProfil *laengsprofil)
/*	  Daten aus LWA_DATA1 in WSP_PROFIL_LISTE bzw PList schreiben
Parameter:
WSP_PROFIL_LISTE* pWPL      :
LWA_DATA1* data             : enthält die einzufügenden Daten
int line                    : Nummer der aktuellen Zeile
Laengsprofil* laengsprofil  : In dieses Laengsprofil soll eingefügt werden
Rückgabewert: 0 Fehler
1 OK
*/
{ 
  laengsprofil->CopyKoordToList(SOHLHOEHE,line,data->station,data->sohlhoehe);
  laengsprofil->CopyKoordToList(BOESCHUNG_LINKS,line,data->station,data->ufer_links);
  laengsprofil->CopyKoordToList(BOESCHUNG_RECHTS,line,data->station,data->ufer_rechts);
  laengsprofil->CopyKoordToList(WASSERSPIEGEL,line,data->station,data->wsp);
  laengsprofil->CopyKoordToList(ABFLUSS,line,data->station,data->abfluss);
  laengsprofil->CopyKoordToList(LAENGE,line,data->station,data->abstand);
  laengsprofil->CopyKoordToList(PROFILART,line,data->station,data->idp);
  laengsprofil->CopyKoordToList(VZKENNG,line,data->station,data->ivz);
  laengsprofil->CopyKoordToList(WSP_BREITE,line,data->station,data->wsp_breite);
  laengsprofil->CopyKoordToList(PROFILKENNG,line,data->station,data->mfbint);
  laengsprofil->CopyKoordToList(SCHLEPPSPANNUNG,line,data->station,data->schlepp_span);
  laengsprofil->CopyKoordToList(AUSUFERUNG_LINKS,line,data->station,data->ausufer_links);
  laengsprofil->CopyKoordToList(AUSUFERUNG_RECHTS,line,data->station,data->ausufer_rechts);
  laengsprofil->CopyKoordToList(ENERGIEHOEHE,line,data->station,data->energiehoehe);
  
  return 1;
}; // LwaData1ToWSP

/******************************************************************************/
int LwaData2ToWSP(WSP_PROFIL_LISTE *pWPL,LWA_DATA2 *data,int line, LaengsProfil *laengsprofil)
{ /*	  Daten aus LWA_DATA2 in WSP_PROFIL_LISTE bzw PList schreiben	*/
  laengsprofil->CopyKoordToList(DKUK,line,data->station,data->dkuk);
  laengsprofil->CopyKoordToList(DKOK,line,data->station,data->dkok);

  return 1;
}
/******************************************************************************/
int LwaData3ToWSP(WSP_PROFIL_LISTE *pWPL,LWA_DATA1 *data,int line, LaengsProfil *laengsprofil)
/*
Daten aus LWA_DATA1 in WSP_PROFIL_LISTE bzw PList schreiben

  Parameter:
  WSP_PROFIL_LISTE* pWPL      : ??? unbenutzt
  LWA_DATA1* data             : hinzuzufügende Daten
  int line:                   : Nummer der aktuellen Zeile
  LaengsProfil* laengsprofil  : in dieses Laengsprofil wird hinzugefügt
  Rückgabewert:
  0 Fehler
  1 Ok
  */
{ 
  laengsprofil->CopyKoordToList( WASSERSPIEGEL_2, line,data->station, data->wsp );
  return 1;
}
/******************************************************************************/

int Ersetze_Station_mit_Abstand_dll(WSP_PROFIL_LISTE *pWPL,DIRECTORY *dirp,char* filename, STRANG *pstrang, int strang_entries)
{
  
  
  int stra, pos =1,n_vzk=-1;
  
  stra=1;
  double alte_station, neue_station, alter_abstand, abstand, letzte_station;
  
  //  while ((stra<=strang_entries)&&(pstrang!=NULL))
  while((stra<=strang_entries) &&(pstrang!=NULL))
  {
    abstand=atof(pstrang->abstand_fluss);
	   abstand=abstand*(-1000);
     /*2.4.*/ 
     if(stra==1)
     {
       alte_station=pstrang->anfang;
       alte_station=alte_station*(-1000);
       neue_station=pstrang->anfang;
       neue_station=neue_station*(-1000);
       
       int test=pWPL->PList->ListSucheStation(alte_station,neue_station, stra, strang_entries);
       alter_abstand=abstand;
     }
     else
     {
       if(letzte_station!=pstrang->anfang) //nicht bei Mehrfeldbrücken
       {
         neue_station=neue_station+alter_abstand;
         //		   neue_station=neue_station*(-1000);
         alte_station=pstrang->anfang;
         alte_station=alte_station*(-1000);
         int test=pWPL->PList->ListSucheStation(alte_station,neue_station, stra, strang_entries);
         alter_abstand=abstand;
       }
       
       if(stra==strang_entries) //letze station
       {
         neue_station=neue_station+abstand;
         alte_station=pstrang->ende;
         alte_station=alte_station*(-1000);
         int test=pWPL->PList->ListSucheStation(alte_station,neue_station, stra, strang_entries);
         alter_abstand=abstand;
         
       }
       
     }
     
     letzte_station=pstrang->abstand;
     pstrang=pstrang->next;
     stra++;
     
  }
  
  return 0;
}
