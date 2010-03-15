/****************************************************************************
*             READPROF .CPP                                                  *
*             10.03.1995                                                    *
****************************************************************************/
#include <windows.h>
#include "xvt.h"

#include "resource.h"

#include "global_types.h"
#include "global_vars.h"
#include "l_typen.h"
#include "typen.h"

#include "..\..\wspdlg\Include\export.h"

#include "list.h"
#include "util.h"
#include "strang.h"
#include "strang2.h"
#include "bce_allg.h"
#include "aufnehm.h"
#include "slist.h"

#include "readprof.h"


// 'lokale' Variablen //

BOOL GaussProfilMitRuecksprung;

// Globale Variablen //

extern char str_zustand[16], str_gewaesser[16];

// Vorwärtsdeklarationen //

int WriteLptext( FILE *file, PROFILDATA* pData );



// Implementation //

void check_header( WSP_SLIST* headerList, bool bQuer )
// Sorgt dafür, dass der Header des Profils in Ordnung ist
// Parameter:
//      WSP_SLIST* headerList: Die zu untersuchende Liste
//      bool bQuer: falls true, ein Querprofilheader, sonst ein Längsschnittheader
// Bemerkung:
//      wird vor dem Schreiben und nach dem Lesen der Profildatei aufgerufen
{
  // falls der Header nicht genau 15 Zeilen hat, nichts tun
  if( !headerList || WspSListCountElem( headerList ) != 15 )
    return;
  
  char gewaesserStr[10];
  gewaesserStr[0] = '\0'; // der gewässerName wird später gebraucht

  for( int i = 0; i < 15; i++ )
  {
    char string[1024]; // Hofentlich ist nie ein Headerstring länger als 1000
    char newString[1024];
    newString[0] = '\0'; // der leere String
    
    // den String aus der Liste kopieren
    GetStringFromWspSList( headerList, i, string );
    
    // prüfen und ändern
    if( bQuer ) // Querprofile
    {
      switch( i )
      {
      case 5:
        {
          // zweite Zeile der Projektbezeichnung
          // an den Stellen 40-49 steht der Gewässername

          // die ersten 40 Zeichen so kopieren
          strncat( newString, string, 40 );

          // ist die Zeile kleiner als 40 Stellen, bis dahin auffüllen
          int newLen = strlen( newString );
          while( newLen++ < 40 )
            strncat( newString, " ", 1 );

          // jetzt den Gewässernamen kopieren
          if( strlen( string ) > 40 )
            strncat( gewaesserStr, string + 40, 9 );
          
          // mögliche Leerzeichen rausfiltern
          char* pStr = gewaesserStr;
          for( int j = 0; j < 10; j++ )
          {
            if( gewaesserStr[j] != ' ' )
              pStr++[0] = gewaesserStr[j];
          } // for j

          if( strlen( gewaesserStr ) == 0 )
            strncpy( gewaesserStr, "Gewaesser", 10 );

          strncat( newString, gewaesserStr, 9 );
        }
        break;

      case 7: // "QUERPROFIL blablabla"
        {
          LPCSTR querP = "QUERPROFIL ";
          int querLen = strlen( querP ); // nur einmal die Länge von QUERPROFIL ausrechnen
          // der neue String muss immer mit 'QUERPROFIL ' anfangen
          strncat( newString, querP, querLen );
          
          char* rest = NULL;
          
          // zuerst prüfen, obs mit "QUERPROFIL anfängt"
          // wenn ja, den Rest übernehmen
          if( _strnicmp( string, querP, querLen ) == 0 )
            rest = string + querLen;
          else
            rest = string;
          
          // der Rest muss auch irgendwas sein ( != "" ), 
          // ansonsten wird 000 genommen
          int restLen = strlen( rest );
          if( restLen == 0 )
            strncat( newString, gewaesserStr, strlen( gewaesserStr ) );
          else
            strncat( newString, rest, max( 0, 34 - querLen ) ); // max 35 Zeichen erlaubt
        }; // case 8
        break;

      case 14:
        {
          // es müssen mindestens 9 durch Leerzeichen getrennte Werte da sein
          // fehlen am Ende Werte, werden einfach '0'en angefügt
          char* str = string; // Zeiger auf die aktuelle Position

          for( int j = 0; j < 9; j++ )
          {
            // erstmal führende Leerzeichen überlesen
            while( str[0] == ' ' )
              str++;

            // und dann die nächste Zahl lesen
            char* strEnd = strchr( str, ' ' );

            if( strEnd == NULL )
            {
              // kein Leerzeichen gefunden: falls noch was übrig ist, dies als
              // Token interpretieren, ansnsten eine '0' anhängen
              int strLen = strlen( str );
              if( strLen > 0 )
              {
                strncat( newString, str, strLen );
                str += strLen;
              }
              else
                strncat( newString, "0", 1 );
            }
            else
            {
              strncat( newString, str, strEnd - str ); // die Zahl anhängen
              str = strEnd + 1; // zeigt hinter das Leerzeichen
            } // if strEnd == NULL

            // immer noch ein Leerzeichen anhängen
            strncat( newString, " ", 1 );
          } // for j

          // zuletzt alles was folgt auch noch anhängen
          if( str != NULL )
            strncat( newString, str, strlen( str ) );
        }
        break;

        // Standardmässig nichts tun
      default:
        strncat( newString, string, strlen( string ) );
        break;
      }; // switch i
    }
    else // Längsschnitt
    {
      switch( i )
      {
        // Standardmässig nichts tun
      case 0:
      default:
        strncat( newString, string, strlen( string ) );
        break;
      }; // siwtch i 
    }; // if bQuer
    
    // und den string wieder zurückschreiben
    ChangeStringInWspSList( headerList, newString, i );
  }; // for i
  
}; // check_header

int ds_get_header( FILE* file, PROFILDATA* pData )
/**************************************************
*     Zeile 1-15 der Profildatei lesen            *
***************************************************/
{
  char temp[LENLINE + 1];
  for (int i = 0; i <= 14; i++ )
  {
    if ( !feof( file ) )
    {
      fgets( temp, LENLINE, file );
      if ( strlen(temp) == 0 )
        strcpy( temp, "\n" );
      else
        temp[strlen(temp) - 1] = '\0';
      pData->slist_header = AppendStringToWspSList( pData->slist_header, temp );
    }
    else if( feof( file ) )
    {
      pData->anzahl_ds = 0; //=ds_info[0]
      pData->ds_info[0] = 0;
      return 0;
    }
  }
  return 1;
}
/**************************************************
*                                                 *
*     Zeile 9: Überprüfung                        *
***************************************************/
void test_line9(char str[25])
{
  char *match="STATION KM";
  char *temp;
  int ok,i,j;
  
  temp=xvt_slist_get_elt(header_profil,8,0);
  ok=xvt_str_compare_ignoring_case(match,temp);
  if (ok)
	 {
    //for (i=10;i<=25;i++)
    //auf 20 gekürzt wg. Parameter, die in Stempel sollen gem. Pasche
    j=0;
    for (i=10;i<19;i++)
		  {
      if(temp[i]!=' ')
        //geändert, damit rechtsbündige Darstellung möglich
        //str[i-10]=temp[i];
      {
        str[j]=temp[i];
        j++;
      }
		  }
    str[j]='\0'; // neu
    str[8]='\0'; //sicherheitshalber
	 }
}

/******************************************************
*     Zeile9 konvertieren                             *
******************************************************/
void write_line9(char str[25])
{
  char match[40]="STATION KM ";
  
  strcat(match,str);
  xvt_slist_change_str(header_profil,match,8);
}
/**************************************************
*             DS_GET_ANZAHL                       *
*     Zeile 14: CHAR -> INT konvertieren          *
***************************************************/
void ds_get_anzahl( PROFILDATA* pData )
{
	 int j,m=0,k=0,i=0;
   char help[5];
   char temp[100], str1[250];
   
   GetStringFromWspSList( pData->slist_header, 13, temp );
   
   for( j = 0; j < 50; j++ )
     pData->ds_info[j] = 0;   //initialisieren
   
   strcpy( str1, temp );
   m = strlen( str1 );
   j = 0;
   while( j < m )
   {
     while( str1[j] == ' ' )
       j++;
     while((str1[j]>='-')&&(j<m))//Dick 8.12.99 damit '-' mit gezählt wird
     {
       help[i]=str1[j];
       i++;
       j++;
     }
     help[i]='\0';
     pData->ds_info[k] = atoi( help );
     i = 0;
     k++;
   }
   
   pData->anzahl_ds = pData->ds_info[0];
   if( ( k - 1 ) < pData->ds_info[0] )   // siehe Def. Zeile 14
     pData->exist_plot = TRUE;
};

/***************************************************/
int ReadComment( FILE *file, int lines, int type, PROFILDATA* pData )
{
  char str[100], *chrptr;
  
  for( int i = 0; i < lines; i++ )
  {
    if (( fgets(str,99,file ))&&(!feof(file)))
    {
      if( type == LP_TEXT )    //d.h. nicht comment sondern LP_TEXT
      {
        chrptr = strchr(str,'\n');
        if (chrptr !=NULL)
          chrptr[0]='\0';
        
        pData->slist_comment = AppendStringToWspSList( pData->slist_comment, str );
      }
      else
      {
        if ((str[0]=='C')&&(str[1]=='C'))
        {
          chrptr = strchr( str + 3, '\n' );
          if( chrptr )
            chrptr[0] = '\0';
          
          pData->slist_comment = AppendStringToWspSList( pData->slist_comment, str + 3 );
        }
      }
    }
    else
    {
      char buf[200];
      xvt_res_get_str(STR_KOMMENT_READ_ERR ,buf,sizeof(buf));
      xvt_dm_post_error("%s",buf); //"Fehler beim lesen der Kommentarzeilen"
      return 0;
    };
  }; // for i
  
  return 1;
}; // ReadComment
/*****************************************************************************/
int read_profildatei( WSP_PROFIL_LISTE *ptrWPL, DIRECTORY* dir, char* filename, BOOLEAN setGlobals /* = TRUE */ )
// liest eine Profildatei ein
// Parameter:
//        WSP_PROFIL_LISTE* ptrWPL: hier wird das neu gelesene Profil angehängt
//        DIRECTORY dir: hier liegt die zu lesende Profildatei
//        char* filename: der Dateiname der zu lesenden Datei

// Rückgabewert: 0: kein Fehler
//               1: Fehler
// Seiteneffekte:
//              file_spec.dir wird auf das Directory der gelesenen Datei gesetzt
//              ds_info wird gesetzt
//              anzahl_ds wird gesetzt
{
  int fehler = 0;
  int err_ds;
  BOOLEAN ret = FALSE;
  char temp[LENLINE +1];
  char help_dat_name[MAX_PATH];
  
  FILE* in;
  
  PROFILDATA* profData = ptrWPL->data;
  
  // den Dateinamen setzen
  strcpy( profData->file.name, filename ); // den Dateinamen setzen
  xvt_fsys_convert_dir_to_str( dir, help_dat_name, MAX_PATH );
  xvt_fsys_convert_str_to_dir( help_dat_name, &profData->file.dir );
  
  strcat( help_dat_name, "\\" );
  strcat( help_dat_name, filename );

  if( ( in = fopen( help_dat_name, "r+" ) ) == NULL )
  {
    char buf[200],buf2[200],buf3[200];
    xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
    xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
    xvt_res_get_str(STR_ERROR,buf3,sizeof(buf3));
    xvt_dm_post_error("%s:\n%s:\n%s\n%s", buf3, buf, help_dat_name, buf2, filename );     //xvt_dm_post_error("Fehler:Datei: %s läßt sich nicht öffnen !",file_spec.name);
    
    char file_str[AUFNEHM_BUFFER_SIZE];
    switch( auswahl2( file_str, FALSE, 40001 ) ) //switch (xvt_dm_post_file_open(&file_spec, "Auswahl Profildatei"))
    {
    case IDCANCEL: 
      fehler = 1;
      break;
      
    case IDOK:
      if( ( in = fopen( file_str,"r+" ) ) == NULL )
        fehler = 1;
      break;
      
    default:
      break;
    }; // switch
  }; // if in
  
  if( fehler != 0 )
    return 1;
  
  if( ptrWPL )
  {
    if( ptrWPL->PList )
      ptrWPL->PList->DeleteList();
  }
  else 
    return 1;
  
  if( profData->slist_header )
  {
    DeleteWspSList( profData->slist_header );
    profData->slist_header = NULL;
  }; 
  
  if( profData->slist_comment )
  {
    DeleteWspSList( profData->slist_comment );
    profData->slist_comment = NULL;
  }; // if profData->slist_comment
  
  if( ds_get_header( in, profData ) )         // Zeile 1-15 in ppsatz[] lesen
    ds_get_anzahl( profData );
  
  if( profData->ds_info[0] == 0 )
  {
    char buf[200];

    xvt_res_get_str(STR_KEINE_DATENSTZE ,buf,sizeof(buf));
    xvt_dm_post_error( "%s:\n%s", buf, help_dat_name ); //"In der Datei sind keine Datensätze vorhanden.\nBitte Profil löschen!"
    fclose( in );
    return 1;
  }
  
  if( profData->exist_plot )
  {
    profData->anzahl_ds--;        //Es existiert ein Plotterdatensatz am
    profData->ds_info[0]--;		 // Ende der Profildatei
  };

  bool bQuer = false;
  
  // Datensätze lesen 
  for( int num_ds = 1; num_ds <= profData->anzahl_ds; num_ds++ )
  {
    ptrWPL->PList->MakeNewNode( num_ds );   // neuen Datensatzknoten generieren
    
    // die folgenden beiden Fkt-aufrufe immer zusammen
    ret = ptrWPL->PList->MakeWriteInfo( in );   //incl.Typdaten ermitteln
    switch( ptrWPL->PList->GetProfilTyp() )    //Datenblocktyp ermitteln
    { 
    case 1:
      profData->exist_comment = TRUE;
      break;
      
    case 2:
      profData->exist_plot = TRUE;
      break;
    }
    // ende :die folgenden beiden Fkt-aufrufe immer zusammen 
    
    if ( ret || feof( in ) )  // Dateiende erreicht
    {
      char buf[200],buf2[200];
      xvt_res_get_str(STR_ANZAHL_DATENSAETZE,buf,sizeof(buf));
      xvt_res_get_str(STR_DATEI_ENDE,buf2,sizeof(buf2));
      
      xvt_dm_post_error("%s%s:\n%s",buf,buf2,help_dat_name);            //xvt_dm_post_error("Anzahl Datensätze in der Datei stimmt nicht mit tatsächlicher Anzahl überein! Dateiende wurde erreicht.");
      
      fclose(in);
      return 1;
    };

    if( typ[num_ds] == GELAENDEHOEHE )
      bQuer = true; // genau die Querprofile haben eine Geländehöhe
    
    if( typ[num_ds] < MAUL )       // kein Sonderprofil
    {
      if( profData->ds_info[num_ds] > KOORDINPROFILANZAHL )
      {
        char buf[200],buf2[200];
        xvt_res_get_str(STR_DATENSATZ,buf,sizeof(buf));
        xvt_res_get_str(STR_HAT_MEHR_ALS_200,buf2,sizeof(buf2));
        xvt_dm_post_error("%s : %i %s",buf,num_ds,buf2);              //xvt_dm_post_error("Datensatz: %i enthält mehr als 200 y-Werte",num_ds);
        fclose(in);
        return 1;
      }
      ptrWPL->PList->MakeNewKoord( profData->ds_info[num_ds] );
      
      ret = ptrWPL->PList->WriteX( in, profData->ds_info[num_ds] );
      if ( ret || feof(in) )  // Dateiende erreicht
      {
        char buf[200],buf2[200];
        xvt_res_get_str(STR_ANZAHL_DATENSAETZE,buf,sizeof(buf));
        xvt_res_get_str(STR_DATEI_ENDE,buf2,sizeof(buf2));
        
        xvt_dm_post_error("%s%s",buf,buf2);             //xvt_dm_post_error("Anzahl Datensätze in der Datei stimmt nicht mit tatsächlicher Anzahl überein! Dateiende wurde erreicht.");
        
        fclose(in);
        return 1;
      }
      
      ret = ptrWPL->PList->WriteY( in, profData->ds_info[num_ds] );
      if( ret || feof(in) )  // Dateiende erreicht
      {
        char buf[200],buf2[200];
        xvt_res_get_str(STR_ANZAHL_DATENSAETZE,buf,sizeof(buf));
        xvt_res_get_str(STR_DATEI_ENDE,buf2,sizeof(buf2));
        
        xvt_dm_post_error("%s%s",buf,buf2);             //xvt_dm_post_error("Anzahl Datensätze in der Datei stimmt nicht mit tatsächlicher Anzahl überein! Dateiende wurde erreicht.");
        
        fclose(in);
        return 1;
      }
    } 
    else                      //  typ[num_ds] >= 16   =Sonderprofil
    {
      if( typ[num_ds] >= MAUL && typ[num_ds] < COMMENT )
      {
        switch( typ[num_ds] )
        {
        case ARMCO71:
        case EIPROFIL:
          ptrWPL->PList->MakeNewKoord( 5 );
          break;                    
        case NWRINNE://Dick 28.07.99 sonst wurde Datensatz danach gelöscht
          ptrWPL->PList->MakeNewKoord( 3 );
          break;
        case KREIS://Dick 28.07.99 sonst wurde Datensatz danach gelöscht
          ptrWPL->PList->MakeNewKoord( 4 );
          break;

        default:
          ptrWPL->PList->MakeNewKoord( 6 );
          break;
        }
        ret = ptrWPL->PList->ReadSonderProfil(in,typ[num_ds]);
        if( ret || feof(in) )  // Dateiende erreicht
        {
          char buf[200],buf2[200];
          xvt_res_get_str(STR_ZU_VIELE_DATENSAETZE,buf,sizeof(buf));
          xvt_res_get_str(STR_DATEI_ENDE,buf2,sizeof(buf2));
          
          xvt_dm_post_error("%s%s",buf,buf2);               //xvt_dm_post_error("Es sind zu viele Datensätze in der Datei! Dateiende wurde erreicht.");
          
          fclose(in);
          return 1;
        }
      }
      else if( typ[num_ds] == COMMENT || typ[num_ds] == LP_TEXT )
        ReadComment( in, profData->ds_info[num_ds], typ[num_ds], profData );
      else if( typ[num_ds] == UNKNOWN || ( typ[num_ds] > 100 && typ[num_ds] <= 137 ) )
      {
        if( profData->ds_info[num_ds] > 0 )
        {
          if( typ[num_ds] == BAUWERK )
            ptrWPL->PList->ReadDummySList( in, profData->ds_info[num_ds], BAUWERK );
          else
          {
            ptrWPL->PList->MakeNewKoord( profData->ds_info[num_ds] );
            
            ret = ptrWPL->PList->WriteX( in, profData->ds_info[num_ds] );
            if ( ret || feof(in) )  // Dateiende erreicht
            {
              char buf[200], buf2[200];
              xvt_res_get_str(STR_ANZAHL_DATENSAETZE,buf,sizeof(buf));
              xvt_res_get_str(STR_DATEI_ENDE,buf2,sizeof(buf2));
              xvt_dm_post_error("%s%s",buf,buf2); //"Anzahl Datensätze in der Datei stimmt nicht mit tatsächlicher Anzahl überein! Dateiende wurde erreicht."
              
              fclose( in );
              return 1;
            }
            
            ret = ptrWPL->PList->WriteY( in, profData->ds_info[num_ds] );
            if( ret || feof(in) )  // Dateiende erreicht
            {
              char buf[200],buf2[200];
              xvt_res_get_str(STR_ANZAHL_DATENSAETZE,buf,sizeof(buf));
              xvt_res_get_str(STR_DATEI_ENDE,buf2,sizeof(buf2));
              xvt_dm_post_error("%s%s",buf,buf2); // "Anzahl Datensätze in der Datei stimmt nicht mit tatsächlicher Anzahl überein! Dateiende wurde erreicht."
              
              fclose( in );
              return 1;
            }
          } //else != BAUWERK
        }
        else
        {
          char buf[200],buf2[200];
          xvt_res_get_str(STR_DATENSATZ_NULL,buf,sizeof(buf));
          xvt_res_get_str(STR_PROGRAMMABBRUCH,buf2,sizeof(buf2));
          xvt_dm_post_fatal_exit( "%s%s\n%s", buf, buf2, help_dat_name ); //("Datensatz hat Länge Null und kann nicht entschlüsselt werden.\nProgramm wird abgebrochen!"
        }
      }
    } // if typ
  }; // for num_ds
  
  for( int i = num_ds; i < TYPE_SIZE - 1; i++ )//Dick 21.08.98 Restliche Müll //Dick 8.12.98
    typ[i] = 0;
  is_profil_open = TRUE;
  new_profil = FALSE;
  
  if( profData->exist_plot )   // Hinweis ausgeben
  {
    /*
    char buf[200];
    xvt_res_get_str(STR_PLOTTERDATENSATZ,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf); //"Vorhandener Plotterdatensatz kann\nnicht aktualisiert werden,\nwird jedoch in Datei '**.plt' \nabgelegt !"
    */
    
    if( typ[ds_info[0] + 1] == DATPLOT ) //Korrektur von ds_info[0] wurde durchgeführt
      write_datplt( in, filename, &profData->exist_plot ,0 );
    else
      write_datplt( in, filename, &profData->exist_plot ,1 );
	 }
  
  err_ds = ptrWPL->PList->check_sort() ; // Sortierung der Stationen testen
  
  if (err_ds != 0 )
    GaussProfilMitRuecksprung = TRUE;
  
  // bis zum Dateiende lesen, falls datplt oder Müll folgt:  //
  if( !feof(in) )
    fgets(temp,LENLINE,in);
  while ( (!feof(in))&&(strlen(temp)==0) )
    fgets(temp,LENLINE,in);
  while ( (!feof(in))&&(temp[0]=='\n') )
    fgets(temp,LENLINE,in);
  
  // bis zum Dateiende lesen, falls datplt oder Müll folgt:
  if( !feof(in) && !profData->exist_plot)
    write_datplt( in, filename, &profData->exist_plot , 0 );
  
  fclose( in );
  
  // den Header der ProfilDatei überprüfen
  check_header( profData->slist_header, bQuer );
  
  // zuletzt noch die globalen Variablen setzen
  if( setGlobals )
  {
    // header_profil
    if( xvt_slist_is_valid( header_profil ) )
    {
      xvt_slist_destroy(header_profil);
      header_profil = NULL;
    };
    
    if( ( header_profil = xvt_slist_create() ) == NULL )
      return 1;
    WSP_SLIST* sList = profData->slist_header;
    while( sList )
    {
      xvt_slist_add_at_elt( header_profil, NULL, sList->string, 0L );
      sList = sList->next;
    }; // while sList
    
    // slist_comment
    if( slist_comment )
    {
      xvt_slist_destroy( slist_comment );
      slist_comment = NULL;
    }; // if slist_comment
    slist_comment = xvt_slist_create();
    sList = profData->slist_comment;
    while( sList )
    {
      xvt_slist_add_at_elt( slist_comment, NULL, sList->string, 0L );
      sList = sList->next;
    }; // while sList
    
    
    // exist ...
    exist_plot = profData->exist_plot;
    IS_COMMENT = profData->exist_comment;
    
    // ds_info und anzahl_ds
    for( int i = 0; i < TYPE_SIZE; i++ )
      ds_info[i] = profData->ds_info[i];
    anzahl_ds = ds_info[0];
  };
  
  // schliesslich noch den typ übertragen
  for( i = 0; i < TYPE_SIZE; i++ )
    profData->typ[i] = typ[i];
  
  return fehler;
}; // read_profildatei
   
//***************************************************
//*             DS_SAVE_HEADER                      *
//*     Zeile 1-15 der Profildatei schreiben        *
//***************************************************
void ds_save_header( FILE* file, PROFILDATA* pData )
{
  if( file )
  {
    for( int i = 0; i < 15; i++ )
    {
      char string[MAX_PATH];
      GetStringFromWspSList( pData->slist_header, i, string );
      if( string )
        fprintf( file, "%s\n", string );
    }; // for i
  }; // if file
}; // ds_save_header

//***************************************************
//*             DS_SAFE_ANZAHL                      *
//*     Zeile 14: INT -> CHAR konvertieren          *
//***************************************************
void  ds_save_anzahl( PROFILDATA* pData )
{
  if( pData->slist_header )
  {
    char temp[LENLINE] = "";
    for( int j = 0; j <= pData->ds_info[0]; j++ )
    {
      char help[15];
      sprintf( help, " %2d", pData->ds_info[j] );
      strcat( temp, help );
    }; // for j
    
    ChangeStringInWspSList( pData->slist_header, temp, 13 );
  }; // if slist_header
  
}; // ds_save_anzahl

   /**************************************************
   *             SAVE_PROFILDATEI                    *
   *            Profildatei schreiben                *
***************************************************/
int save_profildatei( WSP_PROFIL_LISTE* ptrWPL, BOOLEAN getGlobals )
// schreibt eine Profildatei
// Rückgabewert:
//        0: kein Fehler
//        1: Fehler
{
  PROFILDATA* profData = ptrWPL->data;
  FILE* out;
  char datei[MAX_PATH];
  xvt_fsys_convert_dir_to_str( &profData->file.dir, datei, MAX_PATH ); //Dick 5.11.98
  strcat( datei, "\\" );
  strcat( datei, profData->file.name);
  if( ( out = fopen( datei, "w" ) ) == NULL )
  {
    char buf[200],buf2[200];
    xvt_res_get_str( STR_CANNOT_CREATE1, buf, sizeof( buf ) );
    xvt_res_get_str( STR_CANNOT_WRITE, buf2, sizeof( buf2 ) );
    xvt_dm_post_error( "%s%s%s", buf, profData->file.name, buf2 ); 
    return 1;
  };
  
  // zuerst die globalen Variablen auslesen und in die ptrWPL schreiben
  if( getGlobals )
  {
    // header_profil
    if( xvt_slist_is_valid( header_profil ) )
    {
      // die alte header_list zerstören und mit globalen Werten neu füllen
      DeleteWspSList( profData->slist_header );
      profData->slist_header = NULL;
      for( SLIST_ELT e = xvt_slist_get_first( header_profil ); e; e = xvt_slist_get_next( header_profil, e ) )
        profData->slist_header = AppendStringToWspSList( profData->slist_header, xvt_slist_get( header_profil, e, NULL ) );
    }; // if header_profil
    
    // slist_comment
    if( slist_comment )
    {
      DeleteWspSList( profData->slist_comment );
      profData->slist_comment = NULL;
      for( SLIST_ELT e = xvt_slist_get_first( slist_comment ); e; e = xvt_slist_get_next( slist_comment, e ) )
        profData->slist_comment = AppendStringToWspSList( profData->slist_comment, xvt_slist_get( slist_comment, e, NULL ) );
    }; // if slist_comment
    
    // exist ...
    profData->exist_plot = exist_plot;
    profData->exist_comment = IS_COMMENT;
    
    // ds_info
    for( int i = 0; i < TYPE_SIZE; i++ )
    {
      profData->ds_info[i] = ds_info[i];
      profData->typ[i] = typ[i];
    };
  }; // if getGlobals
  
  
  // alle Datenblöcke checken
  bool bQuer = false;
  int check = ptrWPL->PList->ds_check_daten( profData->typ, profData->ds_info, profData->slist_comment );
  
  for( int knoten = 1; knoten <= profData->ds_info[0]; knoten++ )
  {
    if( profData->ds_info[knoten] <= 0 && 
      profData->typ[knoten] != MAUL &&
      profData->typ[knoten] != EIPROFIL &&
      profData->typ[knoten] != KREIS &&
      profData->typ[knoten] != ARMCO84 &&
      profData->typ[knoten] != ARMCO71 &&
      profData->typ[knoten] != NWRINNE &&
      profData->typ[knoten] != FUELLHOEHE &&
      profData->typ[knoten] != NAU_MED &&
      profData->typ[knoten] != TRAPEZ &&
      profData->typ[knoten] != COMMENT )
    {
      ptrWPL->PList->DeleteNode( knoten, profData->ds_info, profData->typ ); //Datensatz mit Länge 0 löschen
      knoten--;
      continue;
    }; // if
    
    switch( profData->typ[knoten] )
    {
    case GELAENDEHOEHE:
      // nix checken, aber merken, dass dies ein Querprofil ist
      bQuer = true;
      break;
      
    case TRENNFLAECHEN:  //  Trennflächen enthalten nur 1 Stationswert
      if( profData->ds_info[knoten] <= 1 )
        ptrWPL->PList->check_durch_bereiche( knoten, 3 );
      break;
      
    case BORDVOLL: //  Bordvollpunkte enthalten nur 1 Stationswert
      if( profData->ds_info[knoten] <= 1 )
      {
        ptrWPL->PList->DeleteNode( knoten, profData->ds_info, profData->typ ); //Datensatz mit Länge 1 löschen
        knoten--;
      }
      break;
      
    case MODELLGRENZEN: //  Modellgrenzen enthalten nur 1 Stationswert
      if( profData->ds_info[knoten] <= 1 )
      {
        ptrWPL->PList->DeleteNode( knoten, profData->ds_info, profData->typ ); //Datensatz mit Länge 1 löschen
        knoten--;
      }
      break;
      
    case DURCHST_BEREICH: //  Durchst.Bereiche enthalten nur 1 Stationswert
      if( profData->ds_info[knoten] <= 1 )
        ptrWPL->PList->check_durch_bereiche( knoten, 2 );
      break;
      
    case NWRINNE:
      if( ptrWPL->PList->check_nwrinne( knoten ) == 1 )
      {
        ptrWPL->PList->DeleteNode( knoten, profData->ds_info, profData->typ ); //Datensatz mit Länge 1 löschen
        knoten--;
      };
      break;
      
    case LP_TEXT:
      ptrWPL->PList->BauwerkansEnde( knoten, profData->ds_info, profData->typ );
      break;
      
    case BAUWERK:
      {
        int dkuk = ptrWPL->PList->ExistDatensatzTyp( DKUK );
        int dkok = ptrWPL->PList->ExistDatensatzTyp( DKOK );
        if( dkuk > 0 && dkok > 0 )
        {      
          int anzahl = max( profData->ds_info[dkok], profData->ds_info[dkuk] );
          
          ptrWPL->PList->DelDummySList();
          anzahl = ptrWPL->PList->BuildBauwerk( anzahl );
          profData->ds_info[knoten] = anzahl;
          ptrWPL->PList->BauwerkansEnde( knoten, profData->ds_info, profData->typ );
        }
        else
        {
          ptrWPL->PList->DeleteNode( knoten, profData->ds_info, profData->typ );
          knoten--;
        };
      }; // case BAUWERK
      break;
      
    case RECHTSWERT:
    case HOCHWERT:
    case COMMENT:
      ptrWPL->PList->BauwerkansEnde( knoten, profData->ds_info, profData->typ );
      break;
    }; // switch typ
  }; // for knoten
  
  
  ds_save_anzahl( profData );
  
  // jetzt den Header prüfen, ob er ok ist
  check_header( profData->slist_header, bQuer );
  
  // und danach den header schreiben schreiben
  ds_save_header( out, profData ); // Zeile 1-15 schreiben
  
  // Datensätze schreiben
  for (int num_ds = 1; num_ds <= profData->ds_info[0]; num_ds++)
  {
    if( profData->typ[num_ds] < MAUL ) // kein Sonderprofil
    {
      if( !LWA_PROJEKT )
      {
        if( WIN_116 || WIN120 )
        {
          if( profData->typ[num_ds] == DURCHST_BEREICH )
          {
            for ( int i = 1; i <= profData->ds_info[0]; i++ )
            {
              if( profData->typ[i] == UK_BRUECKE || profData->typ[i] == OK_BRUECKE )
              {
                if( !ptrWPL->PList->check_durch_bereiche( num_ds, FALSE ) )  //nur testen
                {
                  char buf[200],buf2[200],buf3[200];
                  xvt_res_get_str(STR_JA,buf,sizeof(buf));
                  xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
                  xvt_res_get_str(STR_DURCHBER_ASK,buf3,sizeof(buf3));
                  switch (xvt_dm_post_ask(buf,buf2,NULL,buf3))
                  {
                  case RESP_DEFAULT:
                    ptrWPL->PList->check_durch_bereiche(num_ds,TRUE);  //ändern  !!
                    break;
                  }
                }
                break;
              }
            }; // for i
          }; // if typ == DURCHST_BEREICH
        }; // if win116 || win120
      }
      ptrWPL->PList->SaveXY( out, num_ds );
    }
    else if( profData->typ[num_ds] >= MAUL && profData->typ[num_ds] < COMMENT )
      ptrWPL->PList->SaveSonderProfil(out,num_ds,typ[num_ds]);
    else if( profData->typ[num_ds] == COMMENT)
    {
      ptrWPL->PList->SaveXY( out, num_ds ); // bei Kommentar werden nur die Infozeilen gesichert
      WSP_SLIST* sList = profData->slist_comment;
      while( sList )
      {
        fprintf( out, "CC %s\n", sList->string );
        sList = sList->next;
      }; // while sList
    } // COMMENT
    else if( profData->typ[num_ds] == UNKNOWN )
      ptrWPL->PList->SaveXY( out, num_ds );
    else if( profData->typ[num_ds] >= 101 && profData->typ[num_ds] <= 137 ) // Längsschnitt
    {
      switch( profData->typ[num_ds] )
      {
      case BAUWERK:
        {
          int dkuk = ptrWPL->PList->ExistDatensatzTyp( DKUK );
          int dkok = ptrWPL->PList->ExistDatensatzTyp( DKOK );
          if( dkuk > 0 && dkok > 0 )
          {                               
            ptrWPL->PList->SaveXY( out, num_ds ); //  nur die Infozeilen sichern                        
            ptrWPL->PList->WriteDummySList( out );
          }
        }
        break;
        
      case LP_TEXT:
        ptrWPL->PList->SaveXY( out, num_ds ); // nur die Infozeilen sichern
        WriteLptext( out, profData );
        break;
        
      default:
        ptrWPL->PList->SaveXY( out, num_ds );
        break;
      }; // siwtch
    }; // if typ == ?
  }; // for i
  fclose(out);
  SaveProfilFile = FALSE;
  
  // falls global gearbeitet wird, auch ds_info und typ wieder auffrischen
  if( getGlobals )
  {
    for( int i = 0; i < TYPE_SIZE; i++ )
    {
      ds_info[i] = profData->ds_info[i];
      typ[i] = profData->typ[i];
    };
  }; // 
  
  return 0;
}; // save_profildatei
/********************************************/
int WriteLptext( FILE*file, PROFILDATA* pData )
// schreibt den Kommentar aus pData in eine Datei
// Parameter:
//        FILE* file: eine zum schreiben geöffnete Datei
//        PROFILDATA* pData: die zu schreibenden Daten
// Rückgabewert:
//        int: Fehlercode
//              0 kein Fehler
{
  WSP_SLIST* kommentList = pData->slist_comment;
  while( kommentList )
  {
    fprintf( file, "%s\n", kommentList->string );
    
    kommentList = kommentList->next;
  }; // while kommentList
  
  return 0;
}; // WriteLptext
   /****************************************************************************
   *             READ_DAT .CPP                                                 *
   *             14.10.1994                                                    *
   *    Einlesen der Profildateien / Strangtabelle aus selektierten *.str-Datei*
****************************************************************************/

int read_profil_dat(STRANG *strang_anfang)    // *.str-Datei lesen
{
  FILE *in;
  int len=0,i=0;
  char help[120],str_file[100];
  
  //initialisieren....
  str_gewaesser[0] = '\0';
  str_zustand[0] = '\0';
  str_file[0]='\0';
  
  if (prof_datei)
    xvt_slist_destroy(prof_datei);
  
  if  ((prof_datei = xvt_slist_create())==NULL)
  {
    xvt_dm_post_error(" Can't create _SLIST:prof_datei");
    return -1;
  }
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str_file,70); //280600 Bley auf 70 erhoeht
  strcat(str_file,"\\");
  strcat(str_file,STR_SPEC.name);
  
  if ((in= fopen(str_file,"rt+"))==NULL)
  {
    //xvt_dm_post_error(" Datei : %s läßt sich nicht öffnen !",str_file);
    char buf[200],buf2[200];//Dick 26.11.99
    xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
    xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
    xvt_dm_post_error("%s%s%s",buf,str_file,buf2);
    return -1;
  }
  else
  {
    is_profil_open =TRUE;
    new_profil=FALSE;
    
    fscanf(in,"%d",&anzahl_profil_dat_entries);
    fscanf(in,"%d",&anzahl_strang_entries);
    fscanf(in,"%s",&str_gewaesser );
    fscanf(in,"%s",&str_zustand );
    for (i=0;i<=(INT)strlen(str_zustand);i++)
      if (str_zustand[i]=='\n')  str_zustand[i]='\0';
      
      fgets(help,110,in);   //  '\n' ueberlesen
      if ((anzahl_profil_dat_entries>0)||(anzahl_strang_entries>0))
      {
        while (! feof(in))
        { 
          char m_name[20],station_als_ch[12],pk_ch[7];
          double st_wert;
          int vzk_i=0,pk_i=0;
          PROFIL profile[STRANGANZAHL];
          int k=0,k1,a,len,h;
          for (i=1;i<=anzahl_profil_dat_entries;i++)    //Profiltabelle lesen
          {
            if(!feof(in))
            {
              fgets(help,110,in);
              len=strlen(help);
              if (len>5)
              {
                for(int j=0;j<=(INT)strlen(help);j++)
                {
                  if(help[j]=='\n')
                    help[j]='\0';
                }
                //Neu ,Sortiert einlesen ,Dick 11.07.98      
                sscanf(help,"%s%lf%s%d",m_name,&st_wert,pk_ch,&vzk_i);
                if(st_wert<1000. && st_wert>-100.)
                  sprintf(station_als_ch,"%.4lf",st_wert);//Dick 18.08.98
                else if(st_wert<10000. && st_wert>-1000.)
                  sprintf(station_als_ch,"%.3lf",st_wert);//Dick 18.08.98
                else if(st_wert<100000. && st_wert>-10000.)
                  sprintf(station_als_ch,"%.2lf",st_wert);
                else if(st_wert<1000000. && st_wert>-100000.)
                  sprintf(station_als_ch,"%.1lf",st_wert);
                else 
                  sprintf(station_als_ch,"%.0lf",st_wert);
                a=17;                               
                len=strlen(station_als_ch);
                len=len-1;
                for(h=len;h>=0;h--,a--)
                  help[a]=station_als_ch[h];
                for(h=a;h>9;h--)//Eventuelle RestMüll
                  help[h]=' ';
                
                
                switch(pk_ch[0])
                {
                case '0':
                  pk_i=0;
                  break;
                case 'L':                              
                  pk_i=100;
                  if(pk_ch[2]!='\0')
                    pk_i+=atoi(&pk_ch[2]);                              
                  break;
                case 'F':
                  pk_i=200;
                  if(pk_ch[2]!='\0')
                    pk_i+=atoi(&pk_ch[2]);
                  break;
                case 'R':
                  pk_i=300;
                  if(pk_ch[2]!='\0')
                    pk_i+=atoi(&pk_ch[2]);
                  break;
                default:
                  pk_i=0;
                  break;
                }
                
                if(i==1)
                {
                  profile[i-1].station=st_wert; //1.Element ohne Sortierung
                  profile[i-1].vzk=vzk_i;
                  profile[i-1].pk = pk_i;
                }
                else
                {
                  for( k = 0; k < i - 1; k++ )
                  {
                    if ( CompareKeys( profile[k].station, profile[k].vzk, profile[k].pk, 
                      st_wert, vzk_i, pk_i, TRUE ) == 1 )
                    {
                      for( k1 = i-1; k1 > k; k1-- )//Wenn gefunden ,Platz für den Neue Element freimachen
                        profile[k1] = profile[k1 - 1];
                      break;
                    } // if ...
                  }; // for k
                  
                  profile[k].station = st_wert;
                  profile[k].vzk = vzk_i;
                  profile[k].pk = pk_i;
                }
                xvt_slist_add_at_pos( prof_datei, k, help, i - 1 );
              } 
            } //if (!feof(in))
          }
          
          strang_ptr = strang_anfang;
          
          for (i=1;i<=anzahl_strang_entries;i++)    //Strangtabelle lesen
          {
            if (strang_ptr !=NULL)
            {
              fscanf(in,"%lf", &strang_ptr->anfang);
              fscanf(in,"%lf", &strang_ptr->ende);  
              fscanf(in,"%s",  &strang_ptr->abstand_links);
              fscanf(in,"%s",  &strang_ptr->abstand_fluss);
              fscanf(in,"%s",  &strang_ptr->abstand_rechts);
              fscanf(in,"%s",  &strang_ptr->name_anfang);
              fscanf(in,"%s\n",&strang_ptr->name_ende);
              vzk_einlesen(); //in strang.cpp
              strang_ptr = strang_ptr->next;
            }
          }
          
     }   //end while..
   } //end :if (.. )||(.. ) >0
   fclose(in);
   is_Entry_in_SLIST=TRUE;     // SLISTE ist generiert
 }  //end else...    
 
 return anzahl_profil_dat_entries;//Dick 17.08.98
}


int save_str_datei(void)
//
// speichert die Strangtabelle 
//
// Seiteneffekte:
//            Liest:
//                FILE_SPEC STR_SPEC
//                char netz_dat[3][16]
//                int anzahl_profil_dat_entries
//                SLIST prof_datei
//                STRANG* strang_anfang
//            Ändert:
//                int anzahl_strang_entries
//                char str_gewaesser[16]
//                char str_zustand[16]
//                STRANG* strang_ptr // nur Hilfsvariable
//
{
  char *pstr;
  FILE *out;
  SLIST_ELT e;
  int zaehler=0;
  char temp[250];
  
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, temp,140);
  strcat(temp, "\\");
  strcat(temp, STR_SPEC.name);
  
  if ((out = fopen(temp,"r+"))==NULL)
  {/******** Datei existiert noch nicht - neu anlegen  **********/
    out = fopen(temp,"w+");
    strcpy(str_gewaesser,netz_dat[0]);
    strcpy(str_zustand,netz_dat[2]);
  }
  else
  {
    fclose (out);
    out = fopen(temp,"w+");
  }
  
  if (out==NULL)
    xvt_dm_post_error("***** Vernetzungsdatei *****\nkann nicht gesichert werden!");
  
  if (strang_anfang)
    strang_ptr = strang_anfang;
  else  strang_ptr = NULL;
  
  while (strang_ptr != NULL)
  {
    if ((strang_ptr->anfang != BCE_NAN)&&(strang_ptr->ende != BCE_NAN))
      zaehler++;
    strang_ptr = strang_ptr->next;
  }
  anzahl_strang_entries = zaehler;
  
  fprintf(out,"%5d ",anzahl_profil_dat_entries);
  fprintf(out,"%5d ",anzahl_strang_entries);
  
  fprintf(out,"%10s ",str_gewaesser);       //Gewässername
  fprintf(out,"%10s\n",str_zustand);     // Zustand
  
  for (e=xvt_slist_get_first(prof_datei);e!=NULL;
  e=xvt_slist_get_next(prof_datei,e))
  {
    pstr = xvt_slist_get(prof_datei,e,0L);
    fprintf(out,"%s\n",pstr);
  }
  fprintf(out,"\n");    // Leerzeile
  
  if (strang_anfang)
    strang_ptr = strang_anfang;
  else  strang_ptr = NULL;
  
  while (strang_ptr != NULL)
  {
    zaehler =0;
    if ((strang_ptr->anfang != BCE_NAN)&&(strang_ptr->ende != BCE_NAN))
    {
      fprintf(out,"%8lf ",strang_ptr->anfang);
      zaehler++;
      fprintf(out,"%8lf ",strang_ptr->ende);
      zaehler++;
      if (strlen(strang_ptr->abstand_links)==0)
      {
        strcpy(strang_ptr->abstand_links,"0.0");
      }
      fprintf(out,"%s ", strang_ptr->abstand_links);
      zaehler++;
      if (strlen(strang_ptr->abstand_fluss)==0)
      {
        strcpy(strang_ptr->abstand_fluss,"0.0");
      }
      fprintf(out,"%s ", strang_ptr->abstand_fluss);
      zaehler++;
      if (strlen(strang_ptr->abstand_rechts)==0)
      {
        strcpy(strang_ptr->abstand_rechts,"0.0");
      }
      fprintf(out,"%s ",strang_ptr->abstand_rechts);
      zaehler++;
      if(strlen(strang_ptr->name_anfang)>0)
        fprintf(out,"%s ",strang_ptr->name_anfang);
      else fprintf(out,"xxx.?");
      zaehler++;
      if(strlen(strang_ptr->name_ende)>0)
        fprintf(out,"%s\n",strang_ptr->name_ende);
      else  fprintf(out,"xxx.??\n");
      zaehler++;
      if (zaehler != 7)
      {
        //xvt_dm_post_note("Fehler beim speichern der Strangtabelle");
        char buf[200];
        xvt_res_get_str(STR_SAVE_STRANG_NOTE,buf,sizeof(buf));
        
        xvt_dm_post_error("%s",buf);
      }
    }
    
    strang_ptr = strang_ptr->next;
  }
  
  fclose(out);
  SaveStrangFile = FALSE;
  return 0;
}

/****************Plotterdatenbankroutinen************************/
#define DEFAULT_PLOT "  0  0  0  00  0  0  0  0  0"

/**************************************************
*             GET_Plot_HEADER                       *
*     Zeile 1-15 des Plotters lesen                *
***************************************************/
int get_Plot_header(void)
{
  char *temp;
  char init_line[7];
  char temp_ini[LENLINE+1];
  DWORD dw;
  for (int i=0;i<=14;i++)
  {
    sprintf(init_line,"LINE%d",i);
    dw=GetPrivateProfileString("DXF",init_line," ",temp_ini,LENLINE,Plot_Ini_Datei);
    if(dw>0)
    {
      if(temp_ini[0]=='$') 
        temp=&temp_ini[1];
      else
        temp=&temp_ini[0];
      xvt_slist_add_at_elt(header_profil,NULL,temp,i);
    }
    else
      xvt_slist_add_at_elt(header_profil,NULL,temp_ini,i);
    
  }
  return 1;
}
/*****************************************************************************/
int read_Plot_DB( FILE* in, WSP_PROFIL_LISTE *ptrWPL )
/*****  EINLESEN der Plotterdaten *****
Rückgabe: Lesen OK  : 0
Fehlerfall: 1
*/
{
  int fehler=0;
  int err_ds=0;//Dick 11.02.99
  BOOLEAN ret = FALSE;
  
  SLIST datenblocktypen;
  SLIST_ELT e;
  int i;
  
  if (fehler != 1)
  {
    if (ptrWPL!=NULL)
    {
      if (ptrWPL->PList!=NULL)
        ptrWPL->PList->DeleteList();
      
    }
    else return 1;
    
    if (xvt_slist_is_valid(header_profil))
    {
      xvt_slist_destroy(header_profil);
      header_profil =NULL;
    }
    
    if((header_profil=xvt_slist_create())==NULL)
    {
      fehler = 1;
    }
    if (fehler ==0)
    {
      get_Plot_header();
      if ((datenblocktypen = xvt_slist_create())==NULL)
      {
        // Fehlerbehandlung ;
        xvt_dm_post_error(" Can't create SLIST:datenblocktypen ");
        return 1;
      }
      
      // Datenblocktypen aus Resource:"wspdlg.rc" lesen
      read_res_datentypen(datenblocktypen,2);  //in: util.cpp
      ds_info[0]=xvt_slist_count(datenblocktypen);
      if (ds_info[0]==0)
      {
        //xvt_dm_post_error("In der Resoursen sind keine Datensätze definiert.\nBitte Support anrufen!");
        char buf[200],buf2[200];
        xvt_res_get_str(STR_IN_DATENSATZ,buf,sizeof(buf));
        xvt_res_get_str(STR_Y_SORTIERT,buf2,sizeof(buf2));
        xvt_dm_post_error("%s\n%s",buf,buf2); 
        fehler = 1;
        
        return fehler;
      }
      
      
      anzahl_ds = ds_info[0];   // Anzahl der vorhandenen Datensätze
      i=1;
      for(e=xvt_slist_get_first(datenblocktypen);e!=NULL;e=xvt_slist_get_next(datenblocktypen,e))
      {
        typ[i++]=*xvt_slist_get_data(e);
      }
      
      /********** Datensätze lesen *******************************/
      for (int num_ds = 1; num_ds <= anzahl_ds; num_ds++)
      {
        ptrWPL->PList->MakeNewNode(num_ds);   // neuen Datensatzknoten generieren
        
        ///* die folgenden beiden Fkt-aufrufe immer zusammen */
        ptrWPL->PList->WriteTypDaten(num_ds,typ[num_ds],"Standart");
        switch(ptrWPL->PList->GetProfilTyp())    //Datenblocktyp ermitteln
        { case 1:
        IS_COMMENT = TRUE;
        break;
        case 2:
          exist_plot =TRUE;
          break;
        }
        ptrWPL->PList->GetPlotDatenInfo3(num_ds);
        /* ende :die folgenden beiden Fkt-aufrufe immer zusammen */
      }
      for(int i=num_ds;i<TYPE_SIZE-1;i++)//Dick 21.08.98 Restliche Müll  //Dick 8.12.98
        typ[i]=0;
      is_profil_open = TRUE;
      new_profil = FALSE;
    }
    if( exist_plot )   // Hinweis ausgeben
    {
      /*
      //xvt_dm_post_note("Vorhandener Plotterdatensatz kann\nnicht aktualisiert werden,\nwird jedoch in Datei '**.plt' \nabgelegt !");
      char buf[200];
      xvt_res_get_str(STR_PLOTTERDATENSATZ,buf,sizeof(buf));
      xvt_dm_post_note("%s",buf);
      */
      
      if (typ[ds_info[0]+1]==DATPLOT) //Korrektur von ds_info[0] wurde durchgeführt
        write_datplt(in,file_spec.name,(BOOLEAN*)&exist_plot ,0);
      else
        write_datplt(in,file_spec.name,(BOOLEAN*)&exist_plot ,1);
    }
    
    if (err_ds != 0 )
      GaussProfilMitRuecksprung=TRUE;
  } //-if (fehler !=-1)
  
  xvt_slist_destroy(datenblocktypen);
  return fehler;
}

int save_Plot_header(void)
{
  SLIST_ELT elt;
  char *p ;
  char init_line[7];
  char temp[LENLINE+1];
  BOOL dw;
  int i=0;
  int k;       
  
  if (xvt_slist_is_valid(header_profil))
    for (elt = xvt_slist_get_first(header_profil); elt != NULL;
    elt = xvt_slist_get_next(header_profil,elt) )
    {
      sprintf(init_line,"LINE%d",i++); 
      p=xvt_slist_get(header_profil, elt, NULL);
      strcpy(temp,"$");
      if(i==3 || i==5 || i==6 || i==7) //weil i++
      {
        for(k=1;k<40 && k < (int)strlen(p);k++)
          temp[k]=p[k-1];
        temp[k]='\0';
      }
      else
        strcat(temp,p);
      dw=WritePrivateProfileString("DXF",init_line,temp,Plot_Ini_Datei);
      if(!dw)
        return 0;
    }
    return 1;
}


int  save_Plot_DB(WSP_PROFIL_LISTE *ptrWPL)
{
  int fehler=0;
  anzahl_ds = ds_info[0];   // Anzahl der vorhandenen Datensätze
  if(save_Plot_header())
  {
    for (int num_ds = 1; num_ds <= anzahl_ds; num_ds++)
    {
      if(!ptrWPL->PList->WritePlotDatenInfo3(num_ds))
      {
        fehler=1;
        break;
      }
    }
    
  }
  SaveProfilFile =FALSE;
  return fehler;
}

/**************************************************
*             change_Plot_HEADER                       *
*     Zeile 1-15 des Plotters ändern                *
***************************************************/
int change_Plot_header(void)
{
  char *temp,*temp_header;
  char init_line[7];
  char temp_ini[LENLINE+1];
  DWORD dw;
  int k;
  for (int i=0;i<=14;i++)
  {
    sprintf(init_line,"LINE%d",i);
    dw=GetPrivateProfileString("DXF",init_line," ",temp_ini,LENLINE,Plot_Ini_Datei);
    
    
    if(dw>0)
    {
      if(temp_ini[0]=='$') 
        temp=&temp_ini[1];
      else
        temp=&temp_ini[0];
      switch(i)
      {
      case 0:
      case 8:
      case 13:
        break;
      case 2:
      case 4:
      case 5:
      case 6:
        temp_header=xvt_slist_get_elt(header_profil,i,0);
        for(k=0;k<40 && temp[k]!='\0' && k<(int)strlen(temp_header);k++)
          temp_header[k]=temp[k];
        xvt_slist_change_str(header_profil,temp_header,i);
        break;
      default:
        //temp=xvt_slist_get_elt(header_profil,i,0);                       
        xvt_slist_change_str(header_profil,temp,i);
        break;
        
      }
      
    }
    
    
  }
  return 1;
}

int  change_Plot_profildatei(WSP_PROFIL_LISTE *ptrWPL)
{
  int fehler=0;
  anzahl_ds = ds_info[0];   // Anzahl der vorhandenen Datensätze
  if(change_Plot_header())
  {
    for (int num_ds = 1; num_ds <= anzahl_ds; num_ds++)
    {
      ptrWPL->PList->ChangePlotDatenInfo3(num_ds);            
    }
    
  }
  else
    fehler=1;
  
  return fehler;
}
