#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include <math.h>

#include "global_types.h"

#include "list.h"
#include "typen.h"
#include "strang.h"

#include "global.h"
#include "util2.h"

#include "strang2.h"

/***********************************/
extern char strang_start[15],
				strang_end[15],
				station208[20],
				vzk[20],pk[20],
				uebergabe_name[15];
extern BOOLEAN am_anfang_einfuegen,
					als_naechster_anfang,
					istverzweigt;
extern SLIST auswahl_list2;
SLIST strang_slist; //extern noch in 211
/***********************************/

void strang_in_slist(void)
 {
  //DIE FUNKTION PACKT DIE STRANG-LISTE IN EINE SLIST ZUR AUSGABE IN WSPD211
  //IN EINEM LISTEDIT AUFRUF AUS WSPD211 bei Verzweigten Systemen

  char zeile[200], help_string[15];
  int i;

  strang_slist=xvt_slist_create();
  strang_ptr=strang_anfang;
  if(strang_ptr!=NULL)
	{
	 while(strang_ptr->anfang!=BCE_NAN)
	  {
		for(i=0;i<51;i++)
		 zeile[i]=' ';
		zeile[51]='\0';

       sprintf(help_string,"%lf",strang_ptr->anfang); 
		for(i=0;i<(INT)strlen(help_string);i++)
		  zeile[i]=help_string[i];

		if(strang_ptr->ende!=BCE_NAN)
		 {
       sprintf(help_string,"%lf",strang_ptr->ende); 
		  
			for(i=0;i<(INT)strlen(help_string);i++)
			zeile[i+11]=help_string[i];
		 }

		for(i=0;i<(INT)strlen(strang_ptr->abstand_links);i++)
		 zeile[i+21]=strang_ptr->abstand_links[i];

		for(i=0;i<(INT)strlen(strang_ptr->abstand_fluss);i++)
		 zeile[i+31]=strang_ptr->abstand_fluss[i];

		for(i=0;i<(INT)strlen(strang_ptr->abstand_rechts);i++)
		 zeile[i+41]=strang_ptr->abstand_rechts[i];

		xvt_slist_add_at_elt(strang_slist,NULL,zeile,0L);
		strang_ptr=strang_ptr->next;
	  }
	} //IF STRANG-ANFANG!=NULL
 }

 /**************************************************************************/
void verzweigtes_profil_sortieren(int autosort, BOOL vorwaerts)
//
// sortiert ein profil ( durch station208, vzk, pk und uebergabe_name gegeben) in ein 
// verzweigtes System ein
//
// Parameter:
//        int autosort: falls 1 wird das Profil automatisch einsortiert
//                      falls 0 wird das profil einsortiert wie in dialog 211 festgelegt
// Seiteneffekte:
//          Lesen:
//              char station208[20]: 
//              char vzk[20]
//              char pk[20]
//              char uebergabe_name[20]: die 4 Schlüsseldaten des neuen Profils
//              BOOLEAN am_anfang_einfuegen:  bei manueller Sortierung: Profil wird am Anfang eingefügt
//              BOOLEAN als_naechster_anfang: bei manueller Sortierung: falls TRUE wird das Profil hinter das in Dialog 211 ausgewälte eingefügt, 
//                                                                      falls FALSE, wird nur das Strangende des ausgewälten Profils geändert
//          Ändern:
//              STRANG* strang_anfang: anfang und ende des Strangs werden ggfls. aktualisiert
//              STRANG* strang_ende
{
  // das neue Element erzeugen:
  STRANG* neuer_strang = new STRANG;
  neuer_strang->anfang = atof( station208 );
  strcpy( neuer_strang->name_anfang, uebergabe_name );
  neuer_strang->strang_vzk = atoi( vzk );

  int pkTmp;
  switch( pk[0] )
  {
  case '0':
    pkTmp = 0;
    break;
  case 'L':
    pkTmp = 100;
    if( pk[2]!='\0')
      pkTmp += atoi( &pk[2] );
    break;
  case 'F':
    pkTmp = 200;
    if( pk[2]!='\0' )
      pkTmp += atoi( &pk[2] );
    break;
  case 'R':
    pkTmp = 300;
    if( pk[2]!='\0' )
      pkTmp += atoi( &pk[2] );
    break;
  default:
    pkTmp = 0;
    break;
  } // switch pk
  neuer_strang->strang_pk = pkTmp;

  // Zeiger auf das Element suchen, hinter welchem eingefügt werden soll
  STRANG* such_strang = strang_anfang;
  STRANG* found_strang = NULL;  // dieser Zeiger zeigt nach der Suche auf jenes Element, hinter welchem einsortiert werden soll

  if ( autosort )
  {
    als_naechster_anfang = TRUE; // nur für manuelles einfügen relevant
    while( such_strang && such_strang->anfang != BCE_NAN )
    {
      if ( CompareKeys( neuer_strang->anfang, neuer_strang->strang_vzk, neuer_strang->strang_pk, 
                        such_strang->anfang, such_strang->strang_vzk, such_strang->strang_pk,
                        vorwaerts ) <= 0 )
        break;
      found_strang = such_strang;
      such_strang = such_strang->next;
    }; // while
  }
  else if ( !am_anfang_einfuegen ) 
  {
    double start = atof(strang_start);
    double ende = strang_end[0] == '\0' ? BCE_NAN : atof(strang_end);

    while( such_strang && such_strang->anfang != BCE_NAN )
    {
      if ( such_strang->anfang == start && such_strang->ende == ende )
        break;
      such_strang = such_strang->next;
    }; // while
    found_strang = such_strang;
  };


  // falls nicht 'als_naechster_anfang' einfach das Element found_strang ändern und fertig
  if( !als_naechster_anfang )
  {
    found_strang->ende = neuer_strang->anfang;;
    strcpy( found_strang->name_ende, neuer_strang->name_anfang );
    delete neuer_strang; // wird doch nicht gebraucht
    neuer_strang = NULL;
  }
  else // hinter strang_found einfügen oder ganz am anfang einfügen
  {
    // weil die maximale stranglaenge konstant sein soll ( warum auch immer ) erstmal das Ende finden
    // und löschen
    such_strang = strang_anfang;
    while ( such_strang && such_strang->next && such_strang->next->next )
      such_strang = such_strang->next;
    delete such_strang->next;
    such_strang->next = NULL;
    such_strang->ende = BCE_NAN;
    such_strang->name_ende[0] = '\0';
    strang_ende = such_strang;
    
    
    // jetzt in den Strang einsortieren
    if ( found_strang )
    {
      neuer_strang->next = found_strang->next;
      found_strang->next = neuer_strang;
      
      found_strang->ende = neuer_strang->anfang;
      strcpy( found_strang->name_ende, neuer_strang->name_anfang );
      
      neuer_strang->nummer = found_strang->nummer + 1;
    }
    else // !found_strang:  Am Anfang einsortieren
    {
      neuer_strang->next = strang_anfang;
      strang_anfang = neuer_strang;
      neuer_strang->nummer = 1;
    };
    
    // und noch die restlichen Daten aktualisieren
    if ( neuer_strang->next && neuer_strang->next->anfang != BCE_NAN )
    {
      neuer_strang->ende = neuer_strang->next->anfang;
      strcpy( neuer_strang->name_ende, neuer_strang->next->name_anfang );
    }
    else
    {
      neuer_strang->ende = BCE_NAN;
      neuer_strang->name_ende[0] = '\0';
    };
    
    such_strang = neuer_strang->next;
    while ( such_strang && such_strang->anfang != BCE_NAN )
    {
      such_strang->nummer++;
      such_strang = such_strang->next;
    };
  }; // als_naechster_einfuegen

    
  // zuletzt die Abstände ausrechnen
  if ( neuer_strang )
  {
    if ( neuer_strang->ende != BCE_NAN )
    {
      neuer_strang->abstand = fabs( neuer_strang->ende - neuer_strang->anfang );
      sprintf( neuer_strang->abstand_links, "%lf", neuer_strang->abstand );
      sprintf( neuer_strang->abstand_fluss, "%lf", neuer_strang->abstand );
      sprintf( neuer_strang->abstand_rechts, "%lf", neuer_strang->abstand );
    }
    else
    {
      neuer_strang->abstand = BCE_NAN;
      neuer_strang->abstand_links[0] = '\0';
      neuer_strang->abstand_fluss[0] = '\0';
      neuer_strang->abstand_rechts[0] = '\0';
    }; // if ende != BCE_NAN
  }; // if neuer_strang;

  if ( found_strang )
  {
    if ( found_strang->ende != BCE_NAN )
    {
      found_strang->abstand = fabs( found_strang->ende - found_strang->anfang );
      sprintf( found_strang->abstand_links, "%lf", found_strang->abstand );
      sprintf( found_strang->abstand_fluss, "%lf", found_strang->abstand );
      sprintf( found_strang->abstand_rechts, "%lf", found_strang->abstand );
    }
    else
    {
      found_strang->abstand = BCE_NAN;
      found_strang->abstand_links[0] = '\0';
      found_strang->abstand_fluss[0] = '\0';
      found_strang->abstand_rechts[0] = '\0';
    }; // if ende != BCE_NAN
  };
}; // verzweigtes_profil_sortieren

int CompareKeys( double station1, int vzk1, int pk1, double station2, int vzk2, int pk2, BOOL bRichtung /* = TRUE */ )
// vergleicht zwei Profil-Schlüssel
// Parameter:
//          double station1, int vzk1, int pk1: Daten des ersten Schlüssels
//          double station2, int vzk2, int pk2: Daten des ersten Schlüssels
//          BOOL bRichtung: die Sortierrichtung: TRUE = Vorwärts, FALSE = Rückwärts
// Rückgabewert:
//          -1: schlüssel 1 < schlüssel 2
//           0: schlüssel 1 == schlüssel 2
//           1: schlüssel 1 > schlüssel 2
{
  int vorwaerts;
  if ( bRichtung )
    vorwaerts = 1;
  else
    vorwaerts = -1;

  /************
  s1 == s2 genau dann wenn s1.anfang = s2.anfang && s1.vzk == s2.vzk && s1.pk == s2.pk
  *************/

  if ( station1 == station2 && vzk1 == vzk2 && pk1 == pk2 )
    return 0;

  /**************
  s1 < s2 genau dann wenn
             [ ( s1.vzk == 0 || s2.vzk == 0 ) && 
               ( ( s1.anfang < s2.anfang ) || ( s1.anfang == s2.anfang && s1.pk < s2.pk ) ) ] ||
             [ s1.vzk != 0 && s2.vzk != 0 &&
              ( ( s1.vzk > s2.vzk ) ||
                ( s1.vzk == s2.vzk && s1.station < s2.station ) ||
                ( s1.vzk == s2.vzk && s1.station == s2.station && s1.pk < s2.pk )
              ) ]
  ***************/

  if (
       (
         ( vzk1 == 0 || vzk2 == 0 ) &&
         ( ( station1 < station2 ) || ( station1 == station2 && pk1 < pk2 ) )
       ) ||
       (
         ( vzk1 != 0 && vzk2 != 0 ) &&
         (
           ( vzk1 > vzk2 ) ||
           ( vzk1 == vzk2 && ( station1 < station2 || ( station1 == station2 && pk1 < pk2 )  )  )
         )
       )
     )
    return -1 * vorwaerts;
  else
    return +1 * vorwaerts;
}; // CompareKeys