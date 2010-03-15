/***************************************************
					  CONFIGURATION.CPP

  Hilfsfunktionen zum Auslesen der
  Konfigurationseinstellungen in der WSPWIN.INI

****************************************************/

#include "windows.h"
#include <Tchar.h>

#include "configuration.h"


BOOL GetSortStrangVorwaerts()
// gibt zur�ck, ob die Strangtabelle vorw�rts ( TRUE ) oder
// R�ckw�rts ( FALSE ) sortiert werden soll
{
  TCHAR hlpString[MAX_PATH];
  
  GetPrivateProfileString( CONF_SECTION_NAME, CONF_KEY_SORT_DIR, CONF_VAL_VORWAERTS, hlpString,
                           MAX_PATH, CONF_INI_FILE_NAME );
  
  if ( _tcscmp( hlpString, CONF_VAL_VORWAERTS ) == 0)
    return TRUE;
  else
    return FALSE;
}; // GetSortStrangVorwaerts

BOOL GetSortVerzweigt()
// gibt zur�ck, ob verzweigte Systeme automatisch ( FALSE ) oder
// per Hand ( TRUE ) sortiert werden sollen
{
  TCHAR hlpString[MAX_PATH];

  GetPrivateProfileString( CONF_SECTION_NAME, CONF_KEY_VERZEIGT, CONF_VAL_HAND, hlpString, 
                           MAX_PATH, CONF_INI_FILE_NAME );  

  if ( _tcscmp( hlpString, CONF_VAL_AUTO ) == 0 )
    return FALSE;
  else
    return TRUE;
}; // GetSortVerzweigt