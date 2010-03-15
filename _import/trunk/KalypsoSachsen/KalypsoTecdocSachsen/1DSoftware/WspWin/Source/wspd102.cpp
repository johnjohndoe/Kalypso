/****************************************************************************
*        ABOUT - BOX                                                        *
*                                                                           *
*             WSPD100 .CPP                                                  *
*             25.04.1996                                                    *
****************************************************************************/
#include <windows.h>
#include <locale.h>
#include "xvt.h"

#include "resource.h"

#include "global_vars.h"

#include "..\..\wspdlg\include\export.h"

/*
#define DLB_ABOUT_100

#define DLG_RES_ID DLG_ABOUT
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL
*/

/*	Handler for dialog DLG_ABOUT_100 ("Info-Box" )  */
long about_timer_id;

long XVT_CALLCONV1 DLG_ABOUT_eh XVT_CALLCONV2( WINDOW xdWindow, EVENT* xdEvent )
{
  short xdControlId = xdEvent->v.ctl.id;
  
  switch (xdEvent->type) 
  {
  case E_CREATE:
    {
      char* buffer = (char*)malloc( sizeof( char ) * 1000 );

      HMODULE hModule = GetModuleHandle( NULL );

      // Texte setzen

      // Titel
      xvt_res_get_str( STR_SPLASH_TITLE, buffer, 1000 );
      WINDOW ctrlW = xvt_win_get_ctl( xdWindow, IDC_SPLASH_STATIC_TITLE );
      if( ctrlW != NULL_WIN )
        xvt_vobj_set_title( ctrlW, buffer );

      // Version
      strncpy( buffer, "Version ", 1000 );

      char vStr[256];
      GetFeatureVersion( vStr, hModule );

      strcat( buffer, vStr );
      strcat( buffer, " " );
      if (GetFeature("wsp_lwa_version"))
        strcat( buffer, "LWA" );
      else
        strcat( buffer, "BCE" );

      if( !GetFeature( "wsp_nodemo" ))
        strncat( buffer, " Demo", strlen( " Demo" ) );

      ctrlW = xvt_win_get_ctl( xdWindow, DLG_ABOUT_VERSION );
      if( ctrlW != NULL_WIN )
        xvt_vobj_set_title( ctrlW, buffer );

      // Lizenztext
      xvt_res_get_str( STR_SPLASH_LICENCE, buffer, 1000 );
      ctrlW = xvt_win_get_ctl( xdWindow, IDC_SPLASH_STATIC_LICENCE );
      if( ctrlW != NULL_WIN )
        xvt_vobj_set_title( ctrlW, buffer );
      ctrlW = xvt_win_get_ctl( xdWindow, IDC_SPLASH_STATIC_LICENCE1 );

      LPCSTR licence = GetFeatureLicence( 0 );
      strncpy( buffer, licence, 1000 );
      if( ctrlW != NULL_WIN )
        xvt_vobj_set_title( ctrlW, buffer );

      licence = GetFeatureLicence( 1 );
      strncpy( buffer, licence, 1000 );
      ctrlW = xvt_win_get_ctl( xdWindow, IDC_SPLASH_STATIC_LICENCE2 );
      if( ctrlW != NULL_WIN )
        xvt_vobj_set_title( ctrlW, buffer );

      // Copyright + Datum
      LPSTR newLocale = (LPSTR)malloc( sizeof( char ) * 1000 ); // Hoffentlich nie länger als 1000
      GetLocaleInfo( LOCALE_USER_DEFAULT, LOCALE_SENGLANGUAGE, newLocale, 1000 );

      char* oldLocale = setlocale( LC_TIME, NULL ); // die alte Einstellung merken
      setlocale( LC_TIME, newLocale );

      time_t date = GetFeatureDate( hModule );
      strftime( buffer, 1000, "B C E, %B %Y", gmtime( &date ) );
      ctrlW = xvt_win_get_ctl( xdWindow, DLG_ABOUT_DATE );
      if( ctrlW != NULL_WIN )
        xvt_vobj_set_title( ctrlW, buffer );

      setlocale( LC_TIME, oldLocale );
      free( newLocale );
      free( buffer );
    }
    break;

  case E_DESTROY:
    /*	Dialog has been closed; last event sent to dialog.	*/
    {
    }
    break;
  case E_CLOSE:
      xvt_vobj_destroy( xdWindow);
    break;
    
  case E_CONTROL:
    /*		User operated control in dialog.		*/
    {
      switch(xdControlId) 
      {
      case DLG_ABOUT_OK: /* "OK" */
        xvt_vobj_destroy( xdWindow );
        break;

      default:
        break;
      }
    }
    break;
  case E_TIMER:
    /*		Timer associated with window went off.		*/
    xvt_timer_destroy( about_timer_id );
    xvt_vobj_destroy( xdWindow );
    break;
    
  default:
    break;
  }
  return 0L;
}
