/****************************************************************************
*             Dialog 143 : Anzeige Trennflächen                            *
*             27.01.1995                                                    *
****************************************************************************/

#include <windows.h>
#include "xvt.h"

#include "wsphilfe.h"
#include "resource.h"
#include "wspwin.h"
#include "typen.h"

#include "global_types.h"
#include "global_vars.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"
#include "bce_allg.h"
#include "paint.h"

/*
#define DLG_RES_ID DLG_143
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL
*/

// globale Variablen

WINDOW edit_dlg143[2];
WINDOW heightWnd143[2];

long datenblock_typ; // der DatenblockTyp, den dieser dieser Dialog editiert

// externe globale Variablen

extern XVT_HELP_INFO hi;
extern WINDOW dlg_sonderprofil;
extern int fehler;
extern BOOLEAN sichere_datenblock;
extern MinMax pmm;


/*************   GHJ   *************/
static WNDPROC defWndProc;
LRESULT CALLBACK Dlg143WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
  case WM_HELP:
    {
      LPHELPINFO lphi = (LPHELPINFO)lParam;
      if (hi!=NULL_HELP_INFO)
      {
        xvt_help_display_topic(hi, HID_KAPITEL_4_4_3_3);
      }
    }
    break;
    
  default:
    break;
  }
  return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/

void UpdateWin117()
{
  // neu zeichnen
	if( WIN_117 != NULL_WIN )
	{
		xvt_dwin_invalidate_rect( WIN_117, 0 );
		paint->DrawTrennflaechen( WIN_117, &pmm, scr.datensatz, COLOR_BROWN );
	}
};

/*!
 * Setzt den Wert der Trennfläche neu und aktualisiert den Dialog und
 * das Grafikfenster
 *
 * @param value : der neue Wert
 * @param id : 0 : links, 1 : rechts
 *
 */
void setValuesDlg143( const double leftVal, const double rightVal )
{
  // die Textfelder neu setzen
  if( edit_dlg143[0] != NULL_WIN && edit_dlg143[1] != NULL_WIN && 
    heightWnd143[0] != NULL_WIN && heightWnd143[1] != NULL_WIN  )
  {
    double newLeftVal = leftVal; // den Wert kopieren
    double newRightVal = rightVal;

    // konsistenz checken: falls nicht ok, die alten Werte nehmen
    if( newLeftVal >= newRightVal )
    {
      newLeftVal = scr.z[0];
      newRightVal = scr.z[1];
    };

    // die Geländehöhe an diesen Stationen rausfinden
    double leftHeight = list->Get_Station_Hoehe( newLeftVal );
    double rightHeight = list->Get_Station_Hoehe( newRightVal );

    // jetzt die Textfelder beschreiben
    char str[256];
    sprintf( str, "%.4lf", newLeftVal );
    xvt_vobj_set_title( edit_dlg143[0], str );  //rechts
    sprintf( str, "%.4lf", leftHeight );
    xvt_vobj_set_title( heightWnd143[0], str );
    sprintf( str, "%.4lf", newRightVal );
    xvt_vobj_set_title( edit_dlg143[1], str );  //rechts
    sprintf( str, "%.4lf", rightHeight );
    xvt_vobj_set_title( heightWnd143[1], str );

    if( scr.z[0] != newLeftVal || scr.z[1] != newRightVal )
    {
      scr.z[0] = newLeftVal;
      scr.z[1] = newRightVal;

      // die Daten übernehmen
      SaveProfilFile = TRUE;
      list->SaveSonderprofildaten( &scr, datenblock_typ );
      sichere_datenblock = FALSE;
    } // ...
  }; // if edit_dlg143
}; // setValue

/*!
 * Setzt den Wert der Trennfläche neu, vorher wird
 * validiert, ob die Station existiert
 * Falls nicht, wird eine Fehlermeldung ausgegeben und nichts verändert.
 *
 * @param valueStr : der neue Wert (als String)
 * @param id : 0 : links, 1 : rechts
 *
 */
void validateAndSet()
{
  // Fehlermeldung vorbereiten
  char leftBuf[256], rightBuf[256], buf2[256], buf3[256];
  xvt_res_get_str( STR_LINKE_TRENNFL, leftBuf, sizeof( rightBuf ) );
  xvt_res_get_str( STR_RECHTE_TRENNFL, rightBuf, sizeof( leftBuf ) );
  xvt_res_get_str( STR_Y_WERT, buf2, sizeof( buf2 ) );
  xvt_res_get_str( STR_NOEXIST, buf3, sizeof( buf3 ) );

  // die Textfelder auslesen
  char leftStr[15], rightStr[15];
  xvt_vobj_get_title( edit_dlg143[0], leftStr, 14 );
  xvt_vobj_get_title( edit_dlg143[1], rightStr, 14 );

  double leftVal = get_zahl( leftStr );
  if( leftVal != BCE_NAN && !list->ExistStation( leftVal, 1 ) )
  {
    xvt_dm_post_error( "%s:\n%s %.4lf m %s", leftBuf, buf2, leftVal, buf3 ); // "Linke Trennfläche:\ny-Wert %4.2lf m existiert nicht"
    leftVal = BCE_NAN;
  }; // if value != BCE_NAN

  double rightVal = get_zahl( rightStr );
  if( rightVal != BCE_NAN && !list->ExistStation( rightVal, 1 ) )
  {
    xvt_dm_post_error( "%s:\n%s %.4lf m %s", rightBuf, buf2, rightVal, buf3 ); // "Linke Trennfläche:\ny-Wert %4.2lf m existiert nicht"
    rightVal = BCE_NAN;
  }; // if value != BCE_NAN

  if( leftVal != BCE_NAN && rightVal != BCE_NAN )
  {
    setValuesDlg143( leftVal, rightVal );
    UpdateWin117();
  };
}; // validateAndSet


/* Handler for dialog DLG_143 ("Trennflächen")  */

long XVT_CALLCONV1 DLG_143_eh XVT_CALLCONV2( WINDOW xdWindow, EVENT *xdEvent )
{
  short xdControlId = xdEvent->v.ctl.id;
  
  switch( xdEvent->type ) 
  {
  case E_CREATE:
    {
      datenblock_typ = xvt_vobj_get_data( xdWindow );

      /*************   GHJ   *************/
      if( WIN_116 != NULL_WIN )
        SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(WIN_116, ATTR_NATIVE_WINDOW));
      else if (WIN120!=NULL_WIN)
        SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(WIN120, ATTR_NATIVE_WINDOW));
      defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
      SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg143WindowProc);
      ChangeFontAndSize((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));	// GHJ
      RECT rect;
      POINT pt1, pt2;
      if (WIN_116!=NULL_WIN)
      {
        ::GetWindowRect(::GetDlgItem((HWND)xvt_vobj_get_attr(WIN_116,ATTR_NATIVE_WINDOW), WIN_GRAPH_116_LISTBUTTON_33), &rect);
        pt1.x = rect.left;
        pt1.y = rect.top;
        ::ScreenToClient((HWND)xvt_vobj_get_attr(WIN_116,ATTR_NATIVE_WINDOW), &pt1);
        pt1.y += 3*(rect.bottom-rect.top);
        pt1.x -= 10;
        ::GetWindowRect((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), &rect);
        pt2.x = pt1.x + rect.right-rect.left;
        pt2.y = pt1.y + rect.bottom-rect.top;
        ::MoveWindow((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), pt1.x, pt1.y, pt2.x-pt1.x, pt2.y-pt1.y, TRUE);
      }
      else if (WIN120!=NULL_WIN)
      {
        ::GetWindowRect(::GetDlgItem((HWND)xvt_vobj_get_attr(WIN120,ATTR_NATIVE_WINDOW), WIN_120_LISTBUTTON_33), &rect);
        pt1.x = rect.left;
        pt1.y = rect.top;
        ::ScreenToClient((HWND)xvt_vobj_get_attr(WIN120,ATTR_NATIVE_WINDOW), &pt1);
        pt1.y += 3*(rect.bottom-rect.top);
        pt1.x -= 10;
        ::GetWindowRect((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), &rect);
        pt2.x = pt1.x + rect.right-rect.left;
        pt2.y = pt1.y + rect.bottom-rect.top;
        ::MoveWindow((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), pt1.x, pt1.y, pt2.x-pt1.x, pt2.y-pt1.y, TRUE);
      }
      xvt_vobj_set_visible(xdWindow, TRUE);
      /***********************************/
      
      // Internationale Texte setzen

      // Dialog-Titel
      char resString[256];
      long titleID = 0;
      switch( datenblock_typ )
      {
      case TRENNFLAECHEN:
        titleID = STR_DLG143_TITLE;
        break;

      case BORDVOLL:
        titleID = STR_DLG143_TITLE_BV;
        break;

      case MODELLGRENZEN:
        titleID = STR_DLG143_TITLE_MG;
        break;
      };
      
      xvt_res_get_str( titleID, resString, sizeof(resString) );      
      xvt_vobj_set_title( xvt_win_get_ctl( xdWindow, DLG_143_TEXT_7 ), resString );

      // links und rechts
      xvt_res_get_str( STR_DLG143_LEFT, resString, sizeof(resString) );
      xvt_vobj_set_title( xvt_win_get_ctl( xdWindow, DLG_143_TEXT_1 ), resString );
      xvt_res_get_str( STR_DLG143_RIGHT, resString, sizeof(resString) );
      xvt_vobj_set_title( xvt_win_get_ctl( xdWindow, DLG_143_TEXT_2 ), resString );

      // Lage
      WINDOW locaWindow = xvt_win_get_ctl( xdWindow, DLG_143_PUSHBUTTON_8 );

      xvt_res_get_str( STR_DLG143_LOCA, resString, sizeof(resString) );
      xvt_vobj_set_title( locaWindow, resString );

      if( datenblock_typ != TRENNFLAECHEN )
        xvt_vobj_set_visible( locaWindow, FALSE );

      // Internationale Texte setzen
      dlg_sonderprofil = xdWindow;
      sichere_datenblock = FALSE;
      edit_dlg143[0] = xvt_win_get_ctl(xdWindow,DLG_143_EDIT_3);
      edit_dlg143[1] = xvt_win_get_ctl(xdWindow,DLG_143_EDIT_4);
      heightWnd143[0] = xvt_win_get_ctl( xdWindow, DLG_143_EDIT_HEIGHT_L );
      heightWnd143[1] = xvt_win_get_ctl( xdWindow, DLG_143_EDIT_HEIGHT_R );
      
      double leftVal = scr.z[0];
      if( leftVal == BCE_NAN )
        leftVal = pmm.minX;

      double rightVal = scr.z[1];
      if( rightVal == BCE_NAN )
        rightVal = pmm.maxX;

      setValuesDlg143( leftVal, rightVal );
      UpdateWin117();

      if( hi != NULL_HELP_INFO )
        xvt_help_set_win_assoc( hi, xdWindow, HID_KAPITEL_4_4_3_3, 0L );
    }
    break;

  case E_DESTROY:
    {
      dlg_sonderprofil = NULL_WIN;
      edit_dlg143[0] = NULL_WIN;
      edit_dlg143[1] = NULL_WIN;
      heightWnd143[0] = NULL_WIN;
      heightWnd143[1] = NULL_WIN;
    }
    break;

  case E_CLOSE:
    {
      char buf[200],buf2[200],buf3[200],buf4[200];
      xvt_res_get_str(STR_JA,buf,sizeof(buf));
      xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
      xvt_res_get_str(STR_ABBRECHEN,buf3,sizeof(buf3));
      xvt_res_get_str(STR_SICHERE_DATENBLOCK,buf4,sizeof(buf4));
      if( sichere_datenblock )
      {
        switch( xvt_dm_post_ask( buf, buf2, buf3, "%s", buf4 ) )
        {
        case RESP_DEFAULT:       //SICHERN
          {
            validateAndSet();

            dlg_sonderprofil = NULL_WIN;
            sichere_datenblock = FALSE;
            xvt_vobj_destroy( xdWindow );
          } // case RESP_DEFAULT
          break;

        case RESP_2:             // nicht sichere_datenblock
          sichere_datenblock = FALSE;
          break;             //zurück
        case RESP_3:
          sichere_datenblock = FALSE;
          break;
        } // switch
      } // if sichere_datenblock
      xvt_vobj_destroy( xdWindow );
    }
    break;

  case E_CONTROL:
    {
      switch( xdControlId )
      {
      case DLG_143_PUSHBUTTON_5: /* "OK" */
        {
          validateAndSet();
          xvt_vobj_destroy( xdWindow );
        }
        break;
        
      case DLG_143_PUSHBUTTON_8: /* "Lage.." */
        {
          xvt_dlg_create_res(WD_MODAL,DLG_145, EM_ALL, DLG_145_eh, 0L);
        }
        break;
        
      case DLG_143_EDIT_3:
        {   /*		Edit control was operated.		*/
          if (xdEvent->v.ctl.ci.v.edit.focus_change)
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {  	/*		focus has entered the control		*/
            }
            else
            {  /*		focus has left the control			*/
              validateAndSet();
            }
          }
          else
          { /*		Contents of control were changed		*/
          }
        }
        break;

      case DLG_143_EDIT_4:
        {   /*		Edit control was operated.		*/
          if( xdEvent->v.ctl.ci.v.edit.focus_change )
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {  	/*		focus has entered the control		*/
            }
            else
            {  /*		focus has left the control			*/
              validateAndSet();
            }
          }
          else
          { /*		Contents of control were changed		*/
          }
        }
        break;
      default:
        break;
    }
    }
    break;
  default:
    break;
  }
  return 0L;
}
