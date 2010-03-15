/***************************************************

  allgemeine BCE Funktionen
  
    BCE DLL
    created: 25.03.96 - Andresen
****************************************************/
#include <windows.h>
#include "xvt.h"

#include "resource.h"

#include "global_defs.h"
#include "wspwin.h"

#include "..\..\wspdlg\include\export.h"

#include "bce_allg.h"


/*************************************************************************
set_menu_116

		Menüeinstellungen für Grafikeditor: WIN_116
    
*************************************************************************/
void set_menu_116(WINDOW xdWindow,BOOLEAN zustand )
/*  auf  false bei: Win116-create */
{
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_43_44,zustand); //Projekte speich. unter
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_43_45,zustand); // Projekte
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_43_46,zustand); // Arhivieren(H)
  
  xvt_menu_set_item_enabled( xdWindow, TASK_MENUBAR_2_3, zustand ); // Menu: Massenberechnung
  xvt_menu_set_item_enabled( xdWindow, TASK_MENUBAR_30_7, zustand ); // Menu: Spiegellinienberechnung
  
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_39,zustand); // editieren Zustandsdatei
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_14_16,zustand); // Menu: Abflußdatei
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_14_17,zustand); // Menu: Verlustdatei
  
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_6,zustand); // Menu: Konvertieren

  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_30_29_58,zustand );  //DXF(H)
  
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_67, TRUE );//Plotten:DXF

  
  
  xvt_menu_update(xdWindow);
}
/**************************************************************************/
void disable_menu(WINDOW xdWindow)
{
  
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_50,FALSE); // Menu: Zustandsdatei disabled setzen
  
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_39,FALSE); // Menu: Profildatei enabled setzen
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_14_16,FALSE); // Menu: Abflußdatei
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_14_17,FALSE); // Menu: Verlustdatei


  xvt_menu_set_item_enabled( xdWindow, TASK_MENUBAR_2_3, FALSE ); // Menu: Massenberechnung
  xvt_menu_set_item_enabled( xdWindow, TASK_MENUBAR_30_7, FALSE ); // Menu: Spiegellinienberechnung
  
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_6,FALSE); //Fremddaten
  
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_30,FALSE);
  
  xvt_menu_update(xdWindow);
}

/**************************************************************************/
void set_menu_120(WINDOW xdWindow,BOOLEAN zustand)
{
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_14_16,zustand); // Menu: Abflußdatei
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_14_17,zustand); // Menu: Verlustdatei
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_43_44,zustand); //Projekte speich. unter
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_43_45,zustand); // Projekte
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_43_46,zustand); // Arhivieren(H)
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_6,zustand); //Fremddaten
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_30_29,zustand);//Ergebnisse(H)
  xvt_menu_update(xdWindow);
}

/**************************************************************************/
void set_menu_dlg136(WINDOW xdWindow,BOOLEAN zustand)
{
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_59,zustand); //Profildatei:Interpolation
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_14_16,  zustand); //Randbedingungen Abflussdatei
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_14_17,  zustand); //Randbedingungen Verlustdatei

  xvt_menu_set_item_enabled( xdWindow, TASK_MENUBAR_2_3, zustand ); // Menu: Massenberechnung
  xvt_menu_set_item_enabled( xdWindow, TASK_MENUBAR_30_7, zustand ); // Menu: Spiegellinienberechnung

  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_6,   zustand); //Fremddaten
  
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_60,   !zustand);//Profildatei-Geländeverknüpfung
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_64,   !zustand);//Profildatei-Flächenberechnung
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_39,zustand); // Menu: Zustandsdatei
  
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_30_29_58, zustand );  //DXF(H)
  
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_43_44,zustand); //Projekte speich. unter
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_43_45,zustand); // Projekte
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_43_46,zustand); // Arhivieren(H)
  
  xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_50,zustand);
  
  xvt_menu_update(xdWindow);
}

/**************************************************************************/
int is_zahl( char* str )
/* Die Funktion is_zahl() testet ob der übergebene String str eine
gültige Zahl ist.
Zusätzlich wird eine Konvertierung vom Dezimalkomma in den Dezimalpunkt
durchgeführt.
Gültige Zeichen für str: [ 0..9] '+'  '-'  '.'  ','

  Rückgabewert:
  is_zahl liefert einen Wert größer NULL zurück, wenn str eine gültige
  Zahl ist
  Im Fehlerfall: 0 falsche Ziffer,
  -1 Stringlänge =null                 */
{
  int digit = 0,
    fehler = 1,
    len=0;
  
  len = strlen(str);
  if (len > 0)
  {
    for (int i=0;i<len;i++)
    {
      
      if ((str[i] == '.')&&( digit ))
        fehler = 0;
      
      if ((str[i] == '.')&&( !digit ))
        digit =1;
      
      if ((str[i] == ',')&&( !digit ))
      {
        digit =1;
        str[i]='.';
      }
      if ((str[i] == ',')&&( digit ))
        fehler =0;
      if ((str[i]<'+') || (str[i]>'9'))
      {
        if ((str[i]==' ')&&(str[i+1]=='\0'))
        {
          if (fehler!=0)
            fehler=1;
          str[i]='\0';
        }
        else
          fehler =0;
        
      }
      else
      {
        if (str[i] =='/')        fehler = 0;
        if ((str[i] =='+')&&(i>0)) fehler = 0;
        if ((str[i] =='-')&&(i>0)) fehler = 0;
      }
    }
    if (!digit)
    {
      str[len] = '.';
      str[len+1] = '0';
      str[len+2] = '0';
      str[len+3] = '0';
      str[len+4] = '0';
      str[len+5] = '\0';
    }
  }
  else fehler = -1;
  return fehler;
}

/***** get_zahl *********************************************************************/
/*! 
 * Prüft, ob ein String eine Dezimalzahl repräsentiert
 * Gibt eine Fehlermeldung aus, falls die nicht der Fall ist
 * Benutzt int is_zahl( char* ) zur validierung
 *
 * @param valueStr : der zu prüfende String. Nicht const, da kommas durch Punkte ersetzt werden (für atof)
 *
 * @return double  : der Wert des String, BCE_NAN bei Fehler
 */
double get_zahl( char* valueStr )
{
  if( is_zahl( valueStr ) == 0 )
  {
    char buf[200];
    xvt_res_get_str( STR_ERROR_INPUT, buf, sizeof( buf ) );     // "Fehler in der Eingabe !";
    xvt_dm_post_error( "%s", buf );

    return BCE_NAN;
  }
  else
    return atof( valueStr );
}; // validate


/**************************************************************************/
void NewSetFontFunc(WINDOW win)
{
  HWND hwnd;
  
  hwnd =(HWND) xvt_vobj_get_attr(win,ATTR_NATIVE_WINDOW);
  SendMessage(hwnd, WM_SETFONT,(UINT) GetStockObject(SYSTEM_FIXED_FONT), TRUE);
  //SendMessage(hwnd, WM_SETFONT,(UINT) GetStockObject(OEM_FIXED_FONT), TRUE);
}
/***************************************************************************/
int check_string(char *str)
/* Funktion check_string() testet,
ob der übergebene String *str	Umlaute oder Blank enthält.
Rückgabe:  1  fehlerfrei
0  String enthält falschen Charakter         */
{
  for (int i=0;i<(INT)strlen(str);i++)
  {
    if ( (str[i]<33)||(str[i]>126) )
      return 0;
    if ((str[i]>58)&&(str[i]<64))
      return 0;
    if ( (str[i]==34)||(str[i]==39)||(str[i]==42)||
      (str[i]==43)||(str[i]==44)||(str[i]==47)||
      (str[i]==91)||(str[i]==93)||(str[i]==96)||(str[i]==124) )
      return 0;
    if (str[i]==34)
      return 0;
  }
  return 1;
}
/***************************************************************************/
void  xvt_slist_change_str(SLIST list,char *str,int pos)
{
  /* tauscht alten String an der Stelle pos in SLIST gegen String *str aus*/
  SLIST_ELT e;
  
  e=xvt_slist_get_first(list);
  for (int h=1;h<=pos;h++)
    e=xvt_slist_get_next(list,e);
  xvt_slist_rem(list,e);
  xvt_slist_add_at_pos(list,pos,str,0L);
}
