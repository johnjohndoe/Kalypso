/*	wspw131.cpp: Handler for window WIN_130 ("Optionen Datensatz")*/

#include <windows.h>
#include "xvt.h"

#include "resource.h"
#include "wsphilfe.h"
#include "wspwin.h"
#include "..\..\wspdlg\include\export.h"

#include "typen.h"
#include "l_typen.h"

#include "global_vars.h"

#include "list.h"

/*
#define WIN_RES_ID WIN_130
#define WIN_FLAGS 0x800L
#define WIN_CLASS ""
#define WIN_BORDER W_DOC

*/

typedef struct _DATENSATZOPTIONEN
{
	 int datensatz,
     linientyp,		//Linientyp 3.3.1
     stationswerte,  //Darst. der Stationswerte im Schriftfeld 3.3.3
     hoehenlinie,    //Darstellung der Höhenlinien 3.3.2
     hoehenwerte,    //Darstellung der Höhenwerte im Schriftfeld 3.3.4
     show_as,        //Format für 'hoehenwerte'
     sfeldzeilen,    //Schriftfeldzeile 3.3.5
     slines,         //senkrechte Linien.. 3.3.6
     text,           //Textlage 3.3.7
     text_size,        //Textgröße
     symbolschalter;
} DATENSATZOPTIONEN; // wspw130

DATENSATZOPTIONEN datensatzoptionen;
WINDOW WIN_LINES, lb130[9], WIN130;
int  linestyle, local_ds, art;
char str130[100], local_str[50];
BOOLEAN data_changed = FALSE;

extern BOOLEAN Plot_DB;//Dick 9.12.98
extern WINDOW WIN121;

/*************   GHJ   *************/

extern  XVT_HELP_INFO hi;
static WNDPROC defWndProc;
LRESULT CALLBACK Win130WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
  case WM_HELP:
    {
      LPHELPINFO lphi = (LPHELPINFO)lParam;
      if (hi!=NULL_HELP_INFO)
        xvt_help_display_topic(hi, HID_KAPITEL_6_7_1_1_4);
    }
    break;
    
  default:
    break;
  }
  return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/

/*	Handler for window WIN_130 ("Optionen Datensatz")*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
WIN_130_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
WIN_130_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
  short xdControlId = xdEvent->v.ctl.id;
  
  switch (xdEvent->type) {
  case E_CREATE:
    {
      /*************   GHJ   *************/
      defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
      SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Win130WindowProc);
      /***********************************/
      char buf[200];
      WIN130=xdWindow;//Dick 4.12.98
      /* Bildschirm anpassen*/
      RCT rct_win;
      xvt_vobj_get_outer_rect(xdWindow,&rct_win);
      
      char tmp[15];
      lb130[0]=xvt_win_get_ctl(xdWindow,WIN_130_DATENSATZ);
      lb130[1]=xvt_win_get_ctl(xdWindow,WIN_130_STATION);
      lb130[2]=xvt_win_get_ctl(xdWindow,WIN_130_HLINE);
      lb130[3]=xvt_win_get_ctl(xdWindow,WIN_130_HWERT);
      lb130[4]=xvt_win_get_ctl(xdWindow,WIN_130_SHOW);
      lb130[5]=xvt_win_get_ctl(xdWindow,WIN_130_SFELD);
      lb130[6]=xvt_win_get_ctl(xdWindow,WIN_130_SLINE);
      lb130[7]=xvt_win_get_ctl(xdWindow,WIN_130_TEXT);
      lb130[8]=xvt_win_get_ctl(xdWindow,WIN_130_TEXTSIZE);
      
      xvt_res_get_str(STR_W130_Y_WERT_1,buf,sizeof(buf));
      xvt_list_add(lb130[1],0,buf);
      xvt_res_get_str(STR_W130_Y_WERT_2,buf,sizeof(buf));
      xvt_list_add(lb130[1],1,buf);
      if(!Plot_DB)//Dick 9.12.98
      {
        xvt_res_get_str(STR_W130_Y_WERT_3,buf,sizeof(buf));
        xvt_list_add(lb130[1],2,buf);
      }
      
      xvt_res_get_str(STR_W130_H_LIN_1,buf,sizeof(buf));
      xvt_list_add(lb130[2],0,buf);
      xvt_res_get_str(STR_W130_H_LIN_2,buf,sizeof(buf));
      xvt_list_add(lb130[2],1,buf);
      if(!Plot_DB)//Dick 9.12.98
      {
        xvt_res_get_str(STR_W130_H_LIN_3,buf,sizeof(buf));
        xvt_list_add(lb130[2],2,buf);
      }
      
      xvt_res_get_str(STR_W130_H_WERT_1,buf,sizeof(buf));
      xvt_list_add(lb130[3],0,buf);
      xvt_res_get_str(STR_W130_H_WERT_2,buf,sizeof(buf));
      xvt_list_add(lb130[3],1,buf);
      if(!Plot_DB)//Dick 9.12.98
      {
        xvt_res_get_str(STR_W130_H_WERT_3,buf,sizeof(buf));
        xvt_list_add(lb130[3],2,buf);
      }
      
      xvt_res_get_str(STR_W130_KOMMA_1,buf,sizeof(buf));
      xvt_list_add(lb130[4],0,buf);
      xvt_res_get_str(STR_W130_KOMMA_2,buf,sizeof(buf));
      xvt_list_add(lb130[4],1,buf);
      xvt_res_get_str(STR_W130_KOMMA_3,buf,sizeof(buf));
      xvt_list_add(lb130[4],2,buf);
      xvt_res_get_str(STR_W130_KOMMA_4,buf,sizeof(buf));
      xvt_list_add(lb130[4],3,buf);
      
      xvt_res_get_str(STR_W130_SCHRIFT_1,buf,sizeof(buf));
      xvt_list_add(lb130[5],0,buf);
      xvt_res_get_str(STR_W130_SCHRIFT_2,buf,sizeof(buf));
      xvt_list_add(lb130[5],1,buf);
      xvt_res_get_str(STR_W130_SCHRIFT_3,buf,sizeof(buf));
      xvt_list_add(lb130[5],2,buf);
      xvt_res_get_str(STR_W130_SCHRIFT_4,buf,sizeof(buf));
      xvt_list_add(lb130[5],3,buf);
      xvt_res_get_str(STR_W130_SCHRIFT_5,buf,sizeof(buf));
      xvt_list_add(lb130[5],4,buf);
      
      xvt_res_get_str(STR_W130_LIN_ART_1,buf,sizeof(buf));
      xvt_list_add(lb130[6],0,buf);
      xvt_res_get_str(STR_W130_LIN_ART_2,buf,sizeof(buf));
      xvt_list_add(lb130[6],1,buf);
      xvt_res_get_str(STR_W130_LIN_ART_3,buf,sizeof(buf));
      xvt_list_add(lb130[6],2,buf);
      xvt_res_get_str(STR_W130_LIN_ART_4,buf,sizeof(buf));
      xvt_list_add(lb130[6],3,buf);
      
      xvt_res_get_str(STR_W130_RICHTUNG_1,buf,sizeof(buf));
      xvt_list_add(lb130[7],0,buf);
      xvt_res_get_str(STR_W130_RICHTUNG_2,buf,sizeof(buf));
      xvt_list_add(lb130[7],1,buf);
      xvt_res_get_str(STR_W130_RICHTUNG_3,buf,sizeof(buf));
      xvt_list_add(lb130[7],2,buf);
      xvt_res_get_str(STR_W130_RICHTUNG_4,buf,sizeof(buf));
      xvt_list_add(lb130[7],3,buf);
      
      xvt_res_get_str(STR_W130_SCHRIFTGROSSE_1,buf,sizeof(buf));
      xvt_list_add(lb130[8],0,buf);
      xvt_res_get_str(STR_W130_SCHRIFTGROSSE_2,buf,sizeof(buf));
      xvt_list_add(lb130[8],1,buf);
      xvt_res_get_str(STR_W130_SCHRIFTGROSSE_3,buf,sizeof(buf));
      xvt_list_add(lb130[8],2,buf);
      
      list->GetDateninfo(lb130[0]);  //Datensätze anzeigen
      
      local_ds = datensatzoptionen.datensatz=GELAENDEHOEHE;
      
      list->GetDatenInfo3(&str130[0],datensatzoptionen.datensatz);
      
      char *p130;
      p130=&str130[0];
      while (p130[0]!='\0')
      {
        while ((p130[0]!='\0')&&(p130[0]==' '))
          p130++;
        tmp[0]=p130[0];
        if (p130[1]!=' ')
        {
          tmp[1]=p130[1];
          tmp[2]='\0';
          p130++;
        }
        else tmp[1]='\0';
        datensatzoptionen.linientyp =atoi(tmp);
        
        p130++;
        linestyle =datensatzoptionen.linientyp;
        while ((p130[0]!='\0')&&(p130[0]==' '))
          p130++;
        datensatzoptionen.hoehenlinie = p130[0]-48;p130++;
        xvt_list_set_sel(lb130[2],datensatzoptionen.hoehenlinie,TRUE);
        
        while ((p130[0]!='\0')&&(p130[0]==' '))
          p130++;
        datensatzoptionen.stationswerte = p130[0]-48;p130++;
        xvt_list_set_sel(lb130[1],datensatzoptionen.stationswerte,TRUE);
        
        while ((p130[0]!='\0')&&(p130[0]==' '))
          p130++;
        datensatzoptionen.hoehenwerte = p130[0]-48;p130++;
        xvt_list_set_sel(lb130[3],datensatzoptionen.hoehenwerte,TRUE);
        
        if (p130[0]!=' ')  // !! 2.Ziffer einlesen ??
        {
          datensatzoptionen.show_as = p130[0]-48;p130++;
          xvt_list_set_sel(lb130[4],datensatzoptionen.show_as,TRUE);
        }
        else
        {
          datensatzoptionen.show_as = 0;
          xvt_list_set_sel(lb130[4],datensatzoptionen.show_as,TRUE);
        }
        
        while ((p130[0]!='\0')&&(p130[0]==' '))
          p130++;
        datensatzoptionen.sfeldzeilen = p130[0]-48;p130++;
        xvt_list_set_sel(lb130[5],datensatzoptionen.sfeldzeilen,TRUE);
        
        while ((p130[0]!='\0')&&(p130[0]==' '))
          p130++;
        datensatzoptionen.slines = p130[0]-48;p130++;
        xvt_list_set_sel(lb130[6],datensatzoptionen.slines,TRUE);
        
        while ((p130[0]!='\0')&&(p130[0]==' '))
          p130++;
        datensatzoptionen.text = p130[0]-48;p130++;
        xvt_list_set_sel(lb130[7],datensatzoptionen.text,TRUE);
        
        while ((p130[0]!='\0')&&(p130[0]==' '))
          p130++;
        datensatzoptionen.text_size = p130[0]-48;p130++;
        xvt_list_set_sel(lb130[8],datensatzoptionen.text_size,TRUE);
        
        while ((p130[0]!='\0')&&(p130[0]==' '))
          p130++;
        datensatzoptionen.symbolschalter = p130[0]-48;p130++;
        while (p130[0]!='\0')	p130++;
      }
      
      /*Fenster: Linientypen anzeigen*/
      WIN_LINES=xvt_win_create_res(LINES, (xdWindow), EM_ALL,LINES_eh, 0L);
      if (WIN_LINES == NULL_WIN)
        xvt_dm_post_error("Can't open window:LINES");
      ChangeFontAndSize((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));	// GHJ
      xvt_vobj_set_visible(xdWindow, TRUE);		// GHJ
      BringWindowToTop((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));
    }
    break;
  case E_DESTROY:
    /*	Window has been closed; last event sent to window.	*/
    {
      if (WIN121 != NULL_WIN)
        xvt_vobj_set_visible(WIN121,TRUE);
      
      if (WIN_LINES !=NULL_WIN)
        xvt_vobj_destroy(WIN_LINES);
      WIN_LINES =NULL_WIN ;
      WIN130=NULL;
    }
    return 0L;
  case E_UPDATE:
		/*
    Window requires updating.
    */
    {
      xvt_dwin_clear(xdWindow,WspwinMainBackgroundColor);
    }
    break;
  case E_CLOSE:
    {
      xvt_vobj_destroy(xdWindow);
    }
    break;
  case E_CONTROL:
    /*		User operated control in window.	*/
    {
      switch(xdControlId) {
      case WIN_130_PUSHBUTTON_21: /* "OK" */
        {
          datensatzoptionen.linientyp=linestyle;
          datensatzoptionen.stationswerte= xvt_list_get_sel_index(lb130[1]);
          datensatzoptionen.hoehenlinie= xvt_list_get_sel_index(lb130[2]);
          datensatzoptionen.hoehenwerte= xvt_list_get_sel_index(lb130[3]);
          datensatzoptionen.show_as= xvt_list_get_sel_index(lb130[4]);
          datensatzoptionen.sfeldzeilen= xvt_list_get_sel_index(lb130[5]);
          datensatzoptionen.slines= xvt_list_get_sel_index(lb130[6]);
          datensatzoptionen.text= xvt_list_get_sel_index(lb130[7]);
          datensatzoptionen.text_size= xvt_list_get_sel_index(lb130[8]);
          
          for (int i =0;i<100;i++) str130[i]=' ';  //initialisieren !!
          
          char temp[15];
          itoa(datensatzoptionen.linientyp,temp,10);
          str130[2]=temp[0] ;
          if (temp[1] !='\0')
            str130[3]=temp[1] ;
          
          str130[5]=datensatzoptionen.hoehenlinie+48;
          str130[8]=datensatzoptionen.stationswerte+48;
          
          str130[11]=datensatzoptionen.hoehenwerte+48;
          str130[12]=datensatzoptionen.show_as+48;
          
          str130[15]=datensatzoptionen.sfeldzeilen+48;
          str130[18]=datensatzoptionen.slines+48;
          
          str130[21]=datensatzoptionen.text+48;
          str130[24]=datensatzoptionen.text_size+48;
          
          str130[27]=datensatzoptionen.symbolschalter+48;
          str130[28]='\0';
          
          list->SaveDatenInfo3(&str130[0],datensatzoptionen.datensatz);
          SaveProfilFile=TRUE;
          
          xvt_vobj_destroy(xdWindow);
        }
        break;
      case WIN_130_PUSHBUTTON_22: /* "Abbrechen" */
        {
          if ((data_changed)|| (SaveProfilFile))
          {
            char buf[200],buf2[200],buf3[200],buf4[200];
            xvt_res_get_str(STR_JA,buf,sizeof(buf));
            xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
            xvt_res_get_str(STR_ZURUECK,buf3,sizeof(buf3));
            xvt_res_get_str(STR_PLOT101_ASK,buf4,sizeof(buf4));
            switch(xvt_dm_post_ask(buf,buf2,buf3,buf4))
              //switch(xvt_dm_post_ask("Ja","Nein","Zurück","Sollen geänderte Daten gesichert werden ?"))
            {
            case RESP_DEFAULT:
              {
                xdEvent->type =E_CONTROL;
                xdEvent->v.ctl.id=WIN_130_PUSHBUTTON_21;  // OK-Button
                xvt_win_dispatch_event(xdWindow,xdEvent);
              }
              break;
            case RESP_2:
              xvt_vobj_destroy(xdWindow);
              break;
            case RESP_3:
              break;
            }
          }
          else
            xvt_vobj_destroy(xdWindow);
        }
        break;
      case WIN_130_DATENSATZ: /* "List Button 23" */
        {
          /*  Daten von altem Datensatz vorher sichern */
          datensatzoptionen.linientyp=linestyle;
          datensatzoptionen.stationswerte= xvt_list_get_sel_index(lb130[1]);
          datensatzoptionen.hoehenlinie= xvt_list_get_sel_index(lb130[2]);
          datensatzoptionen.hoehenwerte= xvt_list_get_sel_index(lb130[3]);
          datensatzoptionen.show_as= xvt_list_get_sel_index(lb130[4]);
          datensatzoptionen.sfeldzeilen= xvt_list_get_sel_index(lb130[5]);
          datensatzoptionen.slines= xvt_list_get_sel_index(lb130[6]);
          datensatzoptionen.text= xvt_list_get_sel_index(lb130[7]);
          datensatzoptionen.text_size= xvt_list_get_sel_index(lb130[8]);
          
          for (int i =0;i<100;i++) str130[i]=' ';  //init !!
          
          char temp[15];
          itoa(datensatzoptionen.linientyp,temp,10);
          str130[2]=temp[0] ;
          if (temp[1] !='\0')
            str130[3]=temp[1] ;
          
          str130[5]=datensatzoptionen.hoehenlinie+48;
          str130[8]=datensatzoptionen.stationswerte+48;
          str130[11]=datensatzoptionen.hoehenwerte+48;
          str130[12]=datensatzoptionen.show_as+48;
          str130[15]=datensatzoptionen.sfeldzeilen+48;
          str130[18]=datensatzoptionen.slines+48;
          str130[21]=datensatzoptionen.text+48;
          str130[24]=datensatzoptionen.text_size+48;
          str130[27]=datensatzoptionen.symbolschalter+48;
          str130[28]='\0';
          
          list->SaveDatenInfo3(&str130[0],datensatzoptionen.datensatz);
          /* Ende:Daten von altem Datensatz vorher sichern */
          
          local_ds =(xvt_list_get_sel_index(lb130[0]) + 1  );
          char buf[200];//Dick 26.11.99
          
          /* teste, ob Datensatz KREIS, MAUL ... ist und ausschließen*/
          switch (typ[local_ds])
          {
          case LP_TEXT:
          case BAUWERK:
          case COMMENT:
          case UNKNOWN:
            local_ds = datensatzoptionen.datensatz ;
            xvt_res_get_str(STR_WSPW130_NOTE_1,buf,sizeof(buf));
            xvt_dm_post_error("%s",buf); //"Optionen sind bei diesem Datensatz nicht zulässig !");
            xvt_list_set_sel(lb130[0],datensatzoptionen.datensatz-1,TRUE);
            break;
          default:
            datensatzoptionen.datensatz = local_ds ;
            break;
          }
          
          xvt_list_get_first_sel(lb130[0],local_str,49);
          list->GetDatenInfo3(&str130[0],datensatzoptionen.datensatz);
          
          char tmp[15], *p130;
          
          p130=&str130[0];
          while (p130[0]!='\0')
          {
            while ((p130[0]!='\0')&&(p130[0]==' '))
              p130++;
            
            tmp[0]=p130[0];
            if (p130[1]!=' ')
            {
              tmp[1]=p130[1];
              tmp[2]='\0';
              p130++;
            }
            else tmp[1]='\0';
            datensatzoptionen.linientyp =atoi(tmp);
            
            p130++;
            linestyle =datensatzoptionen.linientyp;
            
            while ((p130[0]!='\0')&&(p130[0]==' '))
              p130++;
            datensatzoptionen.hoehenlinie = p130[0]-48;p130++;
            xvt_list_set_sel(lb130[2],datensatzoptionen.hoehenlinie,TRUE);
            
            while ((p130[0]!='\0')&&(p130[0]==' '))
              p130++;
            datensatzoptionen.stationswerte = p130[0]-48;p130++;
            xvt_list_set_sel(lb130[1],datensatzoptionen.stationswerte,TRUE);
            
            while ((p130[0]!='\0')&&(p130[0]==' '))
              p130++;
            datensatzoptionen.hoehenwerte = p130[0]-48;p130++;
            xvt_list_set_sel(lb130[3],datensatzoptionen.hoehenwerte,TRUE);
            
            if (p130[0]!=' ')  // !! 2.Ziffer einlesen ??
            {
              datensatzoptionen.show_as = p130[0]-48;p130++;
              xvt_list_set_sel(lb130[4],datensatzoptionen.show_as,TRUE);
            }
            else
            {
              datensatzoptionen.show_as = 0;
              xvt_list_set_sel(lb130[4],datensatzoptionen.show_as,TRUE);
            }
            
            while ((p130[0]!='\0')&&(p130[0]==' '))
              p130++;
            datensatzoptionen.sfeldzeilen = p130[0]-48;p130++;
            xvt_list_set_sel(lb130[5],datensatzoptionen.sfeldzeilen,TRUE);
            
            while ((p130[0]!='\0')&&(p130[0]==' '))
              p130++;
            datensatzoptionen.slines = p130[0]-48;p130++;
            xvt_list_set_sel(lb130[6],datensatzoptionen.slines,TRUE);
            
            while ((p130[0]!='\0')&&(p130[0]==' '))
              p130++;
            datensatzoptionen.text = p130[0]-48;p130++;
            xvt_list_set_sel(lb130[7],datensatzoptionen.text,TRUE);
            
            while ((p130[0]!='\0')&&(p130[0]==' '))
              p130++;
            datensatzoptionen.text_size = p130[0]-48;p130++;
            xvt_list_set_sel(lb130[8],datensatzoptionen.text_size,TRUE);
            
            while ((p130[0]!='\0')&&(p130[0]==' '))
              p130++;
            datensatzoptionen.symbolschalter = p130[0]-48;p130++;
            while (p130[0]!='\0')	p130++;
          }
          xvt_dwin_invalidate_rect(WIN_LINES,0);
      }
      break;
    case WIN_130_STATION: /* "List Button 31" */
      {
        xvt_list_get_first_sel(lb130[0],local_str,49);
        datensatzoptionen.stationswerte= xvt_list_get_sel_index(lb130[1]);
        
        if (datensatzoptionen.stationswerte==2)
        {
          art = STATIONSWERTE;
          if (!xvt_dlg_create_res(WD_MODAL, DLG_161, EM_ALL,DLG_161_eh,0L))
            xvt_dm_post_error("Can't open dialog: DLG_161");
        }
        
        data_changed = TRUE;
      }
      break;
    case WIN_130_HLINE: /* "List Button 32" */
      {
        xvt_list_get_first_sel(lb130[0],local_str,49);
        datensatzoptionen.hoehenlinie= xvt_list_get_sel_index(lb130[2]);
        
        if (datensatzoptionen.hoehenlinie==2)
        {
          art = HOEHENWERTE;
          if (!xvt_dlg_create_res(WD_MODAL, DLG_161, EM_ALL,DLG_161_eh,0L))
            xvt_dm_post_error("Can't open dialog: DLG_161");
        }
        data_changed = TRUE;
      }
      break;
    case WIN_130_HWERT: /* "List Button 33" */
      {
        xvt_list_get_first_sel(lb130[0],local_str,49);
        datensatzoptionen.hoehenwerte= xvt_list_get_sel_index(lb130[3]);
        
        if (datensatzoptionen.hoehenwerte==2)
        {
          art = HOEHENWERTE;
          if (!xvt_dlg_create_res(WD_MODAL, DLG_161, EM_ALL,DLG_161_eh,0L))
            xvt_dm_post_error("Can't open dialog: DLG_161");
        }
        data_changed = TRUE;
      }
      break;
      
    case WIN_130_SHOW: /* "List Button 34" */
    case WIN_130_SFELD: /* "List Button 35" */
    case WIN_130_SLINE: /* "List Button 36" */
    case WIN_130_TEXT: /* "List Button 37" */
    case WIN_130_TEXTSIZE: /* "List Button " */
      {
        data_changed = TRUE;
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
  xvt_tx_process_event(xdWindow, xdEvent);
  return 0L;
}
