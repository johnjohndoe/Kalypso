/*	 DLG_136 ("Erfassung / Editierung der Vernetzungsdatei")*/

#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "global_vars.h"
#include "..\..\wspdlg\include\export.h"

#include "strang.h"
#include "list.h"

#include "readprof.h"
#include "read_cfg.h"


#include "global.h"

#include <lzexpand.h>
#include <string>

#include "profproj.h"
#include "profpro2.h"
#include "util2.h"
#include "verzweig.h"
#include "strang2.h"
#include "read.h"
#include "aufnehm.h"

#include "bce_allg.h"
#include "wsphilfe.h"
#include "configuration.h"
#include "util.h"

extern XVT_HELP_INFO hi;

/*
	Information about the dialog
*/
#define DLG_RES_ID DLG_136
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS

#define QUIT       2 // Rückgabewerte aus dlg136 für id_dlg136
#ifndef DELETE
#define DELETE     1
#endif
#define GRAFIK     3
#define NEU        4
#define ALPHA      5

extern char dateiname[15],
            zustand[20],
            pk[20],
            vzk[20],
            station208[20],
            uebergabe_name[15],
            profil_nr_string[8],
            name208[20];//Dick 29.03.99   
extern WSP_PROFIL_LISTE *pWPL;
extern int dlg135ref,
           ok_file,
           anzahl_profile_int;
extern WINDOW listbox129,
    				  Menu_Win;
extern BOOLEAN vergleich,
               kopiert,
               profil_aufnehmen,
               istverzweigt,
               abbruch208;

int lines=0,
    anzahl,
    pos,
    dlg136_range,
    dlg136_fehler[STRANGANZAHL],
    strang_anzahl,
    laenge=0,
    id_dlg136;
BOOLEAN listbox_dbl_click=FALSE,
        editor_closed,
        schluessel_aendern=FALSE;//Dick 28.03.99
char temp_136[11],
     profilstr[100],
     newstr[11],
     str_zustand[16],
     str_gewaesser[16],
     str_help[100],
     name_uebergabe[15],
     profil_name136[15],
     quelle[100],
     ziel[100];
char *profilPStr,
	   *quell_ptr,
     *ziel_ptr;
WINDOW id_dlg138;

bool dlg_136_destroy=FALSE;

extern MENU_ITEM *main_menu_tree;
extern BOOLEAN dlg138_abbruch;
BOOLEAN dlg136_edit_war[30];
static BOOL bInKillFocus=FALSE;

extern long XVT_CALLCONV1
	WIN_120_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

/*************   GHJ   *************/
extern WINDOW main_win;

// forward declarations
void starte_grafikeditor();

static std::string FILE_SPEC2PATH( FILE_SPEC& spec )
{
		  char buffer[MAX_PATH];
		  xvt_fsys_convert_dir_to_str( &spec.dir, buffer, MAX_PATH );
		  std::string path( buffer );
		  path +="\\";
		  path += spec.name;

		  return path;
}

static WNDPROC defWndProc;
LRESULT CALLBACK Dlg136WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_HELP:
		{
      LPHELPINFO lphi = (LPHELPINFO)lParam;
			if (hi!=NULL_HELP_INFO)
			{
				switch (lphi->iCtrlId)
				{
				case DLG_136_EDIT_25:
				case DLG_136_EDIT_26:
				case DLG_136_EDIT_27:
				case DLG_136_EDIT_28:
				case DLG_136_EDIT_29:
				case DLG_136_EDIT_31:
				case DLG_136_EDIT_32:
				case DLG_136_EDIT_33:
				case DLG_136_EDIT_34:
				case DLG_136_EDIT_35:
				case DLG_136_EDIT_37:
				case DLG_136_EDIT_38:
				case DLG_136_EDIT_39:
				case DLG_136_EDIT_40:
				case DLG_136_EDIT_41:
				case DLG_136_EDIT_43:
				case DLG_136_EDIT_44:
				case DLG_136_EDIT_45:
				case DLG_136_EDIT_46:
				case DLG_136_EDIT_47:
				case DLG_136_EDIT_49:
				case DLG_136_EDIT_50:
				case DLG_136_EDIT_51:
				case DLG_136_EDIT_52:
				case DLG_136_EDIT_53:
          xvt_help_display_topic(hi, HID_KAPITEL_6_2_3_1);
          break;
				case DLG_136_PUSHBUTTON_2:		// Schliesses
        case DLG_136_PUSHBUTTON_7:		// Schlüssel ändern                    
          xvt_help_display_topic(hi, HID_KAPITEL_4_4_1_1);
          break;
        case DLG_136_PUSHBUTTON_1:		// Löschen
          xvt_help_display_topic(hi, HID_KAPITEL_6_2_3_4);
          break;
        case DLG_136_PUSHBUTTON_6:		// neu aufnehmen
          xvt_help_display_topic(hi, HID_KAPITEL_6_2_3_3);
          break;
        case DLG_136_PUSHBUTTON_3:		// Grafik-Edit
          xvt_help_display_topic(hi, HID_KAPITEL_4_4_3);
          break;
        case DLG_136_PUSHBUTTON_4:		// neues Profil
        case DLG_136_PUSHBUTTON_5:		// Alpha-Edit
          xvt_help_display_topic(hi, HID_KAPITEL_4_4_2_1);
          break;
        case DLG_136_LBOX_86: /* "List Box 85" */
          xvt_help_display_topic(hi, HID_KAPITEL_6_2_3_2);
          break;
        default:
          xvt_help_display_topic(hi, HID_KAPITEL_4_4);
          break;
        }
      }
    }
		break;

	default:
		break;
	}
	return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/***********************************/


/*	 DLG_136 ("Erfassung / Editierung der Vernetzungsdatei")*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_136_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_136_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
  short xdControlId = xdEvent->v.ctl.id;

  switch (xdEvent->type)
  {
  case E_CREATE:
    {
      /*************   GHJ   *************/
      SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(main_win, ATTR_NATIVE_WINDOW));
      defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
      SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg136WindowProc);
      xvt_vobj_set_visible(xdWindow,TRUE);
      /***********************************/

      dlg_136_destroy=FALSE;
      set_menu_dlg136(Menu_Win,FALSE);

      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_59,TRUE); //Interpolation

      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_60, TRUE ); //Geländeverkn.
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_64, TRUE ); //Flächenber.

      xvt_menu_update(Menu_Win);

		 //aktuelles Verzeichnis sicherheitshalber zurücksetzten:
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str_help,50);
      xvt_fsys_convert_str_to_dir(str_help,&file_spec.dir);

      dlg_136 = xdWindow;
      lwin      = xvt_win_get_ctl(xdWindow,DLG_136_LBOX_86);

      xvt_vobj_set_enabled((xvt_win_get_ctl(xdWindow,DLG_136_EDIT_87)),FALSE);
      xvt_vobj_set_enabled((xvt_win_get_ctl(xdWindow,DLG_136_EDIT_88)),FALSE);
      
      for (int i=0;i<30;i++)
      {
        // Resource Id:DLG_136_EDIT_24 <=> 20
        dlg136_edit[i] = xvt_win_get_ctl(xdWindow,DLG_136_EDIT_24 + i );
        dlg136_edit_war[i]=FALSE;
      }
      for (i=0;i<STRANGANZAHL;i++)
        dlg136_fehler[i] = -1;

      NewSetFontFunc(lwin);    //Schriftart für listbox86 ändern
        
      /***** neue Liste fuer Strangtabelle anlegen:->strang.cpp   ********/
      destroy_strang_tabelle();

      MakeNewStrang(STRANGANZAHL);
      for (i=0;i<5;i++)          //Dateinamen für Strangtabelle
      {
        name_anfang[i][0]='\0';
        name_ende[i][0]='\0';
      }
      //Dick 17.08.98,Anfang   Menu_Win
      CURSOR cur;
      cur = xvt_win_get_cursor(main_win);		   //main_win
      xvt_win_set_cursor(main_win,CURSOR_WAIT);
      //Dick 17.08.98 Ende

      anzahl = read_profil_dat(strang_anfang);      //str-Datei einlesen,in:readprof.cpp

      xvt_win_set_cursor(main_win,cur);//Dick 17.08.98
      dlg136_range = anzahl-3-(anzahl-anzahl_strang_entries-1);
      scroll_position = 1;
      xvt_sbar_set_range((xvt_win_get_ctl(xdWindow,DLG_136_VSCROLL_85)),HVSCROLL,1,dlg136_range);
      xvt_sbar_set_pos((xvt_win_get_ctl(xdWindow,DLG_136_VSCROLL_85)),HVSCROLL,scroll_position);
      if(anzahl!=-1)
      {
        anhaengen(); //in strang.cpp haengt letzten strang dran
        strcpy(netz_dat[0],str_gewaesser);
        strcpy(netz_dat[2],str_zustand);
        xvt_vobj_set_title((xvt_win_get_ctl(xdWindow,DLG_136_EDIT_87)),str_gewaesser);
        xvt_vobj_set_title((xvt_win_get_ctl(xdWindow,DLG_136_EDIT_88)),str_zustand);

        if (prof_datei != NULL)
          xvt_list_add(lwin, -1, (char*)prof_datei);   //Ausgabe in Listbox
        int numberslist=xvt_slist_count(prof_datei);
        SLIST selection=xvt_slist_create();

        if(numberslist>0)
        {
          xvt_list_set_sel(lwin, 0,  TRUE);
          // Listbox vorselektieren
          selektion(); //in aufnehm.cpp
        }
        dlg136_get_daten(scroll_position);
      }
      else
      {
        id_dlg136 = QUIT;
        dlg_136_destroy=TRUE;
      }
      if (hi!=NULL_HELP_INFO)
        xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_4_4, 0L);
		 
      ChangeFontAndSize((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));
    } // E_CREATE
    break;

  case E_DESTROY:
    /*  	Dialog has been closed; last event sent to dialog.	*/
    {
      if (id_dlg138 !=NULL_WIN)
		  {
        xvt_vobj_destroy(id_dlg138);
        id_dlg138 =NULL_WIN;
      }
      dlg_136 =NULL_WIN;
      if(anzahl!=-1)
      {
        read_anfang_ende();
        min_max_in_cfg_list(); //in read_cfg.cpp
        safe_cfg_dat();

        set_menu_dlg136(Menu_Win,TRUE);
      }
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_60,FALSE);   //Bearbeiten-Geländeverknüpfung
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_64,FALSE);   //Bearbeiten-Flächenberechnung

      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_59, TRUE ); //Interpolation

      xvt_menu_update(Menu_Win);
      //xvt_dm_post_note("Ende");
    } // E_DESTROY
    break;
  case E_FOCUS:
    {
      /*			Dialog has lost or gained focus.		*/
      if (xdEvent->v.active)
      {
        /*				Dialog has gained focus			*/
        anzahl= anzahl_profil_dat_entries;
        if(anzahl_strang_entries>0)
          dlg136_range =anzahl_profil_dat_entries -3-(anzahl-anzahl_strang_entries-1);
        else
          dlg136_range =anzahl_profil_dat_entries -3;
        xvt_sbar_set_range((xvt_win_get_ctl(xdWindow,DLG_136_VSCROLL_85)),HVSCROLL,1,dlg136_range);
      }
      else 
      {
        /*				Dialog has lost focus			*/
        if(!bInKillFocus)
        {
          bInKillFocus=TRUE; 
          ::SetFocus((HWND)xvt_vobj_get_attr(main_win, ATTR_NATIVE_WINDOW));
          bInKillFocus=FALSE;
        }
      }
    } // E_FOCUS
    break;
  case E_CLOSE:
		/*
      Request to close dialog; user operated "close" menu item on
      dialog system menu, or operated "close" control on dialog
      frame. Dialog not closed unless xvt_vobj_destroy is called.
    */
		{
      //dlg_136 =NULL_WIN;
      id_dlg136 = QUIT;
      xvt_vobj_destroy(xdWindow);
    } // E_CLOSE
    break;
  case E_CHAR:
		/*
      Character typed.
		*/
    {
    }
		break;
	case E_CONTROL:
		/*
			User operated control in dialog.
		*/
		{
      switch(xdControlId)
      {
      case DLG_136_EDIT_25:
        {
          /* 		Edit control was operated.		*/
          if (xdEvent->v.ctl.ci.v.edit.focus_change)
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {
              /* focus has entered the control */
            }
            else
            {
              /*		focus has left the control	*/
              if(dlg136_edit_war[1]==FALSE)
                break;
              xvt_vobj_get_title(dlg136_edit[1],profilstr,10);
              dlg136_fehler[0] = is_zahl(profilstr);
              xvt_vobj_set_title(dlg136_edit[1],profilstr);
              if (dlg136_fehler[0] ==0)
              {
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                xvt_dm_post_error("%s",buf);
                //xvt_dm_post_error("Falsche Eingabe !");
                xvt_scr_set_focus_vobj(dlg136_edit[1]);
              }
              else
              {
                if (dlg136_fehler[0] >0)
                {
                  if(anzahl_station(profilstr)>1)
                  {
                    if (!xvt_dlg_create_res(WD_MODAL, DLG_138, EM_ALL, DLG_138_eh, 0L))
                      xvt_dm_post_error("Can't open DLG_138");
                    if(dlg138_abbruch)
                      break;
                  }
                  else
                    pos = compare_station(profilstr);
                  strcpy(name_anfang[0],dateiname);
                  if (strlen(name_anfang[0]) >0)
                  {
                    profilPStr=xvt_slist_get_elt(prof_datei,pos,0);
                    convert_profil_str(profilPStr);
                    xvt_vobj_set_title(dlg136_edit[1],newstr);
                    dlg136_save_daten(scroll_position);
                    SaveStrangFile=TRUE;
                    dlg136_edit_war[1]=FALSE;
                  }
                  else
                  {
                    if(profilstr[0]!='\0')
                    {
                      char buf[200];//Dick 26.11.99
                      xvt_res_get_str(STR_WSPD136_NOTE_1,buf,sizeof(buf));
                      xvt_dm_post_error("%s",buf);
                      //xvt_dm_post_error("y-Wert ist in der Profiltabelle\nnicht vorhanden");
                      xvt_vobj_set_title(dlg136_edit[1],"\0");
                      xvt_scr_set_focus_vobj(dlg136_edit[1]);
                    }
                    if(profilstr[0]=='\0')
                    {
                      dlg136_save_daten(scroll_position);
                      SaveStrangFile=TRUE;
                      dlg136_edit_war[1]=FALSE;
                    }
                  }
                } //if dlg126 fehler >0
              } //else falsche eingabe
            } //focus left
          } //focus change
          else
          { 	/*		Contents of control were changed	*/
            xvt_vobj_get_title(dlg136_edit[1],profilstr,10);
            dlg136_edit_war[1]=TRUE;
          }
        } // DLG_136_EDIT_25
        break;
      case DLG_136_EDIT_26:
        {
          /* 		Edit control was operated.		*/
          if (xdEvent->v.ctl.ci.v.edit.focus_change)
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {
              /* focus has entered the control */
              
            }
            else
            {
              /*		focus has left the control	*/
              if(dlg136_edit_war[2]==FALSE)
                break;
              xvt_vobj_get_title(dlg136_edit[2],profilstr,10);
              dlg136_fehler[1] = is_zahl(profilstr);
              xvt_vobj_set_title(dlg136_edit[2],profilstr);
              if (dlg136_fehler[1] ==0)
              {
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                xvt_dm_post_error("%s",buf);
                //xvt_dm_post_error("Falsche Eingabe !");
                xvt_scr_set_focus_vobj(dlg136_edit[2]);
              }
              else
              {
                if (dlg136_fehler[1] >0)
                {
                  if(anzahl_station(profilstr)>1)
                  {
                    if (!xvt_dlg_create_res(WD_MODAL, DLG_138, EM_ALL, DLG_138_eh, 0L))
                      xvt_dm_post_error("Can't open DLG_138");
                    if(dlg138_abbruch)
                      break;
                  }
                  else
                    pos = compare_station(profilstr);
                  strcpy(name_ende[0],dateiname);
                  if (strlen(name_ende[0]) >0)
                  {
                    profilPStr=xvt_slist_get_elt(prof_datei,pos,0);
                    convert_profil_str(profilPStr);
                    xvt_vobj_set_title(dlg136_edit[2],newstr);
                    dlg136_save_daten(scroll_position);
                    SaveStrangFile=TRUE;
                    dlg136_edit_war[2]=FALSE;
                  }
                  
                  else
                  {
                    if(profilstr[0]!='\0')
                    {
                      char buf[200];//Dick 26.11.99
                      xvt_res_get_str(STR_WSPD136_NOTE_1,buf,sizeof(buf));
                      xvt_dm_post_error("%s",buf);
                      //xvt_dm_post_error("y-Wert ist in der Profiltabelle\nnicht vorhanden");
                      xvt_vobj_set_title(dlg136_edit[2],"\0");
                      xvt_scr_set_focus_vobj(dlg136_edit[2]);
                    }
                    if(profilstr[0]=='\0')
                    {
                      dlg136_save_daten(scroll_position);
                      SaveStrangFile=TRUE;
                      dlg136_edit_war[2]=FALSE;
                    }
                    
                  }
                } //if dlg_fehler>0
              } //else iszahl nicht falsch
            } //else focus left controll
          } //focus change
          else
          { 	/*		Contents of control were changed	*/
            xvt_vobj_get_title(dlg136_edit[2],profilstr,10);
            dlg136_edit_war[2]=TRUE;
          }
        }
        break;
        
      case DLG_136_EDIT_27:
        {
          /* Edit control was operated. */
          
          if (xdEvent->v.ctl.ci.v.edit.focus_change)
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {
              /* focus has entered the control */
            }
            else
            {
              /*	focus has left the control */
              xvt_vobj_get_title(dlg136_edit[3],profilstr,10);
              int testezahl=is_zahl(profilstr);
              xvt_vobj_set_title(dlg136_edit[3],profilstr);
              if(testezahl==0)
              {
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                xvt_dm_post_error("%s",buf);
                //xvt_dm_post_error("Falsche Eingabe");
                xvt_scr_set_focus_vobj(dlg136_edit[3]);
              }
              else
              {
                dlg136_save_daten(scroll_position);
                SaveStrangFile=TRUE;
              }
            }  // else focus left
          } //focus change
          else
          {
            /* Contents of control were changed */
            xvt_vobj_get_title(dlg136_edit[3],profilstr,10);
          }
        }
        break;
      case DLG_136_EDIT_28:
        {
          /*	Edit control was operated. */
          if (xdEvent->v.ctl.ci.v.edit.focus_change)
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {
              /* focus has entered the control */
            }
            else
            {
              /*  focus has left the control */
              xvt_vobj_get_title(dlg136_edit[4],profilstr,10);
              int testezahl=is_zahl(profilstr);
              xvt_vobj_set_title(dlg136_edit[4],profilstr);
              
              if(testezahl==0)
              {
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                xvt_dm_post_error("%s",buf);
                //xvt_dm_post_error("Falsche Eingabe");
                xvt_scr_set_focus_vobj(dlg136_edit[4]);
              }
              else
              {
                dlg136_save_daten(scroll_position);
                SaveStrangFile=TRUE;
              }
            }
          }
          else
          {
          /*
          Contents of control were changed
            */
            xvt_vobj_get_title(dlg136_edit[4],profilstr,10);
          }
        }
        break;
      case DLG_136_EDIT_29:
        {
          /* Edit control was operated.	*/
          if (xdEvent->v.ctl.ci.v.edit.focus_change)
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {
              /* focus has entered the control */
            }
            else
            {
              /* focus left*/
              xvt_vobj_get_title(dlg136_edit[5],profilstr,10);
              int testezahl=is_zahl(profilstr);
              xvt_vobj_set_title(dlg136_edit[5],profilstr);
              if(testezahl==0)
              {
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                xvt_dm_post_error("%s",buf);
                //xvt_dm_post_error("Falsche Eingabe");
                xvt_scr_set_focus_vobj(dlg136_edit[5]);
              }
              else
              {
                dlg136_save_daten(scroll_position);
                SaveStrangFile=TRUE;
              }
            }
          }
          else
          {
            /* Contents of control were changed */
            xvt_vobj_get_title(dlg136_edit[5],profilstr,10);
          }
        }
        break;
      case DLG_136_EDIT_31:
        {
          /* 		Edit control was operated.		*/
          if (xdEvent->v.ctl.ci.v.edit.focus_change)
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {
              /* focus has entered the control */
            }
            else
            { 	/*		focus has left the control	*/
              if(dlg136_edit_war[7]==FALSE)
                break;
              xvt_vobj_get_title(dlg136_edit[7],profilstr,10);
              dlg136_fehler[2] = is_zahl(profilstr);
              xvt_vobj_set_title(dlg136_edit[7],profilstr);
              if (dlg136_fehler[2] ==0)
              {
                //xvt_dm_post_error("Falsche Eingabe !");
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                xvt_dm_post_error("%s",buf);
                xvt_scr_set_focus_vobj(dlg136_edit[7]);
              }
              else
              {
                if(anzahl_station(profilstr)>1)
                {
                  if (!xvt_dlg_create_res(WD_MODAL, DLG_138, EM_ALL, DLG_138_eh, 0L))
                    xvt_dm_post_error("Can't open DLG_138");
                  if(dlg138_abbruch)
                    break;
                }
                else
                  pos = compare_station(profilstr);
                strcpy( name_anfang[1],dateiname);
                if (strlen(name_anfang[1]) >0)
                {
                  profilPStr=xvt_slist_get_elt(prof_datei,pos,0);
                  convert_profil_str(profilPStr);
                  xvt_vobj_set_title(dlg136_edit[7],newstr);
                  dlg136_save_daten(scroll_position);
                  SaveStrangFile=TRUE;
                  dlg136_edit_war[7]=FALSE;
                }
                else
                {
                  if(profilstr[0]!='\0')
                  {
                    char buf[200];//Dick 26.11.99
                    xvt_res_get_str(STR_WSPD136_NOTE_1,buf,sizeof(buf));
                    xvt_dm_post_error("%s",buf);
                    //xvt_dm_post_error("y-Wert ist in der Profiltabelle\nnicht vorhanden");
                    xvt_vobj_set_title(dlg136_edit[7],"\0");
                    xvt_scr_set_focus_vobj(dlg136_edit[7]);
                  }
                  if(profilstr[0]=='\0')
                  {
                    dlg136_save_daten(scroll_position);
                    SaveStrangFile=TRUE;
                    dlg136_edit_war[7]=FALSE;
                  }
                }
              } //else
            }  //focus left
          } //focus change
          else
          { 	/*		Contents of control were changed	*/
            xvt_vobj_get_title(dlg136_edit[7],profilstr,10);
            dlg136_edit_war[7]=TRUE;
            
          }
        }
        break;
      case DLG_136_EDIT_32:			{
        /* 		Edit control was operated.		*/
        if (xdEvent->v.ctl.ci.v.edit.focus_change)
        {
          if (xdEvent->v.ctl.ci.v.edit.active)
          {
            /* focus has entered the control */
            /* nachfolgendes jetzt überflüssig
            if (listbox_dbl_click)
            {
            convert_profil_str(profilstr);
            xvt_vobj_set_title(dlg136_edit[8],newstr);
            xvt_vobj_set_title(dlg136_edit[13],newstr);
            listbox_dbl_click =FALSE;
            if (id_dlg138 !=NULL_WIN)
            xvt_vobj_destroy(id_dlg138);
            id_dlg138 =NULL_WIN;
            }
            */
          }
          else
          { 	/*		focus has left the control	*/
            if(dlg136_edit_war[8]==FALSE)
              break;
            xvt_vobj_get_title(dlg136_edit[8],profilstr,10);
            dlg136_fehler[3] = is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[8],profilstr);
            
            if (dlg136_fehler[3] ==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe !");
              xvt_scr_set_focus_vobj(dlg136_edit[8]);
            }
            else
            {
              if(anzahl_station(profilstr)>1)
              {
                if (!xvt_dlg_create_res(WD_MODAL, DLG_138, EM_ALL, DLG_138_eh, 0L))
                  xvt_dm_post_error("Can't open DLG_138");
                if(dlg138_abbruch)
                  break;
              }
              else
                pos = compare_station(profilstr);
              strcpy( name_ende[1],dateiname);
              if (strlen(name_ende[1]) >0)
              {
                profilPStr=xvt_slist_get_elt(prof_datei,pos,0);
                convert_profil_str(profilPStr);
                xvt_vobj_set_title(dlg136_edit[8],newstr);
                dlg136_save_daten(scroll_position);
                SaveStrangFile=TRUE;
                dlg136_edit_war[8]=FALSE;
              }
              else
              {
                if(profilstr[0]!='\0')
                {
                  char buf[200];//Dick 26.11.99
                  xvt_res_get_str(STR_WSPD136_NOTE_1,buf,sizeof(buf));
                  xvt_dm_post_error("%s",buf);
                  //xvt_dm_post_error("y-Wert ist in der Profiltabelle\nnicht vorhanden");
                  xvt_vobj_set_title(dlg136_edit[8],"\0");
                  xvt_scr_set_focus_vobj(dlg136_edit[8]);
                }
                if(profilstr[0]=='\0')
                {
                  dlg136_save_daten(scroll_position);
                  SaveStrangFile=TRUE;
                  dlg136_edit_war[8]=FALSE;
                }
                
              }
            }
          }
        }
        else
        { 	/*		Contents of control were changed	*/
          xvt_vobj_get_title(dlg136_edit[8],profilstr,10);
          dlg136_edit_war[8]=TRUE;
          
        }
                                }
        break;
        
      case DLG_136_EDIT_33:			{
      /*
      Edit control was operated.
        */
        if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
          /*
          focus has entered the control
            */
          } else {
          /*
          focus has left the control
            */
            xvt_vobj_get_title(dlg136_edit[9],profilstr,10);
            int testezahl=is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[9],profilstr);
            
            if(testezahl==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe");
              xvt_scr_set_focus_vobj(dlg136_edit[9]);
            }
            else
            {
              dlg136_save_daten(scroll_position);
              SaveStrangFile=TRUE;
            }
          }
        } else {
        /*
        Contents of control were changed
          */
          xvt_vobj_get_title(dlg136_edit[9],profilstr,10);
          
        }
                                }
        break;
      case DLG_136_EDIT_34:			{
      /*
      Edit control was operated.
        */
        if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
          /*
          focus has entered the control
            */
          } else {
          /*
          focus has left the control
            */
            xvt_vobj_get_title(dlg136_edit[10],profilstr,10);
            int testezahl=is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[10],profilstr);
            
            if(testezahl==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe");
              xvt_scr_set_focus_vobj(dlg136_edit[10]);
            }
            else
            {
              dlg136_save_daten(scroll_position);
              SaveStrangFile=TRUE;
            }
          }
        } else {
        /*
        Contents of control were changed
          */
          xvt_vobj_get_title(dlg136_edit[10],profilstr,10);
          
        }
                                }
        break;
      case DLG_136_EDIT_35:			{
      /*
      Edit control was operated.
        */
        if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
          /*
          focus has entered the control
            */
          } else {
          /*
          focus has left the control
            */
            xvt_vobj_get_title(dlg136_edit[11],profilstr,10);
            int testezahl=is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[11],profilstr);
            
            if(testezahl==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe");
              xvt_scr_set_focus_vobj(dlg136_edit[11]);
            }
            else
            {
              dlg136_save_daten(scroll_position);
              SaveStrangFile=TRUE;
            }
          }
        } else {
        /*
        Contents of control were changed
          */
          xvt_vobj_get_title(dlg136_edit[11],profilstr,10);
          
        }
                                }
        break;
      case DLG_136_EDIT_37:			{
        /* 		Edit control was operated.		*/
        if (xdEvent->v.ctl.ci.v.edit.focus_change)
        {
          if (xdEvent->v.ctl.ci.v.edit.active)
          {
            /* focus has entered the control */
          }
          else
          { 	/*		focus has left the control	*/
            if(dlg136_edit_war[13]==FALSE)
              break;
            xvt_vobj_get_title(dlg136_edit[13],profilstr,10);
            dlg136_fehler[4] = is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[13],profilstr);
            
            if (dlg136_fehler[4] ==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe !");
              xvt_scr_set_focus_vobj(dlg136_edit[13]);
            }
            else
            {
              if(anzahl_station(profilstr)>1)
              {
                if (!xvt_dlg_create_res(WD_MODAL, DLG_138, EM_ALL, DLG_138_eh, 0L))
                  xvt_dm_post_error("Can't open DLG_138");
                if(dlg138_abbruch)
                  break;
              }
              else
                pos = compare_station(profilstr);
              strcpy( name_anfang[2],dateiname);
              if (strlen(name_anfang[2]) >0)
              {
                profilPStr=xvt_slist_get_elt(prof_datei,pos,0);
                convert_profil_str(profilPStr);
                xvt_vobj_set_title(dlg136_edit[13],newstr);
                dlg136_save_daten(scroll_position);
                SaveStrangFile=TRUE;
                dlg136_edit_war[13]=FALSE;
              }
              else
              {
                if(profilstr[0]!='\0')
                {
                  char buf[200];//Dick 26.11.99
                  xvt_res_get_str(STR_WSPD136_NOTE_1,buf,sizeof(buf));
                  xvt_dm_post_error("%s",buf);
                  //xvt_dm_post_error("y-Wert ist in der Profiltabelle\nnicht vorhanden");
                  xvt_vobj_set_title(dlg136_edit[13],"\0");
                  xvt_scr_set_focus_vobj(dlg136_edit[13]);
                }
                if(profilstr[0]=='\0')
                {
                  dlg136_save_daten(scroll_position);
                  SaveStrangFile=TRUE;
                  dlg136_edit_war[13]=FALSE;
                }
                
              }
            }
          }
        }
        else
        { 	/*		Contents of control were changed	*/
          xvt_vobj_get_title(dlg136_edit[13],profilstr,10);
          dlg136_edit_war[13]=TRUE;
        }
                                }
        break;
      case DLG_136_EDIT_38:			{
        /* 		Edit control was operated.		*/
        if (xdEvent->v.ctl.ci.v.edit.focus_change)
        {
          if (xdEvent->v.ctl.ci.v.edit.active)
          {
            /* focus has entered the control */
          }
          else
          { 	/*		focus has left the control	*/
            if(dlg136_edit_war[14]==FALSE)
              break;
            xvt_vobj_get_title(dlg136_edit[14],profilstr,10);
            dlg136_fehler[5] = is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[14],profilstr);
            
            if (dlg136_fehler[5] ==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe !");
              xvt_scr_set_focus_vobj(dlg136_edit[14]);
            }
            else                       
            {
              if(anzahl_station(profilstr)>1)
              {
                if (!xvt_dlg_create_res(WD_MODAL, DLG_138, EM_ALL, DLG_138_eh, 0L))
                  xvt_dm_post_error("Can't open DLG_138");
                if(dlg138_abbruch)
                  break;
              }
              else
                pos = compare_station(profilstr);
              strcpy(name_ende[2],dateiname);
              if (strlen(name_ende[2]) >0)
              {
                profilPStr=xvt_slist_get_elt(prof_datei,pos,0);
                convert_profil_str(profilPStr);
                xvt_vobj_set_title(dlg136_edit[14],newstr);
                SaveStrangFile=TRUE;
                dlg136_edit_war[14]=FALSE;
              }
              else
              {
                if(profilstr[0]!='\0')
                {
                  char buf[200];//Dick 26.11.99
                  xvt_res_get_str(STR_WSPD136_NOTE_1,buf,sizeof(buf));
                  xvt_dm_post_error("%s",buf);
                  //xvt_dm_post_error("y-Wert ist in der Profiltabelle\nnicht vorhanden");
                  xvt_vobj_set_title(dlg136_edit[14],"\0");
                  xvt_scr_set_focus_vobj(dlg136_edit[14]);
                }
                if(profilstr[0]=='\0')
                {
                  dlg136_save_daten(scroll_position);
                  SaveStrangFile=TRUE;
                  dlg136_edit_war[14]=FALSE;
                }
                
              }
            }
          }
        }
        else
        { 	/*		Contents of control were changed	*/
          xvt_vobj_get_title(dlg136_edit[14],profilstr,10);
          dlg136_edit_war[14]=TRUE;
          
        }
                                }
        break;
      case DLG_136_EDIT_39:			{
      /*
      Edit control was operated.
        */
        if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
          /*
          focus has entered the control
            */
          } else {
          /*
          focus has left the control
            */
            xvt_vobj_get_title(dlg136_edit[15],profilstr,10);
            int testezahl=is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[15],profilstr);
            
            if(testezahl==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe");
              xvt_scr_set_focus_vobj(dlg136_edit[15]);
            }
            else
            {
              dlg136_save_daten(scroll_position);
              SaveStrangFile=TRUE;
            }
          }
        } else {
        /*
        Contents of control were changed
          */
          xvt_vobj_get_title(dlg136_edit[15],profilstr,10);
          
        }
                                }
        break;
      case DLG_136_EDIT_40:			{
      /*
      Edit control was operated.
        */
        if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
          /*
          focus has entered the control
            */
          } else {
          /*
          focus has left the control
            */
            xvt_vobj_get_title(dlg136_edit[16],profilstr,10);
            int testezahl=is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[16],profilstr);
            
            if(testezahl==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe");
              xvt_scr_set_focus_vobj(dlg136_edit[16]);
            }
            else
            {
              dlg136_save_daten(scroll_position);
              SaveStrangFile=TRUE;
            }
          }
        } else {
        /*
        Contents of control were changed
          */
          xvt_vobj_get_title(dlg136_edit[16],profilstr,10);
          
        }
                                }
        break;
      case DLG_136_EDIT_41:			{
      /*
      Edit control was operated.
        */
        if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
          /*
          focus has entered the control
            */
          } else {
          /*
          focus has left the control
            */
            xvt_vobj_get_title(dlg136_edit[17],profilstr,10);
            int testezahl=is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[17],profilstr);
            if(testezahl==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe");
              xvt_scr_set_focus_vobj(dlg136_edit[17]);
            }
            else
            {
              dlg136_save_daten(scroll_position);
              SaveStrangFile=TRUE;
            }
          }
        } else {
        /*
        Contents of control were changed
          */
          xvt_vobj_get_title(dlg136_edit[17],profilstr,10);
          
        }
                                }
        break;
      case DLG_136_EDIT_43:			{
        /* 		Edit control was operated.		*/
        if (xdEvent->v.ctl.ci.v.edit.focus_change)
        {
          if (xdEvent->v.ctl.ci.v.edit.active)
          {
            /* focus has entered the control */
          }
          else
          { 	/*		focus has left the control	*/
            if(dlg136_edit_war[19]==FALSE)
              break;
            xvt_vobj_get_title(dlg136_edit[19],profilstr,10);
            dlg136_fehler[6] = is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[19],profilstr);
            if (dlg136_fehler[6] ==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe !");
              xvt_scr_set_focus_vobj(dlg136_edit[19]);
            }
            else
            {
              if(anzahl_station(profilstr)>1)
              {
                if (!xvt_dlg_create_res(WD_MODAL, DLG_138, EM_ALL, DLG_138_eh, 0L))
                  xvt_dm_post_error("Can't open DLG_138");
                if(dlg138_abbruch)
                  break;
              }
              else
                pos = compare_station(profilstr);
              strcpy(name_anfang[3],dateiname);
              if (strlen(name_anfang[3]) >0)
              {
                profilPStr=xvt_slist_get_elt(prof_datei,pos,0);
                convert_profil_str(profilPStr);
                xvt_vobj_set_title(dlg136_edit[19],newstr);
                dlg136_save_daten(scroll_position);
                SaveStrangFile=TRUE;
                dlg136_edit_war[19]=FALSE;
              }
              else
                //if (strlen(profilstr)>0)
              {
                if(profilstr[0]!='\0')
                {
                  char buf[200];//Dick 26.11.99
                  xvt_res_get_str(STR_WSPD136_NOTE_1,buf,sizeof(buf));
                  xvt_dm_post_error("%s",buf);
                  //xvt_dm_post_error("y-Wert ist in der Profiltabelle\nnicht vorhanden");
                  xvt_vobj_set_title(dlg136_edit[19],"\0");
                  xvt_scr_set_focus_vobj(dlg136_edit[19]);
                }
                if(profilstr[0]=='\0')
                {
                  dlg136_save_daten(scroll_position);
                  SaveStrangFile=TRUE;
                  dlg136_edit_war[19]=FALSE;
                }
                
              }
            }
          }
        }
        else
        { 	/*		Contents of control were changed	*/
          xvt_vobj_get_title(dlg136_edit[19],profilstr,10);
          dlg136_edit_war[19]=TRUE;
          
        }
                                }
        break;
      case DLG_136_EDIT_44:
        {
          /* 		Edit control was operated.		*/
          if (xdEvent->v.ctl.ci.v.edit.focus_change)
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {
              /* focus has entered the control */
            }
            else
            { 	/*		focus has left the control	*/
              if(dlg136_edit_war[20]==FALSE)
                break;
              xvt_vobj_get_title(dlg136_edit[20],profilstr,10);
              dlg136_fehler[7] = is_zahl(profilstr);
              xvt_vobj_set_title(dlg136_edit[20],profilstr);
              if (dlg136_fehler[7] ==0)
              {
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                xvt_dm_post_error("%s",buf);
                //xvt_dm_post_error("Falsche Eingabe !");
                xvt_scr_set_focus_vobj(dlg136_edit[20]);
              }
              else
              {
                if(anzahl_station(profilstr)>1)
                {
                  if (!xvt_dlg_create_res(WD_MODAL, DLG_138, EM_ALL, DLG_138_eh, 0L))
                    xvt_dm_post_error("Can't open DLG_138");
                  if(dlg138_abbruch)
                    break;
                }
                else
                  pos = compare_station(profilstr);
                strcpy(name_ende[3],dateiname);
                if (strlen(name_ende[3]) >0)
                {
                  profilPStr=xvt_slist_get_elt(prof_datei,pos,0);
                  convert_profil_str(profilPStr);
                  xvt_vobj_set_title(dlg136_edit[20],newstr);
                  dlg136_save_daten(scroll_position);
                  SaveStrangFile=TRUE;
                  dlg136_edit_war[20]=FALSE;
                }
                else
                {
                  if(profilstr[0]!='\0')
                  {
                    char buf[200];//Dick 26.11.99
                    xvt_res_get_str(STR_WSPD136_NOTE_1,buf,sizeof(buf));
                    xvt_dm_post_error("%s",buf);
                    //xvt_dm_post_error("y-Wert ist in der Profiltabelle\nnicht vorhanden");
                    xvt_vobj_set_title(dlg136_edit[20],"\0");
                    xvt_scr_set_focus_vobj(dlg136_edit[20]);
                  }
                  if(profilstr[0]=='\0')
                  {
                    dlg136_save_daten(scroll_position);
                    SaveStrangFile=TRUE;
                    dlg136_edit_war[20]=FALSE;
                  }
                  
                }
              }
            }
          }
          else
          { 	/*		Contents of control were changed	*/
            xvt_vobj_get_title(dlg136_edit[20],profilstr,10);
            dlg136_edit_war[20]=TRUE;
            
          }
        }
        break;
      case DLG_136_EDIT_45:			{
      /*
      Edit control was operated.
        */
        if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
          /*
          focus has entered the control
            */
          } else {
          /*
          focus has left the control
            */
            xvt_vobj_get_title(dlg136_edit[21],profilstr,10);
            int testezahl=is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[21],profilstr);
            if(testezahl==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe");
              xvt_scr_set_focus_vobj(dlg136_edit[21]);
            }
            else
            {
              dlg136_save_daten(scroll_position);
              SaveStrangFile=TRUE;
            }
          }
        } else {
        /*
        Contents of control were changed
          */
          xvt_vobj_get_title(dlg136_edit[21],profilstr,10);
          
        }
                                }
        break;
      case DLG_136_EDIT_46:			{
      /*
      Edit control was operated.
        */
              if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
          /*
          focus has entered the control
            */
          } else {
          /*
          focus has left the control
            */
            xvt_vobj_get_title(dlg136_edit[22],profilstr,10);
            int testezahl=is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[22],profilstr);
            if(testezahl==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe");
              xvt_scr_set_focus_vobj(dlg136_edit[22]);
            }
            else
            {
              dlg136_save_daten(scroll_position);
              SaveStrangFile=TRUE;
            }
          }
        } else {
        /*
        Contents of control were changed
          */
          xvt_vobj_get_title(dlg136_edit[22],profilstr,10);
          
        }
                                }
        break;
      case DLG_136_EDIT_47:			{
      /*
      Edit control was operated.
        */
        if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
          /*
          focus has entered the control
            */
          } else {
          /*
          focus has left the control
            */
            xvt_vobj_get_title(dlg136_edit[23],profilstr,10);
            int testezahl=is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[23],profilstr);
            if(testezahl==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe");
              xvt_scr_set_focus_vobj(dlg136_edit[23]);
            }
            else
            {
              dlg136_save_daten(scroll_position);
              SaveStrangFile=TRUE;
            }
          }
        } else {
        /*
        Contents of control were changed
          */
          xvt_vobj_get_title(dlg136_edit[23],profilstr,10);
          
        }
                                }
        break;
      case DLG_136_EDIT_49:			{
        /* 		Edit control was operated.		*/
        if (xdEvent->v.ctl.ci.v.edit.focus_change)
        {
          if (xdEvent->v.ctl.ci.v.edit.active)
          {
            /* focus has entered the control */
          }
          else
          { 	/*		focus has left the control	*/
            if(dlg136_edit_war[25]==FALSE)
              break;
            xvt_vobj_get_title(dlg136_edit[25],profilstr,10);
            dlg136_fehler[8] = is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[25],profilstr);
            if (dlg136_fehler[8] ==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe !");
              xvt_scr_set_focus_vobj(dlg136_edit[25]);
            }
            else
            {
              if(anzahl_station(profilstr)>1)
              {
                if (!xvt_dlg_create_res(WD_MODAL, DLG_138, EM_ALL, DLG_138_eh, 0L))
                  xvt_dm_post_error("Can't open DLG_138");
                if(dlg138_abbruch)
                  break;
              }
              else
                pos = compare_station(profilstr);
              strcpy(name_anfang[4],dateiname);
              if (strlen(name_anfang[4]) >0)
              {
                profilPStr=xvt_slist_get_elt(prof_datei,pos,0);
                convert_profil_str(profilPStr);
                xvt_vobj_set_title(dlg136_edit[25],newstr);
                dlg136_save_daten(scroll_position);
                SaveStrangFile=TRUE;
                dlg136_edit_war[25]=FALSE;
              }
              else
                //if (strlen(profilstr)>0)
              {
                if(profilstr[0]!='\0')
                {
                  char buf[200];//Dick 26.11.99
                  xvt_res_get_str(STR_WSPD136_NOTE_1,buf,sizeof(buf));
                  xvt_dm_post_error("%s",buf);
                  //xvt_dm_post_error("y-Wert ist in der Profiltabelle\nnicht vorhanden");
                  xvt_vobj_set_title(dlg136_edit[25],"\0");
                  xvt_scr_set_focus_vobj(dlg136_edit[25]);
                }
                if(profilstr[0]=='\0')
                {
                  dlg136_save_daten(scroll_position);
                  SaveStrangFile=TRUE;
                  dlg136_edit_war[25]=FALSE;
                }
                
              }
            }
          }
        }
        else
        { 	/*		Contents of control were changed	*/
          xvt_vobj_get_title(dlg136_edit[25],profilstr,10);
          dlg136_edit_war[25]=TRUE;
          
        }
                                }
        break;
      case DLG_136_EDIT_50:			{
        /* 		Edit control was operated.		*/
        if (xdEvent->v.ctl.ci.v.edit.focus_change)
        {
          if (xdEvent->v.ctl.ci.v.edit.active)
          {
            /* focus has entered the control */
          }
          else
          { 	/*		focus has left the control	*/
            if(dlg136_edit_war[26]==FALSE)
              break;
            xvt_vobj_get_title(dlg136_edit[26],profilstr,10);
            dlg136_fehler[9] = is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[26],profilstr);
            if (dlg136_fehler[9] ==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe !");
              xvt_scr_set_focus_vobj(dlg136_edit[26]);
            }
            else
            {
              if(anzahl_station(profilstr)>1)
              {
                if (!xvt_dlg_create_res(WD_MODAL, DLG_138, EM_ALL, DLG_138_eh, 0L))
                  xvt_dm_post_error("Can't open DLG_138");
                if(dlg138_abbruch)
                  break;
              }
              else
                pos = compare_station(profilstr);
              strcpy(name_ende[4],dateiname);
              if (strlen(name_ende[4]) >0)
              {
                profilPStr=xvt_slist_get_elt(prof_datei,pos,0);
                convert_profil_str(profilPStr);
                xvt_vobj_set_title(dlg136_edit[26],newstr);
                dlg136_save_daten(scroll_position);
                SaveStrangFile=TRUE;
                dlg136_edit_war[26]=FALSE;
              }
              else
                //if (strlen(profilstr)>0)
              {
                if(profilstr[0]!='\0')
                {
                  char buf[200];//Dick 26.11.99
                  xvt_res_get_str(STR_WSPD136_NOTE_1,buf,sizeof(buf));
                  xvt_dm_post_error("%s",buf);
                  //xvt_dm_post_error("y-Wert ist in der Profiltabelle\nnicht vorhanden");
                  xvt_vobj_set_title(dlg136_edit[26],"\0");
                  xvt_scr_set_focus_vobj(dlg136_edit[26]);
                }
                if(profilstr[0]=='\0')
                {
                  dlg136_save_daten(scroll_position);
                  SaveStrangFile=TRUE;
                  dlg136_edit_war[26]=FALSE;
                }
                
              }
            }
          }
        }
        else
        { 	/*		Contents of control were changed	*/
          xvt_vobj_get_title(dlg136_edit[26],profilstr,10);
          dlg136_edit_war[26]=TRUE;
          
        }
                                }
        break;
      case DLG_136_EDIT_51:			{
      /*
      Edit control was operated.
        */
        if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
          /*
          focus has entered the control
            */
          } else {
          /*
          focus has left the control
            */
            xvt_vobj_get_title(dlg136_edit[27],profilstr,10);
            int testezahl=is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[27],profilstr);
            if(testezahl==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe");
              xvt_scr_set_focus_vobj(dlg136_edit[27]);
            }
            else
            {
              dlg136_save_daten(scroll_position);
              SaveStrangFile=TRUE;
            }
          }
        } else {
        /*
        Contents of control were changed
          */
          xvt_vobj_get_title(dlg136_edit[27],profilstr,10);
          
        }
                                }
        break;
      case DLG_136_EDIT_52:			{
      /*
      Edit control was operated.
        */
        if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
          /*
          focus has entered the control
            */
          } else {
          /*
          focus has left the control
            */
            xvt_vobj_get_title(dlg136_edit[28],profilstr,10);
            int testezahl=is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[28],profilstr);
            if(testezahl==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe");
              xvt_scr_set_focus_vobj(dlg136_edit[28]);
            }
            else
            {
              dlg136_save_daten(scroll_position);
              SaveStrangFile=TRUE;
            }
          }
        } else {
        /*
        Contents of control were changed
          */
          xvt_vobj_get_title(dlg136_edit[28],profilstr,10);
          
        }
                                }
        break;
      case DLG_136_EDIT_53:			{
      /*
      Edit control was operated.
        */
        if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
          /*
          focus has entered the control
            */
          } else {
          /*
          focus has left the control
            */
            xvt_vobj_get_title(dlg136_edit[29],profilstr,10);
            int testezahl=is_zahl(profilstr);
            xvt_vobj_set_title(dlg136_edit[29],profilstr);
            if(testezahl==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Falsche Eingabe");
              xvt_scr_set_focus_vobj(dlg136_edit[29]);
            }
            else
            {
              dlg136_save_daten(scroll_position);
              SaveStrangFile=TRUE;
            }
          }
        } else {
        /*
        Contents of control were changed
          */
          xvt_vobj_get_title(dlg136_edit[29],profilstr,10);
          
        }
                                }
        break;

      case DLG_136_VSCROLL_85: /* "Vertical Scrollbar 85" */
        {
        /*
          Vertical scrollbar control was operated
        */
          switch (xdEvent->v.ctl.ci.v.scroll.what) {
          case SC_LINE_UP:
            if (scroll_position >1)
            {
              scroll_position--;
              dlg136_get_daten(scroll_position);
              xvt_sbar_set_pos((xvt_win_get_ctl(xdWindow,DLG_136_VSCROLL_85)),HVSCROLL,scroll_position);
            }
            break;
          case SC_LINE_DOWN:
            if(scroll_position <anzahl-4-(anzahl-anzahl_strang_entries-1))//Dick 17.08.98
            {                                              
              scroll_position++;
              dlg136_get_daten(scroll_position);
              xvt_sbar_set_pos((xvt_win_get_ctl(xdWindow,DLG_136_VSCROLL_85)),HVSCROLL,scroll_position);
            }
            break;
          case SC_PAGE_UP:
            if (scroll_position >5)
            {
              scroll_position=scroll_position-5;
              dlg136_get_daten(scroll_position);
              xvt_sbar_set_pos((xvt_win_get_ctl(xdWindow,DLG_136_VSCROLL_85)),HVSCROLL,scroll_position);
            }
            else
            {
              scroll_position=1;
              xvt_sbar_set_pos((xvt_win_get_ctl(xdWindow,DLG_136_VSCROLL_85)),HVSCROLL,scroll_position);
            }
            break;
          case SC_PAGE_DOWN:
            if(scroll_position+5 <anzahl-4)//Dick 17.08.98
            {
              scroll_position=scroll_position+5;
              dlg136_get_daten(scroll_position);
              xvt_sbar_set_pos((xvt_win_get_ctl(xdWindow,DLG_136_VSCROLL_85)),HVSCROLL,scroll_position);
            }
            else
            {
              scroll_position=anzahl-4;
              xvt_sbar_set_pos((xvt_win_get_ctl(xdWindow,DLG_136_VSCROLL_85)),HVSCROLL,scroll_position);
            }
            break;
          case SC_THUMB:
            scroll_position=xdEvent->v.ctl.ci.v.scroll.pos;
            dlg136_get_daten(scroll_position);
            xvt_sbar_set_pos((xvt_win_get_ctl(xdWindow,DLG_136_VSCROLL_85)),HVSCROLL,scroll_position);
            break;
          case SC_THUMBTRACK:
            scroll_position=xdEvent->v.ctl.ci.v.scroll.pos;
            dlg136_get_daten(scroll_position);
            xvt_sbar_set_pos((xvt_win_get_ctl(xdWindow,DLG_136_VSCROLL_85)),HVSCROLL,scroll_position);
            break;
          default:
            break;
          }
        } // DLG_136_VSCROLL_85
        break;
      case DLG_136_LBOX_86: /* "List Box 85" */
        {
          /*	List box was operated.*/
          {
            SLIST selection;
            char *str_ptr;
            selection=xvt_slist_create();
            selection = xvt_list_get_sel(lwin);
            
            str_ptr=xvt_slist_get_elt(selection,0,0);
            strcpy(profilstr,str_ptr);
            /* Dateiname nach file_spec.name kopieren  */
            /* Name ab Position 44 in str[]            */
            for (int i=0;i<12;i++)
              file_spec.name[i] = profilstr[i+44];
            file_spec.name[12]='\0';
            
            for (i=0;i<11;i++)
              zustand[i] = profilstr[i+33];
            zustand[i]='\0';
            for (i=0;i<4;i++)
              vzk[i]=profilstr[i+29];
            vzk[i]='\0';
            for (i=0;i<10;i++)
              pk[i]=profilstr[19+i];
            pk[i]='\0';
            /*-----------------------------------------*/
            xvt_slist_destroy(selection);
          }
          if (xdEvent->v.ctl.ci.v.lbox.dbl_click)
          {
          }
          else  //(1)
          {
            /*		single click			*/
            SLIST selection;
            char *str_ptr;
            selection=xvt_slist_create();
            selection = xvt_list_get_sel(lwin);
            str_ptr=xvt_slist_get_elt(selection,0,0);
            
            /* Dateiname nach file_spec.name kopieren  */
            for (int i=0;i<12;i++)
              file_spec.name[i] = str_ptr[i+44];
            file_spec.name[12]='\0';
            /*-----------------------------------------*/
            xvt_slist_destroy(selection);
            
            if ( id_dlg136!=DELETE)
              choice_neu =TRUE;   // neue Wahl
          }  // end else(1)
        } // DLG_136_LBOX_86 
        break;

        //****************************************************************************
      case DLG_136_PUSHBUTTON_1: /* "Löschen" */
        {
          xvt_fsys_set_dir(&STR_SPEC.dir);
          
          if (xvt_list_count_all(lwin)>0)
          {
            id_dlg136=DELETE;
            
            /*****************sicherheitshalber:**********************/
            SLIST selection;
            char *str_ptr;
            selection=xvt_slist_create();
            selection = xvt_list_get_sel(lwin);
            
            str_ptr=xvt_slist_get_elt(selection,0,0);
            strcpy(profilstr,str_ptr);
            /* Dateiname nach file_spec.name kopieren  */
            /* Name ab Position 44 in str[]            */
            for (int i=0;i<12;i++)
              file_spec.name[i] = profilstr[i+44];
            file_spec.name[12]='\0';
            
            for (i=0;i<11;i++)
              zustand[i] = profilstr[i+33];
            zustand[i]='\0';
            for (i=0;i<4;i++)
              vzk[i]=profilstr[i+29];
            vzk[i]='\0';
            for (i=0;i<10;i++)
              pk[i]=profilstr[19+i];
            pk[i]='\0';
            /*-----------------------------------------*/
            xvt_slist_destroy(selection);
            
            double stationdelete;
            
            /********************************************************/
            
            if ( id_dlg136==DELETE)
            {
              int sel=0;
              char buf[200],buf2[200],buf3[200],buf4[200];//Dick 26.11.99
              xvt_res_get_str(STR_NEIN,buf,sizeof(buf));
              xvt_res_get_str(STR_JA,buf2,sizeof(buf2));
              xvt_res_get_str(STR_PROFDAT_DEL_ASK,buf3,sizeof(buf3));
              xvt_res_get_str(STR_LOESCHEN2,buf4,sizeof(buf3));
              switch (xvt_dm_post_ask(buf,buf2,NULL,"%s\n%s\n%s",buf3,file_spec.name,buf4))
              {
              case RESP_2:       //Löschen !!!
                {
                  char temporare[12];
                  id_dlg136=NULL;
                  sel=xvt_list_get_sel_index(lwin);
                  xvt_list_rem(lwin,sel);
                  
                  SLIST_ELT elt = xvt_slist_get_first(prof_datei);
                  for  (int i=0;i<sel;i++)
                    elt = xvt_slist_get_next(prof_datei,elt);
                  if (xvt_slist_rem(prof_datei,elt))
                  {
                    str_ptr = xvt_slist_get(prof_datei,elt,0L);
                    
                    for (int i=0;i<=7;i++)         //Stationswert km ermitteln
                      temporare[i]=str_ptr[10+i];
                    temporare[8]='\0';
                    stationdelete = atof(temporare);
                    int j=0;
                    for(i=44;i<=55;i++)
                    {
                      if(str_ptr[i]!=' ')
                      {
                        profil_name136[j]=str_ptr[i];
                        j++;
                      }
                    }
                    profil_name136[j]='\0';
                    teste_physisch_loeschen(profil_name136); //in profproj.cpp
                    
                    delete_strang_entry_neu();
                    dlg136_get_daten(scroll_position);
                    anzahl_profil_dat_entries--;
                    
                    xvt_res_get_str(STR_WSPD136_NOTE_2,buf,sizeof(buf));
                    xvt_dm_post_error("%s",buf);
                    
                    int numberslist=xvt_slist_count(prof_datei);
                    if(numberslist>0)
                    {
                      xvt_list_set_sel(lwin, 0,  TRUE);
                      // Listbox vorselektieren
                      selektion(); //in aufnehm.cpp
                    }
                    
                    SaveProfilFile = TRUE;
                    save_str_datei();
                  }
                  else
                  {
                    xvt_res_get_str(STR_WSPD136_NOTE_3,buf,sizeof(buf));
                    xvt_dm_post_error("%s\n%s",buf,file_spec.name);
                  }
                }
                break;
              }  //switch
            }  // end delete
          }
          else 
          {
            char buf[200];//Dick 12.01.00
            xvt_res_get_str(STR_WSPD136_NOTE_4,buf,sizeof(buf));
            xvt_dm_post_error("%s",buf);
          }
        
          dlg136_save_daten(scroll_position);
          save_str_datei();
        }
        break;
        //****************************************************************************
    case DLG_136_PUSHBUTTON_2: /* "schliessen" */
      {
        xvt_fsys_set_dir(&STR_SPEC.dir);             
        dlg136_save_daten(scroll_position);
        
        save_str_datei();
        
        is_Entry_in_SLIST=FALSE;
        
        for (int i=0;i<=99;i++)
          str_netz[i]='\0';
        file_spec.name[0]='\0';
        
        if (prof_datei != NULL)
        {
          xvt_slist_destroy(prof_datei);
          prof_datei = NULL;
        }
        
        xvt_vobj_destroy(xdWindow);
      }
      break;
//****************************************************************************
	case DLG_136_PUSHBUTTON_3:
		starte_grafikeditor();
		break;

    case DLG_136_PUSHBUTTON_4: /* " NEUES PROFIL " */
      {
        xvt_fsys_set_dir(&STR_SPEC.dir);
        dlg136_save_daten(scroll_position);
        save_str_datei();
        profilstr[0]='\0';

        is_profil_open = FALSE;
        new_profil =TRUE;

        for (int i=1;i<50;i++)
          ds_info[i] = 0;
        anzahl_ds = ds_info[0] = 1;

        xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_39,FALSE);   // Profildatei
        xvt_vobj_set_visible(dlg_136,FALSE);

        choice_neu =TRUE;
        profil_aufnehmen=FALSE;
        if (!xvt_dlg_create_res(WD_MODAL,DLG_208, EM_ALL, DLG_208_eh,0L))
          xvt_dm_post_error("Can´t open dialog 208");

        if(!abbruch208)
        {
          main_menu_tree=xvt_menu_get_tree(Menu_Win);//Dick 22.07.99
          if (!xvt_win_create_res(WIN_120, TASK_WIN, EM_ALL, WIN_120_eh, 0L))
            xvt_dm_post_error("Can't open window120");
        }
        else
        {
          xvt_menu_set_item_enabled( Menu_Win, TASK_MENUBAR_13_39, TRUE );   // Profildatei
          xvt_vobj_set_visible( dlg_136, TRUE );
        };
      } // case DLG_136_PUSHBUTTON_4: /* " NEUES PROFIL " */
      break;

    case DLG_136_PUSHBUTTON_5: /* "Alpha-Edit" */
      {
        int lesen_ok;
        int numberslist=xvt_slist_count(prof_datei);
        if(numberslist>0)
        {
          selektion();
          xvt_fsys_set_dir(&STR_SPEC.dir);
          dlg136_save_daten(scroll_position);
          save_str_datei();

          lesen_ok = read_profildatei( pWPL, &STR_SPEC.dir, file_spec.name );
          choice_neu =FALSE;

          if (lesen_ok ==0)
          {
            is_profil_open = TRUE;
            new_profil =FALSE;
            
            xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_39,FALSE);   // Profildatei
            xvt_menu_update(Menu_Win);
            xvt_vobj_set_visible(dlg_136,FALSE);
            main_menu_tree=xvt_menu_get_tree(Menu_Win);//Dick 22.07.99
            if (!xvt_win_create_res(WIN_120, TASK_WIN, EM_ALL, WIN_120_eh, 0L))
              xvt_dm_post_error("Can't open window120");
          }
          else 
          {
            char buf[200];
            xvt_res_get_str(STR_PROF_READ_NOTE,buf,sizeof(buf));
            xvt_dm_post_error("%s",buf);
          }
        }
        else
        {
          char buf[200];//Dick 12.01.00
          xvt_res_get_str(STR_WSPD136_NOTE_4,buf,sizeof(buf));
          xvt_dm_post_error("%s",buf);
        }
      } // DLG_136_PUSHBUTTON_5: /* "Alpha-Edit" */
      break;

    case DLG_136_PUSHBUTTON_6: /* "neu aufnehmen" */
      {
        int selection = xvt_list_get_sel_index( lwin );
        if ( selection == -1 )
          selection = 0;

        if (profile_aufnehmen()==IDOK)    // -->file:aufnehm.cpp
        {
          save_str_datei();
          read_profil_dat( strang_anfang );
          xvt_list_clear( lwin );
          xvt_list_add( lwin, -1, (char*)prof_datei );

          int numberslist=xvt_slist_count(prof_datei);
          if(numberslist>0)
          {
            xvt_list_set_sel( lwin, selection,  TRUE );
            // Listbox vorselektieren
            selektion(); //in aufnehm.cpp
          }

        }
      } // DLG_136_PUSHBUTTON_6: / "neu aufnehmen /
      break;

    case DLG_136_PUSHBUTTON_7: /* Schlüssel ändern*/
      {
        char path[SZ_FNAME + 1]; //Quellpfad
        char quelle[SZ_FNAME + 1]; //Quelldatei mit Verzeichnis
        SLIST schreibe_list,strdat_list;
        char strdat_save[SZ_FNAME + 1];
        
        strcpy(strdat_save,STR_SPEC.name);
        xvt_fsys_set_dir(&STR_SPEC.dir);
        
        //Dick 22.04.99
        dlg136_save_daten(scroll_position);
        
        save_str_datei();
        //
        
        if (xvt_list_count_all(lwin)>0)
        {
          /*****************sicherheitshalber:**********************/
          SLIST selection;
          char *str_ptr;

          selection = xvt_list_get_sel(lwin);
          
          str_ptr=xvt_slist_get_elt(selection,0,0);
          strcpy(profilstr,str_ptr);
          /* Dateiname nach file_spec.name kopieren  */
          /* Name ab Position 44 in str[]            */
          for (int i=0;i<12;i++)
            file_spec.name[i] = profilstr[i+44];
          file_spec.name[12]='\0';
          
          for (i=0;i<11;i++)
            zustand[i] = profilstr[i+33];
          zustand[i]='\0';
          for (i=0;i<4;i++)
            vzk[i]=profilstr[i+29];
          vzk[i]='\0';
          for (i=0;i<10;i++)
            pk[i]=profilstr[19+i];
          pk[i]='\0';
          /*-----------------------------------------*/
          xvt_slist_destroy(selection);
          
          xvt_fsys_get_dir(&file_spec.dir);
          xvt_fsys_convert_dir_to_str(&file_spec.dir, path, 80);
          strcpy(quelle,path);
          strcat(quelle,"\\");
          strcat(quelle,file_spec.name);
          
          profil_aufnehmen=TRUE;
          neukopieren=FALSE;
          schluessel_aendern=TRUE;
          schluessel_einlesen(quelle); //in profproj.cpp
          if (!xvt_dlg_create_res(WD_MODAL,DLG_208, EM_ALL, DLG_208_eh,0L))
            xvt_dm_post_error("Can´t open dialog 208");
          profil_aufnehmen=FALSE;
          if(!abbruch208)
          {
            FILE *profprojfile;
            char profprojdatei[100];
            char *vergleichsstring,
              *profilnamevgl,
              *strnamevgl,
              *help,
              *druckstring,
              *changedatei;
            
            vergleichsstring  = new char[100];
            profilnamevgl     = new char[15];
            strnamevgl        = new char[15];
            help              = new char[200];
            druckstring       = new char[200];
            changedatei       = new char[200];
            
            int test,k,l, test2,test3,i,j, zaehler1,zaehler2;
            BOOLEAN blank=FALSE,mehrdatei=FALSE,weiter=FALSE;
            
            schreibe_list=xvt_slist_create();
            strdat_list=xvt_slist_create();
            
            xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,profprojdatei,50);
            strcat(profprojdatei,"\\profproj.txt");
            profprojfile=fopen(profprojdatei,"r");
            fscanf(profprojfile,"%d",&zaehler1);
            fscanf(profprojfile,"%d",&zaehler2);
            fgets(help,101,profprojfile); //\n überlesen
            for(i=1;i<=zaehler1;i++)
            {
              fgets(help,101,profprojfile);
              for(j=0;j<=(INT)strlen(help);j++)
              {
                if(help[j]=='\n')
                  help[j]='\0';
              }
              xvt_slist_add_at_elt(schreibe_list,NULL,help,0L);
            }
            fgets(help,101,profprojfile);
            xvt_slist_add_at_elt(schreibe_list,NULL,"\0",0L);
            strcpy(vergleichsstring,file_spec.name);
            strcat(vergleichsstring," ");
            STR_SPEC.name[12]='\0';
            strcat(vergleichsstring,STR_SPEC.name);
            for(i=1;i<=zaehler2;i++)
            {
              fgets(help,101,profprojfile);
              for(j=0;j<=(INT)strlen(help);j++)
              {
                if(help[j]=='\n')
                  help[j]='\0';
              }
              
              blank=FALSE;
              k=0;
              l=0;
              for(j=0;j<(INT)strlen(help);j++)
              {
                if(help[j]==' ')
                  blank=TRUE;
                if(!blank)
                {
                  profilnamevgl[l]=help[j];
                  l++;
                }
                if((blank) && (help[j]!=' '))
                {
                  strnamevgl[k]=help[j];
                  k++;
                }
              }
              strnamevgl[k]='\0';
              profilnamevgl[l]='\0';
              test=xvt_str_compare_ignoring_case(vergleichsstring,help);
              
              xvt_slist_add_at_elt(schreibe_list,NULL,help,0L);
              
              test2=xvt_str_compare_ignoring_case(profilnamevgl,file_spec.name);
              if(test2==0)
                xvt_slist_add_at_elt(strdat_list,NULL,strnamevgl,0L);
              STR_SPEC.name[12]='\0';
              test3=xvt_str_compare_ignoring_case(strnamevgl,STR_SPEC.name);
              if((test2==0) && (test3!=0))
                mehrdatei=TRUE;
            } //FOR ANZAHL UNTERER TEIL PROFPROJ
            fclose(profprojfile);
            if(mehrdatei)
            {                       
              char buf[200],buf2[200],buf3[200];//Dick 26.11.99
              xvt_res_get_str(STR_JA,buf,sizeof(buf));
              xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
              xvt_res_get_str(STR_WSPD136_ASK,buf3,sizeof(buf3));
              switch (xvt_dm_post_ask(buf,buf2,NULL,"%s",buf3))
                //switch (xvt_dm_post_ask("Ja","Nein",NULL,"Die Profildatei wird noch in anderen \n Zustandsdateien referenziert.\nWollen Sie wirklich den Profilschlüssel ändern?"))
              {
              case RESP_DEFAULT:       //JA - weiter
                weiter=TRUE;
                break;
                
              case RESP_2:             // NEIN - nicht weiter
                weiter=FALSE;
                break;
                
              }
            }
            else
              weiter=TRUE;
            if(weiter)
            {
              test=1;
              char *hilfs_ptr;
              SLIST_ELT ee;
              int x=0,x_rem=-1,h;
              bool sortieren=FALSE;
              //STR ändern
              for(ee=xvt_slist_get_first(strdat_list);
              ee!=NULL;ee=xvt_slist_get_next(strdat_list,ee))
              {
                hilfs_ptr=xvt_slist_get(schreibe_list,ee,0L);
                strcpy(STR_SPEC.name,hilfs_ptr);
                destroy_strang_tabelle();
                MakeNewStrang(STRANGANZAHL);
                int anzahl=read_profil_dat(strang_anfang);      //str-Datei einlesen,in:readprof.cpp
                if(anzahl!=-1)
                  anhaengen();
                change_strang_entry();
                
                istverzweigt=FALSE;
                teste_str_verzweigt_intern(); //IN VERZWEIG.cpp
                BOOL strang_vorwaerts = GetSortStrangVorwaerts();
                if((istverzweigt==FALSE) &&(vzk[0]=='0') && (pk[0]=='0'))
                  sortieren=TRUE;                                 
                else
                {                                         
                  delete_strang_entry_neu(); 
                  BOOL sortVerzweigt = GetSortVerzweigt();
                  if ( sortVerzweigt )
                  {
                    if(!xvt_dlg_create_res(WD_MODAL,DLG_211, EM_ALL, DLG_211_eh, 0L))                                         
                      xvt_dm_post_error("Can't open dialog 211");                                     
                  }
                  
                  if(!abbruch208)
                  {
                    strcpy(uebergabe_name,file_spec.name);
                    if ( sortVerzweigt )
                      verzweigtes_profil_sortieren( 0, strang_vorwaerts );
                    else
                      verzweigtes_profil_sortieren( 1, strang_vorwaerts );
                    wandle_abstand_in_string();
                  }
                  else
                    break;
                }
                
                char *pstr;
                char dummy1[20],dummy2[20],dummy3[20],dummy4[20],dummy5[20],namevergl[20];
                SLIST_ELT e;
                char profilstr[100];
                int zaehly_str=0;
                for (e=xvt_slist_get_first(prof_datei);e!=NULL;
                e=xvt_slist_get_next(prof_datei,e))
                {
                  pstr = xvt_slist_get(prof_datei,e,0L);                                        
                  sscanf(pstr,"%s%s%s%s%s%s",dummy1,dummy2,dummy3,dummy4,dummy5,namevergl);
                  if(xvt_str_compare_ignoring_case(namevergl,file_spec.name)==0)
                  {
                    for (int h=0;h<100;h++)   //init.
                      profilstr[h]=' ';
                    profilstr[99]='\0';
                    for (h=0;h<(INT)strlen(netz_dat[0]);h++)            //Gewässer
                    {
                      if ((netz_dat[0][h]=='\n')||(netz_dat[0][h]=='\0'))
                        profilstr[h]=' ';
                      else
                        profilstr[h]=netz_dat[0][h];
                    }
                    int a=17;                                  //Station
                    int len=strlen(station208);
                    len=len-1;
                    for(h=len;h>=0;h--,a--)
                      profilstr[a]=station208[h];
                    
                    a=26;
                    for(h=(INT)(strlen(pk)-1);h>=0;h--,a--)             //pk
                      profilstr[a]=pk[h];   //Neu Dick 19->26 und minus 10.07.98
                    
                    a=31;
                    for(h=(INT)(strlen(vzk)-1);h>=0;h--,a--)            //vzk
                      profilstr[a]=vzk[h];  //Neu Dick 29->31 und minus 10.07.98
                    
                    for(h=0;h<(INT)strlen(zustand);h++)
                      profilstr[33+h]=zustand[h];
                    
                    for (h=0;h<=11;h++)          // Filename
                    {
                      profilstr[44+h]=file_spec.name[h];
                      
                    }
                    xvt_slist_change_str(prof_datei,profilstr,zaehly_str);
                    break;
                  }
                  zaehly_str++;
                }
                if(sortieren)//weil prof_datei fürs Sortieren aktualisiert werden muß
                {
                  delete_strang_entry_neu();//Dick 24.11.99
                  strcpy(uebergabe_name,file_spec.name);//Dick 24.11.99
                  sort_new_profil( &strang_anfang, &strang_ende, station208, vzk, pk, uebergabe_name, strang_vorwaerts );
                  wandle_abstand_in_string();//Dick 24.11.99
                  sortieren=FALSE;
                }
                
                save_str_datei();
              } // for ee
              strcpy(STR_SPEC.name,strdat_save);
              destroy_strang_tabelle();
              MakeNewStrang(STRANGANZAHL);
              int anzahl=read_profil_dat(strang_anfang);      //str-Datei einlesen,in:readprof.cpp
              if(anzahl!=-1)
                anhaengen();
              //Ende STR ändern
              //profproj ändern
              if(!abbruch208)
              {
                for(ee=xvt_slist_get_first(schreibe_list);
                ee!=NULL;ee=xvt_slist_get_next(schreibe_list,ee))
                {
                  hilfs_ptr=(CHAR*)xvt_slist_get(schreibe_list,ee,0L);
                  changedatei[0]='\0';
                  
                  strcpy(changedatei,hilfs_ptr);
                  
                  if((strlen(changedatei))>=40)
                  {
                    j=0;
                    for(i=44;i<=55;i++)
                    {
                      if(changedatei[i]!=' ')
                      {
                        profilnamevgl[j]=changedatei[i];
                        j++;
                      }
                    }
                    profilnamevgl[j]='\0';
                    test=xvt_str_compare_ignoring_case(profilnamevgl,file_spec.name);
                    if(test==0)
                    {
                      x_rem=x;
                    }
                  } //if strlen changedatei>40
                  x++;
                } //for slist_elem
                
                if (x_rem>-1)
                {
                  x=0;	 
                  ee=xvt_slist_get_first(schreibe_list);
                  x=xvt_slist_count(schreibe_list);
                  for(x=0;x<x_rem;x++)
                  {
                    ee=xvt_slist_get_next(schreibe_list,ee);
                  }
                  
                  strcpy(name208,netz_dat[0]);
                  for (i=0;i<98;i++)
                    druckstring[i]=' ';
                  druckstring[98]='\0';
                  for(i=0;i<(INT)strlen(name208);i++)
                    druckstring[i]=name208[i];
                  for(i=0;i<(INT)strlen(station208);i++)
                    druckstring[i+10]=station208[i];
                  h=26;
                  for(i=(INT)(strlen(pk)-1);i>=0;h--,i--)             //pk
                    druckstring[h]=pk[i];   //Neu Dick 19->26 und minus 10.07.98
                  
                  h=31;
                  for(i=(INT)(strlen(vzk)-1);i>=0;h--,i--)            //vzk
                    druckstring[h]=vzk[i];  //Neu Dick 29->31 und minus 10.07.98
                  
                  for(i=0;i<(INT)strlen(zustand);i++)
                    druckstring[i+33]=zustand[i];
                  for(i=0;i<(INT)strlen(file_spec.name);i++)
                    druckstring[i+44]=file_spec.name[i];
                  xvt_slist_change_str(schreibe_list,druckstring,x_rem);
                }                         
                if(zaehler1!=0)
                {
                  profprojfile=fopen(profprojdatei,"w");
                  fprintf(profprojfile,"%d ",zaehler1);
                  fprintf(profprojfile,"%d\n",zaehler2);
                  for(ee=xvt_slist_get_first(schreibe_list);
                  ee!=NULL;ee=xvt_slist_get_next(schreibe_list,ee))
                  {
                    hilfs_ptr=xvt_slist_get(schreibe_list,ee,0L);
                    druckstring[0]='\0';
                    strcpy(druckstring,hilfs_ptr);
                    fprintf(profprojfile,"%s\n",druckstring);
                  }
                  fclose(profprojfile);
                }
                else
                {
                  remove(profprojdatei);
                }
                
                xvt_list_suspend(lwin);
                xvt_list_clear(lwin);
                if (prof_datei != NULL)
                  xvt_list_add(lwin, -1, (char*)prof_datei);   //Ausgabe in Listbox
                xvt_list_resume(lwin);
                schluessel_nach_profil(quelle);
                dlg136_get_daten(scroll_position);
                int numberslist=xvt_slist_count(prof_datei);
                if(numberslist>0)
                {
                  xvt_list_set_sel(lwin, 0,  TRUE);
                  // Listbox vorselektieren
                  selektion(); //in aufnehm.cpp
                }
              }//if(!abbruch208)
             }
             delete[] vergleichsstring;
             delete[] profilnamevgl   ;
             delete[] strnamevgl;
             delete[] help;
             delete[] druckstring;
             delete[] changedatei;
             fclose(profprojfile);
             
           }
         }
         else 
         {
           char buf[200];//Dick 12.01.00
           xvt_res_get_str(STR_WSPD136_NOTE_4,buf,sizeof(buf));
           xvt_dm_post_error("%s",buf);
           //xvt_dm_post_note("Keine Profile vorhanden !");
         }
         
         dlg136_save_daten(scroll_position);
         save_str_datei();
         schluessel_aendern=FALSE;
       } // DLG_136_PUSHBUTTON_7: /* Schlüssel ändern*/
       break;
//*****************************************************************************

      default:
        break;
      } // switch(xdControlId)
    } // 	case E_CONTROL:
    break;
  case E_TIMER:
		/*
			Timer associated with window went off.
		*/
		{
		}
		break;
	case E_USER:
		/*
			Application initiated.
		*/
		{
      switch (xdEvent->v.user.id) 
      {
      case -1:
      default:
        break;
      }
    }
    break;
  default:
    break;
  } //  switch (xdEvent->type)
  return 0L;
}

void starte_grafikeditor()
{
	const LPCTSTR STR_PROFILEDITOR_EXE = "profileditor.exe";


	std::string profileditordir( start_dir );
	profileditordir += "/profileditor/";
	char* profileditorDirC = new char[profileditordir.length() + 1];
	profileditordir.copy( profileditorDirC, std::string::npos );
	profileditorDirC[profileditordir.length()] = 0;

	// test auf profileditor.exe
	FILE_SPEC exeFile;
	xvt_fsys_convert_str_to_dir( profileditorDirC, &exeFile.dir );

	delete[] profileditorDirC;

	strcpy( exeFile.name, STR_PROFILEDITOR_EXE );
	strcpy( exeFile.type, "" );
	strcpy( exeFile.creator, "" );

	BOOLEAN profileditorIstDa = xvt_fsys_get_file_attr( &exeFile, XVT_FILE_ATTR_EXECUTE );
	BOOLEAN useProfilEditor = !LWA_PROJEKT && GetPrivateProfileInt( "WSPWIN", "PROFILEDITOR", 1,  "WSPWIN.INI" );

	// REMARK: clash mit der Doku von XVT: xvt_sys_get_file_attr gibt '-1' zurück, wenn
	// das Verzeichnis der zu testenden Datei nicht existiert. Also ist in unserem Fall nur '1' gut.
	if( ( profileditorIstDa == 1 ) && useProfilEditor )
	{
		// NEU: starte externen Grafikeditor
		int numberslist=xvt_slist_count(prof_datei);
		if(numberslist>0)
		{
			selektion();
			xvt_fsys_set_dir(&STR_SPEC.dir);
			id_dlg136=GRAFIK;
			dlg136_save_daten(scroll_position);
			
			save_str_datei();
			
			std::string strPath = FILE_SPEC2PATH( STR_SPEC );
			
			std::string cmdLine( profileditordir );
			cmdLine += "\\";
			cmdLine += exeFile.name;
			cmdLine += " -str \"";
			cmdLine += strPath;
			cmdLine += "\" -prf \"";
			cmdLine += file_spec.name;
			cmdLine += "\"";
			
			if( !xvt_dlg_create_res( WD_MODAL, IDD_EXECUTE_EXTERN, EM_ALL, DLG_EXECUTE_EXTERN_eh, long( &cmdLine ) ) )
				xvt_dm_post_error( "Can't open window116" );
		}
		else
		{
			char buf[200];
			xvt_res_get_str(STR_WSPD136_NOTE_4,buf,sizeof(buf));
			xvt_dm_post_error("%s",buf);
		}
	}
	else
	{
		/*	ALT: starte internen Grafikeditor */
        int numberslist=xvt_slist_count(prof_datei);
        if(numberslist>0)
        {
          selektion();
          xvt_fsys_set_dir(&STR_SPEC.dir);
          int lesen_ok;
          id_dlg136=GRAFIK;
          dlg136_save_daten(scroll_position);

          save_str_datei();

          editor_closed = FALSE;

          lesen_ok = read_profildatei( pWPL, &STR_SPEC.dir, file_spec.name );
          choice_neu =FALSE;

          if (lesen_ok ==0)
          {
            is_profil_open = TRUE;
            new_profil =FALSE;
            xvt_vobj_set_visible(dlg_136,FALSE);
                    
            if (!xvt_win_create_res(WIN_GRAPH_116, TASK_WIN, EM_ALL, WIN_GRAPH_116_eh, 0L))
              xvt_dm_post_error("Can't open window116");
          }
          else 
          {
            char buf[200];
            xvt_res_get_str(STR_PROF_READ_NOTE,buf,sizeof(buf));
            xvt_dm_post_error("%s",buf);
          }
        }
        else
        {
          char buf[200];
          xvt_res_get_str(STR_WSPD136_NOTE_4,buf,sizeof(buf));
          xvt_dm_post_error("%s",buf);
        }
	}
}
