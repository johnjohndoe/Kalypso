/***************/
/* Wspw122.cpp */
/***************/
/*
	Handler for window QWERT_EDIT ("Editieren der Abflußdatei:")
*/

#include <windows.h>
#include "xvt.h"

// Globales

#include "global_types.h"
#include "global_vars.h"
#include "..\..\wspdlg\include\export.h"

// Defines

#include "wsphilfe.h"
#include "resource.h"
#include "wspwin.h"
#include "bce_allg.h"

// Funktionen
#include "read.h"
#include "qlist.h"

/*
	Information about the window
*/
/* werden nicht benutzt??
#define WIN_RES_ID QWERT_EDIT
#define WIN_FLAGS 0x883L
#define WIN_CLASS ""
#define WIN_BORDER W_DOC
*/

// globale Variablen auschliesslich in Wspw122.cpp benutzt
char liste[80];
int i122=0;
int letzter_datensatz = 1;
WINDOW listedit122, win122_edit[16]; 

SLIST hilfslist = NULL;
SLIST_ELT e122;
char str122[100], str122_f[100], file_str_122_f[100], file_str_122[100];
char edittest[15];
char edittest_WSF[15];
char neu[5];
int len122 = 0;
int anzahl_qwert;
char* liste_ptr;
char* hilfe2;
char* liste_compare;
WINDOW scrollbar_win = NULL;
BOOLEAN win122_edit_change = FALSE;
int edit_nr = 0;
int counter = 0;
int compare = -1;
int compare_neu = -1;



// Implementation

static WNDPROC defWndProc;
LRESULT CALLBACK Win122WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_HELP:
		{
			LPHELPINFO lphi = (LPHELPINFO)lParam;
			if (hi!=NULL_HELP_INFO)
			{
				xvt_help_display_topic(hi, HID_KAPITEL_4_5_1_1);
			}
		}
		break;

	default:
		break;
	}
	return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/

BOOLEAN Win122NamePruefen( void )
// Testet den aktuellen Namen des Abflusses initialisiert entsprechend die Namen
// Rückgabwert: TRUE, falls Name ok und operationen ausgeführt wurden
// Seiteneffekte:
//          Ändern:
//                QWertDatei* ptr_qwert
//                i122
//                WINDOW listedit: Die ComboBox: hier wird der neue Name eingefügt, und selektiert
//                QWertDatei* ptr_qwert_anfang: Zeigt auf die aktuellen Abflussdaten, das letzte Element wird verändert: der Name wird eingetragen
//          Lesen
//                char liste[80]: der zu testende Namen
//                abflussereignisse
//                letzter_datensatz
//                WINDOW win122_edit[10]: Fokus auf diese Control
{
  char test_name[5];

  if( strlen( liste ) < 2 )
  {
    char buf[200];//Dick 26.11.99
    xvt_res_get_str(STR_WSPW122_NOTE_1,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf);
    //xvt_dm_post_note("Abflussereignis mindestens 2 Buchstaben!");
    xvt_scr_set_focus_vobj( win122_edit[10] ); 
    return FALSE;
  }

  test_name[0]='\0';
  strcat( test_name,"neu" );
  if ( xvt_str_compare_ignoring_case(liste,test_name) == 0 )
  {
    char buf[200];//Dick 26.11.99
    xvt_res_get_str(STR_WSPW122_NOTE_2,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf);
    //xvt_dm_post_note("Bei Neuwahl muß der Name geändert werden");
    xvt_scr_set_focus_vobj( win122_edit[10] ); 
    return FALSE;
  }

  if( !isalpha( liste[0] ) )
  {
      char buf[200];//Dick 26.11.99
      xvt_res_get_str( STR_WSPW122_NOTE_ALPHA, buf, sizeof( buf ) );
      xvt_dm_post_note( "%s", buf ); //xvt_dm_post_note("Das erste Zeichen muss ein Buchstabe sein.");
      xvt_scr_set_focus_vobj( win122_edit[10] ); 
      return FALSE;
  }

  int laenge = strlen(liste);
  for( int i = 0; i < laenge; i++ )
  {
    if( liste[i] == ' ' )
    {
      char buf[200];//Dick 26.11.99
      xvt_res_get_str(STR_WSPW122_NOTE_3,buf,sizeof(buf));
      xvt_dm_post_note("%s",buf);
      //xvt_dm_post_note("Keine Leerzeichen beim Name!");
      xvt_scr_set_focus_vobj( win122_edit[10] ); 
      return FALSE;
    }; // if liste[i] == ' '
  }; // for i

  ptr_qwert = ptr_qwert_anfang;
  for( i122 = 1; i122 < letzter_datensatz; i122++ )
  {
    if ( ptr_qwert )
      ptr_qwert = ptr_qwert->next;
  }
  i122 = xvt_slist_count( abflussereignisse );
  if ( letzter_datensatz == i122 )
    xvt_slist_add_at_pos( abflussereignisse, i122, "neu", 0 );
  
  xvt_slist_change_str( abflussereignisse, liste, letzter_datensatz - 1 );
  strcpy( ptr_qwert->info, liste );
  xvt_list_clear( listedit122 );
  xvt_list_add( listedit122, 0, (char*)abflussereignisse );
  xvt_list_set_sel( listedit122, letzter_datensatz - 1, TRUE );

  return TRUE;
}; // Win122NamePruefen


/*
	Handler for window QWERT_EDIT ("Editieren der Abflußdatei:")
*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
QWERT_EDIT_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
QWERT_EDIT_eh XVT_CALLCONV2 (xdWindow, xdEvent)
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
      defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
      SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Win122WindowProc);
      /***********************************/
      POINT xy;
      RECT rct;
      win122=xdWindow;
      menue_alles_sperren(); // in read.cpp

      if( teilgebiete )
      {
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,WIN_122_TEXT_15)),FALSE);
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,QWERT_EDIT_LISTBUTTON_20)),FALSE);
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,WIN_122_EDIT_11)),FALSE);
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,WIN_122_TEXT_16)),FALSE);
        //Textfeld vergrössern
        GetWindowRect((HWND)xvt_vobj_get_attr(xvt_win_get_ctl(xdWindow,WIN_122_TEXT_14),ATTR_NATIVE_WINDOW),&rct);
        
        xy.x = rct.left;
        xy.y = rct.top;
        ScreenToClient((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW),&xy);
        MoveWindow((HWND)xvt_vobj_get_attr(xvt_win_get_ctl(xdWindow,WIN_122_TEXT_14),ATTR_NATIVE_WINDOW),
          xy.x, xy.y, rct.right-rct.left+30, rct.bottom-rct.top, TRUE );
        
        xvt_vobj_set_title((xvt_win_get_ctl(xdWindow,WIN_122_TEXT_14)),"Teilgebietsnummer");
        xvt_win_set_doc_title(xdWindow,"Editieren der Teilgebietsdatei");
        
        //Scrollbar verschieben
        GetWindowRect((HWND)xvt_vobj_get_attr(xvt_win_get_ctl(xdWindow,WIN_122_VSCROLL_17),ATTR_NATIVE_WINDOW),&rct);
        
        xy.x=rct.left;
        xy.y=rct.top;
        ScreenToClient((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW),&xy);
        MoveWindow((HWND)xvt_vobj_get_attr(xvt_win_get_ctl(xdWindow,WIN_122_VSCROLL_17),ATTR_NATIVE_WINDOW),
          xy.x-137, xy.y, rct.right-rct.left, rct.bottom-rct.top, TRUE );
        
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,WIN_122_EDIT_21)),FALSE);
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,WIN_122_EDIT_22)),FALSE);
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,WIN_122_EDIT_23)),FALSE);
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,WIN_122_EDIT_24)),FALSE);
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,WIN_122_EDIT_25)),FALSE);
        
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,QWERT_EDIT_TEXT_26)),FALSE);
      }
      change_strang = FALSE;
      hilfslist = xvt_slist_create();
      
      if (ptr_qwert_anfang!=NULL)
        Delete_Qwert_Datei();
      
      str122[0]='\0';
      strcpy(str122,STR_SPEC.name);
      len122=strlen(str122);
      str122[len122-3]='\0';
      len122=0;

      strcpy(str122_f,str122);
      strcat(str122_f,"wsf");
      strcpy(wsfix_spec.name,str122_f);
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str122_f, 50);
      xvt_fsys_convert_str_to_dir(str122_f,&wsfix_spec.dir);
      str122_f[0]='\0';
      xvt_fsys_convert_dir_to_str(&wsfix_spec.dir,file_str_122_f,79);
      strcat(file_str_122_f,"\\");
      strcat(file_str_122_f,wsfix_spec.name);
      if ((wsfix_datei =fopen(file_str_122_f,"r+"))==NULL)
        exist_wsf_wert=FALSE;
      else
      {
        exist_wsf_wert=TRUE;
        fclose (wsfix_datei);
      }
      
      if(!teilgebiete)
        strcat(str122,"qwt");
      else
        strcat(str122,"tgb");
      strcpy(qwert_spec.name, str122);
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str122, 50);
      xvt_fsys_convert_str_to_dir(str122,&qwert_spec.dir);
      str122[0]='\0';
      xvt_fsys_convert_dir_to_str(&qwert_spec.dir,file_str_122,79);
      strcat(file_str_122,"\\");
      strcat(file_str_122,qwert_spec.name);
      
      if ((qwert_datei =fopen(file_str_122,"r+"))==NULL)
        exist_qwert=FALSE;
      else
      {
        exist_qwert=TRUE;
        fclose (qwert_datei);
      }
      
      for (i122=0;i122<11;i122++)
        win122_edit[i122]= xvt_win_get_ctl(xdWindow,WIN_122_EDIT_1 +i122);
      
      for (i122=11;i122<16;i122++) //Dick 2.02.99 11-15 Wassersp.fix 3.Spalte
        win122_edit[i122]= xvt_win_get_ctl(xdWindow,i122+9);
      
      
      listedit122=xvt_win_get_ctl(xdWindow,QWERT_EDIT_LISTBUTTON_20);
      if(!teilgebiete)
      {
        // Listbutton vergrössern
        GetWindowRect((HWND)xvt_vobj_get_attr(listedit122,ATTR_NATIVE_WINDOW),&rct);
        
        xy.x=rct.left;
        xy.y=rct.top;
        ScreenToClient((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW),&xy);
        MoveWindow((HWND)xvt_vobj_get_attr(listedit122,ATTR_NATIVE_WINDOW), xy.x, xy.y, 
          rct.right-rct.left+40, rct.bottom-rct.top, TRUE );

        //Editfeld vergrössern
        GetWindowRect((HWND)xvt_vobj_get_attr(xvt_win_get_ctl(xdWindow,WIN_122_EDIT_11),ATTR_NATIVE_WINDOW),&rct);
        
        xy.x=rct.left;
        xy.y=rct.top;
        ScreenToClient((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW),&xy);
        MoveWindow((HWND)xvt_vobj_get_attr(xvt_win_get_ctl(xdWindow,WIN_122_EDIT_11),ATTR_NATIVE_WINDOW),
          xy.x, xy.y, rct.right-rct.left+40, rct.bottom-rct.top, TRUE );
      }

      if (exist_qwert)
      {
        if (abflussereignisse!=NULL)
        {
          xvt_slist_destroy(abflussereignisse);
          abflussereignisse=NULL;
        }
        scr.datensatz=1;
        change_strang=FALSE;
        lese_qwert_datei();
        if ( change_strang )
        {
          if(!teilgebiete)
          {
            char buf[200];
            xvt_res_get_str(STR_WSPW122_NOTE_4,buf,sizeof(buf));
            xvt_dm_post_note("%s",buf);
            //xvt_dm_post_note("Strangtabelle geändert, überprüfen Sie die Abflußdatei\n");
          }
          else
          {
            char buf[200];
            xvt_res_get_str(STR_WSPW122_NOTE_5,buf,sizeof(buf));
            xvt_dm_post_note("%s",buf);
            //xvt_dm_post_note("Strangtabelle geändert, überprüfen Sie die Teilgebietsdatei\n");
          }
          change_strang=FALSE;                                
        }
        anzahl_qwert=TesteEndeQwert();
        
        lese_ereignisse();
        i122=xvt_slist_count(abflussereignisse);
        xvt_slist_add_at_pos(abflussereignisse,i122,"neu",0);
        xvt_list_add(listedit122,0,(char*)abflussereignisse);
        xvt_list_set_sel(listedit122,0,TRUE);
        hilfslist=xvt_list_get_sel(listedit122);
        liste_ptr=xvt_slist_get_elt(hilfslist,0,0);
        strcpy(liste,liste_ptr);
        liste[strlen(liste)]='\0';
        
        letzter_datensatz=1;
        xvt_vobj_set_title(win122_edit[10],&liste[0]);
        xvt_vobj_get_title(win122_edit[10],liste,21);//Dick 8.07.99 10->21
        xvt_scr_set_focus_vobj(win122_edit[10]);
      } //exist-qwert=True
      
      if (((exist_qwert) && (!change_strang)) || (!exist_qwert))
      {
        read_anfang_ende(); //in read.cpp
        anzahl_elem=xvt_slist_count(list_ende);
        if(anzahl_elem>0)
        {
          for (e122=xvt_slist_get_first(list_ende);e122!=NULL;e122=xvt_slist_get_next(list_ende,e122))
            hilfe2=xvt_slist_get(list_ende,e122,0l);
          xvt_slist_add_at_elt(list_anfang,NULL,hilfe2,0);
        }
        anzahl_elem=xvt_slist_count(list_anfang);
        
        if(anzahl_elem>1)
        {
          scr.datensatz=1;
          scr.anzahl=anzahl_elem;
          scr.scrollpos=1;
          
          /*********** Scrollbar **************************/
          scrollbar_win = xvt_win_get_ctl(xdWindow,WIN_122_VSCROLL_17);
          xvt_sbar_set_range(scrollbar_win,HVSCROLL,1,anzahl_elem-3);//Dick 9.02.99
          xvt_sbar_set_pos(scrollbar_win,HVSCROLL,1);
        }
        else
        {
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_WSPW122_NOTE_6,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
          //xvt_dm_post_note("Es müssen mindestens zwei Stationen existieren");
          xvt_vobj_destroy(xdWindow);
          break;
        }
      }// allgemein
      
      if ((exist_qwert==FALSE) && (anzahl_elem>1))    // NEU
      {
        abflussereignisse=xvt_slist_create();
        xvt_slist_add_at_elt(abflussereignisse,NULL,"neu",0);
        xvt_list_add(listedit122,0,(char*)abflussereignisse);
        xvt_list_set_sel(listedit122,0,TRUE);
        hilfslist=xvt_list_get_sel(listedit122);
        liste_ptr=xvt_slist_get_elt(hilfslist,0,0);
        strcpy(liste,liste_ptr);
        liste[strlen(liste)]='\0';
        
        letzter_datensatz=1;
        xvt_vobj_set_title(win122_edit[10],&liste[0]);
        xvt_vobj_get_title(win122_edit[10],liste,10);
        xvt_scr_set_focus_vobj(win122_edit[10]);
        
        anzahl_qwert =1;
        
        if (ptr_qwert_anfang!=NULL)
          Delete_Qwert_Datei();
        
        MakeNewNode();
        MakeNewQSatz(anzahl_elem);
        LeseSlistInQwert(list_anfang,scr.datensatz);
        strcpy(ptr_qwert->info,liste);
      }   // Ende NEU
      
      if (((exist_qwert) && (!change_strang)) || ((!exist_qwert) && (anzahl_elem>1)))
      {
        win122_get_scroll_daten(&scr);
        display_win122_edit(&win122_edit[0],&scr);
        xvt_scr_set_focus_vobj(win122_edit[10]);
      }
    }
    break;

  case E_DESTROY:
    {
      teilgebiete=FALSE;
      Delete_Qwert_Datei();
      if (abflussereignisse!=NULL)
      {
        xvt_slist_destroy(abflussereignisse);
        abflussereignisse=NULL;
      }
      if (hilfslist!=NULL)
      {
        xvt_slist_destroy(hilfslist);
        hilfslist=NULL;
      }
      menue_alles_oeffnen(); // in read.cpp
    }
    win122=NULL_WIN;
    return 0L;

  case E_UPDATE:
    xvt_dwin_clear(xdWindow,WspwinMainBackgroundColor);
    break;

  case E_CLOSE:
    xvt_vobj_destroy(xdWindow);
    break;

  case E_CHAR:
    {
      switch(xdEvent->v.chr.ch)
      {
      case K_TAB:
        {
          WINDOW test_window = xvt_scr_get_focus_vobj();
          int tab_ctr_nr = -1;
          for ( int i = 5; i < 16; i++ )
          {
            if ( test_window == win122_edit[i] )
            {
              tab_ctr_nr = i;
              break;
            };
          };

          switch ( tab_ctr_nr )
          {
          case 5:
          case 6:
          case 7:
          case 8:
          case 9:
            xvt_scr_set_focus_vobj( win122_edit[tab_ctr_nr + 6] );
            break;

          case 10:
            xvt_scr_set_focus_vobj( win122_edit[5] );
            break;

          case 11:
          case 12:
          case 13:
          case 14:
              xvt_scr_set_focus_vobj( win122_edit[tab_ctr_nr - 5] );
              break;

          case 15:
            {
              xvt_vobj_get_title( win122_edit[4], edittest, 10 );
              xvt_vobj_get_title( win122_edit[9], str122, 10 );
              xvt_vobj_get_title( win122_edit[15], edittest_WSF, 10 );
              if ( win122_edit_change )
              {
                fehler = is_zahl(edittest_WSF);
                if ( ( fehler == 0 ) || ( edittest[0] == '\0' ) )
                {
                  char buf[200];//Dick 26.11.99
                  xvt_res_get_str( STR_ERROR_INPUT, buf, sizeof(buf) ); // "Falsche Eingabe"
                  xvt_dm_post_note( "%s", buf );
                  str122[0] = '\0';
                  xvt_vobj_set_title( win122_edit[15], "" ); //edittest_WSF );
                  if( edittest[0] == '\0' )
                    xvt_scr_set_focus_vobj(win122_edit[10]);
                }
                else
                {
                  win122_save_scroll_daten( &scr, edittest_WSF, edit_nr, 1 );
                  win122_edit_change = FALSE;
                }
              }
              if ( scr.scrollpos < anzahl_elem - 4 )
              {
                scr.scrollpos ++;
                xvt_sbar_set_pos( scrollbar_win, HVSCROLL, scr.scrollpos );
                win122_get_scroll_daten( &scr );
                display_win122_edit( &win122_edit[0], &scr );
                xvt_scr_set_focus_vobj( win122_edit[9] );
              }
              else
              {
                xvt_scr_set_focus_vobj(win122_edit[9]);
              }
            }; // case 15:
            break;
          }; // switch tab_ctr_nr
        }
        break;

      case ENTER:
        {
          if ( teilgebiete || Win122NamePruefen() )
          {
            int i, mode = 0;
            BOOLEAN editfeld=FALSE;
            WINDOW test_window = xvt_scr_get_focus_vobj();
            
            for (i=5;i<=9;i++)
            {
              if (test_window==win122_edit[i])
                editfeld=TRUE;
            }
            for (i=11;i<=15;i++) //Dick 2.02.99 neu
            {
              if (test_window==win122_edit[i])
              {
                editfeld=TRUE;
                mode=1;
              }
            }
            if (editfeld)
            {
              str122[0]='\0';
              xvt_vobj_get_title(test_window,str122,10);
              if (win122_edit_change)
              {
                fehler = is_zahl(str122);
                if (fehler == 0) 
                {
                  char buf[200];
                  xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                  xvt_dm_post_note("%s",buf);
                  //xvt_dm_post_note("Falsche Eingabe");
                  str122[0]='\0';
                  xvt_vobj_set_title(test_window,str122);
                }
                else
                {
                  xvt_vobj_set_title(test_window,str122);
                  win122_save_scroll_daten(&scr,str122,edit_nr,mode);
                  win122_edit_change = FALSE;
                }
              }
            }
            
            xvt_scr_set_focus_vobj(xvt_win_get_ctl(xdWindow,WIN_122_PUSHBUTTON_18));
            schreibe_qwert_datei();
            qwert_fehlt=FALSE;
            xvt_vobj_destroy(xdWindow);
          }
        } // case Enter
        break;
      } // switch
    } // case E_CHAR
    break;
    
  case E_CONTROL:
    {
      int station_nr, control_nr;

      switch(xdControlId)
      {
      case WIN_122_EDIT_1:		
      case WIN_122_EDIT_2:			
      case WIN_122_EDIT_3:
      case WIN_122_EDIT_4:
      case WIN_122_EDIT_5:
        {
          if (xdEvent->v.ctl.ci.v.edit.focus_change) 
          {
            if (xdEvent->v.ctl.ci.v.edit.active) 
            {
              if(!teilgebiete)
                Win122NamePruefen();
            }
          }
        }
        break;

      case WIN_122_EDIT_6:
        station_nr = 0;
        control_nr = 5;
        goto E_control_goto_label;

      case WIN_122_EDIT_7:
        station_nr = 1;
        control_nr = 6;
        goto E_control_goto_label;

      case WIN_122_EDIT_8:
        station_nr = 2;
        control_nr = 7;
        goto E_control_goto_label;

      case WIN_122_EDIT_9:
        station_nr = 3;
        control_nr = 8;
        goto E_control_goto_label;

      case WIN_122_EDIT_10:
        station_nr = 4;
        control_nr = 9;
        goto E_control_goto_label;

      case WIN_122_EDIT_21:
        station_nr = 0;
        control_nr = 11;
        goto E_control_goto_label;

      case WIN_122_EDIT_22:
        station_nr = 1;
        control_nr = 12;
        goto E_control_goto_label;

      case WIN_122_EDIT_23:
        station_nr = 2;
        control_nr = 13;
        goto E_control_goto_label;

      case WIN_122_EDIT_24:
        station_nr = 3;
        control_nr = 14;
        goto E_control_goto_label;

      case WIN_122_EDIT_25:
        station_nr = 4;
        control_nr = 15;
        goto E_control_goto_label;

      E_control_goto_label: 
        if ( xdEvent->v.ctl.ci.v.edit.focus_change )
        {
          if ( xdEvent->v.ctl.ci.v.edit.active )
          {	/*	 focus has entered the control			*/
            edit_nr = station_nr;
            if( !teilgebiete )
              Win122NamePruefen();
          }
          else
          {	/*	focus has left the control		  	*/
            edittest[0] = '\0';
            xvt_vobj_get_title( win122_edit[station_nr], edittest, 10 );
            xvt_vobj_get_title( win122_edit[control_nr], str122, 10 );
            if ( win122_edit_change )
            {
              fehler = is_zahl( str122 );
              if ( ( fehler == 0 ) || ( edittest[0] == '\0' ) )
              {
                char buf[200];//Dick 26.11.99
                xvt_res_get_str( STR_ERROR_INPUT, buf, sizeof(buf) );
                xvt_dm_post_note( "%s", buf );
                //xvt_dm_post_note("Falsche Eingabe");
                str122[0] = '\0';
                xvt_vobj_set_title( win122_edit[control_nr], str122 );
                if( edittest[0] == '\0' )
                  xvt_scr_set_focus_vobj( win122_edit[10] );
              }
              else
              {
                xvt_vobj_set_title( win122_edit[control_nr], str122 );
                win122_save_scroll_daten( &scr, str122, edit_nr, control_nr > 10 ? 1 : 0 );
                win122_edit_change = FALSE;
              }
            } //if 122 edit change
          } //focus left controll
        } //focus changed
        else
        {		/*	Contents of control were changed		*/
          xvt_vobj_get_title( win122_edit[control_nr], str122, 10 );
          win122_edit_change = TRUE;
        }
        break;
        
      case WIN_122_EDIT_11:	
        {
          if (!xdEvent->v.ctl.ci.v.edit.focus_change) 
          {		/*	Contents of control were changed	*/
            xvt_vobj_get_title(win122_edit[10],liste,21);
          }
        }
        break;
        
      case QWERT_EDIT_LISTBUTTON_20: /* "List Button 20" */
        {
          hilfslist=xvt_list_get_sel(listedit122);
          liste_ptr=xvt_slist_get_elt(hilfslist,0,0);
          strcpy(liste,liste_ptr);
          liste[strlen(liste)]='\0';
          
          scr.datensatz=1;
          counter=1;
          scr.scrollpos=1;
          compare=-1;
          ptr_qwert=ptr_qwert_anfang;
          
          
          for(e122=xvt_slist_get_first(abflussereignisse);e122!=NULL;e122=xvt_slist_get_next(abflussereignisse,e122))
          {
            
            if (compare!=0)
            {
              liste_compare=xvt_slist_get(abflussereignisse,e122,0L);
              compare=xvt_str_compare_ignoring_case(liste, liste_compare);
              if (compare!=0)
              {
                if (ptr_qwert !=NULL)
                {
                  ptr_qwert=ptr_qwert->next;
                  counter++;
                }
              }
            }
            if (compare==0)
            {
              
              scr.datensatz=counter;
              letzter_datensatz=counter;
              win122_get_scroll_daten(&scr);
              display_win122_edit(&win122_edit[0],&scr);
              
            }
            
            neu[0]='\0';
            strcat(neu,"neu");
            compare_neu=xvt_str_compare_ignoring_case(liste,neu);
            if (compare_neu==0)
            {
              ptr_qwert=ptr_qwert_anfang;
              while(ptr_qwert->next!=NULL)
                ptr_qwert=ptr_qwert->next;
              ptr_qwert_ende=ptr_qwert;
              
              MakeNewNode();
              MakeNewQSatz(anzahl_elem);
              anzahl_qwert=xvt_slist_count(abflussereignisse);
              scr.datensatz=anzahl_qwert;
              letzter_datensatz=anzahl_qwert;
              LeseSlistInQwert(list_anfang,scr.datensatz);
              win122_get_scroll_daten(&scr);
              display_win122_edit(&win122_edit[0],&scr);
              
            } //if
          } //for
          
          xvt_list_set_sel(listedit122,letzter_datensatz-1,TRUE);
          xvt_vobj_set_title(win122_edit[10],&liste[0]);
          xvt_scr_set_focus_vobj(win122_edit[10]);
        }
        break;		
        
      case WIN_122_VSCROLL_17: /* "Vertical Scrollbar 17" */
        {
        /*
        Vertical scrollbar control was operated
          */
          if(!teilgebiete)//Dick 28.10.99
            Win122NamePruefen();
          if( win122_edit_change )
          {
            edittest[0]='\0';
            xvt_vobj_get_title(win122_edit[edit_nr],edittest,10);
            xvt_vobj_get_title(win122_edit[edit_nr+5],str122,10);
            xvt_vobj_get_title(win122_edit[edit_nr+11],edittest_WSF,10);
            fehler = is_zahl(str122);
            if ((fehler == 0) || (edittest[0]=='\0'))
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf); //xvt_dm_post_note("Falsche Eingabe");
              str122[0]='\0';
              xvt_vobj_set_title(win122_edit[edit_nr+5],str122);
              if(edittest[0]=='\0')
                xvt_scr_set_focus_vobj(win122_edit[10]);
              fehler=0;
            }
            else
            {
              xvt_vobj_set_title(win122_edit[edit_nr+5],str122);
              win122_save_scroll_daten(&scr,str122,edit_nr);
              win122_edit_change = FALSE;
            }
            if (fehler != 0)
            {
              fehler = is_zahl(edittest_WSF);
              if ((fehler == 0) || (edittest[0]=='\0'))
              {
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                xvt_dm_post_note("%s",buf); //xvt_dm_post_note("Falsche Eingabe");
                edittest_WSF[0]='\0';
                xvt_vobj_set_title(win122_edit[edit_nr+11],edittest_WSF);
                if(edittest[0]=='\0')
                  xvt_scr_set_focus_vobj(win122_edit[10]);
              }
              else
              {
                xvt_vobj_set_title(win122_edit[edit_nr+11],edittest_WSF);
                win122_save_scroll_daten(&scr,edittest_WSF,edit_nr,1);
                win122_edit_change = FALSE;
              }
            }               
          }
          
          switch (xdEvent->v.ctl.ci.v.scroll.what)
          {
          case SC_LINE_UP:
            {
              if (scr.scrollpos > 1)
              {
                scr.scrollpos --;
              }
            }
            break;
          case SC_LINE_DOWN:
            {
              if (scr.scrollpos < anzahl_elem -4)
              {
                scr.scrollpos ++;
              }
              
            }
            break;
          case SC_PAGE_UP:
            {
              if (scr.scrollpos > 1)
              {
                if((scr.scrollpos-5)>=1) 
                  scr.scrollpos =scr.scrollpos - 5;
                else 
                  scr.scrollpos =1;
              }
            }
            break;
          case SC_PAGE_DOWN:
            {
              if (scr.scrollpos < anzahl_elem -4)
              {
                if((scr.scrollpos+5)<=(anzahl_elem-4)) 
                  scr.scrollpos =scr.scrollpos + 5;
                else 
                  scr.scrollpos =anzahl_elem-4;
              }
              
            }
            
            break;
          case SC_THUMB:
            {
              scr.scrollpos = xdEvent->v.ctl.ci.v.scroll.pos;
            }
            break;
          case SC_THUMBTRACK:
            {
              scr.scrollpos = xdEvent->v.ctl.ci.v.scroll.pos;
            }
            break;
            
          default:
            break;
          } // switch scroll_what

          xvt_sbar_set_pos(scrollbar_win,HVSCROLL,scr.scrollpos);
          win122_get_scroll_daten(&scr);
          display_win122_edit(&win122_edit[0],&scr);



      }
      break;
      
      case WIN_122_PUSHBUTTON_18: /* "OK" */
        { 
          BOOLEAN nameOK = ( teilgebiete || Win122NamePruefen() );


          if( !GetFeature( "wsp_nodemo" ) )
          {
            char buf[200];
            xvt_res_get_str(STR_DEMO_NOTE_3,buf,sizeof(buf));
            xvt_dm_post_note("%s",buf);
            //xvt_dm_post_note("Speichern bei Demo nicht möglich");
          }
          else
          {
            if( nameOK )
              schreibe_qwert_datei();
          };

          if( nameOK )
          {
            qwert_fehlt = FALSE;
            xvt_vobj_destroy(xdWindow);
          }
        }
        break;

      case WIN_122_PUSHBUTTON_19: /* "Abbruch" */
        {
          xvt_vobj_destroy(xdWindow);
          if (abflussereignisse!=NULL)
          {
            xvt_slist_destroy(abflussereignisse);
            abflussereignisse=NULL;
          }
          qwert_fehlt=FALSE;
        }
        break;
      
      default:
        break;
      }; // switch 
    }; // case E_CONTROL
    break;
    
    default:
      break;
  }
  xvt_tx_process_event(xdWindow, xdEvent);

  return 0L;
} // QWERT_EDIT_eh XVT_CALLCONV2 (xdWindow, xdEvent)
