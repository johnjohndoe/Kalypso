
/**************************************************************************
*             DIS_PROF .CPP                                               *
*             23.01.1995                                                  *
***************************************************************************/
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "global_vars.h"
#include "..\..\wspdlg\Include\export.h"

#include "list.h"
#include "dis_prof.h"
#include "l_typen.h"
#include "typen.h"

#ifndef WIN120_SCROLL
#define WIN120_SCROLL 3050    //****  Scrollbalken  WIN120****
#endif

#ifndef WIN_scroll_116
#define WIN_scroll_116 2050    //****  Scrollbalken  WIN_116****
#endif

extern Scroller scr;
extern int typ[TYPE_SIZE];//Dick 8.12.98
extern WINDOW win_120[100], scroll_120, scroll_116;
extern WINDOW  Edit_Win116[15];
extern int w_id,i_d;
extern RCT rect_120, rect_116;
extern WINDOW WIN120,WIN_116,WIN_117;
extern int ds_info[TYPE_SIZE];//Dick 22.06.99
extern int range;

extern double sf;   //Fensterfaktor

WINDOW  dlg_sonderprofil = NULL_WIN;

BOOLEAN sichere_datenblock=FALSE;
BOOLEAN is_display_edit=FALSE;
BOOLEAN neue_datensatz_wahl ;
BOOLEAN schuetz_flag=FALSE;

extern COLOR WspwinMainBackgroundColor; //globale Hintergrunndfarbe für alle Fenster
extern short ch_id; //Win120

extern List *list;
extern ZOOM zoom_info;//Neu 7.07.98

extern BOOLEAN berechnen,editieren;
extern BOOLEAN Ber_edit_sperr=FALSE;

// vorwärtsdeklarationen
extern long XVT_CALLCONV1	DLG_155_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
void win120_create_edit_felder(void);


/******** in: WIN120 Edit-Felder: 3.Spalte loeschen   ********************/
void win120_loesche_edit_felder(void)
{
  if (is_display_edit)
	{
	 for (int i=30;i<=44;i++)
		{
		 if (win_120[i] != NULL_WIN)
			 {
			  xvt_vobj_destroy(win_120[i]);
			  win_120[i] = NULL_WIN;
			 }
		}
	 if ((ch_id>=30)&&(ch_id<=44))	ch_id =-1;
	  /**  SCROLLBAR RECHTS   3-spaltig  loeschen   ***/
	 if (scroll_120 != NULL_WIN)
		 {
		  xvt_vobj_destroy(scroll_120);
		  scroll_120 = NULL_WIN;
		 }
	 xvt_rect_set(&rect_120,(short)(333*sf),(short)( 90*sf), (short)(354*sf),(short) (540*sf));
	 scroll_120 = xvt_ctl_create(WC_VSCROLL, &rect_120, 0L, WIN120, 0L,0L,WIN120_SCROLL);
	 ChangeFontAndSize((HWND)xvt_vobj_get_attr(scroll_120,ATTR_NATIVE_WINDOW));	// GHJ
	 range=ds_info[1]-15;
	 xvt_sbar_set_range(scroll_120, HVSCROLL, 1,range );
	 xvt_sbar_set_pos(scroll_120,HVSCROLL,1);

	 xvt_dwin_update(WIN120);
	 is_display_edit = FALSE;
	}
}
/******** in: WIN120 Edit-Felder: 3.Spalte loeschen   ********************/
void win120_create_edit_felder(void)
{
 char str[15];
	if ( typ[scr.datensatz] >= 2 )    //nicht: Gelände-NN
		  { //lösche Scrollbar 2-spaltig
			if (WIN120 != NULL_WIN)
			 {
			  if (neue_datensatz_wahl)
				  {
				 //	win120_loesche_edit_felder();
					is_display_edit = TRUE;

					if (scroll_120 != NULL_WIN)
						{
						 xvt_vobj_destroy(scroll_120);
						 scroll_120 = NULL_WIN;
						}

					w_id=30;
					i_d=3000;
					for (int i=1;i<=15;i++)      // create 3.Spalte
					 {
					  if (win_120[w_id]!=NULL_WIN)
						 {
						  xvt_vobj_destroy(win_120[w_id]);
						  win_120[w_id]=NULL_WIN;
						 }
					  xvt_rect_set(&rect_120,(short)((63+270)*sf),(short)((60+i*30)*sf),(short)(468*sf),(short)((90+i*30)*sf ));
					  win_120[w_id] = xvt_ctl_create(WC_EDIT, &rect_120, 0L, WIN120, 0L, 0L,i_d + w_id);
					  ChangeFontAndSize((HWND)xvt_vobj_get_attr(win_120[w_id],ATTR_NATIVE_WINDOW));	// GHJ
					  if (win_120[w_id]==NULL_WIN)
                          {
						   //xvt_dm_post_warning("Nicht genügend Speicher, um Anwendung auszuführen.Schließen Sie ein oder mehrere Anwendungen!");
                           char buf[200];//Dick 26.11.99
                           xvt_res_get_str(STR_MEM_OUT,buf,sizeof(buf));
                           xvt_dm_post_note("%s",buf);
                          }
					  w_id++;
					 }

					/****  SCROLLBAR RECHTS   3-spaltig setzen    *******/
					xvt_rect_set(&rect_120,(short) (468*sf),(short)( 90*sf),(short)( 489*sf),(short)( 540*sf));
					scroll_120 = xvt_ctl_create(WC_VSCROLL, &rect_120, 0L, WIN120, 0L,0L,WIN120_SCROLL);
					ChangeFontAndSize((HWND)xvt_vobj_get_attr(scroll_120,ATTR_NATIVE_WINDOW));	// GHJ
					range=ds_info[1]-15;
					xvt_sbar_set_range(scroll_120, HVSCROLL, 1,range );
					xvt_sbar_set_pos(scroll_120,HVSCROLL,1);
					neue_datensatz_wahl = FALSE;
				  }

			  xvt_dwin_clear(WIN120,WspwinMainBackgroundColor);
			  //xvt_dwin_clear(WIN120, (COLOR)xvt_vobj_get_attr(WIN120, ATTR_BACK_COLOR));
			  w_id = 30;
			  for (int i=1;i<=15;i++)      // create 3.Spalte
					{
					 if (scr.z[i-1] == BCE_NAN)
						str[0]='\0';
					 else
                        sprintf(str,"%.4lf",scr.z[i-1]);
						//gcvt(scr.z[i-1],10,str);
					 if (win_120[w_id]!=NULL_WIN)
						xvt_vobj_set_title(win_120[w_id],str);
					 w_id++;
					}
				  }
			xvt_dwin_clear(WIN120,WspwinMainBackgroundColor); 
			//xvt_dwin_clear(WIN120, (COLOR)xvt_vobj_get_attr(WIN120, ATTR_BACK_COLOR));
		  }
}
/*******************  in WIN116 Edit-Felder:Z loeschen   ********************/
void win116_loesche_edit_felder(void)
{
  if (is_display_edit)
	{
	 for (int i=10;i<15;i++)
		{
		 if (Edit_Win116[i] != NULL_WIN)
			 {
			  xvt_vobj_destroy(Edit_Win116[i]);
			  Edit_Win116[i] = NULL_WIN;
			 }
		}
	  /*******  SCROLLBAR RECHTS   3-spaltig     *******/
	 if (scroll_116 != NULL_WIN)
		 {
		  xvt_vobj_destroy(scroll_116);
		  scroll_116 = NULL_WIN;
		 }
	 xvt_rect_set(&rect_116,(short) (798*sf),(short)( 54*sf),(short)( 819*sf),(short)( 204*sf));
	 scroll_116 = xvt_ctl_create(WC_VSCROLL, &rect_116, 0L, WIN_116, 0L,0L,WIN_scroll_116);
	 ChangeFontAndSize((HWND)xvt_vobj_get_attr(scroll_116,ATTR_NATIVE_WINDOW));	// GHJ
     if(zoom_info.level==0) //Neu 7.07.98
         {
	      range=ds_info[1]-3;//Neu 5-->3 02.07.98
	      xvt_sbar_set_range(scroll_116, HVSCROLL, 1,range );
         }
     else
         {
          xvt_sbar_set_range(scroll_116, HVSCROLL,zoom_info.pos_station_min, zoom_info.pos_station_max-3);
         }
	 xvt_sbar_set_pos(scroll_116,HVSCROLL,1);

	 // update window 116
	 xvt_dwin_clear(WIN_116,WspwinMainBackgroundColor);
	 //xvt_dwin_clear(WIN_116, (COLOR)xvt_vobj_get_attr(WIN_116, ATTR_BACK_COLOR));
	 is_display_edit = FALSE;
	}
}
/*************************************************************************/
void win116_create_edit_felder(void)
{
 char str[15];
		 if ( typ[scr.datensatz] >= 2 )    //nicht: Gelände-NN
		  { //lösche Scrollbar 2-spaltig
			if (WIN_116 != NULL_WIN)
			  {
				if (neue_datensatz_wahl)
				 {
					win116_loesche_edit_felder();
					is_display_edit = TRUE;
					w_id=10;
					i_d=2000;

					for (int i=1;i<=5;i++)      // create 3.Spalte
					  {
						if (Edit_Win116[w_id]!=NULL_WIN)
						 {
						  xvt_vobj_destroy(Edit_Win116[w_id]);
						  Edit_Win116[w_id]=NULL_WIN;
						 }
						xvt_rect_set(&rect_116,(short)(798*sf),(short)((24+i*30)*sf),(short)(933*sf),(short)((54+i*30)*sf));
						Edit_Win116[w_id] = xvt_ctl_create(WC_EDIT, &rect_116, 0L, WIN_116, 0L, 0L,i_d + w_id);
						ChangeFontAndSize((HWND)xvt_vobj_get_attr(Edit_Win116[w_id],ATTR_NATIVE_WINDOW));		// GHJ
						w_id++;
					  }
					/*********  SCROLLBAR RECHTS   3-spaltig setzen    *******/
					if (scroll_116 != NULL_WIN)
					 {
						xvt_vobj_destroy(scroll_116);
						scroll_116 = NULL_WIN;
					 }
					xvt_rect_set(&rect_116,(short)( 933*sf),(short)( 54*sf),(short)( 954*sf),(short)( 204*sf));
					scroll_116 = xvt_ctl_create(WC_VSCROLL, &rect_116, 0L, WIN_116, 0L,0L,WIN_scroll_116);
					ChangeFontAndSize((HWND)xvt_vobj_get_attr(scroll_116,ATTR_NATIVE_WINDOW));		// GHJ
					if(zoom_info.level==0) //Neu 7.07.98
                        {
	                     range=ds_info[1]-3;//Neu 5-->3 02.07.98
	                     xvt_sbar_set_range(scroll_116, HVSCROLL, 1,range );
                        }
                    else
                        {
                         xvt_sbar_set_range(scroll_116, HVSCROLL,zoom_info.pos_station_min, zoom_info.pos_station_max-3);
                        }
					xvt_sbar_set_pos(scroll_116,HVSCROLL,1);
					neue_datensatz_wahl = FALSE;
				 }
				w_id=10;
				for (int i=1;i<=5;i++)      // create 3.Spalte
				 {
						if (scr.z[i-1] == BCE_NAN)
							 str[0]='\0';
						else
                             sprintf(str,"%.4lf",scr.z[i-1]);
						//	 gcvt(scr.z[i-1],10,str);
						if (Edit_Win116[w_id] !=NULL_WIN)
							 xvt_vobj_set_title(Edit_Win116[w_id], str);
				  w_id++;
				 }
			  }
			xvt_dwin_clear(WIN_116,WspwinMainBackgroundColor); 
			//xvt_dwin_clear(WIN_116, (COLOR)xvt_vobj_get_attr(WIN_116, ATTR_BACK_COLOR));
  }
}
/********  WIN 120 :Profildaten in Edit-Felder:X-Y-Z ausgeben  ***********/
void display_prof120(WINDOW *win)
{
  int win_id;
  char str[15];
  
  win_id=0;
  double lastX = scr.lastX;
  for( int i = 0; i < 15;i++ )    //  Daten Gelände-NN anzeigen
	{
    double x = scr.x[i];
    bool bRueck = lastX != BCE_NAN && x <= lastX;

    if( x == BCE_NAN )
      str[0] = '\0';
    else                             // horizontal
      sprintf( str, "%.4lf", x );

    if( win[win_id] != NULL_WIN )
    {
      COLOR foreColor = bRueck ? COLOR_RED : COLOR_BLACK;

      XVT_COLOR_COMPONENT ctl_colors[] =
      {
        { XVT_COLOR_FOREGROUND, foreColor },
        { XVT_COLOR_BACKGROUND, COLOR_WHITE },
        { XVT_COLOR_NULL, 0 }
      };

      xvt_ctl_set_colors( win[win_id], (XVT_COLOR_COMPONENT*)ctl_colors, XVT_COLOR_ACTION_SET );
      xvt_vobj_set_title( win[win_id], str );
    }

    win_id++;
    lastX = x;
  } // for i
  
  win_id=15;
  for (i=0;i < 15;i++)
	 {
    if (scr.y[i] == BCE_NAN)
      str[0]='\0';
    else                           // vertikal
      sprintf(str,"%.4lf",scr.y[i]);
		  //gcvt(scr.y[i],10,str);
    if (win[win_id]!=NULL_WIN)
      xvt_vobj_set_title(win[win_id],str);
    win_id++;
	 }
  
  /**** Ausgabe der Sonderprofile in Dialog 140-144  ****/
  win_id = 30;
  if ((dlg_sonderprofil != NULL_WIN)&&(neue_datensatz_wahl))    // falls Dialog geöffnet erst schliessen
  {
    xvt_vobj_destroy(dlg_sonderprofil);
    dlg_sonderprofil = NULL_WIN;
  }
  switch( typ[scr.datensatz] )
  {
	 case SOHLHOEHE:
   case GELAENDEHOEHE:
			  {
          if (neue_datensatz_wahl)
          {
            win120_loesche_edit_felder();
            
            if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_110, EM_ALL, DLG_110_eh, 0L))==NULL_WIN)
              xvt_dm_post_error("Can't open dialog 110");
            neue_datensatz_wahl = FALSE;
          }
        }
        break;
   case AXM:
   case AYM:
   case DPM:
     {
       if (neue_datensatz_wahl)
       {
         win120_create_edit_felder();					
         if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_110, EM_ALL, DLG_110_eh, 0L))==NULL_WIN)
           xvt_dm_post_error("Can't open dialog 110");
         neue_datensatz_wahl = FALSE;
       }
       else
         win120_create_edit_felder();
     }
     break;
   case TRENNFLAECHEN:
   case BORDVOLL:
			  {
          if( neue_datensatz_wahl )
          {
            win120_loesche_edit_felder();
            
            if( ( dlg_sonderprofil = xvt_dlg_create_res( WD_MODELESS,DLG_143, EM_ALL, DLG_143_eh, typ[scr.datensatz] ) ) == NULL_WIN )
              xvt_dm_post_error( "Can't open dialog 143" );
            neue_datensatz_wahl = FALSE;
          }
        }
        break;

	  case STATION:
      {
        char tmp1[251];
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          win120_create_edit_felder();
          neue_datensatz_wahl = FALSE;
        }
        else
          win120_create_edit_felder();
        if (dlg_sonderprofil != NULL_WIN)
        {
          xvt_vobj_destroy(dlg_sonderprofil);
          dlg_sonderprofil = NULL_WIN;
        }
        if (list->ExtractStationsString(scr.datensatz,tmp1,STATION))
        {
          if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_167, EM_ALL, DLG_167_eh, 0L))==NULL_WIN)
            xvt_dm_post_error("Can't open dialog 167");
          xvt_vobj_set_title(xvt_win_get_ctl(dlg_sonderprofil,DLG_167_TEXT),tmp1);
        }
      }
      break;
    case STATION_FIX://Dick 5.02.99
      {
        char tmp1[251];
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          win120_create_edit_felder();
          neue_datensatz_wahl = FALSE;
        }
        else
          win120_create_edit_felder();
        if (dlg_sonderprofil != NULL_WIN)
        {
          xvt_vobj_destroy(dlg_sonderprofil);
          dlg_sonderprofil = NULL_WIN;
        }
        if (list->ExtractStationsString(scr.datensatz,tmp1,STATION_FIX))
        {
          if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_167, EM_ALL, DLG_167_eh, 0L))==NULL_WIN)
            xvt_dm_post_error("Can't open dialog 167");
          xvt_vobj_set_title(xvt_win_get_ctl(dlg_sonderprofil,DLG_167_TEXT),tmp1);
        }
      }
      break;
    case DURCHST_BEREICH:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_140, EM_ALL, DLG_140_eh, 0L))==NULL_WIN)
            xvt_dm_post_error("Can't open dialog 140");
          neue_datensatz_wahl = FALSE;
        }
      }
      break;
    case RAUHIGKEIT:
    case RAUHIGKEIT_KST:     //Dick 5.08.98
      {
        if (neue_datensatz_wahl)
        {
          
          win120_create_edit_felder();
          
          if (!xvt_dlg_create_res(WD_MODELESS,DLG_154, EM_ALL, DLG_154_eh, 0L))
            xvt_dm_post_error("Can't open dialog 154");                    
          neue_datensatz_wahl = FALSE;
        }
        win120_create_edit_felder();
      }
      break;
    case MAUL:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_141, EM_ALL, DLG_141_eh, 0L))==NULL_WIN)
            xvt_dm_post_error("Can't open dialog 141");
          neue_datensatz_wahl = FALSE;
        }
      }
      break;
    case EIPROFIL:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_141, EM_ALL, DLG_141_eh, 0L))==NULL_WIN)
            xvt_dm_post_error("Can't open dialog 141");
          neue_datensatz_wahl = FALSE;
        }
      }
      break;
    case KREIS:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_144, EM_ALL, DLG_144_eh, 0L))==NULL_WIN)
            xvt_dm_post_error("Can't open dialog 144");
          neue_datensatz_wahl = FALSE;
        }
      }
      break;
    case KREISSEGM:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_155, EM_ALL, DLG_155_eh, 0L))==NULL_WIN)
            xvt_dm_post_error("Can't open dialog 155");
          neue_datensatz_wahl = FALSE;
        }
      }
      break;
    case TRAPEZ:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_142, EM_ALL, DLG_142_eh, 0L))==NULL_WIN)
            xvt_dm_post_error("Can't open dialog 142");
          neue_datensatz_wahl = FALSE;
        }
      }
      break;
    case UK_BRUECKE:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          if (!xvt_dlg_create_res(WD_MODELESS,DLG_212, EM_ALL, DLG_212_eh, 0L))
            xvt_dm_post_error("Can't open dialog 212");
          win120_create_edit_felder();
          neue_datensatz_wahl = FALSE;
        }
        else
          win120_create_edit_felder();
      }
      break;
    case OK_BRUECKE:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          if( LWA_PROJEKT )
          {
            if (!xvt_dlg_create_res(WD_MODELESS,DLG_107, EM_ALL, DLG_107_eh, 0L))
              xvt_dm_post_error("Can't open dialog 107");
          }
          win120_create_edit_felder();
          neue_datensatz_wahl = FALSE;
        }
        else
          win120_create_edit_felder();
      }
      break;
    case SCHUETZ:
      {
        if (neue_datensatz_wahl)
        {
          win116_loesche_edit_felder();
          schuetz_flag=TRUE;
          if( LWA_PROJEKT )
          {
            if (!xvt_dlg_create_res(WD_MODELESS,DLG_107, EM_ALL, DLG_107_eh, 0L))
              xvt_dm_post_error("Can't open dialog 107");
          };
          neue_datensatz_wahl = FALSE;
        }
      }
      break;
    case OK_WEHRS:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          win120_create_edit_felder();
          neue_datensatz_wahl = FALSE;
          if( LWA_PROJEKT )
          {
            if (!xvt_dlg_create_res(WD_MODELESS,DLG_160, EM_ALL, DLG_160_eh, 0L))
              xvt_dm_post_error("Can't open dialog 160");
          }
          else
          {
            if (!xvt_dlg_create_res(WD_MODELESS,DLG_213, EM_ALL, DLG_213_eh, 0L))
              xvt_dm_post_error("Can't open dialog 213");
          };
        }
        else
          win120_create_edit_felder();
      }
      break;
    case ARMCO84:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_156, EM_ALL, DLG_156_eh, 0L))==NULL_WIN)
            xvt_dm_post_error("Can't open dialog 156");
          neue_datensatz_wahl = FALSE;
        }
      }
      break;
    case ARMCO71:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_157, EM_ALL, DLG_157_eh, 0L))==NULL_WIN)
            xvt_dm_post_error("Can't open dialog 157");
          neue_datensatz_wahl = FALSE;
        }
      }
      break;
    case BUHNE:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          if (!xvt_dlg_create_res(WD_MODELESS,DLG_337, EM_ALL, DLG_337_eh, 0L))
            xvt_dm_post_error("Can't open dialog 337");
          win120_create_edit_felder();
          neue_datensatz_wahl = FALSE;
        }
        else
          win120_create_edit_felder();
      }
      break;
    case NWRINNE:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_158, EM_ALL, DLG_158_eh, 0L))==NULL_WIN)
            xvt_dm_post_error("Can't open dialog 158");
          neue_datensatz_wahl = FALSE;
        }
      }
      break;
    case GAUSS:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_159, EM_ALL, DLG_159_eh, 0L))==NULL_WIN)
            xvt_dm_post_error("Can't open dialog 159");
          neue_datensatz_wahl = FALSE;
        }
      }
      break;
      
    case GELAENDE2:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_163, EM_ALL, DLG_163_eh, 0L))==NULL_WIN)
            xvt_dm_post_error("Can't open dialog 163");
          win120_create_edit_felder();
          for (int i=0;i<15;i++)  // Edit-Felder 3.Spalte disabled setzen
            if (win_120[30+i] !=NULL_WIN)
              xvt_vobj_set_enabled(win_120[30+i],FALSE);
            neue_datensatz_wahl = FALSE;
        }
        else  win120_create_edit_felder();
      }
      break;
      
    case FLAECHE:
      {
        if (neue_datensatz_wahl)
        {
          win120_loesche_edit_felder();
          if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_FLAECHE, EM_ALL, DLG_FLAECHE_eh, 0L))==NULL_WIN)
            xvt_dm_post_error("Can't open dialog 153");
          win120_create_edit_felder();
          for (int i=0;i<15;i++)    // Edit-Felder 3.Spalte disabled setzen
            if (win_120[30+i] !=NULL_WIN)
              xvt_vobj_set_enabled(win_120[30+i],FALSE);
            neue_datensatz_wahl = FALSE;
        }
        else  win120_create_edit_felder();
      }
      break;
      
    case BAUWERK:
      {
        win120_loesche_edit_felder();
        neue_datensatz_wahl = FALSE;
      }
      break;
    case LP_TEXT:
      {
        win120_loesche_edit_felder();
        neue_datensatz_wahl = FALSE;
      }
      break;
      //Neu Dick 23.09.99 neue Datensätze
    case SCHLEPPSPANNUNG:  
    case AUSUFERUNG_LINKS: 
    case AUSUFERUNG_RECHTS:
    case ENERGIEHOEHE:
      {
        win120_loesche_edit_felder();          
      }
      break;
    default:
      {
        win120_create_edit_felder();
      }
      break;
  }
}

/********  WIN_116: Profildaten in Edit-Felder:X-Y-Z ausgeben  ****************/
void display_prof116( WINDOW *win )
{
  char str[15];

  int win_id = 0;

  double lastX = scr.lastX;
  for( int i = 0;i < 5;i++ )   /* Daten in 1.Spalte: x  ausgeben  */
	{
    double x = scr.x[i];
    bool bRueck = lastX != BCE_NAN && x <= lastX;

    if( x == BCE_NAN )
      str[0] = '\0';
    else
      sprintf( str,"%.4lf", x );

    if( win[win_id] != NULL_WIN )
    {
      COLOR foreColor = bRueck ? COLOR_RED : COLOR_BLACK;

      XVT_COLOR_COMPONENT ctl_colors[] =
      {
        { XVT_COLOR_FOREGROUND, foreColor },
        { XVT_COLOR_BACKGROUND, COLOR_WHITE },
        { XVT_COLOR_NULL, 0 }
      };

      xvt_ctl_set_colors( win[win_id], (XVT_COLOR_COMPONENT*)ctl_colors, XVT_COLOR_ACTION_SET );
      xvt_vobj_set_title( win[win_id], str );
    }

    win_id++;
    lastX = x;
  } // for i

  win_id = 5;
  for (i=0;i < 5;i++)      /* Daten in 2.Spalte: y  ausgeben  */
	 {
    if (scr.y[i] == BCE_NAN)
      str[0]='\0';
    else
      sprintf(str,"%.4lf",scr.y[i]);
    //  gcvt(scr.y[i],10,str);
    if (win[win_id]!=NULL_WIN)
      xvt_vobj_set_title(win[win_id],str);
	  win_id++;
	 }
 /**** Ausgabe der Sonderprofile in Dialog 140-144  ****/
 if ((dlg_sonderprofil != NULL_WIN) &&(neue_datensatz_wahl))   // falls Dialog geöffnet erst schliessen
	  {
	  xvt_vobj_destroy(dlg_sonderprofil);
	  dlg_sonderprofil = NULL_WIN;
	  }
 char tmp1[251];
 if (neue_datensatz_wahl)
 xvt_vobj_set_title(xvt_win_get_ctl(WIN_116,WIN_GRAPH_116_EDIT_53)," ");//Dick 14.12.98 Init
 switch (typ[scr.datensatz])
	{
	 case SOHLHOEHE:     
	 case GELAENDEHOEHE:     
			  {
				if (neue_datensatz_wahl)
				  {
					win116_loesche_edit_felder();
					if(!((berechnen)&&(editieren)) && !Ber_edit_sperr)//Wenn Längsschnitt kein Fenster "Bereiche  Editieren"
					if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_110, EM_ALL, DLG_110_eh, 0L))==NULL_WIN)
						  xvt_dm_post_error( "Can't open dialog 110" );
					neue_datensatz_wahl = FALSE;
				  }
                //Dick 14.12.98
                if(berechnen&&editieren)
                if (list->ExtractInfo2String(scr.datensatz,tmp1))                    
                    xvt_vobj_set_title(xvt_win_get_ctl(WIN_116,WIN_GRAPH_116_EDIT_53),tmp1);
                //    
			  }
		break;
     case SOHLHOEHE_2:
         {
         if (neue_datensatz_wahl)
             {
             win116_loesche_edit_felder();
             win116_create_edit_felder();
             if(!((berechnen)&&(editieren)) && !Ber_edit_sperr)//Wenn Längsschnitt kein Fenster "Bereiche  Editieren"
                 if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_110, EM_ALL, DLG_110_eh, 0L))==NULL_WIN)
                     xvt_dm_post_error("Can't open dialog 110");
                 neue_datensatz_wahl = FALSE;
             }
         //Dick 14.12.98
         if(berechnen&&editieren)
             if (list->ExtractInfo2String(scr.datensatz,tmp1))                    
                 xvt_vobj_set_title(xvt_win_get_ctl(WIN_116,WIN_GRAPH_116_EDIT_53),tmp1);
             //    
         }
     case AXM:
     case AYM:
     case DPM:
         {
				if (neue_datensatz_wahl)
				  {
					win116_create_edit_felder();
					if(!((berechnen)&&(editieren)) && !Ber_edit_sperr)//Wenn Längsschnitt kein Fenster "Bereiche  Editieren"
					if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_110, EM_ALL, DLG_110_eh, 0L))==NULL_WIN)
						  xvt_dm_post_error("Can't open dialog 110");
					neue_datensatz_wahl = FALSE;
				  }
                else
                    win116_create_edit_felder();
			  }
		break;

     case TRENNFLAECHEN:
     case BORDVOLL:
       {
         if( neue_datensatz_wahl )
         {
           win116_loesche_edit_felder();
           if( ( dlg_sonderprofil = xvt_dlg_create_res( WD_MODELESS,DLG_143, EM_ALL, DLG_143_eh, typ[scr.datensatz] ) ) == NULL_WIN )
             xvt_dm_post_error( "Can't open dialog 143" );
           neue_datensatz_wahl = FALSE;
         }
       }
       break;
       
   case WASSERSPIEGEL:
   case WASSERSPIEGEL_2:
   case WSP_FIXIERUNG:
	 case WSP_DIFFERENZ: //Bley 8.11.2000
         {
           if (neue_datensatz_wahl)
           {
             win116_loesche_edit_felder();
             win116_create_edit_felder();
             neue_datensatz_wahl = FALSE;
             if (dlg_sonderprofil != NULL_WIN)
             {
               xvt_vobj_destroy(dlg_sonderprofil);
               dlg_sonderprofil = NULL_WIN;
             }
             if (list->ExtractInfo2String(scr.datensatz,tmp1))
             {
         xvt_vobj_set_title(xvt_win_get_ctl(WIN_116,WIN_GRAPH_116_EDIT_53),tmp1);
		 }

            }
		 else
		     win116_create_edit_felder();
		}
       break;
     case BOESCHUNG_LINKS:
     case BOESCHUNG_RECHTS:
     case BOESCHUNG_LINKS_2:
     case BOESCHUNG_RECHTS_2:
  	 case MODELLGRENZEN:
         {
         win116_create_edit_felder();
         //Dick 14.12.98
         if (dlg_sonderprofil != NULL_WIN)
             {
             xvt_vobj_destroy(dlg_sonderprofil);
             dlg_sonderprofil = NULL_WIN;
             }
         if (list->ExtractInfo2String(scr.datensatz,tmp1))                    
             xvt_vobj_set_title(xvt_win_get_ctl(WIN_116,WIN_GRAPH_116_EDIT_53),tmp1);
         // 
         }
	 break;
     case STATION:
		{
         char tmp1[251];
		 if (neue_datensatz_wahl)
		    {
			 win116_loesche_edit_felder();
		     win116_create_edit_felder();
			 neue_datensatz_wahl = FALSE;
		
		 if (dlg_sonderprofil != NULL_WIN)
		   {
			 xvt_vobj_destroy(dlg_sonderprofil);
			 dlg_sonderprofil = NULL_WIN;
		   }
		 if (list->ExtractStationsString(scr.datensatz,tmp1,STATION))
		 {
         xvt_vobj_set_title(xvt_win_get_ctl(WIN_116,WIN_GRAPH_116_EDIT_53),tmp1);
		 }

           }
		 else
		     win116_create_edit_felder();

		}
       break;
     case STATION_FIX://Dick 05.02.99
		{
         char tmp1[251];
		 if (neue_datensatz_wahl)
		    {
			 win116_loesche_edit_felder();
		     win116_create_edit_felder();
			 neue_datensatz_wahl = FALSE;
		
		 if (dlg_sonderprofil != NULL_WIN)
		   {
			 xvt_vobj_destroy(dlg_sonderprofil);
			 dlg_sonderprofil = NULL_WIN;
		   }
     if (list->ExtractStationsString(scr.datensatz,tmp1,STATION_FIX))
       xvt_vobj_set_title(xvt_win_get_ctl(WIN_116,WIN_GRAPH_116_EDIT_53),tmp1);

           }
		 else
		     win116_create_edit_felder();

		}
       break;
	 case RECHTSWERT:
	 case HOCHWERT:
		 {
				if (neue_datensatz_wahl)
				  {
					win116_loesche_edit_felder();
					win116_create_edit_felder();
					neue_datensatz_wahl=FALSE;
					}
				else
							 win116_create_edit_felder();


		 }
		 break;
	 case DURCHST_BEREICH:
			  {
				  if (neue_datensatz_wahl)
				  {
				   win116_loesche_edit_felder();
				   if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_140, EM_ALL, DLG_140_eh, 0L))==NULL_WIN)
					 xvt_dm_post_error("Can't open dialog 140");
					neue_datensatz_wahl = FALSE;
				  }
			  }
		break;
     case RAUHIGKEIT:
     case RAUHIGKEIT_KST:      //Dick 5.08.98
			  {
				if (neue_datensatz_wahl)
				  {
				
                    win116_create_edit_felder();
                    if(ds_info[1]!=0) //Dick 6.08.98
                      {
				       if (!xvt_dlg_create_res(WD_MODELESS,DLG_154, EM_ALL, DLG_154_eh, 0L))
						  xvt_dm_post_error("Can't open dialog 154");
                    
                        neue_datensatz_wahl = FALSE;
                        EVENT *xdEvent;
                        xdEvent= new EVENT;
                        xdEvent->type=E_MOUSE_DOWN;
                        xdEvent->v.mouse.button=0;
					    xvt_win_dispatch_event(WIN_117,xdEvent);
                        xdEvent->type=E_MOUSE_UP;
                        xvt_win_dispatch_event(WIN_117,xdEvent);
                        delete xdEvent;
                      }
					}
                else
                   win116_create_edit_felder();
			  }
		break;
	 case MAUL:
			 {
				if (neue_datensatz_wahl)
				  {
					win116_loesche_edit_felder();
					if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_141, EM_ALL, DLG_141_eh, 0L))==NULL_WIN)
						 xvt_dm_post_error("Can't open dialog 141");
					neue_datensatz_wahl = FALSE;
				  }
			 }
		break;
	 case EIPROFIL:
			 {
				if (neue_datensatz_wahl)
				  {
					win116_loesche_edit_felder();
					if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_141, EM_ALL, DLG_141_eh, 0L))==NULL_WIN)
						xvt_dm_post_error("Can't open dialog 141");
					neue_datensatz_wahl = FALSE;
				  }
			 }
		break;
	  case UK_BRUECKE:
			 {
				if (neue_datensatz_wahl)
				 {
				  win116_loesche_edit_felder();
				  if (!xvt_dlg_create_res(WD_MODELESS,DLG_212, EM_ALL, DLG_212_eh, 0L))
						  xvt_dm_post_error("Can't open dialog 212");
				  win116_create_edit_felder();
				  neue_datensatz_wahl = FALSE;
				 }
			  else
				  win116_create_edit_felder();
			 }
		break;

	 case OK_BRUECKE:
			 {
				if (neue_datensatz_wahl)
				 {
				  win116_loesche_edit_felder();

          if( LWA_PROJEKT )
          {
            if (!xvt_dlg_create_res(WD_MODELESS,DLG_107, EM_ALL, DLG_107_eh, 0L))
              xvt_dm_post_error("Can't open dialog 107");
          };

				  win116_create_edit_felder();
				  neue_datensatz_wahl = FALSE;
				 }
				else
				  win116_create_edit_felder();
			 }
		break;
	 case SCHUETZ:
			 {
				if (neue_datensatz_wahl)
				 {
				  win116_loesche_edit_felder();
				  schuetz_flag=TRUE;

          if( LWA_PROJEKT )
          {
				    if (!xvt_dlg_create_res(WD_MODELESS,DLG_107, EM_ALL, DLG_107_eh, 0L))
						  xvt_dm_post_error("Can't open dialog 107");
          };
				  neue_datensatz_wahl = FALSE;
				 }
			 }
		break;

	 case OK_WEHRS:
			 {
				if (neue_datensatz_wahl)
				 {
				  win116_loesche_edit_felder();

          if( LWA_PROJEKT )
            {
              if (!xvt_dlg_create_res(WD_MODELESS,DLG_160, EM_ALL, DLG_160_eh, 0L))
                xvt_dm_post_error("Can't open dialog 160");
            }
            else
            {
              if (!xvt_dlg_create_res(WD_MODELESS,DLG_213, EM_ALL, DLG_213_eh, 0L))
                xvt_dm_post_error("Can't open dialog 213");
            };

				  win116_create_edit_felder();
				  neue_datensatz_wahl = FALSE;
				 }
				else
				  win116_create_edit_felder();
			 }
			break;
	 case KREIS:
			 {
				if (neue_datensatz_wahl)
				  {
					win116_loesche_edit_felder();
					if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_144, EM_ALL, DLG_144_eh, 0L))==NULL_WIN)
						xvt_dm_post_error("Can't open dialog 144");
					neue_datensatz_wahl = FALSE;
				  }
			 }
		  break;
  	 case BUHNE:
			 {
				if (neue_datensatz_wahl)
				 {
				  win116_loesche_edit_felder();
				  if (!xvt_dlg_create_res(WD_MODELESS,DLG_337, EM_ALL, DLG_337_eh, 0L))
						  xvt_dm_post_error("Can't open dialog 337");
				  win116_create_edit_felder();
				  neue_datensatz_wahl = FALSE;
				 }
			  else
				  win116_create_edit_felder();
			 }
		  break;
	 case KREISSEGM:
			 {
				if (neue_datensatz_wahl)
				  {
					win116_loesche_edit_felder();
					if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_155, EM_ALL, DLG_155_eh, 0L))==NULL_WIN)
						xvt_dm_post_error("Can't open dialog 155");
					neue_datensatz_wahl = FALSE;
				  }
			 }
	 case ARMCO84:
			if (neue_datensatz_wahl)
			 {
				win116_loesche_edit_felder();
				if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_156, EM_ALL, DLG_156_eh, 0L))==NULL_WIN)
						  xvt_dm_post_error("Can't open dialog 156");
				neue_datensatz_wahl = FALSE;
			 }
	 case ARMCO71:
			if (neue_datensatz_wahl)
			 {
				win116_loesche_edit_felder();
				if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_157, EM_ALL, DLG_157_eh, 0L))==NULL_WIN)
						  xvt_dm_post_error("Can't open dialog 157");
				neue_datensatz_wahl = FALSE;
			 }
	 case NWRINNE:
			if (neue_datensatz_wahl)
			 {
				win116_loesche_edit_felder();
				if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_158, EM_ALL, DLG_158_eh, 0L))==NULL_WIN)
						  xvt_dm_post_error("Can't open dialog 158");
				neue_datensatz_wahl = FALSE;
			 }
	 case GAUSS:
			if (neue_datensatz_wahl)
			 {
				win116_loesche_edit_felder();
				if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_159, EM_ALL, DLG_159_eh, 0L))==NULL_WIN)
						  xvt_dm_post_error("Can't open dialog 159");
				neue_datensatz_wahl = FALSE;
			 }
	 case TRAPEZ:
			 {
				if (neue_datensatz_wahl)
				  {
					win116_loesche_edit_felder();
					if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_142, EM_ALL, DLG_142_eh, 0L))==NULL_WIN)
						 xvt_dm_post_error("Can't open dialog 142");
					neue_datensatz_wahl = FALSE;
				  }
			 }
		break;

	 case GELAENDE2:
			 {
				if (neue_datensatz_wahl)
				  {
					win116_loesche_edit_felder();
					if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_163, EM_ALL, DLG_163_eh, 0L))==NULL_WIN)
						  xvt_dm_post_error("Can't open dialog 163");
					win116_create_edit_felder();
				  for (int i=0;i<5;i++) // Edit Felder 3.Spalte disabled setzen
					  if (Edit_Win116[10+i] !=NULL_WIN)
						  xvt_vobj_set_enabled(Edit_Win116[10+i],FALSE);
				  neue_datensatz_wahl = FALSE;
				 }
				else win116_create_edit_felder();
			 }
			break;

   case FLAECHE:
			 {
				if (neue_datensatz_wahl)
				  {
					win116_loesche_edit_felder();
					if ((dlg_sonderprofil = xvt_dlg_create_res(WD_MODELESS,DLG_FLAECHE, EM_ALL, DLG_FLAECHE_eh, 0L))==NULL_WIN)
						  xvt_dm_post_error("Can't open dialog 153");
					win116_create_edit_felder();
					for (int i=0;i<5;i++) // Edit Felder 3.Spalte disabled setzen
					  if (Edit_Win116[10+i] !=NULL_WIN)
						 xvt_vobj_set_enabled(Edit_Win116[10+i],FALSE);
					neue_datensatz_wahl = FALSE;
				  }
				else win116_create_edit_felder();
			 }
		  break;
//#endif
     case BAUWERK:
		 {
			win116_loesche_edit_felder();
			neue_datensatz_wahl = FALSE;
		 }
		break;
     case LP_TEXT:
		 {
			win116_loesche_edit_felder();
			neue_datensatz_wahl = FALSE;
		 }
		break;
     //Neu Dick 23.09.99 neue Datensätze
     case SCHLEPPSPANNUNG:  
     case AUSUFERUNG_LINKS: 
     case AUSUFERUNG_RECHTS:
     case ENERGIEHOEHE:
         {
          win116_create_edit_felder();          
         }
	 break;
	 default:
		{
		 win116_create_edit_felder();
		}
	 break;
  }
}
/***************************************************************/
