/****************************************************************************
*              Dialog 142 : Anzeige Trapezprofil                            *
*                  14.12.1994                                               *
****************************************************************************/
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"
#include "paint.h"

#include "typen.h"

#include "global.h"//Dick 8.12.98
#include "bce_allg.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_142
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL

extern Scroller scr;
extern int typ[TYPE_SIZE];//Dick 8.12.98
extern BOOLEAN sichere_datenblock;
extern BOOLEAN SaveProfilFile;
extern WINDOW dlg_sonderprofil, WIN_117;
extern int fehler;
extern MinMax pmm;
extern List *list;
extern Paint *paint;
WINDOW edit_dlg142[6];
char daten142[6][15];

/*************   GHJ   *************/
static WNDPROC defWndProc;
LRESULT CALLBACK Dlg142WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_HELP:
		{
			LPHELPINFO lphi = (LPHELPINFO)lParam;
			if (hi!=NULL_HELP_INFO)
			{
				xvt_help_display_topic(hi, HID_KAPITEL_5_3_1);
			}
		}
		break;

	default:
		break;
	}
	return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/

/*	Handler for dialog DLG_142 ("Kenndaten Trapezprofil") */
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_142_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_142_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	int i;
	short xdControlId = xdEvent->v.ctl.id;

	switch (xdEvent->type) {
	case E_CREATE:
		{
         /*************   GHJ   *************/
		 if (WIN_116!=NULL_WIN)
			SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(WIN_116, ATTR_NATIVE_WINDOW));
		 else if (WIN120!=NULL_WIN)
			SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(WIN120, ATTR_NATIVE_WINDOW));
		 defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
		 SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg142WindowProc);
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
		 char str[25];
		 dlg_sonderprofil = xdWindow;
		 edit_dlg142[0]=xvt_win_get_ctl(xdWindow,DLG_142_EDIT_7);
		 edit_dlg142[1]=xvt_win_get_ctl(xdWindow,DLG_142_EDIT_8);
		 edit_dlg142[2]=xvt_win_get_ctl(xdWindow,DLG_142_EDIT_9);
		 edit_dlg142[3]=xvt_win_get_ctl(xdWindow,DLG_142_EDIT_10);
		 edit_dlg142[4]=xvt_win_get_ctl(xdWindow,DLG_142_EDIT_11);
		 edit_dlg142[5]=xvt_win_get_ctl(xdWindow,DLG_142_EDIT_12);
		 for (int i=0;i<6;i++)
			 {
			  if (scr.z[i] == BCE_NAN)
				  str[0]='\0';
			  else
				  sprintf(str,"%.4lf",scr.z[i]);//gcvt(scr.z[i],6,str);
			  xvt_vobj_set_title(edit_dlg142[i],str);
			  strcpy(daten142[i],str);
			 }

		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_5_3_1, 0L);
		}
		break;
	case E_DESTROY:
		{
          char buf[200],buf2[200],buf3[200],buf4[200];
          xvt_res_get_str(STR_JA,buf,sizeof(buf));
          xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
          xvt_res_get_str(STR_ABBRECHEN,buf3,sizeof(buf3));
          xvt_res_get_str(STR_SICHERE_DATENBLOCK,buf4,sizeof(buf4));
			 if (sichere_datenblock)
                 switch (xvt_dm_post_ask(buf,buf2,buf3,"%s",buf4))
				//switch (xvt_dm_post_ask("Ja","Nein","Abbrechen","Daten wurden geändert !\nSpeichern vor Schließen\ndes Fensters ?"))
				 {
				  case RESP_DEFAULT:       //SICHERN
						 {
						  scr.z[0]= atof(daten142[0]);
						  scr.z[1]= atof(daten142[1]);
						  scr.z[2]= atof(daten142[2]);
						  scr.z[3]= atof(daten142[3]);
						  scr.z[4]= atof(daten142[4]);
						  scr.z[5]= atof(daten142[5]);
						  SaveProfilFile =TRUE;
						  list->SaveSonderprofildaten(&scr, TRAPEZ);
						  sichere_datenblock=FALSE;
						 }
						  break;
				  case RESP_2:             // nicht sichere_datenblock
						  sichere_datenblock=FALSE;
						  break;             //zurück
				  case RESP_3:break;
				 }
			 dlg_sonderprofil = NULL_WIN;
		}
		break;
	case E_CLOSE:
		{
		dlg_sonderprofil = NULL_WIN;
		xvt_vobj_destroy(xdWindow);
		}
		break;
	case E_CONTROL:
		{
		switch(xdControlId) {
		case DLG_142_EDIT_7: /* "Edit Control 7" */
			{ 	/*		Edit control was operated.		*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				 {
					/*
						focus has entered the control
					*/
				 }
				else
				 {	/*		focus has left the control	*/
					 fehler = is_zahl(daten142[0]);
					 if (fehler ==0)
						{
                          char buf[200];//Dick 26.11.99
                          xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                          xvt_dm_post_error("%s",buf);
						 //xvt_dm_post_error("Fehler in der Eingabe !");
						 xvt_vobj_set_title(edit_dlg142[0],"\0");
						 xvt_scr_set_focus_vobj(edit_dlg142[0]);
						}
					 else
						{
						 fehler=1;
						 if(daten142[0][0]!='\0')
						  {
							scr.z[0]= atof(daten142[0]);
							list->SaveSonderprofildaten(&scr, TRAPEZ);
							sichere_datenblock=FALSE;
						  }
						 for(i=0;i<6;i++)
						  {
							if((scr.z[i]==BCE_NAN) || (daten142[i]==0))//'\0'))
							 fehler=0;
						  }
						 if(fehler==1)
						  {
							if (WIN_117 != NULL_WIN)
							 {
							  xvt_dwin_invalidate_rect(WIN_117,0);
							  paint->draw_trapez(WIN_117);
							}
						  }
						}
				 }
			  }
			else
			  {
				/*
					Contents of control were changed
				*/
				xvt_vobj_get_title(edit_dlg142[0],daten142[0],9);
				sichere_datenblock = TRUE;
				SaveProfilFile =TRUE;
			  }
			}
			break;
		case DLG_142_EDIT_8: /* "Edit Control 8" */
			{
			/*
				Edit control was operated.
			*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				 {
					/*
						focus has entered the control
					*/
				 }
				else
				 {	/*		focus has left the control	*/
					 fehler = is_zahl(daten142[1]);
					 if (fehler ==0)
						{
                          char buf[200];//Dick 26.11.99
                          xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                          xvt_dm_post_error("%s",buf);
						 //xvt_dm_post_error("Fehler in der Eingabe !");
						 xvt_vobj_set_title(edit_dlg142[1],"\0");
						 xvt_scr_set_focus_vobj(edit_dlg142[1]);
						}
					 else
						{
						 fehler=1;
						 if(daten142[1][0]!='\0')
						  {
							scr.z[1]= atof(daten142[1]);
							list->SaveSonderprofildaten(&scr, TRAPEZ);
							sichere_datenblock=FALSE;
						  }
						 for(i=0;i<6;i++)
						  {
							if((scr.z[i]==BCE_NAN) || (daten142[i]==0))//'\0'))
							 fehler=0;
						  }
						 if(fehler==1)
						  {
							if (WIN_117 != NULL_WIN)
							 {
							  xvt_dwin_invalidate_rect(WIN_117,0);
							  paint->draw_trapez(WIN_117);
							}
						  }
						} //else fehler=1
				 } //else focus left
			  } //focus changed
			 else {
				/*
					Contents of control were changed
				*/
				xvt_vobj_get_title(edit_dlg142[1],daten142[1],9);
				sichere_datenblock = TRUE;
				SaveProfilFile =TRUE;
			}
			}
			break;
		case DLG_142_EDIT_9: /* "Edit Control 9" */
			{
			/*
				Edit control was operated.
			*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change) {
				if (xdEvent->v.ctl.ci.v.edit.active) {
					/*
						focus has entered the control
					*/
				}
				else
				 {	/*		focus has left the control	*/
					 fehler = is_zahl(daten142[2]);
					 if (fehler ==0)
						{
                          char buf[200];//Dick 26.11.99
                          xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                          xvt_dm_post_error("%s",buf);
						 xvt_dm_post_error("Fehler in der Eingabe !");
						 xvt_vobj_set_title(edit_dlg142[2],"\0");
						 xvt_scr_set_focus_vobj(edit_dlg142[2]);
						}
					 else
						{
						 fehler=1;
						 if(daten142[2][0]!='\0')
						  {
							scr.z[2]= atof(daten142[2]);
							list->SaveSonderprofildaten(&scr, TRAPEZ);
							sichere_datenblock=FALSE;
						  }
						 for(i=0;i<6;i++)
						  {
							if((scr.z[i]==BCE_NAN) || (daten142[i]==0))//'\0'))
							 fehler=0;
						  }
						 if(fehler==1)
						  {
							if (WIN_117 != NULL_WIN)
							 {
							  xvt_dwin_invalidate_rect(WIN_117,0);
							  paint->draw_trapez(WIN_117);
							}
						  }
						}
				 }
			  }
			 else {
				/*
					Contents of control were changed
				*/
				xvt_vobj_get_title(edit_dlg142[2],daten142[2],9);
				sichere_datenblock = TRUE;
				SaveProfilFile =TRUE;
			}
			}
			break;
		case DLG_142_EDIT_10: /* "Edit Control 10" */
			{
			/*
				Edit control was operated.
			*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change) {
				if (xdEvent->v.ctl.ci.v.edit.active) {
					/*
						focus has entered the control
					*/
				}
				else
				  {	/*	focus has left the control		*/
					 fehler = is_zahl(daten142[3]);
					 if (fehler ==0)
						{
                          char buf[200];//Dick 26.11.99
                          xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                          xvt_dm_post_error("%s",buf);
						 //xvt_dm_post_error("Fehler in der Eingabe !");
						 xvt_vobj_set_title(edit_dlg142[3],"\0");
						 xvt_scr_set_focus_vobj(edit_dlg142[3]);
						}
					 else
						{
						 fehler=1;
						 if(daten142[3][0]!='\0')
						  {
							scr.z[3]= atof(daten142[3]);
							list->SaveSonderprofildaten(&scr, TRAPEZ);
							sichere_datenblock=FALSE;
						  }
						 for(i=0;i<6;i++)
						  {
							if((scr.z[i]==BCE_NAN) || (daten142[i]==0))//'\0'))
							 fehler=0;
						  }
						 if(fehler==1)
						  {
							if (WIN_117 != NULL_WIN)
							 {
							  xvt_dwin_invalidate_rect(WIN_117,0);
							  paint->draw_trapez(WIN_117);
							}
						  }
						}
				 }
			  }
	 else {
				/*
					Contents of control were changed
				*/
				xvt_vobj_get_title(edit_dlg142[3],daten142[3],9);
				sichere_datenblock = TRUE;
				SaveProfilFile =TRUE;
			}
			}
			break;
		case DLG_142_EDIT_11: /* "Edit Control 11" */
			{
			/*
				Edit control was operated.
			*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change) {
				if (xdEvent->v.ctl.ci.v.edit.active) {
					/*
						focus has entered the control
					*/
				}
				else
				  {/*		focus has left the control		*/
					 fehler = is_zahl(daten142[4]);
					 if (fehler ==0)
						{
                         char buf[200];//Dick 26.11.99
                         xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                         xvt_dm_post_error("%s",buf); 
						 xvt_dm_post_error("Fehler in der Eingabe !");
						 xvt_vobj_set_title(edit_dlg142[4],"\0");
						 xvt_scr_set_focus_vobj(edit_dlg142[4]);
						}
					 else
						{
						 fehler=1;
						 if(daten142[4][0]!='\0')
						  {
							scr.z[4]= atof(daten142[4]);
							list->SaveSonderprofildaten(&scr, TRAPEZ);
							sichere_datenblock=FALSE;
						  }
						 for(i=0;i<6;i++)
						  {
							if((scr.z[i]==BCE_NAN) || (daten142[i]==0))//'\0'))
							 fehler=0;
						  }
						 if(fehler==1)
						  {
							if (WIN_117 != NULL_WIN)
							 {
							  xvt_dwin_invalidate_rect(WIN_117,0);
							  paint->draw_trapez(WIN_117);
							}
						  }
						}
				 }
			  }
			 else {
				/*
					Contents of control were changed
				*/
				xvt_vobj_get_title(edit_dlg142[4],daten142[4],9);
				sichere_datenblock = TRUE;
				SaveProfilFile =TRUE;
			}
			}
			break;
		case DLG_142_EDIT_12: /* "Edit Control 12" */
			{
			/*
				Edit control was operated.
			*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change) {
				if (xdEvent->v.ctl.ci.v.edit.active) {
					/*
						focus has entered the control
					*/
				}
				else
				  {/*	focus has left the control		*/
					 fehler = is_zahl(daten142[5]);
					 if (fehler ==0)
						{
                          char buf[200];//Dick 26.11.99
                          xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                          xvt_dm_post_error("%s",buf);
						 //xvt_dm_post_error("Fehler in der Eingabe !");
						 xvt_vobj_set_title(edit_dlg142[5],"\0");
						 xvt_scr_set_focus_vobj(edit_dlg142[5]);
						}
					 else
						{
						 fehler=1;
						 if(daten142[5][0]!='\0')
						  {
							scr.z[5]= atof(daten142[5]);
							list->SaveSonderprofildaten(&scr, TRAPEZ);
							sichere_datenblock=FALSE;
						  }
						 for(i=0;i<6;i++)
						  {
							if((scr.z[i]==BCE_NAN) || (daten142[i]==0))//'\0'))
							 fehler=0;
						  }
						 if(fehler==1)
						  {
							if (WIN_117 != NULL_WIN)
							 {
							  xvt_dwin_invalidate_rect(WIN_117,0);
							  paint->draw_trapez(WIN_117);
							}
						  }
						}
				 }
			  }
			 else {
				/*
					Contents of control were changed
				*/
				xvt_vobj_get_title(edit_dlg142[5],daten142[5],9);
				sichere_datenblock = TRUE;
				SaveProfilFile =TRUE;
			}
			}
			break;
		case DLG_142_PUSHBUTTON_13: /* "OK" */
			{
			 int help;
			 fehler=1;
			 for(i=0;i<=5;i++)
			  {
				help=is_zahl(daten142[i]);
				if(help==0) fehler=0;
				scr.z[i]= atof(daten142[i]);

				if((scr.z[i]==BCE_NAN) || (daten142[i][0]=='\0'))
				 fehler=0;
			  }
			  if(fehler==1)
				 {
				list->SaveSonderprofildaten(&scr, TRAPEZ);

				if (sichere_datenblock)
				  SaveProfilFile =TRUE;
				sichere_datenblock=FALSE;
				dlg_sonderprofil = NULL_WIN;
				if (WIN_117 != NULL_WIN)
				 {
					xvt_dwin_invalidate_rect(WIN_117,0);
					paint->draw_trapez(WIN_117);
				 }
				xvt_vobj_destroy(xdWindow);
			  }
			  else
                  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
				//xvt_dm_post_note("Fehler in der Dateneingabe");
                  }
			}
			break;
		case DLG_142_PUSHBUTTON_14: /* "Abbruch" */
			{
              char buf[200],buf2[200],buf3[200],buf4[200];
              xvt_res_get_str(STR_JA,buf,sizeof(buf));
              xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
              xvt_res_get_str(STR_ABBRECHEN,buf3,sizeof(buf3));
              xvt_res_get_str(STR_SICHERE_DATENBLOCK,buf4,sizeof(buf4));
			 if (sichere_datenblock)
			  {
                  switch (xvt_dm_post_ask(buf,buf2,buf3,"%s",buf4))
				//switch (xvt_dm_post_ask("Ja","Nein","Abbrechen","Daten wurden geändert !\nSpeichern vor Schließen\ndes Fensters ?"))
				 {
				  case RESP_DEFAULT:       //SICHERN
					{
					 int help;
					 fehler=1;
					 for(i=0;i<=5;i++)
					  {
						help=is_zahl(daten142[i]);
						if(help==0) fehler=0;
						scr.z[i]= atof(daten142[i]);

						if((scr.z[i]==BCE_NAN) || (daten142[i][0]=='\0'))
						 fehler=0;
					  }
					if(fehler==1)
					 {
					  list->SaveSonderprofildaten(&scr, TRAPEZ);
					  if (sichere_datenblock)
						SaveProfilFile =TRUE;
					  sichere_datenblock=FALSE;
					  dlg_sonderprofil = NULL_WIN;
					  if (WIN_117 != NULL_WIN)
						{
						 xvt_dwin_invalidate_rect(WIN_117,0);
						 paint->draw_trapez(WIN_117);
						}
					  xvt_vobj_destroy(xdWindow);
					 }
				  else
                      {
                        //Dick 26.11.99
                        xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                        xvt_dm_post_error("%s",buf);
					   //xvt_dm_post_note("Fehler in der Dateneingabe");
                      }
				 }
				break;
				case RESP_2:             // nicht sichere_datenblock
				 {
				  sichere_datenblock=FALSE;
				  dlg_sonderprofil = NULL_WIN;
				  xvt_vobj_destroy(xdWindow);
				 }
				break;             //zurück
				case RESP_3:
				 {
				  dlg_sonderprofil = NULL_WIN;
				  xvt_vobj_destroy(xdWindow);
				 }
				break;
			  } //switch
			 } //if(sichere_datenblock)
			else
			 {
			  dlg_sonderprofil = NULL_WIN;
			  xvt_vobj_destroy(xdWindow);
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
