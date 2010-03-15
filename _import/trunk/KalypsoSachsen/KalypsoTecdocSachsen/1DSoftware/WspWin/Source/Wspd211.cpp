/*
	Handler for dialog DLG_211 ("Strang in verzweigtem System")
*/
/*
  Der Dialog setzt die folgenden Variablen:
    BOOL am_anfang_einfuegen:   Checkbox 'Am Anfang einfügen'
    BOOL als_naechstesr_anfang: Checkbox 'Endprofil=Anfangsprofil des neuen Strangs'
    char[15] strang_start:      Anfangsstation des selektierten Strangs
    char[15] strang_end:        Endprofil des selektierten Strangs
    BOOL abbruch208:            falls 'Abbrechen' gedrückt wurde
*/

#include <windows.h>
#include "xvt.h"
#include "wspwin.h"

#include "global_types.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"
#include "bce_allg.h"
#include "util.h"

#include "global.h"
#include "strang2.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_211
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL

extern SLIST strang_slist;
extern char station208[20];
extern BOOLEAN abbruch208;
char strang_start[15],
     strang_end[15];    //externe Variablen
WINDOW listbox211,
       edit211,
       checkbox211;
BOOLEAN am_anfang_einfuegen,
		    als_naechster_anfang,
        slashnull;


long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_211_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_211_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;

	char *inhaltslist;
	inhaltslist = new char[200];
	switch (xdEvent->type)
  {
  case E_CREATE:
		/*
			Dialog has been created; first event sent to newly-created
			dialog.
		*/
		{
		 am_anfang_einfuegen=FALSE;
		 xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_211_EDIT_7)),FALSE);
		 xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_211_EDIT_8)),FALSE);
		 xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_211_EDIT_9)),FALSE);
		 xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_211_EDIT_10)),FALSE);
		 xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_211_EDIT_11)),FALSE);
		 abbruch208=FALSE;

		 edit211=xvt_win_get_ctl(xdWindow,DLG_211_EDIT_20);
		 xvt_vobj_set_title(edit211,station208);

		 checkbox211 = xvt_win_get_ctl(xdWindow, DLG_211_CHECKBOX_19);
		 xvt_ctl_set_checked(checkbox211, 1);
		 als_naechster_anfang=TRUE;
		 strang_in_slist(); //IN STRANG2.cpp
		 int i=xvt_slist_count(strang_slist);
		 if (i==0)
     {
       xvt_vobj_destroy(xdWindow);
       am_anfang_einfuegen=TRUE;
     }
		 else
		 {
       listbox211 = xvt_win_get_ctl (xdWindow,DLG_211_LBOX_5);
       NewSetFontFunc(listbox211);    //Schriftart für listbox211 ändern
       zeige_slist(strang_slist);
       xvt_list_add(listbox211, 0, (char*)strang_slist);
       xvt_list_set_sel(listbox211,i-1,TRUE);
       xvt_list_get_elt(listbox211,i-1,inhaltslist,180);
       if(strlen(inhaltslist)>1)
       {
         slashnull=FALSE;
         int j=0;
         for(i=0;i<=10;i++)
         {
           if(inhaltslist[i]=='\0')
             slashnull=TRUE;
           if((inhaltslist[i]!=' ') && (!slashnull))
           {
             strang_start[j]=inhaltslist[i];
             j++;
           }
         }
         strang_start[j]='\0';
         
         if(strlen(inhaltslist)>11)
         {
           j=0;
           slashnull=FALSE;
           
           for (i=11;i<=20;i++)
           {
             if(inhaltslist[i]=='\0')
               slashnull=TRUE;
             if((inhaltslist[i]!=' ') && (!slashnull))
             {
               strang_end[j]=inhaltslist[i];
               j++;
             }
           }
           strang_end[j]='\0';
         } //>11
         else
           strang_end[0]='\0';
       } //inhaltslist>1
       am_anfang_einfuegen=FALSE;
     } //SLIST >0
     if (hi!=NULL_HELP_INFO)
       xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_5_5_1, 0L);
     ChangeFontAndSize((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));	// GHJ
     xvt_vobj_set_visible(xdWindow, TRUE);		// GHJ
    } // E_CREATE
    break;

  case E_DESTROY:
		/*
    Dialog has been closed; last event sent to dialog.
    */
    {
      if(strang_slist!=NULL)
      {
        xvt_slist_destroy(strang_slist);
        strang_slist=NULL;
      }
    }
    break;

  case E_FOCUS:
    {
    /*
    Dialog has lost or gained focus.
      */
      if (xdEvent->v.active)  
      {
      /*
        Dialog has gained focus
      */
      } else 
      {
      /*
        Dialog has lost focus
      */
      }
    }
    break;

  case E_SIZE:
		/*
      Size of dialog has been set or changed; sent when dialog is
      created or subsequently resized by xvt_vobj_move.
    */
    {
    }
    break;

  case E_CLOSE:
		/*
      Request to close dialog; user operated "close" menu item on
      dialog system menu, or operated "close" control on dialog
      frame. Dialog not closed unless xvt_vobj_destroy is called.
    */
    {
      xvt_vobj_destroy(xdWindow);
    }
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
      case DLG_211_LBOX_5: /* "List Box 5" */
        {
        /*
          List box was operated.
        */
          int i=xvt_list_get_sel_index(listbox211);
          xvt_list_get_elt(listbox211,i,inhaltslist,190);
          if(strlen(inhaltslist)>1)
          {
            slashnull=FALSE;
            int j=0;
            for(i=0;i<=10;i++)
            {
              if(inhaltslist[i]=='\0')
                slashnull=TRUE;
              if((inhaltslist[i]!=' ') && (!slashnull))
              {
                strang_start[j]=inhaltslist[i];
                j++;
              }
            }
            strang_start[j]='\0';
            if(strlen(inhaltslist)>11)
            {
              j=0;
              slashnull=FALSE;
              for (i=11;i<=20;i++)
              {
                if(inhaltslist[i]=='\0')
                  slashnull=TRUE;
                if((inhaltslist[i]!=' ') && (!slashnull))
                {
                  strang_end[j]=inhaltslist[i];
                  j++;
                }
              }
              strang_end[j]='\0';
            } //>11
            else
              strang_end[0]='\0';
          } //inhaltslist>1
          
          
          if (xdEvent->v.ctl.ci.v.lbox.dbl_click) 
          {
          /*
            double click
          */
          } else 
          {
          /*
            single click
          */
          }
        }
        break;

      case DLG_211_CHECKBOX_6: /* "Am Anfang einfuegen" */
        {
          WINDOW ctl_win = xvt_win_get_ctl(xdWindow, DLG_211_CHECKBOX_6);
          xvt_ctl_set_checked(ctl_win, !xvt_ctl_is_checked (ctl_win));
          int i=xvt_ctl_is_checked (ctl_win);
          if (i==1)
            am_anfang_einfuegen=TRUE;
          else
            am_anfang_einfuegen=FALSE;
        }
        break;

      case DLG_211_PUSHBUTTON_17: /* "OK" */
        {
          xvt_vobj_destroy(xdWindow);
        }
        break;

      case DLG_211_PUSHBUTTON_18: /*Abbruch*/
        {
          abbruch208=TRUE;
          xvt_vobj_destroy(xdWindow);
        }
        break;

      case DLG_211_CHECKBOX_19: /* "Endprofil=Anfangsprofil des nächsten Stranges" */
        {
          xvt_ctl_set_checked(checkbox211, !xvt_ctl_is_checked (checkbox211));
          int test =xvt_ctl_is_checked (checkbox211);
          if (test==1)
            als_naechster_anfang=TRUE;
          else
            als_naechster_anfang=FALSE;
        }
        break;

      case DLG_211_EDIT_20:		
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
            {
            /*
              focus has left the control
            */
            }
          } 
          else 
          {
          /*
          Contents of control were changed
          */
          }
        }
        break;
        
      default:
        break;
      } // switch(xdControlId) 
    }
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
  }
  delete[]inhaltslist;
  return 0L;
}
