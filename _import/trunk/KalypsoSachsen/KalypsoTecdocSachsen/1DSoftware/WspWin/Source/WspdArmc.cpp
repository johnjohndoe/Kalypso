/* WspDArmc.cpp */

#include <windows.h>
#include "xvt.h"

#include "wspwin.h"
#include "wsphilfe.h"

#include "..\..\wspdlg\include\export.h"

#include "armco.h"



WINDOW Dlg_Armco,listbox_armco;

extern WINDOW edit_dlg156[11], edit_dlg157[10];

extern XVT_HELP_INFO hi;

/* 
Information about the dialog

  #define DLG_RES_ID DLG_TEST
  #define DLG_FLAGS 0x0L
  #define DLG_CLASS ""
  #define DLG_MODE WD_MODELESS
*/


/*
Handler for dialog DLG_ARMCO ("Armco Dialog")
*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_ARMCO_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_ARMCO_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
  short xdControlId = xdEvent->v.ctl.id;
  
  switch (xdEvent->type) {
  case E_CREATE:		
    {
      char addlist[200];
      char armcodaten[100];
      int i,j;
      Dlg_Armco=xdWindow;
      
      listbox_armco = xvt_win_get_ctl (xdWindow,DLG_ARMCO_ARMCOLIST);
      xvt_list_suspend(listbox_armco);
      xvt_list_clear(listbox_armco);
      switch(xvt_vobj_get_data(xdWindow))
      {
      case 1L://MA
        {
          xvt_vobj_set_title(xvt_win_get_ctl (xdWindow,DLG_ARMCO_W1),"R3");
          xvt_vobj_set_title(xvt_win_get_ctl (xdWindow,DLG_ARMCO_W2),"W1");
          xvt_vobj_set_visible(xvt_win_get_ctl (xdWindow,DLG_ARMCO_W3),TRUE);
          for(i=0;i<50;i++)
          {
            strcpy(addlist," ");
            for(j=0;j<8;j++)
            {
              if(j==6)continue; 
              if(Armco.Maul.typ_MA[i][j] < 100.)
                if(Armco.Maul.typ_MA[i][j] < 10.)
                  sprintf(armcodaten,"   %-18.4lf",Armco.Maul.typ_MA[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"  %-18.4lf",Armco.Maul.typ_MA[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"%-18.4lf ",Armco.Maul.typ_MA[i][j]);//,"          ");
                strcat(addlist,armcodaten);
            }
            xvt_list_add(listbox_armco, i,addlist);
          }                   
        }
        break;
      case 2L://MB
        {
          xvt_vobj_set_title(xvt_win_get_ctl (xdWindow,DLG_ARMCO_W1),"R3");
          xvt_vobj_set_title(xvt_win_get_ctl (xdWindow,DLG_ARMCO_W2),"W1");
          xvt_vobj_set_visible(xvt_win_get_ctl (xdWindow,DLG_ARMCO_W3),TRUE);
          for(i=0;i<35;i++)
          {
            strcpy(addlist," ");
            for(j=0;j<8;j++)
            {
              if(j==6)continue; 
              if(Armco.Maul.typ_MB[i][j] < 100.)
                if(Armco.Maul.typ_MB[i][j] < 10.)
                  sprintf(armcodaten,"   %-18.4lf",Armco.Maul.typ_MB[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"  %-18.4lf",Armco.Maul.typ_MB[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"%-18.4lf ",Armco.Maul.typ_MB[i][j]);//,"          ");
                strcat(addlist,armcodaten);
            }
            xvt_list_add(listbox_armco, i,addlist);
          }                   
        }
        break;
      case 3L://WA
        {
          xvt_vobj_set_title(xvt_win_get_ctl (xdWindow,DLG_ARMCO_W1),"R3");
          xvt_vobj_set_title(xvt_win_get_ctl (xdWindow,DLG_ARMCO_W2),"W1");
          xvt_vobj_set_visible(xvt_win_get_ctl (xdWindow,DLG_ARMCO_W3),TRUE);
          for(i=0;i<35;i++)
          {
            strcpy(addlist," ");
            for(j=0;j<8;j++)
            {
              if(j==6)continue; 
              if(Armco.Maul.typ_WA[i][j] < 100.)
                if(Armco.Maul.typ_WA[i][j] < 10.)
                  sprintf(armcodaten,"   %-18.4lf",Armco.Maul.typ_WA[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"  %-18.4lf",Armco.Maul.typ_WA[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"%-18.4lf ",Armco.Maul.typ_WA[i][j]);//,"          ");
                strcat(addlist,armcodaten);
            }
            xvt_list_add(listbox_armco, i,addlist);
          }                   
        }
        break;
      case 4L://WB
        {
          xvt_vobj_set_title(xvt_win_get_ctl (xdWindow,DLG_ARMCO_W1),"R3");
          xvt_vobj_set_title(xvt_win_get_ctl (xdWindow,DLG_ARMCO_W2),"W1");
          xvt_vobj_set_visible(xvt_win_get_ctl (xdWindow,DLG_ARMCO_W3),TRUE);
          for(i=0;i<25;i++)
          {
            strcpy(addlist," ");
            for(j=0;j<8;j++)
            {
              if(j==6)continue; 
              if(Armco.Maul.typ_WB[i][j] < 100.)
                if(Armco.Maul.typ_WB[i][j] < 10.)
                  sprintf(armcodaten,"   %-18.4lf",Armco.Maul.typ_WB[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"  %-18.4lf",Armco.Maul.typ_WB[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"%-18.4lf ",Armco.Maul.typ_WB[i][j]);//,"          ");
                strcat(addlist,armcodaten);
            }
            xvt_list_add(listbox_armco, i,addlist);
          }                   
        }
        break;
      case 5L://EA
        {
          for(i=0;i<35;i++)
          {
            strcpy(addlist," ");
            for(j=0;j<6;j++)
            { 
              if(Armco.Ellipse.typ_EA[i][j] < 100.)
                if(Armco.Ellipse.typ_EA[i][j] < 10.)
                  sprintf(armcodaten,"   %-18.4lf",Armco.Ellipse.typ_EA[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"  %-18.4lf",Armco.Ellipse.typ_EA[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"%-18.4lf ",Armco.Ellipse.typ_EA[i][j]);//,"          ");
                strcat(addlist,armcodaten);
            }
            xvt_list_add(listbox_armco, i,addlist);
          }
          
        }
        break;
      case 6L://EB
        {
          for(i=0;i<35;i++)
          {
            strcpy(addlist," ");
            for(j=0;j<6;j++)
            { 
              if(Armco.Ellipse.typ_EB[i][j] < 100.)
                if(Armco.Ellipse.typ_EB[i][j] < 10.)
                  sprintf(armcodaten,"   %-18.4lf",Armco.Ellipse.typ_EB[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"  %-18.4lf",Armco.Ellipse.typ_EB[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"%-18.4lf ",Armco.Ellipse.typ_EB[i][j]);//,"          ");
                strcat(addlist,armcodaten);
            }
            xvt_list_add(listbox_armco, i,addlist);
          }                   
        }                  
        break;
      case 7L://SE
        {
          for(i=0;i<30;i++)
          {
            strcpy(addlist," ");
            for(j=0;j<6;j++)
            { 
              if(Armco.Ellipse.typ_SE[i][j] < 100.)
                if(Armco.Ellipse.typ_SE[i][j] < 10.)
                  sprintf(armcodaten,"   %-18.4lf",Armco.Ellipse.typ_SE[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"  %-18.4lf",Armco.Ellipse.typ_SE[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"%-18.4lf ",Armco.Ellipse.typ_SE[i][j]);//,"          ");
                strcat(addlist,armcodaten);
            }
            xvt_list_add(listbox_armco, i,addlist);
          }                   
        }                  
        break;
      case 8L://SB
        {
          for(i=0;i<30;i++)
          {
            strcpy(addlist," ");
            for(j=0;j<6;j++)
            { 
              if(Armco.Super.typ_SB[i][j] < 100.)
                if(Armco.Super.typ_SB[i][j] < 10.)
                  sprintf(armcodaten,"   %-18.4lf",Armco.Super.typ_SB[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"  %-18.4lf",Armco.Super.typ_SB[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"%-18.4lf ",Armco.Super.typ_SB[i][j]);//,"          ");
                strcat(addlist,armcodaten);
            }
            xvt_list_add(listbox_armco, i,addlist);
          }                   
        }
        break;
        
      case 9L://Amco71
        {
          xvt_vobj_set_title(xvt_win_get_ctl (xdWindow,DLG_ARMCO_R1),"RCO");
          xvt_vobj_set_title(xvt_win_get_ctl (xdWindow,DLG_ARMCO_R2),"R1CO");                   
          xvt_vobj_set_title(xvt_win_get_ctl (xdWindow,DLG_ARMCO_W1),"R2CO");
          xvt_vobj_set_title(xvt_win_get_ctl (xdWindow,DLG_ARMCO_W2),"BCO");
          
          for(i=0;i<58;i++)
          {
            strcpy(addlist," ");
            for(j=0;j<6;j++)
            {
              
              if(Armco.Maul.typ_73[i][j] < 100.)
                if(Armco.Maul.typ_73[i][j] < 10.)
                  sprintf(armcodaten,"   %-18.4lf",Armco.Maul.typ_73[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"  %-18.4lf",Armco.Maul.typ_73[i][j]);//,"         ");
                else
                  sprintf(armcodaten,"%-18.4lf ",Armco.Maul.typ_73[i][j]);//,"          ");
                strcat(addlist,armcodaten);
            }
            xvt_list_add(listbox_armco, i,addlist);
          }                   
        }
        break;
      default:
        break;
        
             }
             xvt_list_resume(listbox_armco);
             if(xvt_list_count_all(listbox_armco)>0)
               xvt_list_set_sel(listbox_armco,0,TRUE);
             
             if (hi!=NULL_HELP_INFO)
               xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_5_3_2_5, 0L);
    }
    break;
  case E_DESTROY:
		/*
    Dialog has been closed; last event sent to dialog.
    */
    //xdRemoveHelpAssoc( xdWindow );
    {
      
      Dlg_Armco=NULL;
    }
    break;
  case E_FOCUS:
    {
    /*
    Dialog has lost or gained focus.
      */
      if (xdEvent->v.active)  {
      /*
      Dialog has gained focus
        */
      } else {
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
      
      switch(xdControlId) {
      case DLG_ARMCO_OK: /* "Übernehmen" */
        {
          char selected[200];
          double werte[7];
          char uebergabe[7][15];
          int i;
          if(xvt_list_get_first_sel(listbox_armco,selected,200))
          {
            switch(xvt_vobj_get_data(xdWindow))
            {
            case 1L://MA                         
            case 2L://MB                        
            case 3L://WA                         
            case 4L://WB
              {
                sscanf(selected,"%lf%lf%lf%lf%lf%lf%lf",&werte[0],&werte[1],&werte[2],&werte[3],&werte[4],&werte[5],&werte[6]);
                for(i=0;i<7;i++)
                {
                  sprintf(uebergabe[i],"%.4lf",werte[i]);
                  
                }
                xvt_vobj_set_title(edit_dlg156[0],uebergabe[0]);
                xvt_vobj_set_title(edit_dlg156[1],uebergabe[1]);
                xvt_vobj_set_title(edit_dlg156[5],uebergabe[2]);
                xvt_vobj_set_title(edit_dlg156[6],uebergabe[3]);
                xvt_vobj_set_title(edit_dlg156[7],uebergabe[4]);
                xvt_vobj_set_title(edit_dlg156[8],uebergabe[5]);
                xvt_vobj_set_title(edit_dlg156[9],uebergabe[6]);                         
              }
              break;
            case 5L://EA                  
            case 6L://EB
            case 7L://SE              
            case 8L://SB
              {
                sscanf(selected,"%lf%lf%lf%lf%lf%lf",&werte[0],&werte[1],&werte[2],&werte[3],&werte[4],&werte[5]);
                for(i=0;i<6;i++)
                {
                  sprintf(uebergabe[i],"%.4lf",werte[i]);
                  
                }
                xvt_vobj_set_title(edit_dlg156[0],uebergabe[0]);
                xvt_vobj_set_title(edit_dlg156[1],uebergabe[1]);
                xvt_vobj_set_title(edit_dlg156[5],uebergabe[2]);
                xvt_vobj_set_title(edit_dlg156[6],uebergabe[3]);
                xvt_vobj_set_title(edit_dlg156[8],uebergabe[4]);
                xvt_vobj_set_title(edit_dlg156[9],uebergabe[5]);
              }
              break;
              
            case 9L://Amco71
              {
                sscanf(selected,"%lf%lf%lf%lf%lf%lf",&werte[0],&werte[1],&werte[2],&werte[3],&werte[4],&werte[5]);
                for(i=0;i<6;i++)
                {
                  sprintf(uebergabe[i],"%.4lf",werte[i]);
                  
                }
                xvt_vobj_set_title(edit_dlg157[0],uebergabe[0]);
                xvt_vobj_set_title(edit_dlg157[1],uebergabe[1]);
                xvt_vobj_set_title(edit_dlg157[5],uebergabe[2]);
                xvt_vobj_set_title(edit_dlg157[6],uebergabe[3]);
                xvt_vobj_set_title(edit_dlg157[7],uebergabe[4]);
                xvt_vobj_set_title(edit_dlg157[8],uebergabe[5]);
              }
              break;
            default:
              break;
              
            }
          } 
        }
        break;
      case IDCANCEL:
        {
          xvt_vobj_destroy(xdWindow);
        }
        break;
      case DLG_ARMCO_ARMCOLIST: /* "List Box 4" */
        {
          /* List box was operated.	*/
          
          if (xdEvent->v.ctl.ci.v.lbox.dbl_click)
          { 	/*	double click*/
            
          }
          else
          { 		/* 	single click	*/
          }
        }
        break;
      default:
        break;
      }
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
      switch (xdEvent->v.user.id) {
      case -1:
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
