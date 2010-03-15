/* wspw131.cpp: Dialog Linitentyp auswählen */

#include <windows.h>
#include "xvt.h"

#include "wsphilfe.h"
#include "resource.h"
#include "..\..\wspdlg\include\export.h"

#include "global_vars.h"

#include "plot.h"

/* // für xvt-design
#define WIN_RES_ID LINES
#define WIN_FLAGS 0x800L
#define WIN_CLASS ""
#define WIN_BORDER W_PLAIN
*/

XVT_IMAGE  image, new_image;
short width, height;
int ncolors;
RCT src_rect, dst_rect;
CURSOR old_cursor;
static WNDPROC defWndProc;

extern WINDOW WIN_LINES;
extern double res_factor;
extern int linestyle;
extern  XVT_HELP_INFO hi;

/*************   GHJ   *************/
LRESULT CALLBACK Win131WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
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

/*	Handler for window LINES ("Linientyp auswählen aus WIN130")*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
LINES_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
LINES_eh XVT_CALLCONV2 (xdWindow, xdEvent)
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
      SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Win131WindowProc);
      /***********************************/
      
      WIN_LINES = xdWindow;
      
      /* Bildschirm anpassen*/
      RCT rct_win;
      xvt_vobj_get_outer_rect(xdWindow,&rct_win);
      
      char tmp[265];
      strcpy(tmp,start_dir);
      strcat(tmp, "LINES.BMP");
      image = xvt_image_read_bmp(tmp);
      
      if (image == NULL_IMAGE)
      {
        char buf[200];//Dick 26.11.99
        xvt_res_get_str(STR_DEMO_NOTE_3,buf,sizeof(buf));
        xvt_dm_post_error("IMAGEDATEI:%s %s",tmp,buf); //"IMAGEDATEI:%s konnte nicht gelesen werden!"
      }
      else
      {
        xvt_image_get_dimensions(image,&width, &height);
        new_image = xvt_image_create(xvt_image_get_format(image), width, height, NULL);
        
        ncolors = xvt_image_get_ncolors(image);
        xvt_image_set_ncolors(new_image,ncolors);
        
        xvt_image_get_dimensions(new_image, &width, &height);
        xvt_rect_set(&src_rect, 0, 0, width, height);
        xvt_rect_set(&dst_rect, 0, 0, width, height);
        //Dick 10.03.99
        rct_win.right=width+rct_win.left;
        rct_win.bottom=height+rct_win.top;
        xvt_vobj_move(xdWindow,&rct_win);
        //
        xvt_image_transfer(new_image, image, &dst_rect, &src_rect);
        
        xvt_rect_set(&dst_rect, 0, 0, (int)(res_factor*width), (int)(res_factor*height));	// GHJ
        xvt_dwin_draw_image(xdWindow, new_image, &dst_rect, &src_rect); //5-64
        
        mark_line(xdWindow,linestyle);  //->plot.cpp
      }
    }
    break;
  case E_DESTROY:
    /*	Window has been closed; last event sent to window.	*/
    {
      if (image != NULL_IMAGE)
        xvt_image_destroy(image);
      WIN_LINES = NULL_WIN;
    }
    return 0L;
  case E_FOCUS:
    {
    /*
    Window has lost or gained focus.
      */
      if (xdEvent->v.active)
      {	/*	Window has gained focus	*/
        old_cursor=xvt_win_get_cursor(xdWindow);
        xvt_win_set_cursor(xdWindow,CURSOR_BEAM);
      }
      else
      {	/*	Window has lost focus	*/
        xvt_win_set_cursor(xdWindow,old_cursor);
      }
    }
    break;
  case E_SIZE:
		/*
    Size of window has been set or changed; sent when window is
    created or subsequently resized by user or via xvt_vobj_move.
    */
    {
    }
    break;
  case E_UPDATE:
		/*
    Window requires updating.
    */
    {
      xvt_dwin_clear(xdWindow, (COLOR)xvt_vobj_get_attr(xdWindow, ATTR_BACK_COLOR));
      if (new_image != NULL_IMAGE)
      {
        xvt_dwin_draw_image(xdWindow, new_image, &dst_rect, &src_rect); //5-64
        mark_line(xdWindow,linestyle);  //->plot.cpp
      }
    }
    break;
  case E_CLOSE:
    {
      xvt_vobj_destroy(xdWindow);
    }
    break;
  case E_MOUSE_UP:
		/*
    Mouse was released
    */
    {
    }
    break;
  case E_MOUSE_DOWN:
    {
      if (xdEvent->v.mouse.button ==0)  //linke Maus-Taste
      {
        mark_line(xdWindow,linestyle);  //->plot.cpp
        /************************ GHJ ************************/
        if ((xdEvent->v.mouse.where.v>0)  &&(xdEvent->v.mouse.where.v<(int)(res_factor*30)))
          linestyle=1;
        if ((xdEvent->v.mouse.where.v>(int)(res_factor*31)) &&(xdEvent->v.mouse.where.v<(int)(res_factor*60)))
          linestyle=2;
        if ((xdEvent->v.mouse.where.v>(int)(res_factor*61)) &&(xdEvent->v.mouse.where.v<(int)(res_factor*90)))
          linestyle=3;
        if ((xdEvent->v.mouse.where.v>(int)(res_factor*91)) &&(xdEvent->v.mouse.where.v<(int)(res_factor*118)))
          linestyle=4;
        if ((xdEvent->v.mouse.where.v>(int)(res_factor*119))&&(xdEvent->v.mouse.where.v<(int)(res_factor*147)))
          linestyle=5;
        if ((xdEvent->v.mouse.where.v>(int)(res_factor*148))&&(xdEvent->v.mouse.where.v<(int)(res_factor*176)))
          linestyle=6;
        if ((xdEvent->v.mouse.where.v>(int)(res_factor*177))&&(xdEvent->v.mouse.where.v<(int)(res_factor*206)))
          linestyle=7;
        if ((xdEvent->v.mouse.where.v>(int)(res_factor*207))&&(xdEvent->v.mouse.where.v<(int)(res_factor*235)))
          linestyle=8;
        if ((xdEvent->v.mouse.where.v>(int)(res_factor*236))&&(xdEvent->v.mouse.where.v<(int)(res_factor*264)))
          linestyle=9;
        if ((xdEvent->v.mouse.where.v>(int)(res_factor*265))&&(xdEvent->v.mouse.where.v<(int)(res_factor*293)))
          linestyle=10;
        if ((xdEvent->v.mouse.where.v>(int)(res_factor*294))&&(xdEvent->v.mouse.where.v<(int)(res_factor*322)))
          linestyle=11;
        if ((xdEvent->v.mouse.where.v>(int)(res_factor*323))&&(xdEvent->v.mouse.where.v<(int)(res_factor*351)))
          linestyle=12;
        if ((xdEvent->v.mouse.where.v>(int)(res_factor*352))&&(xdEvent->v.mouse.where.v<(int)(res_factor*380)))
          linestyle=13;
        if ((xdEvent->v.mouse.where.v>(int)(res_factor*381))&&(xdEvent->v.mouse.where.v<(int)(res_factor*409)))
          linestyle=14;
        if ((xdEvent->v.mouse.where.v>(int)(res_factor*410))&&(xdEvent->v.mouse.where.v<(int)(res_factor*435)))
          linestyle=0;
        /*************************************************/
        mark_line(xdWindow,linestyle);  //->plot.cpp
      }
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
  xvt_tx_process_event(xdWindow, xdEvent);
  return 0L;
}
