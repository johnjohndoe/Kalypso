/**********************************************************
*  Funktionen fuer interaktiven / graphischen Editor      *
*        PAINT.CPP                                        *
**********************************************************/
#define _TEST_DRAW  0

#include <windows.h>
#include <math.h>

#include "xvt.h"

#include "typen.h"
#include "l_typen.h"
#include "wspwin.h"
#include "global_types.h"
#include "global.h"

#include "bce_allg.h"
#include "wspd143.h"
#include "paint.h"

// Defines

#define OFFSET 1
#define FAKTOR 2
#define NEU    3
#define ANFANG 1
#define ENDE   2

// 'lokale Variablen

DRAW_DIMENSIONS WspDrawDimensions;
PNT pnt_save[STRANGANZAHL];
WINDOW *ptr_win;     //Dialog 143/140  ptr auf Edit-Felder

// globale Variablen

extern PNT profil_pnt[STRANGANZAHL];
extern MinMax pmm;
extern BOOLEAN checkbox, checkbox_alle, checkbox_edit, checkbox_edit_v, checkbox_edit_h;
extern WINDOW dlg_sonderprofil;
extern ZOOM zoom_info;
extern int zoom_zustand;
extern BOOLEAN win117_mouse_move;//Dick 25.7.98


#pragma optimize( "", off ) // ???


// Hilfsfunktionen //

int bce_Projektion(int *x,int *y,int *rx , int *ry,double *m1,double *m3)
{
  double ex,ey,ox,oy,d;
  d=sqrt(pow(x[1]-x[0],2.)+pow(y[1]-y[0],2.));
  if(d<1.0e-06)return 1;
  ex=(x[1]-x[0])/d;
  ey=(y[1]-y[0])/d;
  ox=ey;
  oy=-ex;
  *m3=(x[0]-x[2])*ox+(y[0]-y[2])*oy;
  *rx=(int)(x[2]+ox*(*m3));//Dick 11.02.99 
  *ry=(int)(y[2]+oy*(*m3));//Dick 11.02.99
  *m1=(double)(*rx-x[1]+*ry-y[1])/(double)(x[0]-x[1]+y[0]-y[1]);
  return 0;
}



Paint::Paint()
{
  ptr_anfang = list->get_anfang();
  ptr_profil = ptr_ende = ptr_anfang;
  
  
  zoom_left_x = NULL;
  zoom_right_x = NULL;
}

Paint::~Paint(void)
{
};

void Paint::draw_rect(WINDOW xdWindow )
/***  X Y Koordinatensystem zeichnen   *******/
{
  PNT pnt;
  DRAW_CTOOLS rect_tools;
  
  xvt_app_get_default_ctools(&rect_tools);
  rect_tools.pen.width = WspDrawDimensions.wspLineWidth0;
  xvt_dwin_set_draw_ctools(xdWindow, &rect_tools);
  xvt_dwin_set_back_color(xdWindow, COLOR_LTGRAY);
  
  /***  X Y Koordinatensystem zeichnen   *******/
  rect_tools.pen.width = WspDrawDimensions.wspLineWidth3;
  xvt_dwin_set_draw_ctools(xdWindow, &rect_tools);
  /* Y-LINE LEFT */
  pnt.h = WspDrawDimensions.DX;
  pnt.v = WspDrawDimensions.DYY;
  xvt_dwin_draw_set_pos(xdWindow, pnt);
  pnt.h = WspDrawDimensions.DX;
  pnt.v = WspDrawDimensions.DELTA_Y+WspDrawDimensions.DYY+WspDrawDimensions.AB;
  xvt_dwin_draw_line(xdWindow, pnt);
  
  /* X-LINE BOTTOM  */
  pnt.h = WspDrawDimensions.DX-WspDrawDimensions.AB;
  pnt.v = WspDrawDimensions.DELTA_Y+WspDrawDimensions.DYY;
  xvt_dwin_draw_set_pos(xdWindow, pnt);
  pnt.h = WspDrawDimensions.DELTA_X+WspDrawDimensions.DX+WspDrawDimensions.AB;
  pnt.v = WspDrawDimensions.DELTA_Y+WspDrawDimensions.DYY;
  xvt_dwin_draw_line(xdWindow, pnt);
  
  /* Y-LINE RIGHT  */
  pnt.h = WspDrawDimensions.DELTA_X+WspDrawDimensions.DX;
  pnt.v = WspDrawDimensions.DYY;
  xvt_dwin_draw_set_pos(xdWindow, pnt);
  pnt.h = WspDrawDimensions.DELTA_X+WspDrawDimensions.DX;
  pnt.v = WspDrawDimensions.DELTA_Y+WspDrawDimensions.DYY+WspDrawDimensions.AB;
  xvt_dwin_draw_line(xdWindow, pnt);
}

/******************************************************************/
/*  Marker Rechteck setzen, PNT als y-z Koordinaten in [m]        */
void Paint::draw_marker(WINDOW win, int position)
{
  RCT rct;
  PNT pt;
  CBRUSH brush;
  
  pt.h = profil_pnt[position].v;
  pt.v = profil_pnt[position].h;
  
  
  brush.pat =   PAT_CROSS;
  brush.color = COLOR_YELLOW;
  xvt_dwin_set_cbrush (win,&brush);
  xvt_dwin_set_std_cpen(win,TL_PEN_BLACK);
  xvt_dwin_set_draw_mode(win,M_XOR);
  xvt_rect_set(&rct,pt.v-5,pt.h-5,pt.v+5,pt.h+5);
  
  xvt_dwin_draw_rect(win,&rct);
}
/****************************************************************/

void Paint::draw_min_max_werte(WINDOW xdWindow)
//minX-, maxX Werte an die Achsen schreiben   ****
{
  char strtemp[25];
  int pos,maxY_pnt=(int)(BCE_NAN),minY_pnt=0;
  static int invalid=0;
  static double  maxY_save=0.,minY_save=0.;
  XVT_FONT_STYLE_MASK old_mask;
  
  //Neu
  for(int i=0;i<STRANGANZAHL;i++)
  {
    if(profil_pnt[i].v!=0 && profil_pnt[i].v > minY_pnt)
      minY_pnt=profil_pnt[i].v;
    if(profil_pnt[i].v!=0 && profil_pnt[i].v < maxY_pnt)
      maxY_pnt=profil_pnt[i].v;
  }
  //Neu End
  if(pmm.maxY!=maxY_save || pmm.minY!=minY_save)
  {
    maxY_save=pmm.maxY;
    minY_save=pmm.minY;
    invalid=1;
  }
  
  xvt_dwin_set_font_family(xdWindow,XVT_FFN_TIMES);
  xvt_dwin_set_font_size(xdWindow,(int)(8*WspDrawDimensions.wspLineWidth1*res_factor));		// GHJ
  old_mask=xvt_dwin_get_font_style(xdWindow);
  
  sprintf(strtemp,"%.4lf",pmm.minX);              
  pos=strlen(strtemp);
  strtemp[pos]=' ';
  strtemp[pos+1]='m';
  strtemp[pos+2]='\0';
  xvt_dwin_draw_text(xdWindow,
    (2*WspDrawDimensions.AB)+WspDrawDimensions.offset,//X
    (WspDrawDimensions.DELTA_Y+WspDrawDimensions.DY+7*WspDrawDimensions.offset),//Y
    strtemp,strlen(strtemp));
  
  sprintf(strtemp,"%.4lf",pmm.maxX);
  pos=strlen(strtemp);
  strtemp[pos]='m';
  strtemp[pos+1]='\0';                               
  xvt_dwin_draw_text(xdWindow,
    (WspDrawDimensions.DELTA_X+2*WspDrawDimensions.AB)+WspDrawDimensions.offset-6*(pos-4), //X
    (WspDrawDimensions.DELTA_Y+WspDrawDimensions.DY+7*WspDrawDimensions.offset),   //Y
    strtemp,strlen(strtemp));
  
  xvt_dwin_set_font_style(xdWindow,old_mask|XVT_FS_UNDERLINE);
  
  xvt_dwin_set_fore_color(xdWindow,COLOR_RED);
  if(pmm.minY<1000.)
    sprintf(strtemp,"%-6.1lf",pmm.minY);
  else
    sprintf(strtemp,"%-6.0lf",pmm.minY);
  xvt_dwin_draw_text(xdWindow,
    (WspDrawDimensions.AB/2)+WspDrawDimensions.offset,
    (WspDrawDimensions.DELTA_Y-WspDrawDimensions.DY-WspDrawDimensions.DYY+7*WspDrawDimensions.offset-15),
    // minY_pnt,
    strtemp,strlen(strtemp));
  
  if(pmm.maxY<1000.)
    sprintf(strtemp,"%-6.1lf",pmm.maxY);
  else
    sprintf(strtemp,"%-6.0lf",pmm.maxY);
  xvt_dwin_draw_text(xdWindow,
    (WspDrawDimensions.AB/2)+WspDrawDimensions.offset,
    WspDrawDimensions.DY+7*WspDrawDimensions.offset,
    // maxY_pnt,
    strtemp,strlen(strtemp));
  
  xvt_dwin_set_font_style(xdWindow,old_mask);
  xvt_dwin_set_fore_color(xdWindow,COLOR_BLACK);
  
  xvt_dwin_draw_text(xdWindow,
    (WspDrawDimensions.AB/2)+WspDrawDimensions.offset,//+6, //Dick 31.05.99
    WspDrawDimensions.DY+7*WspDrawDimensions.offset-22,//-14, //Dick 31.05.99
    "[NN+m]",strlen("[NN+m]"));//Dick 31.05.99
  xvt_dwin_set_font_style(xdWindow,old_mask|XVT_FS_UNDERLINE);
  
  
  //Neu Dick 15.03.99 scalierung
  
  double ersterpunkt,letzterpunkt;
  
  double delta_station;
  double schritt=0.1;//default 0.1
  bool gefunden=FALSE;
  int anzahl_punkte=3;//default 3
  
  int delta_pnt=abs(2*WspDrawDimensions.DY-WspDrawDimensions.DELTA_Y+WspDrawDimensions.DYY+15);
  double factorY;
  
  double grenze;
  factorY= WspDrawDimensions.Y/ pmm.distanceY;
  
  ersterpunkt=floor(pmm.minY);
  letzterpunkt=ceil(pmm.maxY);
  delta_station=pmm.maxY-pmm.minY;
  i=0;
  while(!gefunden)
  {
    switch(i)
    {
    case 0:
      if((anzahl_punkte=(int)(delta_station*10))<=8)
      {
        schritt=0.1;
        gefunden=TRUE;
        grenze=0.06;
      }
      break;
    case 1:
      if((anzahl_punkte= (int)(delta_station*5))<=8)
      {
        schritt=0.2;
        gefunden=TRUE;
        grenze=0.1;
      }
      break;
    case 2:
      if((anzahl_punkte= (int)(delta_station*2))<=8)
      {
        schritt=0.5;
        gefunden=TRUE;
        grenze=0.3;
      }
      break;
    case 3:
      if((anzahl_punkte= (int)(delta_station*1))<=8)
      {
        schritt=1.0;
        gefunden=TRUE;
        grenze=0.4;
      }
      break;
    case 4:
      if((anzahl_punkte= (int)(delta_station/2))<=8)
      {
        schritt=2.0;
        gefunden=TRUE;
        grenze=0.5;
      }
      break;
    case 5:
      if((anzahl_punkte= (int)(delta_station/5))<=8)
      {
        schritt=5.0;
        gefunden=TRUE;
        grenze=0.5;
      }
      break;
    case 6:
      if((anzahl_punkte= (int)(delta_station/10))<=8)
      {
        schritt=10.0;
        gefunden=TRUE;
        grenze=5.;
      }
      break;
    case 7:
      if((anzahl_punkte= (int)(delta_station/20))<=8)
      {
        schritt=20.0;
        gefunden=TRUE;
        grenze=10.;
      }
      break;
    case 8:
      if((anzahl_punkte= (int)(delta_station/50))<=8)
      {
        schritt=50.0;
        gefunden=TRUE;
        grenze=25.;
      }
      break;
    default:
      if((anzahl_punkte= (int)(delta_station/(100*(i-8))))<=8)
      {
        schritt=100*(i-8);
        if(schritt>200 && schritt<=500)
          schritt=500.;
        else if(schritt>500 && schritt<=1000)
          schritt=1000.;
        else if(schritt>1000)
          schritt=1000.*(i-17);
        gefunden=TRUE;
        grenze=schritt/2.;
      }
    }
    i++;
    if(i>10000)break;
  }
  anzahl_punkte=(int)((letzterpunkt-ersterpunkt)/schritt);
  for(i=0;i<=anzahl_punkte;i++)
  {
    if(  (((ersterpunkt+schritt*i-pmm.minY) < grenze)&&
      !(fabs((ersterpunkt+schritt*i-pmm.minY) - grenze)<1e-6)  )  
      || (((pmm.maxY-(ersterpunkt+schritt*i)) < grenze) &&
      !(fabs((pmm.maxY-(ersterpunkt+schritt*i)) - grenze)<1e-6) ))
      continue;
    if(ersterpunkt+schritt*i<1000.)
      sprintf(strtemp,"%-6.1lf",ersterpunkt+schritt*i);
    else
      sprintf(strtemp,"%-6.0lf",ersterpunkt+schritt*i);
    xvt_dwin_draw_text(xdWindow,
      (WspDrawDimensions.AB/2)+WspDrawDimensions.offset,
      //ersterpunkt_pnt-schritt_pnt,
      (int)( WspDrawDimensions.vertDist -(  (ersterpunkt+schritt*i - pmm.minY) * factorY )),
      strtemp,strlen(strtemp));
  }
  //ende Scalierung
  xvt_dwin_set_font_style(xdWindow,old_mask);
}
/************************************************************************/
void Paint::draw_trapez(WINDOW win)
{
  PNT pnt[4];
  DRAW_CTOOLS tools;
  double factorX,factorY;
  double help;
  double D,H,S,x,y;
  
  D = scr.z[0];
  H = scr.z[1];
  S = scr.z[2];
  x = scr.z[4];
  y = scr.z[5];
  
  if(!WspDrawDimensions.isPrinting)
    draw_min_max_werte(win);
  
  factorX= WspDrawDimensions.DELTA_X / pmm.distanceX;
  factorY= WspDrawDimensions.Y / pmm.distanceY;
  
  
  xvt_app_get_default_ctools(&tools);
  tools.pen.width    = WspDrawDimensions.wspLineWidth1;
  tools.pen.color    = COLOR_BLACK;
  tools.brush.pat    = PAT_CROSS;
  tools.brush.color = COLOR_YELLOW;
  xvt_dwin_set_draw_ctools(win, &tools);
  
  if((scr.z[0]!=BCE_NAN) &&(scr.z[1]!=BCE_NAN) && (scr.z[2]!=BCE_NAN)
    &&(scr.z[4]!=BCE_NAN) &&(scr.z[5]!=BCE_NAN)&&(pmm.minY !=BCE_NAN))
  {
    help =  x - 0.5 * D;
    pnt[0].h =(short) (( (help - pmm.minX) * factorX) + WspDrawDimensions.DX);
    pnt[0].v =(short)  (WspDrawDimensions.vertDist -(  (y - pmm.minY) * factorY ));
    
    help =  x + 0.5 * D;
    pnt[1].h =(short) (((help - pmm.minX) * factorX) + WspDrawDimensions.DX);
    pnt[1].v =(short)  (WspDrawDimensions.vertDist -(  (y - pmm.minY) * factorY ));
    
    help =  x + 0.5 * ( D + 2.0 * H * S);
    pnt[2].h =(short)(( (help - pmm.minX) * factorX) + WspDrawDimensions.DX);
    pnt[2].v =(short) ( WspDrawDimensions.vertDist -(  (y + H - pmm.minY) * factorY ));
    
    help =  x - 0.5 * ( D + 2.0 * H * S);
    pnt[3].h =(short) (( (help - pmm.minX) * factorX) + WspDrawDimensions.DX);
    pnt[3].v =(short)  (WspDrawDimensions.vertDist -(  (y + H - pmm.minY) * factorY ));
    
    xvt_dwin_draw_polygon(win, pnt,4);
  }
}
/************************************************************************/
void Paint::draw_kreis(WINDOW win,int typ)
{
  RCT rct;
  DRAW_CTOOLS tools;
  double factorX,factorY;
  double D,x,y;
  
  xvt_app_get_default_ctools(&tools);
  tools.pen.width    = WspDrawDimensions.wspLineWidth1;
  tools.pen.color    = COLOR_BLACK;
  tools.brush.pat    = PAT_CROSS;
  tools.brush.color = COLOR_YELLOW;
  xvt_dwin_set_draw_ctools(win, &tools);
  
  if (typ == KREIS)
  {
    D = scr.z[0];
    x = scr.z[2];
    y = scr.z[3];
    
    if((scr.z[0]!=BCE_NAN) &&(scr.z[2]!=BCE_NAN)&&
      (scr.z[3]!=BCE_NAN) &&(pmm.minY!=BCE_NAN))
    {
      factorX= WspDrawDimensions.DELTA_X / pmm.distanceX;
      factorY= WspDrawDimensions.Y / pmm.distanceY;
      
      rct.top    =(short) ( WspDrawDimensions.vertDist -(  (y + D - pmm.minY) * factorY ));
      rct.left   =(short) ( ( (x -(0.5*D) - pmm.minX) * factorX) + WspDrawDimensions.DX);
      rct.bottom =(short) ( WspDrawDimensions.vertDist -(  (y - pmm.minY) * factorY ));
      rct.right  =(short) ( ( (x +(0.5*D) - pmm.minX) * factorX) + WspDrawDimensions.DX);
      
      xvt_dwin_draw_oval(win, &rct);
    }
  }
  else // KREISSEGM
  {
    
  /* int start_x, start_y ,stop_x, stop_y;
  
    ptr_profil = ptr_anfang;
    pp=ptr_profil->datensatz;
    if (pp!=NULL)
    {
    D = scr.z[0];
    x = pp->x;   // erster Stationswert in Gelände
    y = pp->y;
    
      factorX= WspDrawDimensions.DELTA_X / pmm.distanceX;
      factorY= WspDrawDimensions.Y / pmm.distanceY;
      
        rct.left = ( (x - pmm.minX) * factorX) + WspDrawDimensions.DX;
        rct.right  =  ( (x + D - pmm.minX) * factorX) + WspDrawDimensions.DX;
        rct.top    =  WspDrawDimensions.vertDist -(  (y + (0.5* D) - pmm.minY) * factorY );
        rct.bottom =  WspDrawDimensions.vertDist -(  (y - (0.5* D) - pmm.minY) * factorY );
        start_x = rct.right;
        stop_x  = rct.left;
        start_y = stop_y = WspDrawDimensions.vertDist -(  (y  - pmm.minY) * factorY );
        
          xvt_dwin_draw_pie (win, &rct, start_x, start_y ,stop_x, stop_y);
          }
    */
  }
  
}
/************************************************************************/
void Paint::draw_eiprofil(WINDOW win)
{
  const unsigned short punkteZahl = 201;
  PNT pnt[punkteZahl];
  double erg_x[punkteZahl], erg_y[punkteZahl];
  double station,sohle,hoehe;
  double factorX,factorY;
  DRAW_CTOOLS tools;
  
  factorX= WspDrawDimensions.DELTA_X / pmm.distanceX;
  factorY= WspDrawDimensions.Y / pmm.distanceY;
  
  xvt_app_get_default_ctools(&tools);
  tools.pen.width    = WspDrawDimensions.wspLineWidth1;
  tools.pen.color    = COLOR_BLACK;
  tools.brush.pat    = PAT_CROSS;
  tools.brush.color = COLOR_YELLOW;
  xvt_dwin_set_draw_ctools(win, &tools);
  
  double beta[100] ={ 0.1137 , 0.1583 , 0.1908 , 0.2166 , 0.2380 , 0.2561 ,
    0.2716 , 0.2863, 0.3006 , 0.3146 , 0.3281 , 0.3414 , 0.3543 , 0.3668 ,
    0.3790 , 0.3910, 0.4025 , 0.4138 , 0.4248 , 0.4355 , 0.4459 , 0.4561 ,
    0.4659 , 0.4755, 0.4848 , 0.4938 , 0.5026 , 0.5111 , 0.5194 , 0.5274 ,
    0.5351 , 0.5426, 0.5499 , 0.5569 , 0.5637 , 0.5703 , 0.5766 , 0.5827 ,
    0.5886 , 0.5942, 0.5997 , 0.6049 , 0.6098 , 0.6146 , 0.6192 , 0.6235 ,
    0.6276 , 0.6315, 0.6352 , 0.6387 , 0.6420 , 0.6450 , 0.6479 , 0.6506 ,
    0.6530 , 0.6553, 0.6573 , 0.6591 , 0.6608 , 0.6622 , 0.6635 , 0.6645 ,
    0.6653 , 0.6660, 0.6664 , 0.6666 , 0.6666 , 0.6661 , 0.6650 , 0.6633 ,
    0.6610 , 0.6581, 0.6545 , 0.6503 , 0.6455 , 0.6400 , 0.6338 , 0.6270 ,
    0.6194 , 0.6110, 0.6019 , 0.5919 , 0.5811 , 0.5694 , 0.5568 , 0.5431 ,
    0.5283 , 0.5122, 0.4949 , 0.4761 , 0.4556 , 0.4333 , 0.4087 , 0.3816 ,
    0.3512 , 0.3166, 0.2764 , 0.2274 , 0.1621 , 0.1050 };
  
  if((scr.z[3]!=BCE_NAN) &&(scr.z[4]!=BCE_NAN) && (scr.z[1]!=BCE_NAN)
    &&(pmm.minY !=BCE_NAN))
  {
    
    station = scr.z[3];
    sohle   = scr.z[4];
    hoehe   = scr.z[1];
    
    erg_x [1] = station;
    erg_x[101]=station;
    erg_y [1] =sohle;
    erg_y[101]=sohle+hoehe;
    
    for (int i=2; i<=100; i++)
    {
      erg_y[i] = sohle + ( i - 1 ) * ( hoehe / 100 );
      erg_y[punkteZahl + 1 - i] = erg_y[i];
      erg_x[i] = station - 0.5 * beta [i-2] * hoehe;
      erg_x [punkteZahl + 1 - i] = station + 0.5 * beta[i - 2] * hoehe;
    }
    
    for ( i = 0; i < punkteZahl; i++ )
    {
      pnt[i].h =(short) (( (erg_x[i+1] - pmm.minX) * factorX) + WspDrawDimensions.DX);
      pnt[i].v =(short) ( WspDrawDimensions.vertDist -(  (erg_y[i+1] - pmm.minY) * factorY ));
    }
    xvt_dwin_draw_polygon( win,pnt, punkteZahl - 1 );
  }
}

/************************************************************************/
void Paint::draw_maulprofil(WINDOW win)
{
  const int punkteZahl = 201;
  PNT pnt[punkteZahl];
  double erg_x[punkteZahl], erg_y[punkteZahl];
  double station,sohle,hoehe;
  double factorX,factorY;
  DRAW_CTOOLS tools;
  
  double beta[100] ={0.3260 , 0.4601 , 0.5625 , 0.6483 , 0.7234 , 0.7909 ,
    0.8527 , 0.9098 , 0.9631 , 1.0132 , 1.0580 , 1.0940 , 1.1242 , 1.1503 ,
    1.1733 , 1.1936 , 1.2119 , 1.2282 , 1.2430 , 1.2563 , 1.2683 , 1.2790 ,
    1.2886 , 1.2972 , 1.3047 , 1.3113 , 1.3170 , 1.3218 , 1.3258 , 1.3289 ,
    1.3312 , 1.3326 , 1.3333 , 1.3333 , 1.3329 , 1.3323 , 1.3313 , 1.3301 ,
    1.3285 , 1.3266 , 1.3245 , 1.3220 , 1.3192 , 1.3162 , 1.3128 , 1.3090 ,
    1.3050 , 1.3007 , 1.2960 , 1.2910 , 1.2857 , 1.2800 , 1.2740 , 1.2676 ,
    1.2610 , 1.2539 , 1.2465 , 1.2387 , 1.2306 , 1.2220 , 1.2131 , 1.2038 ,
    1.1940 , 1.1839 , 1.1733 , 1.1623 , 1.1508 , 1.1389 , 1.1265 , 1.1136 ,
    1.1001 , 1.0862 , 1.0716 , 1.0565 , 1.0408 , 1.0245 , 1.0075 , 0.9898 ,
    0.9714 , 0.9522 , 0.9322 , 0.9113 , 0.8894 , 0.8666 , 0.8426 , 0.8175 ,
    0.7910 , 0.7632 , 0.7337 , 0.7024 , 0.6690 , 0.6333 , 0.5948 , 0.5528 ,
    0.5066 , 0.4549 , 0.3955 , 0.3241 , 0.2301 , 0.2000 };
  
  
  if((scr.z[3]!=BCE_NAN) &&(scr.z[4]!=BCE_NAN) && (scr.z[1]!=BCE_NAN)
    && (pmm.minY !=BCE_NAN))
  {
    factorX= WspDrawDimensions.DELTA_X / pmm.distanceX;
    factorY= WspDrawDimensions.Y / pmm.distanceY;
    
    xvt_app_get_default_ctools(&tools);
    tools.pen.width    = WspDrawDimensions.wspLineWidth1;
    xvt_dwin_set_draw_mode(win,M_COPY);
    tools.pen.color    = COLOR_BLACK;
    tools.brush.pat    = PAT_CROSS;
    tools.brush.color = COLOR_YELLOW;
    xvt_dwin_set_draw_ctools(win, &tools);
    
    station = scr.z[3];
    sohle   = scr.z[4];
    hoehe   = scr.z[1];
    
    erg_x [1] = station;
    erg_x[101]=station;
    erg_y [1] =sohle;
    erg_y[101]=sohle+hoehe;
    
    for ( int i = 2; i <= 100; i++ )
    {
      erg_y[i] = sohle + ( i - 1 ) * ( hoehe / 100 );
      erg_y[punkteZahl + 1 - i] = erg_y[i];
      erg_x[i] = station - ( 0.5 * beta[i - 2] * hoehe );
      erg_x[punkteZahl + 1 - i] = station + ( 0.5 * beta[i - 2] * hoehe );
    }
    
    for ( i = 0; i < punkteZahl; i++ )
    {
      pnt[i].h = (short) (( (erg_x[i + 1] - pmm.minX) * factorX) + WspDrawDimensions.DX);
      pnt[i].v =(short) ( WspDrawDimensions.vertDist -(  (erg_y[i+1] - pmm.minY) * factorY ));
    }
    xvt_dwin_draw_polygon(win,pnt, punkteZahl - 1);
  }
}

/***********************************************************************/

void Paint::draw_117(WINDOW win,MMP *mmp)
{
  int num_ds=1;
  int datensatz;
  int ds_typ;
  extern int draw_liste[50];
  extern int typ[TYPE_SIZE];//Dick 8.12.98
  extern List *list;
  extern Scroller scr;
  int datensatz_save; //Dick 5.03.99
  
  if (checkbox || checkbox_alle)
    num_ds=ds_info[0];      // alle Datensätze zeichnen !!
  // ansonsten nur den von Position 1
  else if(scr.datensatz!=1) //Dick 12.03.99 TY
    num_ds=2;
  
  
  datensatz_save=scr.datensatz;//Dick 5.03.99
  for (int m=1; m<=num_ds;m++)
  {
    if (checkbox || checkbox_alle) datensatz=m;
    else datensatz = scr.datensatz;
    
    GetMinMax(&pmm,scr.datensatz);
    
    if(!WspDrawDimensions.isPrinting && m==1)
      draw_min_max_werte(win);
    
    if ((draw_liste[m]==1)&&(checkbox || checkbox_alle))
    {
      ds_typ= typ[m];
      scr.datensatz=m;//Dick 5.03.99
      GetScrollDaten(&scr);//Dick 5.03.99
    }
    else      
      ds_typ = typ[draw_liste[m-1]]; //Dick 12.03.99 TY 1 -> m-1
    
    switch (ds_typ)
    {
    case SOHLHOEHE:
    case GELAENDEHOEHE:
      Paint117(win,&pmm);   //  Profillinie zeichnen
      break;
    case GELAENDE2:
    case SOHLHOEHE_2:
      PaintBruecke(win,&pmm,datensatz,RGB(170,0,140));
      break;
    case FLAECHE:  // bei Datensatz FLAECHE wird Gelände2 gezeichnet
      {
        for (int n=1;n<=ds_info[0];n++)
          if (typ[n]==GELAENDE2)
          {
            PaintBruecke(win,&pmm,n,COLOR_BROWN);
            break;
          }
      }
      break;
    case TRENNFLAECHEN:
      DrawTrennflaechen(win,&pmm,datensatz,COLOR_BROWN);
      break;
    case BORDVOLL:
      DrawTrennflaechen(win,&pmm,datensatz,COLOR_RED);
      break;
    case MODELLGRENZEN:
      DrawTrennflaechen(win,&pmm,datensatz,COLOR_BROWN);
    case RAUHIGKEIT:
    case RAUHIGKEIT_KST:
      DrawRauhigkeit(win,&pmm,datensatz);
      Highligth_mouse_move(win,mmp);
      break;
    case PUNKT_NR:
      break;
    case WASSERSPIEGEL:
      PaintBruecke(win,&pmm,datensatz,RGB(255,100,0));
      break;
    case AUSUFERUNG_LINKS:
      PaintBruecke(win,&pmm,datensatz,RGB(0,255,0));
      break;
    case AUSUFERUNG_RECHTS:
      PaintBruecke(win,&pmm,datensatz,RGB(0,128,128));
      break;
    case ENERGIEHOEHE:
      PaintBruecke(win,&pmm,datensatz,RGB(0,0,255));
      break;
    case WASSERSPIEGEL_2:
      PaintBruecke(win,&pmm,datensatz,RGB(255,255,0));
      break;
    case WSP_FIXIERUNG:
      PaintWSF(win,&pmm,datensatz,RGB(174,0,87));
      break;
    case STATION:  //identisch mit 'WSP-HOEHE'
      PaintStation(win,&pmm,datensatz,COLOR_BLUE);
      break;
    case STATION_FIX:  //identisch mit 'WSP-HOEHE'
      PaintStation(win,&pmm,datensatz,RGB(150,0,0));
      break;
    case DURCHST_BEREICH:
      DurchstBereiche(win, &pmm,datensatz);
      break;
    case OK_WEHRS:
      PaintBruecke(win,&pmm,datensatz,COLOR_DKGRAY);
      break;
    case TRENN_WEHR:
      DrawTrennflaechen(win,&pmm,datensatz,COLOR_GREEN);
      break;
    case UK_BRUECKE:
      PaintBruecke(win,&pmm,datensatz,COLOR_GREEN);
      break;
    case BUHNE:
      PaintBuhne(win,&pmm,datensatz,COLOR_GREEN);
      break;
    case OK_BRUECKE:
      PaintBruecke(win,&pmm,datensatz,COLOR_GREEN);
      break;
    case BOESCHUNG_LINKS:
      PaintBruecke(win,&pmm,datensatz,COLOR_BROWN);
      break;
    case BOESCHUNG_RECHTS:
      PaintBruecke(win,&pmm,datensatz,RGB(0,128,255));
      break;
    case BOESCHUNG_LINKS_2:
      PaintBruecke(win,&pmm,datensatz,RGB(0,200,255));
      break;
    case BOESCHUNG_RECHTS_2:
      PaintBruecke(win,&pmm,datensatz,RGB(0,128,128));
      break;
    case KASTEN:
      PaintBruecke(win,&pmm,datensatz,COLOR_DKGRAY);
      break;
    case OK_GELAENDE:
      PaintBruecke(win,&pmm,datensatz,COLOR_GREEN);
      break;
    case WASSERSP1:
    case WASSERSP100:
    case WASSERSP5:
      break;
    case AXM:
    case AYM:
    case DPM:
      Paint117(win,&pmm);   //Dick 27.07.99  Profillinie zeichnen sonst konflikt mit UK-OK-BRUECKE
      PaintAX(win,&pmm,datensatz,COLOR_BROWN);
      DrawBewuchs(win,&pmm,datensatz);//Dick 16.04.99
      Highligth_mouse_move(win,mmp); //Dick 16.04.99
      break;
    case MAUL:
      draw_maulprofil(win);
      break;
    case EIPROFIL:
      draw_eiprofil(win);
      break;
    case TRAPEZ:
      draw_trapez(win);
      break;
    case KREIS:
      draw_kreis(win,KREIS);
      break;
    case KREISSEGM:
      draw_kreis(win,KREISSEGM);
      break;
    case DKOK:
    case DKUK:
      PaintDK(win,&pmm,datensatz,RGB(0,0,0));
      break;
    case BVHOEHE:
      PaintBruecke(win,&pmm,datensatz,COLOR_DKGRAY);
      break;
    default: break;
  }
 }
 
 scr.datensatz=datensatz_save;//Dick 5.03.99
 if(typ[scr.datensatz]!=UK_BRUECKE && typ[scr.datensatz]!=OK_BRUECKE &&typ[scr.datensatz]!=OK_GELAENDE && typ[scr.datensatz]!=BUHNE)//Dick 29.04.99
 {
   Paint117(win,&pmm);   //  Profillinie zeichnen
   Update_Scr(0,&scr);
 }
 if(checkbox || checkbox_alle)    //Dick 12.03.99
 {
   Get_Num_Station_Zoom(&zoom_info,2);//Dick 20.04.99
   Set_Zoom_Marker(&zoom_info);  //Dick 20.04.99
   GetScrollDaten(&scr);//Dick 12.03.99
 }
}
/*******************************************************************/
int Paint::Get_Mouse_Position(int hor,int ver)
{
  // Graphisches Editieren:bei mouse_down finden des Stationswertes
  // Rückgabe:  nicht gefunden=zulezt gespeicherte // int i :=position in ptr_profil-Liste
  int gefunden =0;
  int anzahl,
    change=0;
  static int AltWert=0;
  int save_hor=0;
  
  /*Berechnung von anzahl durch zählen in profil_pnt ist sicherer ,13.03.96*/
  
	 {
     anzahl = ds_info[1];
     // Korrektur, falls BCE_NAN -Werte im Gelände
			  for (int k=anzahl;k>=0;k--)
        {
          if ((profil_pnt[k].h==0)&&(profil_pnt[k].v==0))
          {
            anzahl--;
            change=1;
          }
        }
        if (change) anzahl++;
   }
   for (int i=0;i<anzahl;i++)
   {
     if(zoom_zustand==1 && !win117_mouse_move)
     {
       if((profil_pnt[i].h >= hor)&&(!gefunden) )
         gefunden = i+1;
     }
     else
       if(zoom_zustand==2)
       {
         if(profil_pnt[i].h <= hor )
           gefunden = i+1;
       }
       else        
         if((hor-5<=profil_pnt[i].h && hor+5>=profil_pnt[i].h)&&((!gefunden)|| abs(profil_pnt[i].h - hor)< save_hor)) //Dick 25.07.98
         {
           gefunden = i+1;
           AltWert=gefunden;
           save_hor=abs(profil_pnt[i].h - hor);
         }
   }//for (int i=0;i<anzahl;i++)
   if(gefunden>0)
     return gefunden;
   else 
     return AltWert;//Dick 25.07.98
}
/***************************************************/
/*******************************************************************/
int Paint::Get_Mouse_Position_GH(int hor,int ver)
{
  // Graphisches Editieren:bei mouse_down finden des Stationswertes nur für Geländehöhen und Fläche 
  // Rückgabe:  nicht gefunden=0 // int i :=position in ptr_profil-Liste
  int gefunden =0;
  int anzahl,
    change=0;
  
  /*Berechnung von anzahl durch zählen in profil_pnt ist sicherer ,13.03.96*/
	 {
     anzahl = ds_info[1];
     // Korrektur, falls BCE_NAN -Werte im Gelände
     for (int k=anzahl;k>=0;k--)
     {
       if ((profil_pnt[k].h==0)&&(profil_pnt[k].v==0))
       {
         anzahl--;
         change=1;
       }
     }
     if (change) anzahl++;
   }
   for (int i=0;i<anzahl;i++)
   {
     if((hor-5<=profil_pnt[i].h && hor+5>=profil_pnt[i].h)&&(ver-5<=profil_pnt[i].v && ver+5>=profil_pnt[i].v)&&(!gefunden)) //Dick 25.07.98
       gefunden = i+1;
     
     
   }//for (int i=0;i<anzahl;i++)
   
   return gefunden;
   
   
}
/***************************************************/
/*******************************************************************/
int Paint::Get_Einf_Position(int hor,int ver)
{
  // Graphisches Editieren:bei mouse_down finden die Einfügeposition  
  // Rückgabe:  nicht gefunden=0 // int i :=position vor der  in ptr_profil-Liste eingefügt wird
  // gefunden ==1 => am Anfang , ob es am Ende eingefügt werden soll, wird in Koord_Einf entschieden
  // Das Suchverfahren ist auf die Gleichung der Sekante  
  //              Y=((f(x(k)-f(x(k-1))/(x(k)-x(k-1)))*(X-x(k))-f(k) ,
  // unter Bedienung  das X zwieschen x(k) und x(k-1) liegt, gebaut.
  int gefunden =0;  
  int anzahl,
    change=0;
  
  double V_Test, V_Test_Sekante,V_Test_Vektor,//Y-Koordinate für X=hor
    fehler_ab_min,fehler_ab_i;//Fehlerabschätzungen
  
  /*Berechnung von anzahl durch zählen in profil_pnt ist sicherer ,13.03.96*/
  
  
  anzahl = ds_info[1];
  // Korrektur, falls BCE_NAN -Werte im Gelände
  for (int k=anzahl;k>=0;k--)
		{
		  if ((profil_pnt[k].h==0)&&(profil_pnt[k].v==0))
      {
        anzahl--;
        change=1;
      }
		}
  if (change) anzahl++;
  
  int x_neu,y_neu;
  double mue1,mue2;
  int fehler_pro;
  int xk[3],yk[3];
	 if(anzahl>=2)//Es muß mindestens eine Linie vorhanden sein
     for (int i=1;i<anzahl;i++)
     {
       xk[0]=profil_pnt[i-1].h; yk[0]=profil_pnt[i-1].v;
       xk[1]=profil_pnt[i].h;   yk[1]=profil_pnt[i].v;
       xk[2]=hor;               yk[2]=ver;
       fehler_pro=bce_Projektion(xk,yk,&x_neu,&y_neu,&mue1,&mue2);
       if( (hor <= profil_pnt[i].h && hor >= profil_pnt[i-1].h)  ||
         (hor >= profil_pnt[i].h && hor <= profil_pnt[i-1].h)      )
       {
         if((profil_pnt[i].h - profil_pnt[i-1].h)!=0) 
           V_Test_Sekante=((double)(profil_pnt[i].v - profil_pnt[i-1].v)/(double)(profil_pnt[i].h - profil_pnt[i-1].h))
           * (double)(hor - profil_pnt[i].h) + profil_pnt[i].v;
         else
           V_Test_Sekante=ver;
         V_Test_Sekante=fabs((double)(ver - V_Test_Sekante));
       }
       else
         V_Test_Sekante=-BCE_NAN;
       if(!fehler_pro&&(mue1>=0&&mue1<=1))
       {            
         V_Test_Vektor=sqrt(pow(hor-x_neu,2.)+pow(ver-y_neu,2.));
       }
       else
         V_Test_Vektor=-BCE_NAN;
       if(V_Test_Vektor<=V_Test_Sekante)
         V_Test=V_Test_Vektor;
       else
         V_Test=V_Test_Sekante;
       if(V_Test_Sekante!=-BCE_NAN ||  V_Test_Vektor!=-BCE_NAN)
       {
         if(gefunden==0)
         {
           fehler_ab_min=V_Test;
           gefunden=i+1;
         }
         else
         {
           fehler_ab_i= V_Test;
           if(fehler_ab_i < fehler_ab_min)
           {
             gefunden=i+1;
             fehler_ab_min=fehler_ab_i;
           }
         }
       }         
     }//for (int i=0;i<anzahl;i++)
     if(hor < profil_pnt[0].h)  //am Anfang
       return 1;
     if(hor > profil_pnt[anzahl-1].h)
     {
       if(typ[scr.datensatz]==UK_BRUECKE||typ[scr.datensatz]==OK_BRUECKE || typ[scr.datensatz]==BUHNE ||
         typ[scr.datensatz]==OK_WEHRS || typ[scr.datensatz]==OK_GELAENDE)
         return ds_info[scr.datensatz]+1; //am Ende
       else
         return ds_info[1]+1; //am Ende
     }
     if(gefunden > 0)
     {
       if(zoom_info.level>0 &&!(typ[scr.datensatz]==UK_BRUECKE||typ[scr.datensatz]==OK_BRUECKE ||typ[scr.datensatz]==BUHNE || //Dick 19.04.99
         typ[scr.datensatz]==OK_WEHRS || typ[scr.datensatz]==OK_GELAENDE))
         gefunden=gefunden+zoom_info.pos_station_min-1;//Korrektur für Zoom-Modus 
       return gefunden;
     }
     else
       if(typ[scr.datensatz]==UK_BRUECKE||typ[scr.datensatz]==OK_BRUECKE || typ[scr.datensatz]==BUHNE ||
         typ[scr.datensatz]==OK_WEHRS || typ[scr.datensatz]==OK_GELAENDE)
         return ds_info[scr.datensatz]+1; //am Ende
       else 
         return ds_info[1]+1; //am Ende
       
}
/***************************************************/
int Paint::Get_MRPosition(int hor,int ver)
{
  // Graphisches Editieren:bei mouse_down finden des Stationswertes nur für Rauhigkeit
  // Rückgabe:  nicht gefunden=0 // int i :=position in ptr_profil-Liste
  // es wird versucht den größten Wert zu finden
  int gefunden =0;
  int max=0;
  
  for (int i=0;i<ds_info[1];i++)
    if ((profil_pnt[i].h  <= hor)&&(profil_pnt[i+1].h  >hor))
    {
      if (!gefunden)
        gefunden = i+1;
      if(profil_pnt[i].h>max)
        max = profil_pnt[i].h;
    }
    if (!gefunden)  // letzten Stationswert testen
    {
      //	if (hor >=545)
      {
        for (i=ds_info[1];i>=0;i--)
          if ((profil_pnt[i].h!=0)&&(profil_pnt[i].v!=0) )
            if ((!gefunden)&&(hor>=profil_pnt[i].h))
              gefunden = i+1;
      }
    }
    return gefunden;
}

/***************************************************/
void Paint::Paint117(WINDOW win, MinMax *pmm)
/*
Paint zeichnet die Profillinie -Datensatz :GELAENDE muss an erster
Position in ptr_anfang stehen !!
Bildschirm-Daten stehen anschl. in  profil_pnt[]
*/
{
  DRAW_CTOOLS rect_tools;
  PNT pt,h_pnt; 
  int i=0,
    rueckwaerts=0, //Bley 20.10.2000
    zaehler=0; //Bley 20.10.2000
  double factorX,factorY;
  
  ptr_anfang = list->get_anfang();
  ptr_profil = ptr_anfang;
  if( ptr_profil )
    pp = ptr_profil->datensatz;
  else
    pp = NULL;
  
  factorX = WspDrawDimensions.DELTA_X / pmm->distanceX;
  factorY = WspDrawDimensions.Y / pmm->distanceY;
  
  zoom_left_x = list->get_zoom_ptr( ANFANG );
  zoom_right_x = list->get_zoom_ptr( ENDE );
  while( pp )
  {
    if( pp->y != BCE_NAN && 
      ( zoom_info.level == 0 || 
      ( zoom_left_x && zoom_left_x->ds_nr <= pp->ds_nr && zoom_right_x && pp->ds_nr <= zoom_right_x->ds_nr ) 
      )
      )
    {
      profil_pnt[i].h = (short)(( (pp->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      profil_pnt[i].v = (short)( WspDrawDimensions.vertDist - ( (pp->y - pmm->minY) * factorY ));
      
      pt.h = profil_pnt[i].h;
      pt.v = WspDrawDimensions.vertMax;
      xvt_app_get_default_ctools(&rect_tools);
      rect_tools.pen.width = WspDrawDimensions.wspLineWidth1;
      rect_tools.pen.color = COLOR_GRAY;
      xvt_dwin_set_draw_ctools(win, &rect_tools);
      
      xvt_dwin_draw_set_pos(win,profil_pnt[i]);
      xvt_dwin_draw_line(win,pt);
      if( i == 0 )
      {
        h_pnt.h=(short)(pp->x);
        h_pnt.v=(short)(pp->y);
      }
      i++;
    }; // if pp->y != BCE_NAN && ...
    
    pp = pp->next_ds;
  }; // while pp
  
  for(int j1=0;j1<STRANGANZAHL;j1++)
  {
    pnt_save[j1].h=profil_pnt[j1].h;
    pnt_save[j1].v=profil_pnt[j1].v;      
  }
  xvt_app_get_default_ctools(&rect_tools);
  rect_tools.pen.width = WspDrawDimensions.wspLineWidth2;
  rect_tools.pen.color = COLOR_MAGENTA;
  xvt_dwin_set_draw_ctools(win, &rect_tools);
  
  xvt_dwin_draw_polyline(win, profil_pnt,i);
}
/**************************************************************************/
void Paint::DrawTrennflaechen(WINDOW win, MinMax *pmm,int datensatz,long color)
{
  double factorX; 		 // factorY= 220 / pmm->distanceY;
  PNT pnt;
  DRAW_CTOOLS rect_tools;
  
  factorX= WspDrawDimensions.DELTA_X / pmm->distanceX;
  
  xvt_app_get_default_ctools(&rect_tools);
  rect_tools.pen.width = WspDrawDimensions.wspLineWidth2;
  rect_tools.pen.color = color;
  xvt_dwin_set_draw_ctools(win, &rect_tools);
  
  ptr_profil=ptr_anfang;
  while((ptr_profil->ds_nummer < datensatz) && (ptr_profil!=NULL))
    ptr_profil=ptr_profil->next;
  pp=ptr_profil->datensatz;
  
  while(pp!=NULL)
  {
    if ((pp->y !=BCE_NAN) && (pp->x!=BCE_NAN))
    {
      pnt.h=(short) (( (pp->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      pnt.v= WspDrawDimensions.DY;
      pp->x_pnt = pnt.h;  //Grafikkoordinaten sichern
      pp->y_pnt = WspDrawDimensions.offset;
      xvt_dwin_draw_set_pos(win,pnt);
      
      pnt.v= WspDrawDimensions.vertMax;
      xvt_dwin_draw_line(win,pnt);
    }
    pp=pp->next_ds;
  }
}
/**************************************************************************/
void Paint::DrawRauhigkeit(WINDOW win, MinMax *pmm,int datensatz)
/*  Rauhigkeitswerte werden rechts von der Station gezeichnet
Darstellung :  (n-1) Werte
28.06.95  n.Bespr. Dr.Pasche                                  

  ----------------------------------------------------------------------------
  10.07.97
  Korrektur wegen Probleme mit Grafikkarte S3-Virge in Verbindung
  mit xvt_dwin_draw_rect()
  
    ---> ersetzt durch API-Funktionen, 
	   jedoch Rectangle()-Funktion macht ebenfalls Probleme, daher:  Polygon()
     ----------------------------------------------------------------------------
     
       */
{
  double factorX;
  RCT rct;
  DRAW_CTOOLS rect_tools;
  double max;
  
  if (pmm->distanceX != 0.0)
    factorX= WspDrawDimensions.DELTA_X / pmm->distanceX;
  else 
  {
    return;
  }
  Koord *pp_help;
  
  
  xvt_app_get_default_ctools(&rect_tools);
  rect_tools.pen.width = WspDrawDimensions.wspLineWidth2;
  rect_tools.pen.color = COLOR_LTGRAY;
  rect_tools.fore_color =COLOR_LTGRAY;
  rect_tools.brush.color =COLOR_LTGRAY;
  rect_tools.brush.pat = PAT_SOLID;
  
  xvt_dwin_set_draw_ctools(win, &rect_tools);
  
  
  ptr_profil=ptr_anfang;
  while((ptr_profil->ds_nummer < datensatz) && (ptr_profil!=NULL))
    ptr_profil=ptr_profil->next;
  pp=ptr_profil->datensatz;
  
  if(pp!=NULL)//Dick 6.08.98 Sonst Absturz
  {
    max = pp->y;
    while (pp->next_ds !=NULL)
    {
      if (pp->next_ds->y >max)
        max = pp->next_ds->y;
      pp=pp->next_ds;
    }
    
    if (max <=0.0) max =1;
    pmm->sec_maxY = max;     //max.Rauhigkeit zwischenspeichern für editieren
    
    pp=ptr_profil->datensatz;
  }
  while(pp != NULL)
  {
    if ((pp->next_ds !=NULL)&&(pp->x == pp->next_ds->x))
			 {	  // 	DL = 0;  DR=0;
      ; // xvt_dm_post_note("Fehler in: paint.cpp/");
			 }
    else
      if(pp->y > 0.0)
      {
        rct.top    =(short) ((WspDrawDimensions.vertMax+WspDrawDimensions.AB)- (WspDrawDimensions.res1* pp->y /max));//337 - (80* pp->y /max);
        rct.bottom = WspDrawDimensions.vertMax;
        rct.left   =(short) (((pp->x - pmm->minX) * factorX) + WspDrawDimensions.DX) ;
        
        if (rct.top >rct.bottom)   //Fehler abfangen:invalid rct-format
          rct.top =rct.bottom;
        
        if( (pp->next_ds !=NULL)&&(pp->next_ds->y !=BCE_NAN))
        {
          if (pp->x < pp->next_ds->x)
            rct.right  = (short)(( (pp->next_ds->x   -pmm->minX) * factorX) + WspDrawDimensions.DX );
          else
          {
            rct.right  = rct.left;
            rct.left = (short)(((pp->next_ds->x   -pmm->minX) * factorX) + WspDrawDimensions.DX);
          }
        }
        else
          if(pp->next_ds !=NULL)//Dick 19.01.00 anstatt das was siehe unten
          {
            pp_help = pp->next_ds;
            while ((pp_help->next_ds !=NULL)&&(pp_help->y ==BCE_NAN))
              pp_help = pp_help->next_ds;
            
            if ((pp_help!=NULL)&&(pp->x < pp_help->x))
              rct.right  =(short) (((pp_help->x   -pmm->minX) * factorX) + WspDrawDimensions.DX );
            else  if ((pp_help!=NULL)&&(pp_help->x==BCE_NAN))
            {
              rct.right  = rct.left;
              rct.left =(short) (((pp_help->x   -pmm->minX) * factorX) + WspDrawDimensions.DX);
            }
          }
          
          if (rct.left+3 <= rct.right-1)
          {
            rct.left = rct.left+3;
            rct.right = rct.right-1;
          }
          if (rct.right >(INT) (WspDrawDimensions.DX+WspDrawDimensions.DELTA_X))  
            rct.right = WspDrawDimensions.DX+WspDrawDimensions.DELTA_X;
          
          if (rct.left<(INT)WspDrawDimensions.DX) 
            rct.left=(INT)WspDrawDimensions.DX;
          if (rct.right<(INT)WspDrawDimensions.DX) 
            rct.right=(INT)WspDrawDimensions.DX;
          
          if ((rct.left<= rct.right)&&(rct.top <=rct.bottom))
          {
            xvt_dwin_draw_rect(win, &rct);
          }
          if(!WspDrawDimensions.isPrinting)
          {
            pp->x_pnt=rct.left;   //linke Kante sichern
            pp->y_pnt=rct.right;  //rechte Kante sichern
          }
          
      }
      pp = pp->next_ds;
  } //-while
}
/**************************************************************************/
void Paint::DurchstBereiche(WINDOW win, MinMax *pmm,int datensatz)
{
  PNT pnt,h_pnt;
  DRAW_CTOOLS rect_tools;
  int left,right;
  
  double factorX = WspDrawDimensions.DELTA_X / pmm->distanceX;
  double factorY= WspDrawDimensions.Y / pmm->distanceY;
  
  xvt_app_get_default_ctools(&rect_tools);
  rect_tools.pen.width = WspDrawDimensions.wspLineWidth2;
  rect_tools.pen.color = COLOR_BLUE;
  rect_tools.mode = M_COPY;
  xvt_dwin_set_draw_ctools(win, &rect_tools);
  
  Profildatei* ptr_profil = ptr_anfang;
  while( ptr_profil != NULL && ptr_profil->ds_nummer < datensatz )
    ptr_profil = ptr_profil->next;
  
  if( ptr_profil == NULL )  // falls Datensatz nicht existiert, gleich zurück
    return;
  
  pp = ptr_profil->datensatz;
  if( pp != NULL )
  {
    if(pp->x!=BCE_NAN)
    {
      pnt.h=(short) (( (pp->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      pnt.v= WspDrawDimensions.DY;
      left = pp->x_pnt = pnt.h;
      xvt_dwin_draw_set_pos(win,pnt);
      pnt.v= WspDrawDimensions.vertMax;
      xvt_dwin_draw_line(win,pnt);
    }
    if (pp->next_ds !=NULL)
		  {
      pp=pp->next_ds;
      if(pp->x!=BCE_NAN)
      {
        pnt.h=(short) (( (pp->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
        right = pp->x_pnt = pnt.h;
        pnt.v= WspDrawDimensions.DY;
        xvt_dwin_draw_set_pos(win,pnt);
        pnt.v= WspDrawDimensions.vertMax;
        xvt_dwin_draw_line(win,pnt);
        
        xvt_app_get_default_ctools(&rect_tools);
        rect_tools.pen.width = WspDrawDimensions.wspLineWidth1;
        rect_tools.pen.color = COLOR_BLUE;
        xvt_dwin_set_draw_ctools(win, &rect_tools);
        h_pnt.v = WspDrawDimensions.DY;  //(33-35)
        h_pnt.h = left;
        xvt_dwin_draw_set_pos(win,h_pnt);
        h_pnt.h = right;
        xvt_dwin_draw_line(win,h_pnt);
      }
		  }
  }
}

/***************************************************/
void Paint::PaintBruecke(WINDOW win, MinMax *pmm,int datensatz,COLOR color)
{
  DRAW_CTOOLS rect_tools;
  int i=0,k,j1=0,j2=0;
  double factorX,factorY;
  
  ptr_profil = ptr_anfang;
  while((ptr_profil->ds_nummer < datensatz) && (ptr_profil!=NULL))
    ptr_profil=ptr_profil->next;
  pp=ptr_profil->datensatz;
  factorX= WspDrawDimensions.DELTA_X / pmm->distanceX;
  factorY= WspDrawDimensions.Y / pmm->distanceY;
  
  while( pp )
  {
    if( pp->y != BCE_NAN && ( zoom_info.level == 0 || ( zoom_left_x && zoom_left_x->x <= pp->x &&
      zoom_right_x && pp->x <= zoom_right_x->x )
      ) )
    {
      profil_pnt[i].h =(short) (((pp->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      //Dick 19.04.99
      if(profil_pnt[i].h < (int)WspDrawDimensions.DX || pp->x - pmm->minX<0.0 )//Dick 15.04.99 damit Grafik auserhalb nicht geht
        profil_pnt[i].h=WspDrawDimensions.DX;//Dick 4.08.99
      if(profil_pnt[i].h>(int)(WspDrawDimensions.DELTA_X + WspDrawDimensions.DX) ||  pmm->maxX-pp->x<0.0)
        profil_pnt[i].h=(WspDrawDimensions.DELTA_X + WspDrawDimensions.DX);
      
      if(pp->x - pmm->minX<0.0)
        j1++;
      if(pmm->maxX-pp->x<0.0)
        j2++;
      profil_pnt[i].v =(short) (WspDrawDimensions.vertDist -(  (pp->y - pmm->minY) * factorY));
      i++;
    }
    pp = pp->next_ds;
  }
  if(j1==i || j2==i)
    i=0;
  
  k=i;
  while((profil_pnt[k].h!=0||profil_pnt[k].v!=0)&&k<STRANGANZAHL)//Dick 18.08.98
  {
    profil_pnt[k].h=0;
    profil_pnt[k].v=0;
    k++;
  }
  xvt_app_get_default_ctools(&rect_tools);
  rect_tools.pen.width = WspDrawDimensions.wspLineWidth2;
  rect_tools.pen.color = color;
  rect_tools.brush.pat = PAT_CROSS;
  xvt_dwin_set_draw_ctools(win, &rect_tools);
  xvt_dwin_draw_polyline(win, profil_pnt,i);
}
//**************************************************************************
/***************************************************/
void Paint::PaintStation(WINDOW win, MinMax *pmm,int datensatz,COLOR color)
{
  DRAW_CTOOLS rect_tools;
  PNT pnt;
  PNT pnt_poly[STRANGANZAHL];
  int i=0,k,j1;
  double factorX,factorY;
  for(j1=0;j1<STRANGANZAHL;j1++)
  {      
    pnt_poly[j1].h=0;
    pnt_poly[j1].v=0;
  }
  ptr_profil = ptr_anfang;
  while((ptr_profil->ds_nummer < datensatz) && (ptr_profil!=NULL))
    ptr_profil=ptr_profil->next;
  pp=ptr_profil->datensatz;
  
  if(WspDrawDimensions.isPrinting)
    int dummy=0;
  
  factorX= WspDrawDimensions.DELTA_X / pmm->distanceX;
  factorY= WspDrawDimensions.Y / pmm->distanceY;
  zoom_left_x = list->get_zoom_ptr(ANFANG);
  zoom_right_x = list->get_zoom_ptr(ENDE);
  while (pp != NULL)
  {
    if (pp->y != BCE_NAN)
    {
      profil_pnt[i].h =(short) (((pp->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      if(profil_pnt[i].h < (int)WspDrawDimensions.DX || ((pp->x - pmm->minX<0.0 || pmm->maxX-pp->x<0.0) && i%2==0))//Dick 15.04.99 damit Grafik auserhalb nicht geht
        profil_pnt[i].h=WspDrawDimensions.DX;
      if(profil_pnt[i].h>(int)(WspDrawDimensions.DELTA_X + WspDrawDimensions.DX) || ((pp->x - pmm->minX<0.0 || pmm->maxX-pp->x<0.0) && i%2!=0))
        if(pp->pre_ds!=NULL)
          if((pp->pre_ds->x - pmm->minX<0.0  && pp->x - pmm->minX<0.0)||(pmm->maxX-pp->pre_ds->x<0.0 && pmm->maxX-pp->x<0.0) )
            profil_pnt[i].h=WspDrawDimensions.DX;
          else
            profil_pnt[i].h=(WspDrawDimensions.DELTA_X + WspDrawDimensions.DX);
          profil_pnt[i].v =(short) (WspDrawDimensions.vertDist -(  (pp->y - pmm->minY) * factorY));
          i++;
		  }
    pp = pp->next_ds;
  }
  k=i;
  while((profil_pnt[k].h!=0||profil_pnt[k].v!=0)&&k<STRANGANZAHL)//Dick 18.08.98
  {
    profil_pnt[k].h=0;
    profil_pnt[k].v=0;
    k++;
  }
  xvt_app_get_default_ctools(&rect_tools);
  rect_tools.pen.width = WspDrawDimensions.wspLineWidth2;
  rect_tools.pen.color = color;
  rect_tools.brush.pat = PAT_CROSS;
  rect_tools.brush.color = COLOR_GREEN;
  xvt_dwin_set_draw_ctools(win, &rect_tools);
  j1=0;
  for(int j=0;j<i;j+=2)
  {
    pnt.v=profil_pnt[j].v; 
    pnt.h=profil_pnt[j].h;
    xvt_dwin_draw_set_pos(win,pnt);
    pnt.v=profil_pnt[j+1].v; 
    pnt.h=profil_pnt[j+1].h;
    xvt_dwin_draw_line(win,pnt);
    k=0;
    pnt_poly[j1].h=profil_pnt[j].h;
    pnt_poly[j1].v=profil_pnt[j].v+1;
    j1++;
    while((pnt_save[k].h!=0||pnt_save[k].v!=0)&&k<STRANGANZAHL)//Dick 18.08.98
    {
      if(pnt_save[k].h>=profil_pnt[j].h && pnt_save[k].h<=profil_pnt[j+1].h && pnt_save[k].v >= profil_pnt[j].v)
      {
        pnt_poly[j1].h=pnt_save[k].h;
        pnt_poly[j1].v=pnt_save[k].v-1;
        j1++;
      }
      k++;
    }
    pnt_poly[j1].h=profil_pnt[j+1].h;
    pnt_poly[j1].v=profil_pnt[j+1].v+1;
    j1++;
    
  }
  
  HWND hWnd;
  HDC hDC ;
  if(WspDrawDimensions.isPrinting==0)
  {
    hWnd = (HWND)xvt_vobj_get_attr(win,ATTR_NATIVE_WINDOW);
    hDC = GetDC(hWnd);
  }
  else
  {
    PRINTDLG pdlg;
    pdlg.hDevMode=NULL;
    pdlg.hDevNames=NULL;
    pdlg.Flags =PD_RETURNDC | PD_RETURNDEFAULT ;
    PrintDlg(&pdlg);
    hDC= pdlg.hDC;
  }
  HBRUSH hBrush, hOldBrush;
  HPEN hPen, hOldPen;
  POINT *pt_array;
  LOGBRUSH lplb;
  pt_array=new POINT[j1];
  for(i=0;i<j1;i++)
  {
    pt_array[i].x=pnt_poly[i].h;
    pt_array[i].y=pnt_poly[i].v;
  }
  lplb.lbStyle=BS_HATCHED ;
  lplb.lbColor=RGB(0,127,255);
  lplb.lbHatch=HS_DIAGCROSS ;
  hBrush=CreateBrushIndirect(&lplb);
  hPen = (HPEN)GetStockObject(NULL_PEN);
  hOldBrush = (HBRUSH)SelectObject(hDC, hBrush);
  hOldPen = (HPEN)SelectObject(hDC, hPen);
  DWORD err=GetLastError();
  SelectObject(hDC, hOldBrush);
  SelectObject(hDC, hOldPen);
  if(!WspDrawDimensions.isPrinting)
    ReleaseDC(hWnd, hDC);
  delete[] pt_array;
  DeleteObject(hBrush);
}
//**************************************************************************

void Paint::edit_list_pnt(MMP *mmp,MinMax *pmm)
{
/* Ersetzen / editieren einer x-y-Koordinate aus WIN117
	 *** Grafisches Editieren ****						  */
  Koord *pp_neu;
  Profildatei *ptr_prof;
  ptr_anfang = list->get_anfang();//Dick 21.08.98 weil nicht immer rechtzeitlich aktualisiert wird
  ptr_prof = ptr_anfang;
  while ((ptr_prof->ds_nummer < mmp->ds_nummer) &&(ptr_prof != NULL))
    ptr_prof = ptr_prof->next ;
  
  pp_neu=ptr_prof->datensatz;
  
  switch( ptr_prof->profiltyp )
	 {
	 case GELAENDEHOEHE:
     {
       if (mmp->position_mouse_down >0)
         Edit_Gelaende(WIN_117,pp_neu,mmp,pmm);
     }
     break;
   case FLAECHE:
   case GELAENDE2:  // Gelände1 editieren
     {
       pp_neu = ptr_anfang->datensatz; // wieder auf Gelände  setzen
       if (mmp->position_mouse_down >0)
         Edit_Gelaende(WIN_117,pp_neu,mmp,pmm);
     }
     break;
   case RAUHIGKEIT:
   case RAUHIGKEIT_KST:
     //Dick 16.04.99
   case AXM:
   case AYM:
   case DPM:		  
     {
       Highligth_mouse_move(WIN_117,mmp);
     }
     break;
   case TRENNFLAECHEN:
   case BORDVOLL:
   case MODELLGRENZEN:
     Edit_Trennflaechen( WIN_117, pp_neu, mmp, pmm, ptr_prof->profiltyp );
     break;
     
   case BUHNE:
     {
       Edit_Buhnen(WIN_117,pp_neu,mmp,pmm);
     }
     break;
     
   case DURCHST_BEREICH:
     Edit_DurchstBereiche(WIN_117,pp_neu,mmp);
     break;
   case UK_BRUECKE:
   case OK_BRUECKE:
     {
     }
     break;
   default:break;
	 };
}
//**************************************************************************
void Paint::Highligth_mouse_move(WINDOW win,MMP *mmp)
{
  /*       Rauhigkeit markieren       */
  DRAW_CTOOLS rect_tools;
  xvt_app_get_default_ctools(&rect_tools);
  rect_tools.pen.width = WspDrawDimensions.wspLineWidth1;
  rect_tools.pen.color = COLOR_YELLOW;
  rect_tools.fore_color =COLOR_YELLOW;
  rect_tools.brush.color =COLOR_YELLOW;
  rect_tools.brush.pat = PAT_SOLID;
  rect_tools.mode = M_XOR;
  xvt_dwin_set_draw_ctools(win, &rect_tools);
  
  if (mmp->active)
    if (mmp->last_rct.left<=mmp->last_rct.right)
      xvt_dwin_draw_rect(win,&mmp->last_rct ); //alte Grafik löschen
    RCT rct;
    rct.top=WspDrawDimensions.vertDist;  rct.bottom=WspDrawDimensions.vertMax;//Dick 4.08.99
    if ((int)mmp->mouse_down_h>(int)WspDrawDimensions.DX) //Dick 4.08.99
      rct.left =mmp->mouse_down_h;
    else rct.left =WspDrawDimensions.DX; //Dick 4.08.99
    if (mmp->mouse_h<(int)(WspDrawDimensions.DELTA_X + WspDrawDimensions.DX))//Dick 4.08.99
      rct.right = mmp->mouse_h;
    else rct.right = (WspDrawDimensions.DELTA_X + WspDrawDimensions.DX);//Dick 4.08.99
    if (rct.left<=rct.right)
      xvt_dwin_draw_rect(win,&rct );  // neue Grafik zeichnen
    
    
    mmp->last_rct.top = rct.top;   // neue Werte speichern
    mmp->last_rct.bottom = rct.bottom;
    mmp->last_rct.left = rct.left;
    mmp->last_rct.right = rct.right;
    mmp->active = FALSE;
}
/***********************************************************************/

void Paint::Edit_Gelaende(WINDOW win,Koord *pp,MMP *mmp,MinMax *pmm)
{
  DRAW_CTOOLS rect_tools;
  PNT edit_pnt[3];
  Koord *help;
  int i;
  double factorX,factorY;
  char temp[25],temp_x[25];
  
  for (i=1;i<mmp->position_mouse_down;i++)  // Zeiger weiterhängen
    if (pp->next_ds!=NULL)
      pp=pp->next_ds;
    
    
    // alte Grafik löschen (3Punkte) :
    xvt_app_get_default_ctools(&rect_tools);
    rect_tools.pen.width = WspDrawDimensions.wspLineWidth2;
    rect_tools.pen.color = COLOR_WHITE;
    xvt_dwin_set_draw_mode(win,M_XOR);
    xvt_dwin_set_draw_ctools(win, &rect_tools);
    
    factorX= WspDrawDimensions.DELTA_X / pmm->distanceX;
    factorY= WspDrawDimensions.Y / pmm->distanceY;
    i=0;
    if ((pp !=NULL)&&(pp->pre_ds !=NULL))   //alte Linie löschen
    {
      if (pp->pre_ds->y !=BCE_NAN)
      {
        edit_pnt[0].v =(short) (WspDrawDimensions.vertDist -(  (pp->pre_ds->y - pmm->minY) * factorY ));
        edit_pnt[0].h =(short) (((pp->pre_ds->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      }
      else   // Falls Vorgänger Mindouble Wert hat nicht zeichnen
      {
        help =pp->pre_ds;
        while ((help->pre_ds!=NULL)&&(help->y ==BCE_NAN))
          help =help->pre_ds;
        edit_pnt[0].v =(short) ( WspDrawDimensions.vertDist -(  (help->y - pmm->minY) * factorY ));
        edit_pnt[0].h =(short) (((help->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      }
      
      i++;
    }
    if (pp !=NULL)
    {
      edit_pnt[1].h =(short) (( (pp->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      edit_pnt[1].v =(short) ( WspDrawDimensions.vertDist -(  (pp->y - pmm->minY) * factorY ));
      i++;
    }
    if ((pp !=NULL)&&(pp->next_ds !=NULL))
    {
      if (pp->next_ds->y !=BCE_NAN)
      {
        edit_pnt[2].v =(short) (WspDrawDimensions.vertDist -(  (pp->next_ds->y - pmm->minY) * factorY ));
        edit_pnt[2].h =(short) (((pp->next_ds->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      }
      else // Falls Vorgänger Mindouble Wert hat nicht zeichnen
      {
        help =pp->next_ds;
        while ((help->next_ds!=NULL)&&(help->y ==BCE_NAN))
          help =help->next_ds;
        edit_pnt[2].v =(short) (WspDrawDimensions.vertDist -(  (help->y - pmm->minY) * factorY ));
        edit_pnt[2].h =(short) (((help->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      }
      
      i++;
    }
    if (pp !=NULL)
      if (pp->pre_ds ==NULL)//Dick 15.01.99
      {
        edit_pnt[0].v=edit_pnt[1].v;
        edit_pnt[0].h=edit_pnt[1].h;
        edit_pnt[1].v=edit_pnt[2].v;
        edit_pnt[1].h=edit_pnt[2].h;
      }
      if (pp !=NULL)
        if (pp->pre_ds !=NULL && zoom_info.level>0)//Dick 15.01.99
        {
          if(pp->ds_nr==zoom_info.pos_station_min)
          {
            edit_pnt[0].v=edit_pnt[1].v;
            edit_pnt[0].h=edit_pnt[1].h;
            edit_pnt[1].v=edit_pnt[2].v;
            edit_pnt[1].h=edit_pnt[2].h;
            i=2;
          }
          else
            if(pp->ds_nr==zoom_info.pos_station_max)           
              i=2;
        }
        xvt_dwin_draw_polyline(WIN_117, edit_pnt,i);
        if(mmp->position >0)
        {
          profil_pnt[mmp->position-1].v=edit_pnt[1].v;
          profil_pnt[mmp->position-1].h=edit_pnt[1].h;
        }
        //  neuen Wert eintragen
        sprintf(temp,"%.4lf",mmp->vertikal);
        sprintf(temp_x,"%.4lf",mmp->horizontal);
        //gcvt(mmp->vertikal,10,temp);
        for (i=0;i<=(INT)strlen(temp);i++)
        {
          if (temp[i]=='.')
            temp[i+4]='\0';
          if (temp[i]==',')
          {
            temp[i]='.' ;
            temp[i+4]='\0';
          }
        }
        if (pp!=NULL)
        {
          if(checkbox_edit && checkbox_edit_v && pp->y!=BCE_NAN) //Dick 29.04.99                
            pp->y = atof(temp);      
          if(checkbox_edit && checkbox_edit_h && pp->y!=BCE_NAN) //Dick 29.04.99
            list->Change_Station_Allg(atof(temp_x),pp->x,list->Get_Station_Num(pp->x,1),mmp->position);
        }
        // neue Grafik updaten (3Punkte) :
        xvt_app_get_default_ctools(&rect_tools);
        rect_tools.pen.width = WspDrawDimensions.wspLineWidth2;
        rect_tools.pen.color = COLOR_MAGENTA;
        xvt_dwin_set_draw_mode(win,M_XOR);
        xvt_dwin_set_draw_ctools(win, &rect_tools);
        i=0;
        if (pp!=NULL)
          if (pp->pre_ds !=NULL)
          {
            if (pp->pre_ds->y !=BCE_NAN)
            {
              edit_pnt[0].v =(short) (WspDrawDimensions.vertDist -(  (pp->pre_ds->y - pmm->minY) * factorY ));
              edit_pnt[0].h =(short) (((pp->pre_ds->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
            }
            else   // Falls Vorgänger Mindouble Wert hat nicht zeichnen
            {
              help =pp->pre_ds;
              while ((help->pre_ds!=NULL)&&(help->y ==BCE_NAN))
                help =help->pre_ds;
              edit_pnt[0].v =(short) ( WspDrawDimensions.vertDist -(  (help->y - pmm->minY) * factorY ));
              edit_pnt[0].h =(short)(((help->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
            }
            i++;
          }
          if (pp !=NULL)
          {
            edit_pnt[1].h =(short) (( (pp->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
            edit_pnt[1].v =(short) ( WspDrawDimensions.vertDist -(  (pp->y - pmm->minY) * factorY) );
            i++;
          }
          if (pp!=NULL)
            if (pp->next_ds !=NULL)
            {
              if (pp->next_ds->y !=BCE_NAN)
              {
                edit_pnt[2].v =(short) (WspDrawDimensions.vertDist -(  (pp->next_ds->y - pmm->minY) * factorY ));
                edit_pnt[2].h =(short) (((pp->next_ds->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
              }
              else // Falls Vorgänger Mindouble Wert hat nicht zeichnen
              {
                help =pp->next_ds;
                while ((help->next_ds!=NULL)&&(help->y ==BCE_NAN))
                  help =help->next_ds;
                edit_pnt[2].v =(short) ( WspDrawDimensions.vertDist -(  (help->y - pmm->minY) * factorY ));
                edit_pnt[2].h =(short) (((help->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
              }
              i++;
            }
            if (pp!=NULL)
              if (pp->pre_ds ==NULL)//Dick 15.01.99
              {
                edit_pnt[0].v=edit_pnt[1].v;
                edit_pnt[0].h=edit_pnt[1].h;
                edit_pnt[1].v=edit_pnt[2].v;
                edit_pnt[1].h=edit_pnt[2].h;
              }
              if (pp!=NULL)
                if (pp->pre_ds !=NULL && zoom_info.level>0)//Dick 15.01.99
                {
                  if(pp->ds_nr==zoom_info.pos_station_min)
                  {
                    edit_pnt[0].v=edit_pnt[1].v;
                    edit_pnt[0].h=edit_pnt[1].h;
                    edit_pnt[1].v=edit_pnt[2].v;
                    edit_pnt[1].h=edit_pnt[2].h;
                    i=2;
                  } 
                  else
                    if(pp->ds_nr==zoom_info.pos_station_max)           
                      i=2;           
                }
                
                xvt_dwin_draw_polyline(win, edit_pnt,i);
}
/*********************************************************************/
void Paint::Edit_Rauhigkeit(double faktor,int typ,MMP *mmp)
{
  Koord *pp_neu;
  Profildatei *ptr_prof;
  double wert;
  
  ptr_prof = ptr_anfang;
  while ((ptr_prof->ds_nummer < mmp->ds_nummer) &&(ptr_prof != NULL))
    ptr_prof = ptr_prof->next ;
  
  pp_neu=ptr_prof->datensatz;
  while ((pp_neu->ds_nr < mmp->position_mouse_down)&&(pp_neu->next_ds !=NULL))
    pp_neu = pp_neu->next_ds;
  
  while ((pp_neu !=NULL)&&(pp_neu->ds_nr <= mmp->position_mouse_up))
  {
    switch (typ)
    {
    case OFFSET:
      {
        //			 if (pp_neu->y !=BCE_NAN)
        {
          wert = pp_neu->y + faktor;
          if (wert >0)
            pp_neu->y = wert;
          else
            pp_neu->y = 0.0;
        }
      }
      break;
    case FAKTOR:
      {
        //			 if (pp_neu->y !=BCE_NAN)
        {
          wert = pp_neu->y * faktor;
          if (wert >0)
            pp_neu->y = wert;
          else
            pp_neu->y = 0.0;
        }
      }
      break;
    case NEU:
      {
        //			 if (pp_neu->y !=BCE_NAN)
        {
          if (faktor >0)
            pp_neu->y = faktor;
          else
            pp_neu->y =0.0;
        }
      }
      break;
    }
    pp_neu = pp_neu->next_ds;
  }
}
/*********************************************************************/
void Paint::Edit_Rauhigkeit(double faktor,int typ,double anfang,double ende,int datensatz)
{    //Manuelles Editieren der Rauhigkeit //Dick 5.08.98
  Koord *pp_neu;
  Profildatei *ptr_prof;
  double wert;
  
  ptr_prof =  list->get_anfang();
  while ((ptr_prof->ds_nummer < datensatz) &&(ptr_prof != NULL))
    ptr_prof = ptr_prof->next ;
  
  pp_neu=ptr_prof->datensatz;
  while ((pp_neu->x < anfang)&&(pp_neu->next_ds !=NULL))
    pp_neu = pp_neu->next_ds;
  
  while ((pp_neu !=NULL)&&(pp_neu->x <= ende))
  {
    switch (typ)
    {
    case OFFSET:
      {
        //			 if (pp_neu->y !=BCE_NAN)
        {
          wert = pp_neu->y + faktor;
          if (wert >0)
            pp_neu->y = wert;
          else
            pp_neu->y = 0.0;
        }
      }
      break;
    case FAKTOR:
      {
        //			 if (pp_neu->y !=BCE_NAN)
        {
          wert = pp_neu->y * faktor;
          if (wert >0)
            pp_neu->y = wert;
          else
            pp_neu->y = 0.0;
        }
      }
      break;
    case NEU:
      {
        //			 if (pp_neu->y !=BCE_NAN)
        {
          if (faktor >0)
            pp_neu->y = faktor;
          else
            pp_neu->y =0.0;
        }
      }
      break;
    }
    pp_neu = pp_neu->next_ds;
  }
}
void Paint::Edit_Trennflaechen( WINDOW win, Koord* pp, MMP* mmp, MinMax* pmm, long typ )
{ 
  double links,rechts;
  
  if( dlg_sonderprofil == NULL_WIN )
  {
    if( !xvt_dlg_create_res( WD_MODELESS, DLG_143, EM_ALL, DLG_143_eh, typ ) )
      xvt_dm_post_error( "Can't open dialog143" );
  };
  
  if( pp->y == 1 || pp->y == 3 )
    links = pp->x;
  else	
    links = 0.0;
  
  if( links == mmp->hor_m_down )  // Wert gefunden ->editieren
  {
    pp->y_pnt = 1;
    pp->next_ds->y_pnt = 0;
    mmp->active =TRUE;
  }
  
  
  if( pp->next_ds->y == 2 || pp->next_ds->y == 4 )
    rechts = pp->next_ds->x;
  else 
    rechts = 0.0;

  if( rechts == mmp->hor_m_down )  // Wert gefunden ->editieren
  {
    pp->next_ds->y_pnt = 1;
    pp->y_pnt=0;
    mmp->active =TRUE;
  }
  
  if( mmp->active )
  {
    PNT pnt;
    if( pp->y_pnt == 1 )
      pnt.h = pp->x_pnt; // alte Koord.
    else if( pp->next_ds->y_pnt == 1 )
      pnt.h = pp->next_ds->x_pnt;
    //----------------------------------
    
    DRAW_CTOOLS rect_tools;
    
    xvt_app_get_default_ctools( &rect_tools );
    rect_tools.pen.width = WspDrawDimensions.wspLineWidth3;
    rect_tools.pen.color = COLOR_RED;
    xvt_dwin_set_draw_ctools(win, &rect_tools);
    xvt_dwin_set_draw_mode(win,M_XOR);
    
    //alte Grafik löschen
    pnt.v= WspDrawDimensions.DX;//Dick 4.08.99
    if ((pnt.h>=(int)WspDrawDimensions.DX)&&(pnt.h<=(int)(WspDrawDimensions.DELTA_X + WspDrawDimensions.DX)))
    {
      xvt_dwin_draw_set_pos(win,pnt);
      pnt.v= WspDrawDimensions.vertMax;
      xvt_dwin_draw_line(win,pnt);
    }
    // neue Trennfläche zeichnen
    pnt.h = mmp->mouse_h;
    pnt.v= WspDrawDimensions.DX;//Dick 4.08.99
    if( pp->y_pnt == 1 || pp->next_ds->y_pnt == 1 )
    {
      if( pnt.h >= (int)WspDrawDimensions.DX && pnt.h <= (int)( WspDrawDimensions.DELTA_X + WspDrawDimensions.DX) )//Dick 4.08.99
      {
        xvt_dwin_draw_set_pos( win,pnt );
        pnt.v= WspDrawDimensions.vertMax;
        xvt_dwin_draw_line(win,pnt);
      }
    }
      
    mmp->last_rct.left = mmp->mouse_h;
      //----------------------------------------------------------
    if (pp->y_pnt==1)
      pp->x_pnt = pnt.h;
    
    if( !pp || !pp->next_ds )
      return;

    double oldLeftVal = pp->x;
    double oldRightVal = pp->next_ds->x;

    double newLeftVal = oldLeftVal;
    double newRightVal = oldRightVal;

    if( pp->y_pnt == 1 )  //links
      newLeftVal = mmp->horizontal;
          
    if( pp->next_ds->y_pnt == 1 )
      pp->next_ds->x_pnt = pnt.h;

    if( pp->next_ds->y_pnt == 1 ) // rechts
      newRightVal = mmp->horizontal;

    if( newLeftVal != oldLeftVal || newRightVal != oldRightVal )
      setValuesDlg143( newLeftVal, newRightVal );
  }
} // Edit_Trennflaechen
/*********************************************************************/
void Paint::Edit_Buhnen(WINDOW win,Koord *pp,MMP *mmp,MinMax *pmm)
{
  DRAW_CTOOLS rect_tools;
  PNT edit_pnt[3];
  Koord *help;
  int i;
  double factorX,factorY;
  char temp[25],temp_x[25];
  
  for (i=1;i<mmp->position_mouse_down;i++)  // Zeiger weiterhängen
    if (pp->next_ds!=NULL)
      pp=pp->next_ds;
    
    
    // alte Grafik löschen (3Punkte) :
    xvt_app_get_default_ctools(&rect_tools);
    rect_tools.pen.width = WspDrawDimensions.wspLineWidth2;
    rect_tools.pen.color = COLOR_WHITE;
    xvt_dwin_set_draw_mode(win,M_XOR);
    xvt_dwin_set_draw_ctools(win, &rect_tools);
    
    factorX= WspDrawDimensions.DELTA_X / pmm->distanceX;
    factorY= WspDrawDimensions.Y / pmm->distanceY;
    i=0;
    if ((pp !=NULL)&&(pp->pre_ds !=NULL))   //alte Linie löschen
    {
      if (pp->pre_ds->y !=BCE_NAN)
      {
        edit_pnt[0].v =(short) (WspDrawDimensions.vertDist -(  (pp->pre_ds->y - pmm->minY) * factorY ));
        edit_pnt[0].h =(short) (((pp->pre_ds->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      }
      else   // Falls Vorgänger Mindouble Wert hat nicht zeichnen
      {
        help =pp->pre_ds;
        while ((help->pre_ds!=NULL)&&(help->y ==BCE_NAN))
          help =help->pre_ds;
        edit_pnt[0].v =(short) ( WspDrawDimensions.vertDist -(  (help->y - pmm->minY) * factorY ));
        edit_pnt[0].h =(short) (((help->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      }
      
      i++;
    }
    if (pp !=NULL)
    {
      edit_pnt[1].h =(short) (( (pp->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      edit_pnt[1].v =(short) ( WspDrawDimensions.vertDist -(  (pp->y - pmm->minY) * factorY ));
      i++;
    }
    if ((pp !=NULL)&&(pp->next_ds !=NULL))
    {
      if (pp->next_ds->y !=BCE_NAN)
      {
        edit_pnt[2].v =(short) (WspDrawDimensions.vertDist -(  (pp->next_ds->y - pmm->minY) * factorY ));
        edit_pnt[2].h =(short) (((pp->next_ds->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      }
      else // Falls Vorgänger Mindouble Wert hat nicht zeichnen
      {
        help =pp->next_ds;
        while ((help->next_ds!=NULL)&&(help->y ==BCE_NAN))
          help =help->next_ds;
        edit_pnt[2].v =(short) (WspDrawDimensions.vertDist -(  (help->y - pmm->minY) * factorY ));
        edit_pnt[2].h =(short) (((help->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      }
      
      i++;
    }
    if (pp !=NULL)
      if (pp->pre_ds ==NULL)//Dick 15.01.99
      {
        edit_pnt[0].v=edit_pnt[1].v;
        edit_pnt[0].h=edit_pnt[1].h;
        edit_pnt[1].v=edit_pnt[2].v;
        edit_pnt[1].h=edit_pnt[2].h;
      }
      if (pp !=NULL)
        if (pp->pre_ds !=NULL && zoom_info.level>0)//Dick 15.01.99
        {
          if(pp->ds_nr==zoom_info.pos_station_min)
          {
            edit_pnt[0].v=edit_pnt[1].v;
            edit_pnt[0].h=edit_pnt[1].h;
            edit_pnt[1].v=edit_pnt[2].v;
            edit_pnt[1].h=edit_pnt[2].h;
            i=2;
          }
          else
            if(pp->ds_nr==zoom_info.pos_station_max)           
              i=2;
        }
        xvt_dwin_draw_polyline(WIN_117, edit_pnt,i);
        if(mmp->position >0)
        {
          profil_pnt[mmp->position-1].v=edit_pnt[1].v;
          profil_pnt[mmp->position-1].h=edit_pnt[1].h;
        }
        //  neuen Wert eintragen
        sprintf(temp,"%.4lf",mmp->vertikal);
        sprintf(temp_x,"%.4lf",mmp->horizontal);
        //gcvt(mmp->vertikal,10,temp);
        for (i=0;i<=(INT)strlen(temp);i++)
        {
          if (temp[i]=='.')
            temp[i+4]='\0';
          if (temp[i]==',')
          {
            temp[i]='.' ;
            temp[i+4]='\0';
          }
        }
        if (pp!=NULL)
        {
          if(checkbox_edit && checkbox_edit_v && pp->y!=BCE_NAN) //Dick 29.04.99                
            pp->y = atof(temp);      
          if(checkbox_edit && checkbox_edit_h && pp->y!=BCE_NAN) //Dick 29.04.99
            pp->x=atof(temp_x); //8.3.2001
        }
        // neue Grafik updaten (3Punkte) :
        xvt_app_get_default_ctools(&rect_tools);
        rect_tools.pen.width = WspDrawDimensions.wspLineWidth2;
        rect_tools.pen.color = COLOR_MAGENTA;
        xvt_dwin_set_draw_mode(win,M_XOR);
        xvt_dwin_set_draw_ctools(win, &rect_tools);
        i=0;
        if (pp!=NULL)
          if (pp->pre_ds !=NULL)
          {
            if (pp->pre_ds->y !=BCE_NAN)
            {
              edit_pnt[0].v =(short) (WspDrawDimensions.vertDist -(  (pp->pre_ds->y - pmm->minY) * factorY ));
              edit_pnt[0].h =(short) (((pp->pre_ds->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
            }
            else   // Falls Vorgänger Mindouble Wert hat nicht zeichnen
            {
              help =pp->pre_ds;
              while ((help->pre_ds!=NULL)&&(help->y ==BCE_NAN))
                help =help->pre_ds;
              edit_pnt[0].v =(short) ( WspDrawDimensions.vertDist -(  (help->y - pmm->minY) * factorY ));
              edit_pnt[0].h =(short)(((help->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
            }
            i++;
          }
          if (pp !=NULL)
          {
            edit_pnt[1].h =(short) (( (pp->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
            edit_pnt[1].v =(short) ( WspDrawDimensions.vertDist -(  (pp->y - pmm->minY) * factorY) );
            i++;
          }
          if (pp!=NULL)
            if (pp->next_ds !=NULL)
            {
              if (pp->next_ds->y !=BCE_NAN)
              {
                edit_pnt[2].v =(short) (WspDrawDimensions.vertDist -(  (pp->next_ds->y - pmm->minY) * factorY ));
                edit_pnt[2].h =(short) (((pp->next_ds->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
              }
              else // Falls Vorgänger Mindouble Wert hat nicht zeichnen
              {
                help =pp->next_ds;
                while ((help->next_ds!=NULL)&&(help->y ==BCE_NAN))
                  help =help->next_ds;
                edit_pnt[2].v =(short) ( WspDrawDimensions.vertDist -(  (help->y - pmm->minY) * factorY ));
                edit_pnt[2].h =(short) (((help->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
              }
              i++;
            }
            if (pp!=NULL)
              if (pp->pre_ds ==NULL)//Dick 15.01.99
              {
                edit_pnt[0].v=edit_pnt[1].v;
                edit_pnt[0].h=edit_pnt[1].h;
                edit_pnt[1].v=edit_pnt[2].v;
                edit_pnt[1].h=edit_pnt[2].h;
              }
              if (pp!=NULL)
                if (pp->pre_ds !=NULL && zoom_info.level>0)//Dick 15.01.99
                {
                  if(pp->ds_nr==zoom_info.pos_station_min)
                  {
                    edit_pnt[0].v=edit_pnt[1].v;
                    edit_pnt[0].h=edit_pnt[1].h;
                    edit_pnt[1].v=edit_pnt[2].v;
                    edit_pnt[1].h=edit_pnt[2].h;
                    i=2;
                  } 
                  else
                    if(pp->ds_nr==zoom_info.pos_station_max)           
                      i=2;           
                }
                
                xvt_dwin_draw_polyline(win, edit_pnt,i);
  }
  
  /******************************************************************/
  void Paint::Edit_DurchstBereiche(WINDOW win,Koord *pp,MMP *mmp)
  {
    double links,rechts;
    PNT pnt;
    char str[10];
    EVENT *xdEvent;
    
    xdEvent= new EVENT;
    
    
    if (*ptr_win==NULL_WIN)
      if (!xvt_dlg_create_res(WD_MODELESS, DLG_140, EM_ALL, DLG_140_eh, 0L))
        xvt_dm_post_error("Can't open dialog140");
      
      links = pp->x;
      rechts =pp->next_ds->x;
      
      if (links == mmp->hor_m_down)  // Wert gefunden ->editieren
      {
        pp->y_pnt=1;
        pp->next_ds->y_pnt=0;
      }
      if (rechts == mmp->hor_m_down)  // Wert gefunden ->editieren
      {
        pp->next_ds->y_pnt=1;
        pp->y_pnt=0;
      }
      //if(mmp->active)
      {
        if (pp->y_pnt==1)
          pnt.h = pp->x_pnt; // alte Koord.
        else
          if (pp->next_ds->y_pnt==1)
            pnt.h = pp->next_ds->x_pnt;
          
          
          DRAW_CTOOLS rect_tools;
          
          xvt_app_get_default_ctools(&rect_tools);
          rect_tools.pen.width = WspDrawDimensions.wspLineWidth2;
          rect_tools.pen.color = COLOR_BLUE;
          xvt_dwin_set_draw_ctools(win, &rect_tools);
          xvt_dwin_set_draw_mode(win,M_XOR);
          
          //alte Grafik löschen
          pnt.v= WspDrawDimensions.DX;//Dick 4.08.99
          if ((pnt.h>=(int)WspDrawDimensions.DX)&&
            (pnt.h<=(int)(WspDrawDimensions.DELTA_X + WspDrawDimensions.DX)))//Dick 4.08.99
          {
            xvt_dwin_draw_set_pos(win,pnt);
            pnt.v= WspDrawDimensions.vertMax;
            xvt_dwin_draw_line(win,pnt);
          }
          // neue Trennfläche zeichnen
          pnt.h = mmp->mouse_h;
          pnt.v= WspDrawDimensions.DX; //Dick 4.08.99
          if ((pp->y_pnt==1)|| (pp->next_ds->y_pnt==1))
            if ((pnt.h>=(int)WspDrawDimensions.DX)&&
              (pnt.h<=(int)(WspDrawDimensions.DELTA_X + WspDrawDimensions.DX)))//Dick 4.08.99
            {
              xvt_dwin_draw_set_pos(win,pnt);
              pnt.v= WspDrawDimensions.vertMax;
              xvt_dwin_draw_line(win,pnt);
            }
            mmp->last_rct.left = mmp->mouse_h;
            //----------------------------------------------------------
            if (pp->y_pnt==1)
              pp->x_pnt = pnt.h;
            
            if ((mmp->horizontal<rechts)&&(pp->y_pnt==1))  //links
            {
              pp->x = mmp->horizontal;
              scr.z[0]= pp->x;
              
              if (ptr_win[0]!=NULL_WIN)//  Dialog 140 ptr auf Edit-Felder
              {
                sprintf(str,"%.4lf",pp->x);
                xvt_vobj_set_title(ptr_win[0],str);
                
                scr.z[1]=paint->Get_Station_Hoehe(scr.z[0]);
                sprintf(str,"%.4lf",scr.z[1]);
                is_zahl(str);
                xvt_vobj_set_title(ptr_win[1],str);
                
                paint->SaveSonderprofildaten( &scr, DURCHST_BEREICH );
                
                xdEvent->type =E_CONTROL;
                xdEvent->v.ctl.id=DLG_140_EDIT_9;
                xdEvent->v.ctl.ci.v.edit.focus_change = FALSE;
                xdEvent->v.ctl.ci.v.edit.active = FALSE;
                xvt_win_dispatch_event(dlg_sonderprofil,xdEvent);
              }
            }
            
            
            if(pp->next_ds->y_pnt==1)
              pp->next_ds->x_pnt = pnt.h;
            
            if ((mmp->horizontal>links)&&(pp->next_ds->y_pnt==1)) // rechts
            {
              pp->next_ds->x = mmp->horizontal;
              scr.z[2] = pp->next_ds->x;
              
              if (ptr_win[1]!=NULL_WIN)//  Dialog 140 ptr auf Edit-Felder
              {
                sprintf(str,"%.4lf",pp->next_ds->x);
                xvt_vobj_set_title(ptr_win[2],str);
                
                
                scr.z[3]=paint->Get_Station_Hoehe(scr.z[2]);
                sprintf(str,"%.4lf",scr.z[3]);
                is_zahl(str);
                xvt_vobj_set_title(ptr_win[3],str);
                
                paint->SaveSonderprofildaten( &scr, DURCHST_BEREICH );
                
                xdEvent->type =E_CONTROL;
                xdEvent->v.ctl.id=DLG_140_EDIT_12;
                xdEvent->v.ctl.ci.v.edit.focus_change = FALSE;
                xdEvent->v.ctl.ci.v.edit.active = FALSE;
                xvt_win_dispatch_event(dlg_sonderprofil,xdEvent);
              }
              
            }
      }
      delete xdEvent;
      
}

/***************************************************************************/
void CreateTree(WINDOW win,COLOR color,PNT *orig)
{
  DRAW_CTOOLS rect_tools;
  PNT pnt;
  
  xvt_app_get_default_ctools(&rect_tools);
  rect_tools.pen.width = 1;//WspDrawDimensions.wspLineWidth2;
  rect_tools.pen.color = XVT_MAKE_COLOR(128,64,0);
  rect_tools.brush.pat = PAT_CROSS;
  xvt_dwin_set_draw_ctools(win, &rect_tools);
  
  pnt.h = orig->h - 3;
  pnt.v = orig->v; 
  xvt_dwin_draw_set_pos(win,pnt);
  pnt.h = orig->h + 3;
  pnt.v = orig->v;
  xvt_dwin_draw_line(win,pnt);
  
  pnt.h = orig->h;
  pnt.v = orig->v; 
  xvt_dwin_draw_set_pos(win,pnt);
  pnt.h = orig->h;
  pnt.v = orig->v-13;
  xvt_dwin_draw_line(win,pnt);
  //-------------------------------Blätter
  xvt_app_get_default_ctools(&rect_tools);
  rect_tools.pen.width = 1;//WspDrawDimensions.wspLineWidth2;
  rect_tools.pen.color = XVT_MAKE_COLOR(0,100,0);
  xvt_dwin_set_draw_ctools(win, &rect_tools);
  
  pnt.h = orig->h-1;
  pnt.v = orig->v-5; 
  xvt_dwin_draw_set_pos(win,pnt);
  pnt.h = orig->h-4;
  pnt.v = orig->v-2;
  xvt_dwin_draw_line(win,pnt);
  pnt.h = orig->h+1;
  pnt.v = orig->v-5; 
  xvt_dwin_draw_set_pos(win,pnt);
  pnt.h = orig->h+4;
  pnt.v = orig->v-2;
  xvt_dwin_draw_line(win,pnt);
  
  pnt.h = orig->h-1;
  pnt.v = orig->v-8; 
  xvt_dwin_draw_set_pos(win,pnt);
  pnt.h = orig->h-3;
  pnt.v = orig->v-6;
  xvt_dwin_draw_line(win,pnt);
  pnt.h = orig->h+1;
  pnt.v = orig->v-8; 
  xvt_dwin_draw_set_pos(win,pnt);
  pnt.h = orig->h+3;
  pnt.v = orig->v-6;
  xvt_dwin_draw_line(win,pnt);
  
  pnt.h = orig->h-1;
  pnt.v = orig->v-10; 
  xvt_dwin_draw_set_pos(win,pnt);
  pnt.h = orig->h-2;
  pnt.v = orig->v-9;
  xvt_dwin_draw_line(win,pnt);
  pnt.h = orig->h+1;
  pnt.v = orig->v-10; 
  xvt_dwin_draw_set_pos(win,pnt);
  pnt.h = orig->h+2;
  pnt.v = orig->v-9;
  xvt_dwin_draw_line(win,pnt);
  
}
/***************************************************************************/
void Paint::PaintAX(WINDOW win, MinMax *pmm,int datensatz,COLOR color)
{
  Koord *gel;
  //Dick 11.02.99 weg DRAW_CTOOLS rect_tools;
  int i=0;
  double factorX,factorY;
  PNT orig;
  int dist_ppx;//Dick 11.02.99 weg  ,distX;
  //Dick 11.02.99 weg int axPix;
  int n=0;
  
  ptr_profil = ptr_anfang;
  gel = ptr_profil->datensatz; // zeigt auf Gelände
  
  while((ptr_profil->ds_nummer < datensatz) && (ptr_profil!=NULL))
    ptr_profil=ptr_profil->next;
  pp=ptr_profil->datensatz;
  factorX= WspDrawDimensions.DELTA_X / pmm->distanceX;
  factorY= WspDrawDimensions.Y / pmm->distanceY;
  
  while ((pp->next_ds)&&(gel->next_ds))
  {
    if (pp->y != BCE_NAN && pp->y !=0.0)//Dick 28.11.98
		  {
      dist_ppx = profil_pnt[i+1].h - profil_pnt[i].h; //Abstand in Pixeln
      
      orig.h = profil_pnt[i].h + ((profil_pnt[i+1].h - profil_pnt[i].h)/2);
      orig.v = profil_pnt[i].v + ((profil_pnt[i+1].v - profil_pnt[i].v)/2);
      CreateTree(win,color,&orig);
      i++;
		  }
    else if(pp->y ==0.0)
      i++;
    pp = pp->next_ds;
    gel = gel->next_ds;
  }
  
}

/**************************************************************************/
void Paint::DrawBewuchs(WINDOW win, MinMax *pmm,int datensatz)
/*  Bewuchswerte 

  */
{
  double factorX;
  RCT rct;
  DRAW_CTOOLS rect_tools;
  double max;
  Koord *k_ax,*k_ay,*k_dp,*k_bewuchs,*k_bewuchs_anfang,*k_bewuchs_tmp;
  
  k_ax=HoleDatensatz(AXM);
  k_ay=HoleDatensatz(AYM);
  k_dp=HoleDatensatz(DPM);
  
  k_bewuchs=new Koord;
  k_bewuchs->ds_nr = 1;
  k_bewuchs->x=BCE_NAN;
  k_bewuchs->y=BCE_NAN;
  k_bewuchs->pre_x=0;
  k_bewuchs->pre_y=0;
  k_bewuchs->attr=0;
  k_bewuchs->status =-1;
  k_bewuchs->next_ds=NULL;
  k_bewuchs->pre_ds=NULL;
  
  k_bewuchs_anfang=k_bewuchs;
  switch(typ[datensatz])
  {
  case AXM:
    {
      while(k_ax!=NULL)
      {
        k_bewuchs->x=k_ax->x;
        if(k_ay!=NULL && k_dp!=NULL)
        {
          if(k_ay->y>0.0 && k_dp->y>0.0 && k_ax->y>0.0)
            k_bewuchs->y=k_dp->y/(k_ax->y*k_ay->y);
          else
            k_bewuchs->y=0.0;
          k_ay=k_ay->next_ds;
          k_dp=k_dp->next_ds;
        }
        else
          k_bewuchs->y=BCE_NAN;
        k_ax=k_ax->next_ds;
        k_bewuchs_tmp=new Koord;
        k_bewuchs_tmp->ds_nr=k_bewuchs->ds_nr++;
        k_bewuchs_tmp->x=BCE_NAN;
        k_bewuchs_tmp->y=BCE_NAN;
        k_bewuchs_tmp->pre_x=0;
        k_bewuchs_tmp->pre_y=0;
        k_bewuchs_tmp->attr=0;
        k_bewuchs_tmp->status =-1;
        k_bewuchs->next_ds=k_bewuchs_tmp;
        k_bewuchs->next_ds->pre_ds=k_bewuchs;
        k_bewuchs->next_ds->next_ds=NULL;
        k_bewuchs=k_bewuchs->next_ds;
      }
    }
    break;
  case AYM:
    {
      while(k_ay!=NULL)
      {
        k_bewuchs->x=k_ay->x;
        if(k_ax!=NULL && k_dp!=NULL)
        {
          if(k_ay->y>0.0 && k_dp->y>0.0 && k_ax->y>0.0)
            k_bewuchs->y=k_dp->y/(k_ax->y*k_ay->y);
          else
            k_bewuchs->y=0.0;
          k_ax=k_ax->next_ds;
          k_dp=k_dp->next_ds;
        }
        else
          k_bewuchs->y=BCE_NAN;
        k_ay=k_ay->next_ds;
        k_bewuchs_tmp=new Koord;
        k_bewuchs_tmp->ds_nr=k_bewuchs->ds_nr++;
        k_bewuchs_tmp->x=BCE_NAN;
        k_bewuchs_tmp->y=BCE_NAN;
        k_bewuchs_tmp->pre_x=0;
        k_bewuchs_tmp->pre_y=0;
        k_bewuchs_tmp->attr=0;
        k_bewuchs_tmp->status =-1;
        k_bewuchs->next_ds=k_bewuchs_tmp;
        k_bewuchs->next_ds->pre_ds=k_bewuchs;
        k_bewuchs->next_ds->next_ds=NULL;
        k_bewuchs=k_bewuchs->next_ds;
      }
    }
    break;
  case DPM:
    {
      while(k_dp!=NULL)
      {
        k_bewuchs->x=k_dp->x;
        if(k_ax!=NULL && k_ay!=NULL)
        {
          if(k_ay->y>0.0 && k_dp->y>0.0 && k_ax->y>0.0)
            k_bewuchs->y=k_dp->y/(k_ax->y*k_ay->y);
          else
            k_bewuchs->y=0.0;
          k_ax=k_ax->next_ds;
          k_ay=k_ay->next_ds;
        }
        else
          k_bewuchs->y=BCE_NAN;
        k_dp=k_dp->next_ds;
        k_bewuchs_tmp=new Koord;
        k_bewuchs_tmp->ds_nr=k_bewuchs->ds_nr++;
        k_bewuchs_tmp->x=BCE_NAN;
        k_bewuchs_tmp->y=BCE_NAN;
        k_bewuchs_tmp->pre_x=0;
        k_bewuchs_tmp->pre_y=0;
        k_bewuchs_tmp->attr=0;
        k_bewuchs_tmp->status =-1;
        k_bewuchs->next_ds=k_bewuchs_tmp;
        k_bewuchs->next_ds->pre_ds=k_bewuchs;
        k_bewuchs->next_ds->next_ds=NULL;
        k_bewuchs=k_bewuchs->next_ds;
      }
    }
    break;
  }
  if (pmm->distanceX != 0.0)
    factorX= WspDrawDimensions.DELTA_X / pmm->distanceX;
  else 
  {
    return;
  }
  Koord *pp_help;
  
  
  xvt_app_get_default_ctools(&rect_tools);
  rect_tools.pen.width = WspDrawDimensions.wspLineWidth2;
  rect_tools.pen.color = COLOR_LTGRAY;
  rect_tools.fore_color =COLOR_LTGRAY;
  rect_tools.brush.color =COLOR_LTGRAY;
  rect_tools.brush.pat = PAT_SOLID;
  
  xvt_dwin_set_draw_ctools(win, &rect_tools);
  
  
  ptr_profil=ptr_anfang;
  pp=k_bewuchs_anfang;
  
  if(pp!=NULL)//Dick 6.08.98 Sonst Absturz
  {
    max = pp->y;
    while (pp->next_ds !=NULL)
    {
      if (pp->next_ds->y >max)
        max = pp->next_ds->y;
      pp=pp->next_ds;
    }
    
    if (max <=0.0) max =1;
    pmm->sec_maxY = max;     //max.Rauhigkeit zwischenspeichern für editieren
    
    pp=k_bewuchs_anfang;
  }
  while(pp != NULL)
  {
    if ((pp->next_ds !=NULL)&&(pp->x == pp->next_ds->x))
			 {	  // 	DL = 0;  DR=0;
      ; // xvt_dm_post_note("Fehler in: paint.cpp/");
			 }
    else
      if(pp->y > 0.0)
      {
        rct.top    =(short) ((WspDrawDimensions.vertMax+WspDrawDimensions.AB)- (WspDrawDimensions.res1* pp->y /max));//337 - (80* pp->y /max);
        rct.bottom = WspDrawDimensions.vertMax;
        rct.left   =(short) (((pp->x - pmm->minX) * factorX) + WspDrawDimensions.DX) ;
        
        if (rct.top >rct.bottom)   //Fehler abfangen:invalid rct-format
          rct.top =rct.bottom;
        
        if( (pp->next_ds !=NULL)&&(pp->next_ds->y !=BCE_NAN))
        {
          if (pp->x < pp->next_ds->x)
            rct.right  = (short)(( (pp->next_ds->x   -pmm->minX) * factorX) + WspDrawDimensions.DX );
          else
          {
            rct.right  = rct.left;
            rct.left = (short)(((pp->next_ds->x   -pmm->minX) * factorX) + WspDrawDimensions.DX);
          }
        }
        else
          if(pp->next_ds !=NULL)
          {
            pp_help = pp->next_ds;
            while ((pp_help->next_ds !=NULL)&&(pp_help->y ==BCE_NAN))
              pp_help = pp_help->next_ds;
            
            if ((pp_help->next_ds!=NULL)&&(pp_help->x < pp_help->next_ds->x))
              rct.right  =(short) (((pp_help->next_ds->x   -pmm->minX) * factorX) + WspDrawDimensions.DX );
            else  if ((pp_help->next_ds!=NULL)&&(pp_help->next_ds->x==BCE_NAN))
            {
              rct.right  = rct.left;
              rct.left =(short) (((pp_help->next_ds->x   -pmm->minX) * factorX) + WspDrawDimensions.DX);
            }
          }
          
          if (rct.left+3 <= rct.right-1)
          {
            rct.left = rct.left+3;
            rct.right = rct.right-1;
          }
          if (rct.right >(INT) (WspDrawDimensions.DX+WspDrawDimensions.DELTA_X))  
            rct.right = WspDrawDimensions.DX+WspDrawDimensions.DELTA_X;
          
          if (rct.left<(INT)WspDrawDimensions.DX) 
            rct.left=(INT)WspDrawDimensions.DX;
          if (rct.right<(INT)WspDrawDimensions.DX) 
            rct.right=(INT)WspDrawDimensions.DX;
          
          if ((rct.left<= rct.right)&&(rct.top <=rct.bottom))
          {
            xvt_dwin_draw_rect(win, &rct);
          }
          if(!WspDrawDimensions.isPrinting)
          {
            pp->x_pnt=rct.left;   //linke Kante sichern
            pp->y_pnt=rct.right;  //rechte Kante sichern
          }
          
      }
      pp = pp->next_ds;
  } //-while
  k_bewuchs=k_bewuchs_anfang;
  while (k_bewuchs !=NULL)
  {
		  k_bewuchs_tmp = k_bewuchs;
      k_bewuchs = k_bewuchs->next_ds;
      delete k_bewuchs_tmp;
  }
}

/***************************************************************************/
void Paint::PaintDK(WINDOW win, MinMax *pmm,int datensatz,COLOR color)
{
  Koord *gel;
  DRAW_CTOOLS rect_tools;
  int i=0,j,k=0;
  double factorX,factorY;
  PNT orig;
  
  int n=0;
  int datensatz_dk=0;
  bool poly_dk=TRUE;
  PNT pnt_dkuk[STRANGANZAHL];
  PNT pnt_dkok[STRANGANZAHL];
  int dk_typ[2];
  dk_typ[0]=DKOK;
  dk_typ[1]=DKUK;
  for(j=0;j<2;j++)
  {
    ptr_profil = ptr_anfang;
    gel = ptr_profil->datensatz; // zeigt auf Gelände
    i=0;
    k=0;
    pp=HoleDatensatz(dk_typ[j]);
    if(pp==NULL)
    {
      poly_dk=FALSE;
      continue;
    }
    else
    {
      for(datensatz_dk=1;datensatz_dk<=ds_info[0]&& typ[datensatz_dk]!=dk_typ[j];datensatz_dk++)
      {}
      if(datensatz_dk>ds_info[0])
      {
        datensatz_dk=0;
        continue;
      }
      
    }
    factorX= WspDrawDimensions.DELTA_X / pmm->distanceX;
    factorY= WspDrawDimensions.Y / pmm->distanceY;
    
    xvt_app_get_default_ctools(&rect_tools);
    rect_tools.pen.width = WspDrawDimensions.wspLineWidth2;
    rect_tools.pen.color = color;
    xvt_dwin_set_draw_ctools(win, &rect_tools);
    
    while (pp !=NULL )
    {
      if (pp->y != BCE_NAN)
      {
        if(gel!=NULL)
        {
          if(gel->x <= pmm->maxX && gel->x >= pmm->minX &&gel->ds_nr>=pmm->posMinX && gel->ds_nr<=pmm->posMaxX)
          {
            if(gel->x == pp->x && gel->x <= pmm->maxX && gel->x >= pmm->minX &&
              gel->ds_nr>=pmm->posMinX && gel->ds_nr<=pmm->posMaxX &&
              Get_Station_pos_allg(gel->x,gel->ds_nr,1)==Get_Station_pos_allg(pp->x,pp->ds_nr,datensatz_dk))
            {
              if(i==0)
                orig.h = profil_pnt[i].h;
              else 
              {
                if(profil_pnt[i].h - profil_pnt[i-1].h==0)
                {
                  int pos=i;
                  while((profil_pnt[pos].h - profil_pnt[pos-1].h)==0)
                  {
                    pos--;
                    if(pos<1)
                    {
                      orig.h = profil_pnt[0].h;
                      break;
                    }                                 
                  }
                  if(pos>0)
                    orig.h = profil_pnt[pos-1].h + ((profil_pnt[pos].h - profil_pnt[pos-1].h)/2)+(profil_pnt[pos].h - profil_pnt[pos-1].h)*(Get_Station_pos_allg(pp->x,pp->ds_nr,datensatz_dk)-1)/num_station_in_ds(pp->x ,datensatz_dk);
                }
                else
                  orig.h = profil_pnt[i-1].h + ((profil_pnt[i].h - profil_pnt[i-1].h)/2);
              }
              orig.v = (short) (WspDrawDimensions.vertDist -(  (pp->y - pmm->minY) * factorY));
              xvt_dwin_draw_set_pos(win,orig);
              if(j==0)
              {
                pnt_dkok[k].h=orig.h;
                pnt_dkok[k++].v=orig.v;
              }
              else
              {
                pnt_dkuk[k].h=orig.h;
                pnt_dkuk[k++].v=orig.v;
              }
              if(gel->next_ds != NULL )
              {
                if(gel->next_ds->x <= pmm->maxX && gel->next_ds->x >= pmm->minX &&
                  gel->next_ds->ds_nr>=pmm->posMinX && gel->next_ds->ds_nr<=pmm->posMaxX)
                {
                  if(profil_pnt[i+1].h - profil_pnt[i].h==0)
                  {
                    int pos=i;
                    while((profil_pnt[pos+1].h - profil_pnt[pos].h)==0)
                    {
                      pos++;
                      if(pos>pmm->posMaxX)
                      {
                        orig.h = profil_pnt[pmm->posMaxX].h;
                        break;
                      }                                 
                    }
                    if(pos<=pmm->posMaxX)
                      orig.h = profil_pnt[pos].h + ((profil_pnt[pos+1].h - profil_pnt[pos].h)/2)-(profil_pnt[pos+1].h - profil_pnt[pos].h)*(num_station_in_ds(pp->x ,datensatz_dk)-Get_Station_pos_allg(pp->x,pp->ds_nr,datensatz_dk))/num_station_in_ds(pp->x ,datensatz_dk);
                  }
                  else
                    orig.h = profil_pnt[i].h + ((profil_pnt[i+1].h - profil_pnt[i].h)/2);
                }
                else
                  orig.h = profil_pnt[i].h;
              }
              else
                orig.h = profil_pnt[i].h;
              xvt_dwin_draw_line(win,orig);
              
              if(j==0)
              {
                pnt_dkok[k].h=orig.h;
                pnt_dkok[k++].v=orig.v;
              }
              else
              {
                pnt_dkuk[k].h=orig.h;
                pnt_dkuk[k++].v=orig.v;
              }
              pp = pp->next_ds;
            }
            else
            {
              
            }
            i++;
          }
          
        }
        else
        {
          gel=ptr_anfang->datensatz;
          i=0;
          pp = pp->next_ds;
        }
      }
      else
        pp = pp->next_ds;
      gel = gel->next_ds;
    }
     }//for(j)
     if(poly_dk)
     {
       HWND hWnd;
       HDC hDC;
       PRINTDLG pdlg;
       if(WspDrawDimensions.isPrinting==0)
       {
         hWnd = (HWND)xvt_vobj_get_attr(win,ATTR_NATIVE_WINDOW);
         hDC = GetDC(hWnd);
       }
       else
       {
         
         pdlg.lStructSize=sizeof(PRINTDLG);
         pdlg.hDevMode=NULL;
         pdlg.hDevNames=NULL;
         pdlg.Flags =PD_RETURNDC | PD_RETURNDEFAULT ;
         pdlg.hwndOwner=NULL;
         PrintDlg(&pdlg);
         DWORD err=CommDlgExtendedError(); 
         hDC= pdlg.hDC;          
       }
       HBRUSH hBrush, hOldBrush;
       HPEN hPen, hOldPen;
       POINT *pt_array;
       LOGBRUSH lplb;
       pt_array=new POINT[4];
       
       if(WspDrawDimensions.isPrinting==0)
       {
         lplb.lbStyle=BS_HATCHED ;
         lplb.lbColor=RGB(0,0,0);
         lplb.lbHatch=HS_FDIAGONAL ;
         hBrush=CreateBrushIndirect(&lplb);
         hPen = (HPEN)GetStockObject(BLACK_PEN);
         hOldBrush = (HBRUSH)SelectObject(hDC, hBrush);
         hOldPen = (HPEN)SelectObject(hDC, hPen);
       }
       else
       {
         //
         xvt_app_get_default_ctools(&rect_tools);
         rect_tools.pen.width    = WspDrawDimensions.wspLineWidth1;
         rect_tools.pen.color    = COLOR_BLACK;
         rect_tools.brush.pat    = PAT_FDIAG;
         rect_tools.brush.color = RGB(0,0,0);
         xvt_dwin_set_draw_ctools(win, &rect_tools);
         //
       }
       PNT pnt_druck[STRANGANZAHL];
       
       
       for(i=0;i<k;i+=2)
       {
         if(WspDrawDimensions.isPrinting==0)
         {
           pt_array[0].x=pnt_dkok[i].h;
           pt_array[0].y=pnt_dkok[i].v;
           pt_array[1].x=pnt_dkok[i+1].h;
           pt_array[1].y=pnt_dkok[i+1].v;
           pt_array[2].x=pnt_dkuk[i+1].h;
           pt_array[2].y=pnt_dkuk[i+1].v;
           pt_array[3].x=pnt_dkuk[i].h;
           pt_array[3].y=pnt_dkuk[i].v;
           
           BOOL  ok=Polygon(hDC,pt_array, 4);
         }
         else
         {
           pnt_druck[0].h= pnt_dkok[i].h;
           pnt_druck[0].v= pnt_dkok[i].v;
           pnt_druck[1].h= pnt_dkok[i+1].h;
           pnt_druck[1].v= pnt_dkok[i+1].v;
           pnt_druck[2].h= pnt_dkuk[i+1].h;
           pnt_druck[2].v= pnt_dkuk[i+1].v;
           pnt_druck[3].h= pnt_dkuk[i].h;
           pnt_druck[3].v= pnt_dkuk[i].v;
           xvt_dwin_draw_polygon(win, pnt_druck,4);
         }
       }
       DWORD err=GetLastError();
       SelectObject(hDC, hOldBrush);
       SelectObject(hDC, hOldPen);
       if(!WspDrawDimensions.isPrinting)
         ReleaseDC(hWnd, hDC);
       delete[] pt_array;
       DeleteObject(hBrush);
     }
     
}
/***************************************************************************/
void Paint::PaintWSF(WINDOW win, MinMax *pmm,int datensatz,COLOR color)
{
  Koord *gel;
  DRAW_CTOOLS rect_tools;
  int i=0;
  double factorX,factorY;
  PNT orig;
  int n=0;
  
  ptr_profil = ptr_anfang;
  gel = ptr_profil->datensatz; // zeigt auf Gelände
  
  while((ptr_profil->ds_nummer < datensatz) && (ptr_profil!=NULL))
    ptr_profil=ptr_profil->next;
  pp=ptr_profil->datensatz;
  factorX= WspDrawDimensions.DELTA_X / pmm->distanceX;
  factorY= WspDrawDimensions.Y / pmm->distanceY;
  
  xvt_app_get_default_ctools(&rect_tools);
  rect_tools.pen.width = WspDrawDimensions.wspLineWidth2;
  rect_tools.pen.color = color;
  xvt_dwin_set_draw_ctools(win, &rect_tools);
  
  while (pp !=NULL )
  {
    if (pp->y != BCE_NAN)
		  {
      if(gel!=NULL)
      {
        if(gel->x <= pmm->maxX && gel->x >= pmm->minX)
        {
          if(gel->x == pp->x && gel->x <= pmm->maxX && gel->x >= pmm->minX)
          {
            
            orig.h = profil_pnt[i].h-3;
            orig.v = (short) (WspDrawDimensions.vertDist -(  (pp->y - pmm->minY) * factorY)+3);
            xvt_dwin_draw_set_pos(win,orig);
            
            orig.h = profil_pnt[i].h+3;
            orig.v = (short) (WspDrawDimensions.vertDist -(  (pp->y - pmm->minY) * factorY)-3);
            xvt_dwin_draw_line(win,orig);
            
            orig.h = profil_pnt[i].h+3;
            orig.v = (short) (WspDrawDimensions.vertDist -(  (pp->y - pmm->minY) * factorY)+3);
            xvt_dwin_draw_set_pos(win,orig);
            
            orig.h = profil_pnt[i].h-3;
            orig.v = (short) (WspDrawDimensions.vertDist -(  (pp->y - pmm->minY) * factorY)-3);
            xvt_dwin_draw_line(win,orig);
            pp = pp->next_ds;
          }
          else
          {
            
          }
          i++;
        }
        
      }
      else
      {
        gel=ptr_anfang->datensatz;
        i=0;
        pp = pp->next_ds;
      }
		  }
    else
      pp = pp->next_ds;
    gel = gel->next_ds;
  }
  
}
/****************************************************/
void Paint::PaintBuhne(WINDOW win, MinMax *pmm,int datensatz,COLOR color)
{
  PNT hilfs_pnt[STRANGANZAHL];
  DRAW_CTOOLS rect_tools;
  int i=0,k,j1=0,j2=0, drei=0;
  double factorX,factorY;
  
  ptr_profil = ptr_anfang;
  while((ptr_profil->ds_nummer < datensatz) && (ptr_profil!=NULL))
    ptr_profil=ptr_profil->next;
  pp=ptr_profil->datensatz;
  factorX= WspDrawDimensions.DELTA_X / pmm->distanceX;
  factorY= WspDrawDimensions.Y / pmm->distanceY;
  
  while (pp != NULL)
  {
    if (pp->y != BCE_NAN)
		  {
      profil_pnt[i].h =(short) (((pp->x - pmm->minX) * factorX) + WspDrawDimensions.DX);
      //Dick 19.04.99
      if(profil_pnt[i].h < (int)WspDrawDimensions.DX || pp->x - pmm->minX<0.0 )//Dick 15.04.99 damit Grafik auserhalb nicht geht
        profil_pnt[i].h=WspDrawDimensions.DX;//Dick 4.08.99
      if(profil_pnt[i].h>(int)(WspDrawDimensions.DELTA_X + WspDrawDimensions.DX) ||  pmm->maxX-pp->x<0.0)
        profil_pnt[i].h=(WspDrawDimensions.DELTA_X + WspDrawDimensions.DX);
      //
      if(pp->x - pmm->minX<0.0)
        j1++;
      if(pmm->maxX-pp->x<0.0)
        j2++;
      profil_pnt[i].v =(short) (WspDrawDimensions.vertDist -(  (pp->y - pmm->minY) * factorY));
      i++;
      drei++;
		  }
    
			 if(j1==i || j2==i)
         i=0;
       
       k=i;
       while((profil_pnt[k].h!=0||profil_pnt[k].v!=0)&&k<STRANGANZAHL)//Dick 18.08.98
       {
         profil_pnt[k].h=0;
         profil_pnt[k].v=0;
         k++;
       }
       
       xvt_app_get_default_ctools(&rect_tools);
       rect_tools.pen.width = WspDrawDimensions.wspLineWidth2;
       rect_tools.pen.color = color;
       rect_tools.brush.pat = PAT_CROSS;
       xvt_dwin_set_draw_ctools(win, &rect_tools);
       
       
       int wievielbuhnen=i/2;
       for (int zaehler=0; zaehler<wievielbuhnen; zaehler++)
       {
         if((profil_pnt[1+zaehler*3].h!=0) && (profil_pnt[1+zaehler*3].v!=0))
         {
           hilfs_pnt[0].v=profil_pnt[0+zaehler*3].v;
           hilfs_pnt[1].v=profil_pnt[1+zaehler*3].v;
           
           hilfs_pnt[0].h=profil_pnt[0+zaehler*3].h;
           hilfs_pnt[1].h=profil_pnt[1+zaehler*3].h;
           
           xvt_dwin_draw_polyline(win, hilfs_pnt,2);
         }
         if((profil_pnt[2+zaehler*3].h!=0) && (profil_pnt[2+zaehler*3].v!=0))
         {
           hilfs_pnt[0].v=profil_pnt[1+zaehler*3].v;
           hilfs_pnt[0].h=profil_pnt[1+zaehler*3].h;
           
           hilfs_pnt[1].h=profil_pnt[2+zaehler*3].h;
           hilfs_pnt[1].v=profil_pnt[2+zaehler*3].v;
           
           xvt_dwin_draw_polyline(win, hilfs_pnt,2);
         }
       }
       
       drei=0;
       
       pp = pp->next_ds;
  }
}
