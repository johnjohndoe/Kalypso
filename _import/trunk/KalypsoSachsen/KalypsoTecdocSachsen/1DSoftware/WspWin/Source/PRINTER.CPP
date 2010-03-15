/************************************************************

  PRINTER.CPP   Version WIN95 incl.Grafik-Drucken  26.11.96

************************************************************/

#include <windows.h>
#include "xvt.h"
#include "resource.h"

#include "global_types.h"

#include "slist.h"

#include "wspwin.h"		// GHJ

#include "list.h"
#include "paint.h"

#include "printer.h"
#include "math.h"


struct PRINT_INFO // printer
{
  WINDOW win;
  int sheet_h,       //Druckbreite
		  sheet_v;       //Druckhöhe
  int chr_per_line,  //Zeichen pro Zeile
		  line_per_page; //Zeilen pro Seite
  int heigth;        //Zeilenhöhe = Zeichenhöhe in pixel
  int x,             // obere,linke Blattposition für 1.Zeichen
		  y;
  char *textfile;    // Name der zu druckenden Textdatei
  BYTE textzustand;
  PRINT_RCD *prn_rcd;   //XVT-Struct ->Reference-Buch
};

typedef struct _GRAPHIC_DIM
{
  WINDOW win;
  long sheet_h,       //Druckbreite
			 sheet_v;       //Druckhöhe
  UINT vRes,hRes;
  RCT dim;           // tatsächliche Zeichenkoordinaten/Abmessung von Rahmen
  char *textfile;    // Name der zu druckenden Textdatei
  PRINT_RCD *prn_rcd;   //XVT-Struct ->Reference-Buch
} GRAPHIC_DIM; // printer

#define NEWPAGE 12 //ASCII 12

static  XVT_FNTID font_id;
extern FILE_SPEC  STR_SPEC;

extern MMP        mmp;
extern Paint      *paint;
extern List       *list;

extern FILE_SPEC file_spec;
extern WINDOW WIN_117;

extern Scroller scr;
extern DRAW_DIMENSIONS WspDrawDimensions;
PRINT_INFO *prn_tx;

BOOLEAN druckvorgang=TRUE;

// vorwärtsdeklarationen
BOOLEAN wsp_init_page(PRINT_INFO *prn_tx); 
BOOLEAN XVT_CALLCONV1 wsp_print_graphics(long data); 
int GraphicDrawRect(GRAPHIC_DIM *); 
void InitGraphicDimension(GRAPHIC_DIM *); 
void GetGraphicDimension(GRAPHIC_DIM *); 
int GraphicDrawTitle(GRAPHIC_DIM *); 
void SetDrawDim(GRAPHIC_DIM *gInfo); 
void GraphicDrawMinMaxWerte(GRAPHIC_DIM *g); 
void GraphicDrawXY(GRAPHIC_DIM *gInfo); 


/******************************************************************************/

BOOLEAN wsp_init_page(PRINT_INFO *prn_tx)
{
long height,width,
	  vres,hres;      // Druckerauflösung z.B. 300dpi
int h_pos,vl_pos,vr_pos;
int text_width;
BOOLEAN valid_printer;

 valid_printer = xvt_print_open_page(prn_tx->prn_rcd);
 if (valid_printer)
  {  
	xvt_dwin_set_font(prn_tx->win,font_id);
	xvt_app_escape(XVT_ESC_GET_PRINTER_INFO,prn_tx->prn_rcd,&height,&width,&vres,&hres);

	vl_pos =(int)(width / 14);  // linker Rand;
	vr_pos =(int)(width / 27);  // rechter Rand;
	h_pos =(int)(height / 28); // oberer/unterer Rand

	text_width = xvt_dwin_get_text_width(prn_tx->win,"W",-1);

	prn_tx->sheet_h = (int)(width -vl_pos -vr_pos);  // tatsächliche Druckbreite
	prn_tx->sheet_v = (int)(height -2*h_pos);      //tatsächliche Druckhöhe
	prn_tx->chr_per_line = prn_tx->sheet_h / text_width;  // Zeichen pro Zeile
	prn_tx->line_per_page = prn_tx->sheet_v / prn_tx->heigth;    //Zeilen
	prn_tx->x= vl_pos;   //linke Ecke oben
	prn_tx->y= h_pos;    //linke Ecke oben
  }
 return valid_printer;
}
/******************************************************************************/
/*********************  GRAFIK DRUCKEN    *************************************/
BOOLEAN XVT_CALLCONV1 wsp_print_graphics(long data) // Callback-Funktion
{
 int ok_file=IDOK;
 int size_of_rcd;
 BOOLEAN printer_ok =FALSE;
 //MinMax pmm;
 druckvorgang=TRUE;
 
 GRAPHIC_DIM *GDim;


 long height,width,
	  vres,hres;      // Druckerauflösung z.B. 300dpi

printer_ok=FALSE;
GDim = (GRAPHIC_DIM*)data;

font_id = xvt_font_create();
GDim->prn_rcd = xvt_print_create(&size_of_rcd);

if (xvt_print_is_valid(GDim->prn_rcd))


if (size_of_rcd>0)
 {
  /*    Seitenformat einrichten    */
    druckvorgang=FALSE;
    char buf[200],buf2[200],buf3[200],buf4[200];//Dick 26.11.99
    
    xvt_res_get_str(STR_DRUCKEN,buf,sizeof(buf));
    xvt_res_get_str(STR_ABBRECHEN,buf2,sizeof(buf2));   
    xvt_res_get_str(STR_EINRICHTEN,buf3,sizeof(buf3));
    xvt_res_get_str(STR_DRUCKVORGANG,buf4,sizeof(buf4));
    while(!druckvorgang)
//   	switch (xvt_dm_post_ask("Drucken","Abbrechen","Einrichten..","Druckvorgang"))
  switch(xvt_dm_post_ask(buf,buf2,buf3,buf4 ))

	{
				  case RESP_DEFAULT:       //SICHERN
						 {
						   druckvorgang=TRUE;
						 }
						  break;
				  case RESP_2:
							xvt_print_destroy(GDim->prn_rcd);
                            xvt_font_destroy(font_id);
                            druckvorgang=TRUE;
                            return 1;
						  break;             //zurück
				  case RESP_3:
                      xvt_dm_post_page_setup(GDim->prn_rcd);
                      break;
				 }
  //xvt_dm_post_page_setup(GDim->prn_rcd);

 xvt_app_escape(XVT_ESC_GET_PRINTER_INFO,GDim->prn_rcd,&height,&width,&vres,&hres);
 GDim->sheet_h =width;
 GDim->sheet_v=height;
 GDim->vRes=vres;
 GDim->hRes=hres;



  if ((GDim->prn_rcd) && (xvt_print_is_valid(GDim->prn_rcd)))
  {
	GDim->win = xvt_print_create_win(GDim->prn_rcd,"Grafik");
	if (GDim->win != NULL_WIN)
	 {
	  if( xvt_print_open_page(GDim->prn_rcd))
		 {
	      while ((xvt_print_get_next_band()) != NULL)
		  {
		   GetGraphicDimension(GDim);
		   SetDrawDim(GDim);

		   GraphicDrawRect(GDim);
		   GDim->textfile = new char[100];
		   strcpy(GDim->textfile,file_spec.name);

		   GraphicDrawTitle(GDim);

           GraphicDrawXY(GDim);
           
		   GraphicDrawMinMaxWerte(GDim);

		   paint->draw_117(GDim->win,&mmp);  //andere Datensätze darstellen, incl. Profillinie
		  }
          xvt_print_close_page(GDim->prn_rcd);

		  SetWin117Dim(); // Werte wieder rücksetzen für Win117
		  xvt_dwin_invalidate_rect(WIN_117,0);

		  printer_ok=TRUE;
		 }
	  else
		 {
		  //xvt_dm_post_error("Kein Zugriff auf Druckmanager");
          //char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_PRINTER_NOTE_2,buf,sizeof(buf));
          xvt_dm_post_error("%s",buf);
		  printer_ok = FALSE;
		 }
	 } //  -if(prn_tx->win)
	else //NULL_WIN oder Abbruch getätigt
	  {
        xvt_print_destroy(GDim->prn_rcd);
        xvt_font_destroy(font_id);
        return printer_ok;
	  }
  }
else
  {
	xvt_dm_post_error("WSP-Druck:PRINT_RCD is not valid");
	printer_ok = FALSE;
  }
if (GDim->win != NULL_WIN)
  {
	xvt_vobj_destroy(GDim->win);
	GDim->win = NULL_WIN;
  }
}
xvt_print_destroy(GDim->prn_rcd);
xvt_font_destroy(font_id);
return printer_ok;
}
/*-----------------------------------------------------------------*/
DWORD print_graphic(LPVOID *lPtr)
  {
	 GRAPHIC_DIM *GraphicDimensions,gInfo;

	 PRINT_INFO *print_info,info;
	 print_info = &info;

	 InitGraphicDimension(&gInfo);
	 GraphicDimensions= &gInfo;

	 if (!xvt_print_start_thread(wsp_print_graphics,(long)GraphicDimensions))
	    return 1;//	 xvt_dm_post_error("Druckerfehler !");
   return 1;
  }
/*-----------------------------------------------------------------*/
void InitGraphicDimension(GRAPHIC_DIM *g)
{
 g->win=NULL_WIN;
 g->sheet_h=g->sheet_v=0;
 g->dim.top=g->dim.bottom=g->dim.right=g->dim.left=0;
 g->textfile=NULL;
 g->prn_rcd=NULL;
}
/*-----------------------------------------------------------------*/
int GraphicDrawTitle(GRAPHIC_DIM *g)
{
 char *temp,date[100];


 GetDateString(date);
 if ((g->textfile!=NULL)&&(g->win !=NULL_WIN))
  {
	temp= new char[strlen(g->textfile)+100];
	strcpy(temp,g->textfile);
	strcat(temp,"  Datum: ");
	strcat(temp,date);
	xvt_dwin_set_font_family(g->win,XVT_FFN_TIMES);
	xvt_dwin_set_font_size(g->win,10);
	xvt_dwin_draw_text(g->win,g->dim.left,g->dim.top-10,temp,strlen(temp));
	delete[] temp;
	return TRUE;
 }
 return FALSE;
}
/*-----------------------------------------------------------------*/
void GetGraphicDimension(GRAPHIC_DIM *gInfo)
{
  if (gInfo->sheet_v > gInfo->sheet_h ) // =Hochformat
	 {
	  gInfo->dim.left =gInfo->sheet_h/15;   //Breite
	  gInfo->dim.top =gInfo->sheet_v/50;   //Höhe
	  gInfo->dim.right =gInfo->sheet_h-(gInfo->sheet_h/60);   //Breite
	  gInfo->dim.bottom =(gInfo->sheet_v-(gInfo->sheet_v/25))/2;   //Höhe

	 }
  else  // Querformat
	 {
	  gInfo->dim.left =gInfo->sheet_h/60;   //Breite
	  gInfo->dim.top =gInfo->sheet_v/15;   //Höhe
	  gInfo->dim.right =gInfo->sheet_h-(gInfo->sheet_h/65);   //Breite
	  gInfo->dim.bottom =gInfo->sheet_v-(gInfo->sheet_v/65);   //Höhe
	 }
}
/*-----------------------------------------------------------------*/
void SetDrawDim(GRAPHIC_DIM *gInfo)
{
 double line=1,faktor=1;

 if (gInfo->sheet_v > gInfo->sheet_h ) // =Hochformat
  {
	WspDrawDimensions.MAIN_DRAW_WIN_X = (gInfo->dim.right - gInfo->dim.left);
	WspDrawDimensions.MAIN_DRAW_WIN_Y = (UINT)(WspDrawDimensions.MAIN_DRAW_WIN_X /1.5);

	WspDrawDimensions.DX         = (UINT)(gInfo->sheet_h /7.0);
	WspDrawDimensions.DY         = (UINT)(gInfo->sheet_v /10);
	WspDrawDimensions.DYY        = (UINT)(gInfo->sheet_v /24)+3*gInfo->dim.top;
    

	WspDrawDimensions.DELTA_X    = (UINT)(WspDrawDimensions.MAIN_DRAW_WIN_X - (1.5*WspDrawDimensions.DX));   //max Zeichenbreite
	WspDrawDimensions.DELTA_Y    = WspDrawDimensions.MAIN_DRAW_WIN_Y - WspDrawDimensions.DY -WspDrawDimensions.DYY ;
	WspDrawDimensions.AB         = (WspDrawDimensions.MAIN_DRAW_WIN_X / 112);


	if ( (gInfo->hRes<500)||(gInfo->vRes<500)) faktor =1.5;
	else faktor =2.2;
	if (WspDrawDimensions.MAIN_DRAW_WIN_X>3000) line =1.3;
	if (WspDrawDimensions.MAIN_DRAW_WIN_X>5000) line =2;

	WspDrawDimensions.wspLineWidth0 =(UINT)(1.0*faktor*line);
	WspDrawDimensions.wspLineWidth1 =(UINT)(2.0*faktor*line);
	WspDrawDimensions.wspLineWidth2 =(UINT)(3.0*faktor*line);
	WspDrawDimensions.wspLineWidth3 =(UINT)(4.0*faktor*line);
	WspDrawDimensions.wspLineWidth4 =(UINT)(5.0*faktor*line);

	WspDrawDimensions.offset=2*gInfo->dim.top;
	WspDrawDimensions.vertDist=(UINT)(WspDrawDimensions.MAIN_DRAW_WIN_Y/1.4); //1,45
	WspDrawDimensions.vertMax=WspDrawDimensions.DELTA_Y+WspDrawDimensions.DYY-5;//334

	WspDrawDimensions.res1=WspDrawDimensions.vertMax-WspDrawDimensions.vertDist;  //=80

	WspDrawDimensions.Y=WspDrawDimensions.DELTA_Y-WspDrawDimensions.res1;//220;
  }
 else  //Querformat
  {
	WspDrawDimensions.MAIN_DRAW_WIN_X = (gInfo->dim.right - gInfo->dim.left);
	WspDrawDimensions.MAIN_DRAW_WIN_Y = (UINT)(WspDrawDimensions.MAIN_DRAW_WIN_X /1.5);

	WspDrawDimensions.DX         = (UINT)(gInfo->sheet_h /18.6);
	WspDrawDimensions.DY         = (UINT)(gInfo->sheet_v /10);
	WspDrawDimensions.DYY        = (UINT)(gInfo->sheet_v /24)+3*gInfo->dim.top;
    

	WspDrawDimensions.DELTA_X    = (UINT)(WspDrawDimensions.MAIN_DRAW_WIN_X - (1.5*WspDrawDimensions.DX));   //max Zeichenbreite
	WspDrawDimensions.DELTA_Y    = WspDrawDimensions.MAIN_DRAW_WIN_Y - WspDrawDimensions.DY -WspDrawDimensions.DYY ;
	WspDrawDimensions.AB         = (WspDrawDimensions.MAIN_DRAW_WIN_X / 112);


	if ( (gInfo->hRes<500)||(gInfo->vRes<500)) faktor =1.5;
	else faktor =2.2;
	if (WspDrawDimensions.MAIN_DRAW_WIN_X>3000) line =1.3;
	if (WspDrawDimensions.MAIN_DRAW_WIN_X>5000) line =2;

	WspDrawDimensions.wspLineWidth0 =(UINT)(1.0*faktor*line);
	WspDrawDimensions.wspLineWidth1 =(UINT)(2.0*faktor*line);
	WspDrawDimensions.wspLineWidth2 =(UINT)(3.0*faktor*line);
	WspDrawDimensions.wspLineWidth3 =(UINT)(4.0*faktor*line);
	WspDrawDimensions.wspLineWidth4 =(UINT)(5.0*faktor*line);

	WspDrawDimensions.offset=2*gInfo->dim.top;
	WspDrawDimensions.vertDist=(UINT)(WspDrawDimensions.MAIN_DRAW_WIN_Y/1.4); //1,45
	WspDrawDimensions.vertMax=WspDrawDimensions.DELTA_Y+WspDrawDimensions.DYY-5;//334

	WspDrawDimensions.res1=WspDrawDimensions.vertMax-WspDrawDimensions.vertDist;  //=80
	WspDrawDimensions.Y=(UINT)(WspDrawDimensions.DELTA_Y-0.8*WspDrawDimensions.res1);//220;
  }
 WspDrawDimensions.isPrinting=TRUE;
}
/*-----------------------------------------------------------------*/
/*-----------------------------------------------------------------*/
void SetWin117Dim(void)
{
 WspDrawDimensions.MAIN_DRAW_WIN_X = 560;  //Fenstergrösse X
 WspDrawDimensions.MAIN_DRAW_WIN_Y = 370;  //Fenstergrösse Y
 WspDrawDimensions.DELTA_X    = (int)(res_factor*560 - 45*res_factor);   //max Zeichenbreite
 WspDrawDimensions.DELTA_Y    = (int)(res_factor*370 - 50*res_factor);   //max.Zeichenhöhe
 WspDrawDimensions.AB         = (int)(5*res_factor);			// GHJ
 WspDrawDimensions.DX         = (int)(30*res_factor);
 WspDrawDimensions.DY         = (int)(35*res_factor);
 WspDrawDimensions.DYY        = (int)(15*res_factor);
 WspDrawDimensions.wspLineWidth0 =0;
 WspDrawDimensions.wspLineWidth1 =1;
 WspDrawDimensions.wspLineWidth2 =2;
 WspDrawDimensions.wspLineWidth3 =3;
 WspDrawDimensions.wspLineWidth4 =4;
 WspDrawDimensions.offset=0;  /* muss 0 sein !!!!! */
 WspDrawDimensions.vertDist=(int)(res_factor*255);
 WspDrawDimensions.vertMax=(int)(res_factor*334);
 WspDrawDimensions.Y=(int)(res_factor*220);
 WspDrawDimensions.res1=(int)(res_factor*80);
 WspDrawDimensions.isPrinting=FALSE;
}
/*-----------------------------------------------------------------*/
int GraphicDrawRect(GRAPHIC_DIM *gInfo)
{    /*  äußeren Blatt - Rahmen zeichnen */
 PNT pnt;
 DRAW_CTOOLS cTools;

 if (gInfo->win !=NULL_WIN)
 {
  xvt_app_get_default_ctools(&cTools);
  cTools.pen.width = WspDrawDimensions.wspLineWidth2;
  cTools.pen.style =P_SOLID;
  cTools.pen.color = COLOR_BLACK;
  xvt_dwin_set_draw_ctools(gInfo->win, &cTools);

  pnt.h =gInfo->dim.left;  pnt.v =gInfo->dim.bottom;
  xvt_dwin_draw_set_pos(gInfo->win, pnt);
  pnt.h =gInfo->dim.left;  pnt.v =gInfo->dim.top;   // links vert
  xvt_dwin_draw_line(gInfo->win, pnt);

  pnt.h =gInfo->dim.right;  pnt.v =gInfo->dim.top;   // rechts vert
  xvt_dwin_draw_set_pos(gInfo->win, pnt);
  pnt.h =gInfo->dim.right;  pnt.v =gInfo->dim.bottom;
  xvt_dwin_draw_line(gInfo->win, pnt);

  pnt.h =gInfo->dim.left;  pnt.v =gInfo->dim.bottom;   // unten hor.
  xvt_dwin_draw_set_pos(gInfo->win, pnt);
  pnt.h =gInfo->dim.right;  pnt.v =gInfo->dim.bottom;
  xvt_dwin_draw_line(gInfo->win, pnt);


  pnt.h =gInfo->dim.right;  pnt.v =gInfo->dim.top;   // oben hor.
  xvt_dwin_draw_set_pos(gInfo->win, pnt);
  pnt.h =gInfo->dim.left;  pnt.v =gInfo->dim.top;
  xvt_dwin_draw_line(gInfo->win, pnt);
 }
 return TRUE;
}
/*-----------------------------------------------------------------*/
void GraphicDrawMinMaxWerte(GRAPHIC_DIM *g)
{
 MinMax pmm;
 char strtemp[15];
 int heigth;//Dick 11.02.99 weg pos,
 int leadingp,ascentp,descentp;
 XVT_FONT_STYLE_MASK old_mask;//Dick 5.08.99

 list->GetMinMax(&pmm,scr.datensatz);//Dick 5.08.99  scr.datensatz statt 0
 xvt_dwin_set_font_family(g->win,XVT_FFN_TIMES);
 xvt_dwin_set_font_size(g->win,8);
 old_mask=xvt_dwin_get_font_style(g->win);//Dick 5.08.99

 xvt_dwin_get_font_metrics(g->win,&leadingp,&ascentp,&descentp);
 heigth = leadingp+ascentp+descentp;   // Zeilenhöhe

 sprintf(strtemp,"%.2f",pmm.minX);
 xvt_dwin_draw_text(g->win,			
					(WspDrawDimensions.DX-WspDrawDimensions.AB),//X
					(int)(WspDrawDimensions.DELTA_Y+WspDrawDimensions.DYY+WspDrawDimensions.AB+1.4*heigth),//Y
					strtemp,strlen(strtemp));


 strcpy(strtemp,"[m]");
 xvt_dwin_draw_text(g->win,		
					WspDrawDimensions.DELTA_X+WspDrawDimensions.DX-WspDrawDimensions.AB, //X
					(int)(WspDrawDimensions.DELTA_Y+WspDrawDimensions.DYY+WspDrawDimensions.AB+2.6*heigth),   //Y
					strtemp,strlen(strtemp));
 
 sprintf(strtemp,"%.2f",pmm.maxX);
 xvt_dwin_draw_text(g->win,		
					WspDrawDimensions.DELTA_X+WspDrawDimensions.DX-WspDrawDimensions.AB , //X
					(int)(WspDrawDimensions.DELTA_Y+WspDrawDimensions.DYY+WspDrawDimensions.AB+1.4*heigth),   //Y
					strtemp,strlen(strtemp));

 

  // linke Zahlen
 strcpy(strtemp,"[NN+m]");//Dick 5.08.99
 xvt_dwin_draw_text(g->win,  // Einheit links-oben
					(g->dim.left + WspDrawDimensions.AB),
					(int)(WspDrawDimensions.vertDist-WspDrawDimensions.Y-1.2*heigth),//Dick 11.02.99
					strtemp,strlen(strtemp));
 
 xvt_dwin_set_font_style(g->win,old_mask|XVT_FS_UNDERLINE);//Dick 5.08.99
 sprintf(strtemp,"%.2f",pmm.minY);
 xvt_dwin_draw_text(g->win,  //unten y-Achse
					g->dim.left + WspDrawDimensions.AB,
					WspDrawDimensions.vertDist,
					strtemp,strlen(strtemp));

 sprintf(strtemp,"%.2f",pmm.maxY);
 xvt_dwin_draw_text(g->win, //oben y-Achse
					g->dim.left + WspDrawDimensions.AB,
					(int)(WspDrawDimensions.vertDist-WspDrawDimensions.Y), //Dick 11.02.99
					strtemp,strlen(strtemp));


 //Neu Dick 15.03.99 scalierung
              
 double ersterpunkt,letzterpunkt;
 
 double delta_station;
 double schritt=0.1;//default 0.1
 bool gefunden=FALSE;
 int anzahl_punkte=3;//default 3
 int i;
 int delta_pnt=WspDrawDimensions.Y;
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
 //schritt_pnt=(int)((ersterpunkt_pnt-letzterpunkt_pnt)*schritt/(letzterpunkt-ersterpunkt));
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
     xvt_dwin_draw_text(g->win,
         (g->dim.left + WspDrawDimensions.AB),
         //ersterpunkt_pnt-schritt_pnt,
         (int)( WspDrawDimensions.vertDist- (  (ersterpunkt+schritt*i - pmm.minY) * factorY )),
         strtemp,strlen(strtemp));
     }
 //ende Scalierung


 xvt_dwin_set_font_style(g->win,old_mask);//Dick 5.08.99
}
/*-----------------------------------------------------------------*/
void GraphicDrawXY(GRAPHIC_DIM *gInfo)
/***  X Y Koordinatensystem zeichnen   *******/
{
	PNT pnt;
	DRAW_CTOOLS rect_tools;

	  xvt_app_get_default_ctools(&rect_tools);

	  /***  X Y Koordinatensystem zeichnen   *******/
	  rect_tools.pen.width = WspDrawDimensions.wspLineWidth3;
	  xvt_dwin_set_draw_ctools(gInfo->win, &rect_tools);
	  /* Y-LINE LEFT */
	  pnt.h = WspDrawDimensions.DX;
      pnt.v=WspDrawDimensions.vertDist-WspDrawDimensions.Y-WspDrawDimensions.AB;
	  //	  pnt.v = WspDrawDimensions.DYY;
	  xvt_dwin_draw_set_pos(gInfo->win, pnt);
	  pnt.h = WspDrawDimensions.DX;
	  pnt.v = WspDrawDimensions.DELTA_Y+WspDrawDimensions.DYY+WspDrawDimensions.AB;
	  xvt_dwin_draw_line(gInfo->win, pnt);

	  /* X-LINE BOTTOM  */
	  pnt.h = WspDrawDimensions.DX-WspDrawDimensions.AB;
	  pnt.v = WspDrawDimensions.DELTA_Y+WspDrawDimensions.DYY;
	  xvt_dwin_draw_set_pos(gInfo->win, pnt);
	  pnt.h = WspDrawDimensions.DELTA_X+WspDrawDimensions.DX+WspDrawDimensions.AB;
	  pnt.v = WspDrawDimensions.DELTA_Y+WspDrawDimensions.DYY;
	  xvt_dwin_draw_line(gInfo->win, pnt);

	  /* Y-LINE RIGHT  */
	  pnt.h = WspDrawDimensions.DELTA_X+WspDrawDimensions.DX;
      pnt.v=WspDrawDimensions.vertDist-WspDrawDimensions.Y-WspDrawDimensions.AB;
//	  pnt.v = WspDrawDimensions.DYY;
	  xvt_dwin_draw_set_pos(gInfo->win, pnt);
	  pnt.h = WspDrawDimensions.DELTA_X+WspDrawDimensions.DX;
	  pnt.v = WspDrawDimensions.DELTA_Y+WspDrawDimensions.DYY+WspDrawDimensions.AB;
	  xvt_dwin_draw_line(gInfo->win, pnt);
}
/*-----------------------------------------------------------------*/
/*-----------------------------------------------------------------*/
