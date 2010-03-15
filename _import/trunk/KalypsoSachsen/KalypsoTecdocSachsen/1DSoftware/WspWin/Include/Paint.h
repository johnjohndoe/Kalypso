/////////////
// paint.h //
/////////////

#ifndef _PAINT_H_INCLUDED_
#define _PAINT_H_INCLUDED_

#include "list.h"

#define COLOR_BROWN 0x0CAFAF00L // paint.cpp, wspd143, laengs.dll
#define COLOR_GRUEN 0x0CAFAF00L
#define COLOR_ROT 0x0CAFAF00L

typedef struct _DRAW_DIMENSIONS // printer, wspd110, wspd154, wspw117
{
  UINT MAIN_DRAW_WIN_X;  //Fenstergrösse X
  UINT MAIN_DRAW_WIN_Y;  //Fenstergrösse Y
  UINT DELTA_X   ;   //max Zeichenbreite
  UINT DELTA_Y   ;   //max.Zeichenhöhe
  UINT AB        ;
  UINT DX        ;
  UINT DY        ;
  UINT DYY       ;
  
  BYTE wspLineWidth0;
  BYTE wspLineWidth1;
  BYTE wspLineWidth2;
  BYTE wspLineWidth3;
  BYTE wspLineWidth4;
  UINT offset,res1,res2;
  UINT vertMax,vertDist,Y;
  BYTE isPrinting;
  
} DRAW_DIMENSIONS;

typedef struct _MouseRect
{
  int links,rechts,oben,unten;
  double d_links,d_rechts,d_oben,d_unten;
  BOOLEAN treffer;
} MouseRect;

class Paint : public List
{
public:
  Paint();
  ~Paint(void);
  void draw_rect(WINDOW xdWindow); // hier, wspw117
  void draw_marker(WINDOW win,int position); // wspw116, wspw117
  void draw_min_max_werte(WINDOW xdWindow); // hier
  void draw_trapez(WINDOW); // wspd142
  void draw_kreis(WINDOW,int); // wspd144, 155
  void draw_eiprofil(WINDOW); // wspd141
  void draw_maulprofil(WINDOW); // wspd141
  void draw_117(WINDOW ,MMP *); // printer, wspw117
  int  Get_Mouse_Position(int hor,int ver); // wspw117
  int  Get_Mouse_Position_GH(int hor,int ver); // dito
  int  Get_Einf_Position(int hor,int ver); // wspd137
  int  Get_MRPosition(int hor,int ver); // wspw117
  void Paint117(WINDOW xdWindow, MinMax *pmm); // hier, wspw117
  void DrawTrennflaechen(WINDOW win, MinMax *pmm,int datensatz,long color); // hier, wspd143
  void DrawRauhigkeit(WINDOW win, MinMax *pmm,int datensatz); // hier
  void DurchstBereiche(WINDOW win, MinMax *pmm,int datensatz); // wspd140
  void PaintBruecke(WINDOW win, MinMax *pmm,int datensatz,COLOR color); // hier
  void edit_list_pnt(MMP *,MinMax *); // wspd110, wspd154, w117
  void Edit_Rauhigkeit(double faktor,int typ,MMP *mmp); // wspd154
  void Edit_Rauhigkeit(double faktor,int typ,double anfang,double ende,int datensatz); // dito
  void PaintAX(WINDOW win, MinMax *pmm,int datensatz,COLOR color); // hier
  void PaintDK(WINDOW win, MinMax *pmm,int datensatz,COLOR color);// hier
  void PaintStation(WINDOW win, MinMax *pmm,int datensatz,COLOR color);// hier
  void PaintWSF(WINDOW win, MinMax *pmm,int datensatz,COLOR color);// hier
  void DrawBewuchs(WINDOW win, MinMax *pmm,int datensatz);//Dick 16.04.99 // hier
  void PaintBuhne(WINDOW win, MinMax *pmm,int datensatz,COLOR color);
  
private:
  void Edit_Gelaende(WINDOW win,Koord *pp,MMP *mmp,MinMax *pmm);
  void Edit_Trennflaechen( WINDOW win, Koord* pp, MMP* mmp, MinMax* pmm, long typ );
  void Edit_Buhnen(WINDOW win,Koord *pp,MMP *mmp,MinMax *pmm);
  void Edit_DurchstBereiche(WINDOW win,Koord *pp,MMP *mmp);
  void Highligth_mouse_move(WINDOW,MMP*);
};



#endif _PAINT_H_INCLUDED_

