////////////
// plot.h //
////////////

/*Funktionen für plot100.cpp*/

#ifndef _PLOT_H_INCLUDED_
#define _PLOT_H_INCLUDED_

typedef struct _LINE12
{
  char anlage[15];        // Blatt-Anlage Nr
  
  int  stationsbeschr,    // stationsbeschriftung [0,1,2]
    antragsteller,     // antragsteller/änderungsschild [0...7]
    querfelder,        // max. 3
    schriftfeldzeilen[3], // Anzahl ..
    textzeilen[3];        // Anzahl ..
  char text[6][10];       // Textzeilen 
  
} LINE12; // plot, plot100

typedef struct _LINE15
{
  char dist_schriftfeld[15],
		  paperlength[15],
      paperhigh[15],
      bezugshoehe_NN[15],
      x_mass[15],
      y_mass[15],
      u_schriftfeld,
      u_werte,
      u_bzghoehe,
      schriftfeldzeilen[25];
} LINE15; // plot, plot101

typedef struct _PLOTTER
{
	 char layer[5];
   int color;
   int line;
} PLOTTER;


int get_line_12(LINE12*); // plot100
int save_line_12(LINE12*); // plot100
int get_line_15(LINE15*,SLIST ); // plot101
int save_line_15(LINE15*); // plot101
void set_options(WINDOW *lb,WINDOW *edit_pl100, LINE12 *l12); // plot100
void mark_line(WINDOW, int); // wspw131
int read_plotter_cfg(PLOTTER *); // wspw133
int save_plotter_cfg(PLOTTER *); // wspw133


extern void xdCheckRadioButton XVT_CC_ARGS((WINDOW xdWindow, int check, int start, int end)); // definiert in wspwin.cpp, oft

extern long XVT_CALLCONV1
PLOT_100_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define plot_101_eh PLOT_101_eh
extern long XVT_CALLCONV1
PLOT_101_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent)); // definiert in plot100.cpp, benutzt in wspw121


#define PLOT_100 104
#define PLOT_100_PB_OK 1
#define PLOT_100_PB_CANCEL 2
#define PLOT_100_TEXT_1 3
#define PLOT_100_TEXT_2 4
#define PLOT_100_TEXT_3 5
#define PLOT_100_EDIT_4 6
#define PLOT_100_LB_9 7
#define PLOT_100_LB_10 8
#define PLOT_100_TEXT_12 9
#define PLOT_100_TEXT_13 10
#define PLOT_100_TEXT_14 11
#define PLOT_100_GROUPBOX_21 12
#define PLOT_100_TEXT_24 13
#define PLOT_100_EDIT_25 14
#define PLOT_100_EDIT_26 15
#define PLOT_100_EDIT_27 16
#define PLOT_100_EDIT_28 17
#define PLOT_100_EDIT_29 18
#define PLOT_100_EDIT_30 19
#define PLOT_100_LB_31 20
#define PLOT_100_LB_32 21
#define PLOT_100_LB_33 22
#define PLOT_100_LB_34 23
#define PLOT_100_LB_35 24
#define PLOT_100_LB_36 25
#define PLOT_100_LB_37 26

#define PLOT_101 105
#define PLOT_101_PB_OK 1
#define PLOT_101_PB_CANCEL 2
#define PLOT_101_TEXT_1 3
#define PLOT_101_TEXT_2 4
#define PLOT_101_TEXT_3 5
#define PLOT_101_TEXT_4 6
#define PLOT_101_TEXT_5 7
#define PLOT_101_EDIT_8 8
#define PLOT_101_EDIT_9 9
#define PLOT_101_EDIT_10 10
#define PLOT_101_TEXT_11 11
#define PLOT_101_LB_12 12
#define PLOT_101_CHECKBOX_14 13
#define PLOT_101_CHECKBOX_15 14
#define PLOT_101_TEXT_16 15
#define PLOT_101_EDIT_20 16
#define PLOT_101_EDIT_21 17
#define PLOT_101_EDIT_22 18
#define PLOT_101_EDIT_23 19
#define PLOT_101_EDIT_24 20
#define PLOT_101_TEXT_25 21
#define PLOT_101_LB_26 22
#define PLOT_101_LB_27 23
#define PLOT_101_LB_28 24

#endif _PLOT_H_INCLUDED_