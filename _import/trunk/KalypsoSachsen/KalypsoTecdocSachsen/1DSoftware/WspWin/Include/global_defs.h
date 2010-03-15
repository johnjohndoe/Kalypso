//
//  globale Definitionen für wspwin
//

#ifndef _GLOBAL_DEFS_H_
#define _GLOBAL_DEFS_H_

// aus bce_allg.h
#define LENLINE 5000 

#define PATH_LEN 1024

/**********    aus scr_edit.h  **********/
#define INI_FILE_NAME "WSPWIN.INI"  // win.ini /wspwin.ini

#define IDC_AUTO	      101
#define IDC_GROUPBOX1	105
#define IDC_DISPLAY	   125
#define IDC_RADIO1024	104
#define IDC_RADIO800	   103
#define IDC_RADIO640	   102

#define MODEAUTO 0
#define MODE640  640
#define MODE800  800
#define MODE1024 1024




#define ZOOM_1 101
#define ZOOM_1_TEXT_1 1
#define ZOOM_2 102
#define ZOOM_2_TEXT_1 1

#define TYPE_SIZE 60

// aus bce_type.h
#define BCE_NAN      -1.23456789E+10


// aus values.h

#define MAXDOUBLE   1.797693E+308
#define MAXFLOAT    3.37E+38F
#define MINDOUBLE   2.225074E-308
#define MINFLOAT    8.43E-37F
#define MAXINT      0x7FFFFFFF


// aus strang.h
#define STRANGANZAHL 3000
#define KOORDINPROFILANZAHL	1000

// aus init.h
#define NO_STD_HELP_MENU
#define NO_HELP_RESOURCES

// wspw...

#define K_TAB 9
#define ENTER 13
#define ESC   27

#define E_USER_RAUH 0
#define E_USER_BEWUCHS 1
#define E_USER_ZOOM 2
#define E_USER_ZOOM_NORMAL 3
#define E_USER_AUTOSAVE 4
#define E_USER_UEBERFALLBEIWERT 5

// wspw120, w116

#define K_X 0
#define K_Y 1
#define K_Z 2

#define EVENT_ZOOM_NORMAL 1


// wspw130, wspd161

#define STATIONSWERTE 1
#define HOEHENWERTE   2


// aus wspm001.cpp

#define E_AUTOLOAD 1


#define PRECISION_KRD 0.00001   // mit dieser Precision werden Koordinatenwerte verglichen


/**********************/

#endif
