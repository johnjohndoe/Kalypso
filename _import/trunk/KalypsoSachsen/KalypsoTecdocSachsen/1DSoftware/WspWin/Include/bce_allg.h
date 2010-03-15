#ifndef _BCEDLL_H
#define _BCEDLL_H
/***************************************************
					Headerdatei zu BCE_DLL.CPP

					allgemeine BCE Funktionen

****************************************************/

#include <windows.h>

void set_menu_116(WINDOW xdWindow,BOOLEAN zustand ); // wspw116.cpp
void set_menu_120(WINDOW xdWindow,BOOLEAN zustand); // wspw120.cpp
void set_menu_dlg136(WINDOW xdWindow,BOOLEAN zustand); // wspd136.cpp
void disable_menu(WINDOW xdWindow); // wspd148.cpp, wspwin.cpp

int  is_zahl( char* str ); // oft in wspwin.exe
double get_zahl( char* valueStr );
void NewSetFontFunc(WINDOW win); // oft in wspwin.exe
int  check_string(char *str); // wspd128.cpp, wspd147.cpp
void xvt_slist_change_str(SLIST list,char *str,int pos); // oft in wspwin.exe

#endif _BCEDLL_H
