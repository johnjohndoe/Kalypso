//////////////
// wspdde.h //
//////////////

#ifndef _WSPDDE_H_INCLUDED_
#define _WSPDDE_H_INCLUDED_

BOOL InitDDE(char* szApplication, char* szCommandString, char* szTopicRequest, char* szData, BOOL bTerminateExe=FALSE); // wspm001
LRESULT APIENTRY DDEWndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam); // wspwin
HWND CreateDDEWnd(HINSTANCE hinstance, HWND hParent); // wspwin

#endif _WSPDDE_H_INCLUDED_