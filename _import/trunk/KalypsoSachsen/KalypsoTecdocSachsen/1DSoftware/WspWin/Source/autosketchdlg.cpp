/****************************************************************************
*             AUTOSKETCHDLG.CPP                                                  *
*             02.04.1996                                                    *
*                                                                           *
*   Kompileroption:  LARGE Memory Modell !!!!                               *
*****************************************************************************/
#include <windows.h>
#include "xvt.h"

#include "resource.h"

#include "autosketchdlg.h"

int programm = -1;

typedef struct _LOADPARMS 
{
	 WORD   segEnv;                  /* child environment  */
	 LPSTR  lpszCmdLine;             /* child command tail */
	 LPWORD lpwShow;                 /* how to show child  */
	 LPWORD lpwReserved;             /* must be NULL       */
} LOADPARMS;


/****************************************************************************/
BOOL DlgAutoSketch(HWND hDlg,WORD msg,WPARAM wParam,LPARAM lParam)
{
  char path[256],*tmp;
  int i;
  OPENFILENAME aSketch;
  char szFilter[256],
    szDirName[256];
  char szText[]      = "*.exe Programmdateien (*.exe)|*.exe||";
  
  switch (msg)
  {
  case WM_INITDIALOG:
    {
      switch(programm)
      {
      case MODE_ASKETCH_OPTIONS:
        SetWindowText(hDlg,"CAD-Programm");
        GetPrivateProfileString("WSPWIN","AUTOSKETCH","unbekannt",path,255,"WSPWIN.INI");
        SetDlgItemText(hDlg,IDC_PATH,path);
        break;
      case MODE_EDITOR_OPTIONS:
        SetWindowText(hDlg,"Editor");
        GetPrivateProfileString("WSPWIN","EDITOR","unbekannt",path,255,"WSPWIN.INI");
        SetDlgItemText(hDlg,IDC_PATH,path);
        break;
      case MODE_SONDERPROG_OPTIONS:
        SetWindowText(hDlg,"Sonderprogramm");
        GetPrivateProfileString("WSPWIN","SONDERPROGRAMM","unbekannt",path,255,"WSPWIN.INI");
        SetDlgItemText(hDlg,IDC_PATH,path);
        break;
      }
    }
    break;
  case WM_COMMAND:
    switch(wParam)
    {
		  case IDC_OK:
        {
          switch(programm)
          {
          case MODE_ASKETCH_OPTIONS:
            
            GetDlgItemText(hDlg,IDC_PATH,path,255);
            if (!WritePrivateProfileString("WSPWIN","AUTOSKETCH",path,"WSPWIN.INI"))
              xvt_dm_post_error("Änderungen in WSPWIN.INI können nicht geschrieben werden.Schreibrechte beachten.");
            EndDialog(hDlg,TRUE);
            break;
          case MODE_EDITOR_OPTIONS:
            
            GetDlgItemText(hDlg,IDC_PATH,path,255);
            if (!WritePrivateProfileString("WSPWIN","EDITOR",path,"WSPWIN.INI"))
              xvt_dm_post_error("Änderungen in WSPWIN.INI können nicht geschrieben werden.Schreibrechte beachten.");
            EndDialog(hDlg,TRUE);
            break;
          case MODE_SONDERPROG_OPTIONS:
            
            GetDlgItemText(hDlg,IDC_PATH,path,255);
            if (!WritePrivateProfileString("WSPWIN","SONDERPROGRAMM",path,"WSPWIN.INI"))
              xvt_dm_post_error("Änderungen in WSPWIN.INI können nicht geschrieben werden.Schreibrechte beachten.");
            EndDialog(hDlg,TRUE);
            break;
          }
        }
        return TRUE;
      case IDC_CANCEL:
        EndDialog(hDlg,wParam);
        return TRUE;
      case IDC_SEARCH:
        {
          switch(programm)
          {
          case MODE_ASKETCH_OPTIONS:
            
            GetPrivateProfileString("WSPWIN","AUTOSKETCH","unbekannt",path,255,"WSPWIN.INI");
            strcpy(szDirName,path);
            tmp = strrchr(szDirName,'\\');
            if (tmp) tmp[0]='\0';
            
            memset(&aSketch,0,sizeof(OPENFILENAME));
            for(i=0; szText[i]!= '\0'; ++i)
              szFilter[i] = szText[i] == '|' ? '\0' : szText[i];
            
            aSketch.hwndOwner = hDlg;
            aSketch.lpstrTitle ="CAD-Programm suchen";
            aSketch.lStructSize = sizeof(OPENFILENAME);
            aSketch.lpstrFilter = szFilter;
            aSketch.nMaxFile = 255;
            aSketch.nMaxFileTitle = _MAX_FNAME + _MAX_EXT;
            aSketch.lpstrDefExt = "exe" ;
            aSketch.Flags = OFN_FILEMUSTEXIST | OFN_HIDEREADONLY |OFN_PATHMUSTEXIST;
            aSketch.lpstrFile = (LPSTR)path;
            aSketch.lpstrInitialDir=szDirName;
            
            if(GetOpenFileName(&aSketch))
            {
              strcpy(path, aSketch.lpstrFile);
              SetDlgItemText(hDlg,IDC_PATH,path);
            }
            break;
          case MODE_EDITOR_OPTIONS:
            
            GetPrivateProfileString("WSPWIN","EDITOR","unbekannt",path,255,"WSPWIN.INI");
            strcpy(szDirName,path);
            tmp = strrchr(szDirName,'\\');
            if (tmp) tmp[0]='\0';
            
            memset(&aSketch,0,sizeof(OPENFILENAME));
            for(i=0; szText[i]!= '\0'; ++i)
              szFilter[i] = szText[i] == '|' ? '\0' : szText[i];
            
            aSketch.hwndOwner = hDlg;
            aSketch.lpstrTitle ="Editor suchen";
            aSketch.lStructSize = sizeof(OPENFILENAME);
            aSketch.lpstrFilter = szFilter;
            aSketch.nMaxFile = 255;
            aSketch.nMaxFileTitle = _MAX_FNAME + _MAX_EXT;
            aSketch.lpstrDefExt = "exe" ;
            aSketch.Flags = OFN_FILEMUSTEXIST | OFN_HIDEREADONLY |OFN_PATHMUSTEXIST;
            aSketch.lpstrFile = (LPSTR)path;
            aSketch.lpstrInitialDir=szDirName;
            
            if(GetOpenFileName(&aSketch))
            {
              strcpy(path, aSketch.lpstrFile);
              SetDlgItemText(hDlg,IDC_PATH,path);
            }
            break;
          case MODE_SONDERPROG_OPTIONS:
            
            GetPrivateProfileString("WSPWIN","SONDERPROGRAMM","unbekannt",path,255,"WSPWIN.INI");
            strcpy(szDirName,path);
            tmp = strrchr(szDirName,'\\');
            if (tmp) tmp[0]='\0';
            
            memset(&aSketch,0,sizeof(OPENFILENAME));
            for(i=0; szText[i]!= '\0'; ++i)
              szFilter[i] = szText[i] == '|' ? '\0' : szText[i];
            
            aSketch.hwndOwner = hDlg;
            aSketch.lpstrTitle ="Sonderprogramm suchen";
            aSketch.lStructSize = sizeof(OPENFILENAME);
            aSketch.lpstrFilter = szFilter;
            aSketch.nMaxFile = 255;
            aSketch.nMaxFileTitle = _MAX_FNAME + _MAX_EXT;
            aSketch.lpstrDefExt = "exe" ;
            aSketch.Flags = OFN_FILEMUSTEXIST | OFN_HIDEREADONLY |OFN_PATHMUSTEXIST;
            aSketch.lpstrFile = (LPSTR)path;
            aSketch.lpstrInitialDir=szDirName;
            
            if(GetOpenFileName(&aSketch))
            {
              strcpy(path, aSketch.lpstrFile);
              SetDlgItemText(hDlg,IDC_PATH,path);
            }
            break;
            
            
          }
        }
        break;
     }
     
     break;
  default: break;
  }
  return FALSE;
}

/****************************************************************************/
int StartAutoSketch(WINDOW parentWin,int mode)
{
  HWND hwndParent;
  LOADPARMS parms;
  WORD awShow[2] = { 2, SW_SHOWMAXIMIZED };
  switch(mode)
  {
  case MODE_ASKETCH_OPTIONS: //if (mode==MODE_ASKETCH_OPTIONS)     // Optionendialog anzeigen
    {
      if(hwndParent = (HWND)xvt_vobj_get_attr(parentWin,ATTR_NATIVE_WINDOW))
      {
        programm=MODE_ASKETCH_OPTIONS;
        DialogBox(GetModuleHandle( NULL ),"DLG_AUTOSKETCH",hwndParent,(DLGPROC)DlgAutoSketch);
        programm=-1;
        return TRUE;
      }
      else return FALSE;
    }
  case MODE_ASKETCH_START: //if (mode ==MODE_ASKETCH_START) // Pfad aus win.ini lesen und Autosketch starten
    {
      char path[256];
      OFSTRUCT ofs;
      hwndParent = (HWND)xvt_vobj_get_attr(parentWin,ATTR_NATIVE_WINDOW);
      ofs.cBytes=sizeof(ofs);
      GetPrivateProfileString("WSPWIN","AUTOSKETCH","unbekannt",path,255,"WSPWIN.INI");
      LZOpenFile(path,&ofs,OF_EXIST);
      if (!ofs.nErrCode)
      {
        
        parms.segEnv = 0;               /* child inherits environment */
        parms.lpszCmdLine = (LPSTR) "";     /* no command line        */
        parms.lpwShow = (LPWORD) awShow;    /* shows child as an icon */
        parms.lpwReserved = (LPWORD) NULL;  /* must be NULL           */
        
        WinExec(path,SW_SHOWNORMAL);
      }
      else 
      {
        char buf[200],buf2[200];//Dick 26.11.99
        xvt_res_get_str(STR_PROGRAMM_NICHT_DA,buf,sizeof(buf));
        xvt_res_get_str(STR_CAD_PROGRAMM,buf2,sizeof(buf2));
        MessageBox(hwndParent,buf,buf2,MB_OK|MB_ICONHAND); // "Programm kann nicht ausgeführt werden","AutoSketch"
      }
      return TRUE;
    }
  case MODE_EDITOR_OPTIONS: //if (mode==MODE_ASKETCH_OPTIONS)     // Optionendialog anzeigen
    {
      if(hwndParent = (HWND)xvt_vobj_get_attr(parentWin,ATTR_NATIVE_WINDOW))
      {
        programm=MODE_EDITOR_OPTIONS;
        DialogBox( GetModuleHandle( NULL ),"DLG_AUTOSKETCH",hwndParent,(DLGPROC)DlgAutoSketch );
        programm=-1;
        return TRUE;
      }
      else return FALSE;
    }
  case MODE_EDITOR_START: //if (mode ==MODE_ASKETCH_START) // Pfad aus win.ini lesen und Autosketch starten
    {
      char path[256];
      OFSTRUCT ofs;
      hwndParent = (HWND)xvt_vobj_get_attr(parentWin,ATTR_NATIVE_WINDOW);
      ofs.cBytes=sizeof(ofs);
      GetPrivateProfileString("WSPWIN","EDITOR","unbekannt",path,255,"WSPWIN.INI");
      LZOpenFile(path,&ofs,OF_EXIST);
      if (!ofs.nErrCode)
      {
        
        parms.segEnv = 0;               /* child inherits environment */
        parms.lpszCmdLine = (LPSTR) "";     /* no command line        */
        parms.lpwShow = (LPWORD) awShow;    /* shows child as an icon */
        parms.lpwReserved = (LPWORD) NULL;  /* must be NULL           */
        
        WinExec(path,SW_SHOWNORMAL);
      }
      else 
      {
        char buf[200],buf2[200];//Dick 26.11.99
        xvt_res_get_str(STR_PROGRAMM_NICHT_DA,buf,sizeof(buf));
        xvt_res_get_str(STR_EDITOR2,buf2,sizeof(buf2));
        MessageBox(hwndParent,buf,buf2,MB_OK|MB_ICONHAND);
      }
      return TRUE;
    }
  case MODE_SONDERPROG_OPTIONS: //if (mode==MODE_ASKETCH_OPTIONS)     // Optionendialog anzeigen
    {
      if(hwndParent = (HWND)xvt_vobj_get_attr(parentWin,ATTR_NATIVE_WINDOW))
      {
        programm=MODE_SONDERPROG_OPTIONS;
        DialogBox(GetModuleHandle(NULL),"DLG_AUTOSKETCH",hwndParent,(DLGPROC)DlgAutoSketch);
        programm=-1;
        return TRUE;
      }
      else return FALSE;
    }
  case MODE_SONDERPROG_START: //if (mode ==MODE_ASKETCH_START) // Pfad aus win.ini lesen und Autosketch starten
    {
      char path[256];
      OFSTRUCT ofs;
      hwndParent = (HWND)xvt_vobj_get_attr(parentWin,ATTR_NATIVE_WINDOW);
      ofs.cBytes=sizeof(ofs);
      GetPrivateProfileString("WSPWIN","SONDERPROGRAMM","unbekannt",path,255,"WSPWIN.INI");
      LZOpenFile(path,&ofs,OF_EXIST);
      if (!ofs.nErrCode)
      {
        
        parms.segEnv = 0;               /* child inherits environment */
        parms.lpszCmdLine = (LPSTR) "";     /* no command line        */
        parms.lpwShow = (LPWORD) awShow;    /* shows child as an icon */
        parms.lpwReserved = (LPWORD) NULL;  /* must be NULL           */
        
        WinExec(path,SW_SHOWNORMAL);
      }
      else 
      {
        char buf[200],buf2[200];//Dick 26.11.99
        xvt_res_get_str(STR_PROGRAMM_NICHT_DA,buf,sizeof(buf));
        xvt_res_get_str(STR_SONDERPROGRAMM,buf2,sizeof(buf2));
        MessageBox(hwndParent,buf,buf2,MB_OK|MB_ICONHAND);
        //MessageBox(hwndParent,"Programm kann nicht ausgeführt werden","Sonderprogramm",MB_OK|MB_ICONHAND);
      }
      return TRUE;
    }
       }//switch
       return FALSE;
}
/****************************************************************************/
