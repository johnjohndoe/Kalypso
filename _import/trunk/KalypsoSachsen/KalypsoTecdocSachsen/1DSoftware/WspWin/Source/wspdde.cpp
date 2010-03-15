/* wspdde.cpp */

#include <windows.h>
#include "xvt.h"

#include <tchar.h>

#include "resource.h"

#include "global_vars.h"

#include "aufnehm.h"
#include "util.h"

static BOOL fInInitiate;	// acknowledgement from WM_DDE_INITIATE
static BOOL fInCommand;		// acknowledgement from WM_DDE_EXECUTE
static BOOL fInRequest;		// acknowledgement from WM_DDE_REQUEST

static HANDLE hProcess = NULL;				// handle to server process
static char lpszApplication[MAX_PATH];	// application name
static char lpszCommandString[MAX_PATH];	// command for WM_DDE_EXECUTE
static char lpszTopic[MAX_PATH];			// topic for WM_DDE_REQUEST
static HWND hWndClient = NULL;				// handle to client DDE window
static HWND hWndServer = NULL;				// handle to server DDE window

static BOOL bAutoTerm;						// when TRUE terminate server process on WM_DDE_TERMINATE
static ATOM atomList[10] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
static HWND hwndList[10] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

ATOM atomApp;//=GlobalAddAtom("WSPWIN.EXE");
ATOM atomTopic;//=GlobalAddAtom("system");

HWND clientlist[100];
int num_clientlist = 0;

void OnDDECommand(TCHAR* szCommand)
{
  char *ComCopy,*pdest,strProjekt[250],strZustand[20],strProfil[20];
  int result;
  ComCopy=strlwr(strdup(szCommand));
  if(strncmp( ComCopy, "[greditor(\"" , 11 )==0)
  {
    ComCopy=&ComCopy[11];
    pdest=strchr( ComCopy, '\"');
    result = pdest - ComCopy ;
    if(pdest!=NULL)
    {
      strncpy(strProjekt,ComCopy,result);
      strProjekt[result]='\0';
      ComCopy=&ComCopy[result];
      if(strncmp( ComCopy, "\",\"" , 3 )==0)
      {
        ComCopy=&ComCopy[3];
        pdest=strchr( ComCopy, '\"');
        result = pdest - ComCopy ;
        if(pdest!=NULL)
        {
          strncpy(strZustand,ComCopy,result);
          strZustand[result]='\0';
          ComCopy=&ComCopy[result];
          if(strncmp( ComCopy, "\",\"" , 3 )==0)
          {
            ComCopy=&ComCopy[3];
            pdest=strchr( ComCopy, '\"');
            result = pdest - ComCopy;
            if(pdest!=NULL)
            {
              strncpy(strProfil,ComCopy,result);
              strProfil[result]='\0';
              ProjektZustandProfilOpen(strProjekt,strZustand,strProfil);
            }
          } 
        }
      }
    }
  }
}

BOOL InitDDE( char* szApplication, char* szCommandString, char* szTopicRequest , char* szData, BOOL bTerminateExe ) 
{
	char szTopic[] = "system";				// topic for WM_DDE_INITIATE
	ATOM atomApplication, atomTopic;
	BOOL bSuccess = FALSE;
	
	fInInitiate = FALSE;
	fInCommand = FALSE;
	fInRequest = FALSE;
	LPTSTR lpszTemp = (LPTSTR)szApplication;
	for (LPCTSTR lpsz = szApplication; *lpsz != '\0'; lpsz = _tcsinc(lpsz))
	{
		// remember last directory/drive separator
		if (*lpsz == '\\' || *lpsz == '/' || *lpsz == ':')
			lpszTemp = (LPTSTR)_tcsinc(lpsz);
	}
	
	strcpy(lpszApplication,lpszTemp);
	for( LPTSTR lptsz = lpszApplication; *lptsz != '\0'; lptsz = _tcsinc(lptsz))
	{
		// remember last directory/drive separator
		if (*lptsz == '.')
			*lptsz = '\0';
	}


	strcpy(lpszCommandString, szCommandString);
	if (szTopicRequest!=NULL && szData!=NULL)
		strcpy(lpszTopic, szTopicRequest);		// data requested
	else
		lpszTopic[0] = 0;				// no data requested
	lpszData = szData;
	bAutoTerm = bTerminateExe;			// terminate server when conversation terminates?
	atomApplication = *lpszApplication == 0 ? NULL : GlobalAddAtom((LPTSTR)lpszApplication); 
	atomTopic = *szTopic == 0 ? NULL : GlobalAddAtom((LPTSTR) szTopic); 
  
	hWndServer = NULL;
	for (int i=0; i<10; i++)
	{
		if (atomList[i]!=0 && atomApplication==atomList[i])
		{
			hWndServer = hwndList[i];
			break;
		}
		if (atomList[i]==0)
		{
			atomList[i] = atomApplication;
			break;
		}
	}
	if (hWndServer!=NULL)
	{
		if (IsWindow(hWndServer))
			bSuccess = TRUE;
		else
			hWndServer = NULL;
	}
	
	if (!bSuccess)
	{
		STARTUPINFO sui;
		PROCESS_INFORMATION pi;
		
		::GetStartupInfo(&sui);
		
		sui.lpReserved = NULL;
		if(lpszTemp!=NULL)
			strcpy(sui.lpTitle, lpszTemp);
		sui.lpTitle = NULL;
		sui.dwFlags |= STARTF_USESHOWWINDOW;
		sui.wShowWindow = SW_SHOW;
		
		bSuccess = ::CreateProcess(NULL, szApplication, NULL, NULL, TRUE, NORMAL_PRIORITY_CLASS, NULL, NULL, &sui, &pi);
		hProcess = pi.hProcess;
	}

	if (bSuccess && hWndClient!=NULL)
	{
		HWND hWndTarget = (HWND)-1;        /* broadcasts message          */ 
		if (hWndServer!=NULL)
			hWndTarget = hWndServer;
		
		::WaitForInputIdle(hProcess, INFINITE);
		fInInitiate = TRUE; 
		::SendMessage(hWndTarget,
			WM_DDE_INITIATE,          /* initiates conversation      */ 
			(WPARAM) hWndClient,   /* handle of client DDE window */ 
			MAKELONG(atomApplication, /* application-name atom       */ 
			atomTopic));          /* topic-name atom             */
		if (atomApplication != NULL) 
			GlobalDeleteAtom(atomApplication); 
		if (atomTopic != NULL) 
			GlobalDeleteAtom(atomTopic);
		
	}
	return bSuccess;
}

LRESULT APIENTRY DDEWndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) 
{ 
  BOOL bTerminate = FALSE;
  
  switch( uMsg )
  { 
  case WM_DDE_INITIATE:
    {   
		if( !hWndServer )
		{
			ATOM tmpAtom = (ATOM)LOWORD( lParam );
			char buffer[256];
			GlobalGetAtomName( tmpAtom, LPTSTR(&buffer), 256 );


			if( atomApp != 0 && atomApp == tmpAtom )
			{
				if( atomTopic!= 0 && atomTopic==(ATOM)HIWORD(lParam) )
				{
					SendMessage((HWND)wParam,
						WM_DDE_ACK, 
						(WPARAM) hWndClient, 
						MAKELONG(atomApp, atomTopic));
					//  GlobalDeleteAtom(atomApp);
					clientlist[num_clientlist++]=(HWND)wParam;
				}
				//GlobalDeleteAtom(atomTopic); 
			}                 
        }
    }
    break;
  case WM_DDE_EXECUTE:
    {                                      
      UINT unused;
      HGLOBAL hData;
      if(UnpackDDElParam(WM_DDE_EXECUTE, lParam, &unused, (UINT*)&hData))
      {
      }
      
      // get the command string
      TCHAR szCommand[_MAX_PATH * 2];
      LPCTSTR lpsz = (LPCTSTR)GlobalLock(hData);
      lstrcpyn(szCommand, lpsz, sizeof(szCommand)/sizeof(szCommand[0]));
      GlobalUnlock(hData);                
      OnDDECommand(szCommand);
      ::PostMessage((HWND)wParam, WM_DDE_ACK, (WPARAM)hWndClient,
        ReuseDDElParam(lParam, WM_DDE_EXECUTE, WM_DDE_ACK,
        (UINT)0x8000, (UINT)hData));
      
    }
    break;

  case WM_DDE_ACK:
      if (fInInitiate)
      {
        hWndServer	 = (HWND)wParam;
        for (int i=0; i<10; i++)
        {
          if (LOWORD(lParam)==atomList[i])
            hwndList[i] = hWndServer;
        }
        HGLOBAL hCommand;
        char* lpCommand;
        //		TCHAR szCommandString[] = _T("[project(\"c:\\iwatest3\")]");
        fInInitiate = FALSE;
        fInCommand = TRUE;
        
        if (!(hCommand = GlobalAlloc(GMEM_MOVEABLE | GMEM_DDESHARE, 
          sizeof(lpszCommandString) + 1))) 
          return 0L; 
        if (!(lpCommand = (char*)GlobalLock(hCommand)))
        { 
          GlobalFree(hCommand); 
          bTerminate = TRUE;
          break;
        } 
        
        lstrcpy(lpCommand, lpszCommandString); 
        GlobalUnlock(hCommand); 
        if (!::PostMessage((HWND)wParam, 
          WM_DDE_EXECUTE, 
          (WPARAM) hwnd, 
          PackDDElParam(WM_DDE_EXECUTE, 0, (UINT) hCommand)))
        { 
          GlobalFree(hCommand); 
          FreeDDElParam(WM_DDE_EXECUTE, lParam);
          bTerminate = TRUE;
          break;
        } 
      }
      else if (fInCommand)
      {
        fInCommand = FALSE;
        if (lpszTopic[0]!=0)
        {	// we have a topic so request for data
          ATOM atomItem;
          
          if ((atomItem = ::GlobalAddAtom(lpszTopic)) != 0)
          { 
            if (!::PostMessage((HWND)wParam, 
              WM_DDE_REQUEST, 
              (WPARAM) hwnd, 
              PackDDElParam(WM_DDE_REQUEST, CF_TEXT, atomItem))) 
              GlobalDeleteAtom(atomItem); 
            else
              fInRequest = TRUE;
          } 
          else
          {
            bTerminate = TRUE;
            break;
          }
        }
        else	// no topic so end conversation
        {
          bTerminate = TRUE;
          break;
        }
      }            
      else if (fInRequest)
      {	// request for data failed
        fInRequest = FALSE;
        bTerminate = TRUE;
        break;
      }
      break;
      
    case WM_DDE_DATA:
      {	// data from request recieved
        ATOM atomItem;
        HGLOBAL hData;
        DDEDATA *lpDDEData;
        BOOL bRelease = FALSE;
        
        fInRequest = FALSE;
        UnpackDDElParam(WM_DDE_DATA, lParam, (PUINT) &hData, 
          (PUINT) &atomItem); 
        if (!(lpDDEData = (DDEDATA FAR*) GlobalLock(hData)) 
          || (lpDDEData->cfFormat != CF_TEXT))
        { 
          ::PostMessage((HWND)wParam, 
            WM_DDE_ACK, 
            (WPARAM) hwnd, 
            ::PackDDElParam(WM_DDE_ACK, 0, atomItem)); /* negative ACK */ 
        } 
        
        if (lpDDEData!=NULL)
        {
          // copy data
          strcpy(lpszData, (char*)lpDDEData->Value);//Dick 21.03.99
          
          if (lpDDEData->fAckReq)
          { 
            ::PostMessage((HWND)wParam, 
              WM_DDE_ACK, 
              (WPARAM) hwnd, 
              PackDDElParam(WM_DDE_ACK, 0x8000, 
              atomItem)); /* positive ACK */ 
          } 
          
          bRelease = lpDDEData->fRelease;
        }
        GlobalUnlock(hData); 
        if (bRelease) 
          GlobalFree(hData);
        
        // now terminate the conversation
        bTerminate = TRUE;
        if(!strcmp(lpszApplication,"interp") && strlen(lpszData)>3)
        {
          if(strncmp(lpszData,"Fehler",6))
          {
            interp_prog=TRUE;
            
            if (profile_aufnehmen()==IDOK)    // -->file:aufnehm.cpp
            {
              char buf[200];//Dick 26.11.99
              LoadString(NULL,STR_INTERP_OK,buf,sizeof(buf));
              MessageBox(hwnd,buf,"WSPWIN",MB_OK|MB_ICONINFORMATION|MB_APPLMODAL);
              //MessageBox(hwnd,"Interpolation erfolgreich beendet!","WSPWIN",MB_OK|MB_ICONINFORMATION|MB_APPLMODAL );
            }
            else
            {
              char message[300];
              char buf[200],buf2[200];//Dick 26.11.99
              LoadString(NULL,STR_INTERP_PROF,buf,sizeof(buf));
              LoadString(NULL,STR_NICHT_AUFNEHMEN,buf2,sizeof(buf2));
              sprintf(message,"%s %s %s",buf,lpszData,buf2);
              //sprintf(message,"Die interpolierte Profile %s konnten nicht aufgenommen werden !",lpszData);
              MessageBox(hwnd,message,"WSPWIN",MB_OK|MB_ICONERROR|MB_APPLMODAL );
            }
            interp_prog=FALSE;
          }
          else
          {
            char buf[200];//Dick 26.11.99
            LoadString(NULL,STR_INTERP_ERROR,buf,sizeof(buf));
            MessageBox(hwnd,buf,"WSPWIN",MB_OK|MB_ICONERROR|MB_APPLMODAL);                         
            //MessageBox(hwnd,"Interpolation fehlerhaft beendet. Sehen Sie die Datei error.log  im Projektverzeichnis ein.","WSPWIN",MB_OK|MB_ICONERROR|MB_APPLMODAL );
          }
          
        }
      }
      break;
      
    case WM_DDE_TERMINATE:
      {
        if (bAutoTerm && hProcess!=NULL)
        {
          if (TerminateProcess(hProcess, 0))
          {
            hProcess = NULL;
            hWndServer = NULL;
          }
        }
        for(int i=0;i<num_clientlist;i++)
        {
          if(clientlist[i]==(HWND)wParam)
          {
            ::PostMessage((HWND)wParam, WM_DDE_TERMINATE, (WPARAM)hwnd, lParam);
            if(i==num_clientlist || (i==0 && num_clientlist==1))
              clientlist[i]=NULL;
            else
              for(int j=i;j<num_clientlist-1;j++)
                clientlist[j]=clientlist[j+1];
              num_clientlist--;
              if(num_clientlist<0)num_clientlist=0;
          }
        }
        //DestroyWindow(hwnd);
        if(dlg_136!=NULL_WIN)
        {
          //xvt_vobj_destroy(dlg_136);
          //dlg_136=NULL_WIN;
          EnableWindow((HWND)xvt_vobj_get_attr(dlg_136,ATTR_NATIVE_WINDOW),TRUE);
        }
      }
      break;
      
    case WM_DESTROY:
      hWndClient = NULL;
      GlobalDeleteAtom(atomApp);
      GlobalDeleteAtom(atomTopic);
      break;
      
    default: 
      return DefWindowProc(hwnd, uMsg, wParam, lParam); 
    } 
    
    if (bTerminate)
    {
      ::PostMessage((HWND)wParam, WM_DDE_TERMINATE, 
        PackDDElParam(WM_DDE_TERMINATE, (UINT) hwnd, 0), 0); 
    }
    return 0L; 
}

HWND CreateDDEWnd(HINSTANCE hInstance, HWND hParent) 
{ 
  WNDCLASS wc; 
  
  // Register the main window class. 
  wc.style = CS_HREDRAW | CS_VREDRAW; 
  wc.lpfnWndProc = (WNDPROC) DDEWndProc; 
  wc.cbClsExtra = 0; 
  wc.cbWndExtra = 0; 
  wc.hInstance = hInstance; 
  wc.hIcon = NULL; 
  wc.hCursor = NULL; 
  wc.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH); 
  wc.lpszMenuName =  NULL; 
  wc.lpszClassName = "DDEWindowClass"; 
  
  if (hWndClient!=NULL)
    DestroyWindow(hWndClient);
  UnregisterClass(wc.lpszClassName, hInstance);
  
  if (!RegisterClass(&wc)) 
    return NULL; 
  hWndClient = CreateWindow(wc.lpszClassName, "DDE", WS_CHILD/* | WS_DISABLED*/,
    CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
    hParent, NULL, hInstance, NULL);
  if(hWndClient)
  {
    atomApp=GlobalAddAtom("WSPWIN");
    atomTopic=GlobalAddAtom("system");
  }
  return hWndClient;
}