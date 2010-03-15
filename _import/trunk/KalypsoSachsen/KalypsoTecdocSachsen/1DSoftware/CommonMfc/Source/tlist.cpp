#include <windows.h>

#include <stdio.h>
#include <tlhelp32.h>

#include "tlist.h"

typedef struct _TASK_LIST_ENUM
{
    PTASK_LIST  tlist;
    DWORD       numtasks;
} TASK_LIST_ENUM, *PTASK_LIST_ENUM;

//
// manafest constants
//
#define INITIAL_SIZE        51200
#define EXTEND_SIZE         25600
#define REGKEY_PERF         "software\\microsoft\\windows nt\\currentversion\\perflib"
#define REGSUBKEY_COUNTERS  "Counters"
#define PROCESS_COUNTER     "process"
#define PROCESSID_COUNTER   "id process"
#define UNKNOWN_TASK        "unknown"

// Type definitions for pointers to call tool help functions. 
typedef BOOL (WINAPI *MODULEWALK)(HANDLE hSnapshot, 
    LPMODULEENTRY32 lpme); 
typedef BOOL (WINAPI *THREADWALK)(HANDLE hSnapshot, 
    LPTHREADENTRY32 lpte); 
typedef BOOL (WINAPI *PROCESSWALK)(HANDLE hSnapshot, 
    LPPROCESSENTRY32 lppe); 
typedef HANDLE (WINAPI *CREATESNAPSHOT)(DWORD dwFlags, 
    DWORD th32ProcessID); 
 
// File scope globals. These pointers are declared because of the need 
// to dynamically link to the functions.  They are exported only by 
// the Windows 95 kernel. Explicitly linking to them will make this 
// application unloadable in Microsoft(R) Windows NT(TM) and will 
// produce an ugly system dialog box. 
static CREATESNAPSHOT pCreateToolhelp32Snapshot = NULL; 
static MODULEWALK  pModule32First  = NULL; 
static MODULEWALK  pModule32Next   = NULL; 
static PROCESSWALK pProcess32First = NULL; 
static PROCESSWALK pProcess32Next  = NULL; 
static THREADWALK  pThread32First  = NULL; 
static THREADWALK  pThread32Next   = NULL; 
 
// Function that initializes tool help functions. 
BOOL InitToolhelp32 (void) 
{ 
    BOOL   bRet  = FALSE; 
    HMODULE hKernel = NULL; 
 
    // Obtain the module handle of the kernel to retrieve addresses of 
    // the tool helper functions. 
    hKernel = GetModuleHandle("KERNEL32.DLL"); 
 
    if (hKernel){ 
        pCreateToolhelp32Snapshot = 
            (CREATESNAPSHOT)GetProcAddress(hKernel, 
            "CreateToolhelp32Snapshot"); 
 
        pModule32First  = (MODULEWALK)GetProcAddress(hKernel, 
            "Module32First"); 
        pModule32Next   = (MODULEWALK)GetProcAddress(hKernel, 
            "Module32Next"); 
 
        pProcess32First = (PROCESSWALK)GetProcAddress(hKernel, 
            "Process32First"); 
        pProcess32Next  = (PROCESSWALK)GetProcAddress(hKernel, 
            "Process32Next"); 
 
        pThread32First  = (THREADWALK)GetProcAddress(hKernel, 
            "Thread32First"); 
        pThread32Next   = (THREADWALK)GetProcAddress(hKernel, 
            "Thread32Next"); 
 
        // All addresses must be non-NULL to be successful. 
        // If one of these addresses is NULL, one of 
        // the needed lists cannot be walked. 
        bRet =  pModule32First && pModule32Next  && pProcess32First && 
                pProcess32Next && pThread32First && pThread32Next && 
                pCreateToolhelp32Snapshot; 
    } 
    else 
        bRet = FALSE; // could not even get the module handle of kernel 
 
    return bRet; 
} 
  
//The following function takes a snapshot of the modules in the address space of a specified Win32 process and retrieves information for a specific module from the list recorded in the snapshot.
  
// Returns TRUE if there is information about the specified module or 
//   FALSE if it could not enumerate the modules in the process or 
//   the module is not found in the process. 
// dwPID - identifier of the process that owns the module to 
//   retrieve information about. 
// dwModuleID - tool help identifier of the module within the 
//   process 
// lpMe32 - structure to return data about the module 
// cbMe32 - size of the buffer pointed to by lpMe32 (to ensure 
//   the buffer is not over filled) 
BOOL GetProcessModule (DWORD dwPID, DWORD dwModuleID, 
        LPMODULEENTRY32 lpMe32, DWORD cbMe32) 
{ 
    BOOL          bRet        = FALSE; 
    BOOL          bFound      = FALSE; 
    HANDLE        hModuleSnap = NULL; 
    MODULEENTRY32 me32        = {0}; 
 
    // Take a snapshot of all modules in the specified process. 
    hModuleSnap = pCreateToolhelp32Snapshot(TH32CS_SNAPMODULE, dwPID); 
    if (hModuleSnap == (HANDLE)-1) 
        return (FALSE); 
 
    // Fill the size of the structure before using it. 
    me32.dwSize = sizeof(MODULEENTRY32); 
 
    // Walk the module list of the process, and find the module of 
    // interest. Then copy the information to the buffer pointed 
    // to by lpMe32 so that it can be returned to the caller. 
    if (pModule32First(hModuleSnap, &me32)) { 
        do { 
            if (me32.th32ModuleID == dwModuleID) { 
                CopyMemory (lpMe32, &me32, cbMe32); 
                bFound = TRUE; 
            } 
        } 
        while (!bFound && pModule32Next(hModuleSnap, &me32)); 
 
        bRet = bFound;   // if this sets bRet to FALSE, dwModuleID 
                         // no longer exists in specified process 
    } 
    else 
        bRet = FALSE;           // could not walk module list 
 
    // Do not forget to clean up the snapshot object. 
    CloseHandle (hModuleSnap); 
 
    return (bRet); 
} 

BOOL CALLBACK EnumWindowsProc(HWND hwnd, LPARAM lParam)
{
    DWORD             pid = 0;
    DWORD             i;
    CHAR              buf[TITLE_SIZE];
    PTASK_LIST_ENUM   te = (PTASK_LIST_ENUM)lParam;
    PTASK_LIST        tlist = te->tlist;
    DWORD             numTasks = te->numtasks;


    //
    // get the processid for this window
    //
    if (!GetWindowThreadProcessId( hwnd, &pid ))
	{
        return TRUE;
    }

    //
    // look for the task in the task list for this window
    //
    for (i=0; i<numTasks; i++)
	{
        if (tlist[i].dwProcessId == pid)
		{
            tlist[i].hwnd = hwnd;
            //
			// we found the task so lets try to get the
            // window text
            //
            if (GetWindowText((HWND)tlist[i].hwnd, buf, sizeof(buf) ))
			{
                //
				// got it, so lets save it
                //
                strcpy( tlist[i].WindowTitle, buf );
            }
            break;
        }
    }

    //
    // continue the enumeration
    //
    return TRUE;
}

void GetWindowTitles(PTASK_LIST_ENUM te)
{
    EnumWindows( EnumWindowsProc, (LPARAM) te );
}

//    Provides an API for getting a list of tasks running at the time of the
//    API call.  This function uses the registry performance data to get the
//    task list and is therefor straight WIN32 calls that anyone can call.
DWORD GetTaskList(PTASK_LIST  pTask, DWORD dwNumTasks)
{
	OSVERSIONINFO osVI;
	DWORD dwLimit = dwNumTasks - 1;

	osVI.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	GetVersionEx(&osVI);
	if (osVI.dwPlatformId==VER_PLATFORM_WIN32_NT)
	{	// Win NT
		DWORD                        rc;
		HKEY                         hKeyNames;
		DWORD                        dwType;
		DWORD                        dwSize;
		LPBYTE                       buf = NULL;
		CHAR                         szSubKey[1024];
		LANGID                       lid;
		LPSTR                        p;
		LPSTR                        p2;
		PPERF_DATA_BLOCK             pPerf;
		PPERF_OBJECT_TYPE            pObj;
		PPERF_INSTANCE_DEFINITION    pInst;
		PPERF_COUNTER_BLOCK          pCounter;
		PPERF_COUNTER_DEFINITION     pCounterDef;
		DWORD                        i;
		DWORD                        dwProcessIdTitle;
		DWORD                        dwProcessIdCounter;
		CHAR                         szProcessName[MAX_PATH];
		
		
		
		//
		// Look for the list of counters.  Always use the neutral
		// English version, regardless of the local language.  We
		// are looking for some particular keys, and we are always
		// going to do our looking in English.  We are not going
		// to show the user the counter names, so there is no need
		// to go find the corresponding name in the local language.
		//
		lid = MAKELANGID( LANG_ENGLISH, SUBLANG_NEUTRAL );
		sprintf( szSubKey, "%s\\%03x", REGKEY_PERF, lid );
		rc = RegOpenKeyEx( HKEY_LOCAL_MACHINE,
			szSubKey,
			0,
			KEY_READ,
			&hKeyNames
			);
		if (rc != ERROR_SUCCESS)
		{
			goto exit;
		}
		
		//
		// get the buffer size for the counter names
		//
		rc = RegQueryValueEx( hKeyNames,
			REGSUBKEY_COUNTERS,
			NULL,
			&dwType,
			NULL,
			&dwSize
			);
		
		if (rc != ERROR_SUCCESS)
		{
			goto exit;
		}
		
		//
		// allocate the counter names buffer
		//
		buf = (LPBYTE) malloc( dwSize );
		if (buf == NULL)
		{
			goto exit;
		}
		memset( buf, 0, dwSize );
		
		//
		// read the counter names from the registry
		//
		rc = RegQueryValueEx( hKeyNames,
			REGSUBKEY_COUNTERS,
			NULL,
			&dwType,
			buf,
			&dwSize
			);
		
		if (rc != ERROR_SUCCESS)
		{
			goto exit;
		}
		
		//
		// now loop thru the counter names looking for the following counters:
		//
		//      1.  "Process"           process name
		//      2.  "ID Process"        process id
		//
		// the buffer contains multiple null terminated strings and then
		// finally null terminated at the end.  the strings are in pairs of
		// counter number and counter name.
		//
		
		p = (char*)buf;
		while (*p)
		{
			if (p > (char*)buf)
			{
				for( p2=p-2; isdigit(*p2); p2--);
            }
			if (stricmp(p, PROCESS_COUNTER) == 0)
			{
				//
				// look backwards for the counter number
				//
				for( p2=p-2; isdigit(*p2); p2--);
				strcpy( szSubKey, p2+1 );
			}
			else if (stricmp(p, PROCESSID_COUNTER) == 0)
			{
				//
				// look backwards for the counter number
				//
				for( p2=p-2; isdigit(*p2); p2--);
				dwProcessIdTitle = atol( p2+1 );
			}
			//
			// next string
			//
			p += (strlen(p) + 1);
		}
		
		//
		// free the counter names buffer
		//
		free( buf );
		
		
		//
		// allocate the initial buffer for the performance data
		//
		dwSize = INITIAL_SIZE;
		buf = (LPBYTE)malloc( dwSize );
		if (buf == NULL)
		{
			goto exit;
		}
		memset( buf, 0, dwSize );
		
		
		while (TRUE)
		{
			rc = RegQueryValueEx( HKEY_PERFORMANCE_DATA,
				szSubKey,
				NULL,
				&dwType,
				buf,
				&dwSize
				);
			
			pPerf = (PPERF_DATA_BLOCK) buf;
			
			//
			// check for success and valid perf data block signature
			//
			if ((rc == ERROR_SUCCESS) &&
				(dwSize > 0) &&
				(pPerf)->Signature[0] == (WCHAR)'P' &&
				(pPerf)->Signature[1] == (WCHAR)'E' &&
				(pPerf)->Signature[2] == (WCHAR)'R' &&
				(pPerf)->Signature[3] == (WCHAR)'F' )
			{
				break;
			}
			
			//
			// if buffer is not big enough, reallocate and try again
			//
			if (rc == ERROR_MORE_DATA)
			{
				dwSize += EXTEND_SIZE;
				buf = (LPBYTE)realloc( buf, dwSize );
				memset( buf, 0, dwSize );
			}
			else
			{
				goto exit;
			}
		}
		
		//
		// set the perf_object_type pointer
		//
		pObj = (PPERF_OBJECT_TYPE) ((DWORD)pPerf + pPerf->HeaderLength);
		
		//
		// loop thru the performance counter definition records looking
		// for the process id counter and then save its offset
		//
		pCounterDef = (PPERF_COUNTER_DEFINITION) ((DWORD)pObj + pObj->HeaderLength);
		for (i=0; i<(DWORD)pObj->NumCounters; i++)
		{
			if (pCounterDef->CounterNameTitleIndex == dwProcessIdTitle)
			{
				dwProcessIdCounter = pCounterDef->CounterOffset;
				break;
			}
			pCounterDef++;
		}
		
		dwNumTasks = min( dwLimit, (DWORD)pObj->NumInstances );
		
		pInst = (PPERF_INSTANCE_DEFINITION) ((DWORD)pObj + pObj->DefinitionLength);
		
		//
		// loop thru the performance instance data extracting each process name
		// and process id
		//
		for (i=0; i<dwNumTasks; i++)
		{
			//
			// pointer to the process name
			//
			p = (LPSTR) ((DWORD)pInst + pInst->NameOffset);
			
			//
			// convert it to ascii
			//
			rc = WideCharToMultiByte( CP_ACP,
				0,
				(LPCWSTR)p,
				-1,
				szProcessName,
				sizeof(szProcessName),
				NULL,
				NULL
				);
			
			if (!rc)
			{
				//
				// if we cant convert the string then use a default value
				//
				strcpy( pTask->ProcessName, UNKNOWN_TASK );
			}
			
			if (strlen(szProcessName)+4 <= sizeof(pTask->ProcessName))
			{
				strcpy( pTask->ProcessName, szProcessName );
				strcat( pTask->ProcessName, ".exe" );
			}
			
			//
			// get the process id
			//
			pCounter = (PPERF_COUNTER_BLOCK) ((DWORD)pInst + pInst->ByteLength);
			pTask->flags = 0;
			pTask->dwProcessId = *((LPDWORD) ((DWORD)pCounter + dwProcessIdCounter));
			if (pTask->dwProcessId == 0)
			{
				pTask->dwProcessId = (DWORD)-2;
			}
			
			//
			// next process
			//
			pTask++;
			pInst = (PPERF_INSTANCE_DEFINITION) ((DWORD)pCounter + pCounter->ByteLength);
		}
		
exit:
		if (buf)
		{
			free( buf );
		}
		
		RegCloseKey( hKeyNames );
		RegCloseKey( HKEY_PERFORMANCE_DATA );
	}
	else if (osVI.dwPlatformId==VER_PLATFORM_WIN32_WINDOWS)
	{	// Win 95
		HANDLE hProcessSnap = NULL; 
		PROCESSENTRY32 pe32 = {0};
		
		if (!InitToolhelp32())
			return 0;
		//  Take a snapshot of all processes currently in the system. 
		hProcessSnap = pCreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); 
		if (hProcessSnap == (HANDLE)-1) 
			return 0; 
		
		//  Fill in the size of the structure before using it. 
		pe32.dwSize = sizeof(PROCESSENTRY32);
		
		dwNumTasks = 0;
		//  Walk the snapshot of the processes, and for each process, get 
		//  the executable filename, etc. 
		if (pProcess32First(hProcessSnap, &pe32)) 
		{ 
			BOOL          bGotModule = FALSE; 
			MODULEENTRY32 me32       = {0}; 
			
			do 
			{ 
				
				bGotModule = GetProcessModule(pe32.th32ProcessID, 
					pe32.th32ModuleID, &me32, sizeof(MODULEENTRY32)); 
				if (bGotModule && dwNumTasks<=dwLimit)
				{ 
					strcpy(pTask->ProcessName, me32.szModule);
					pTask->dwProcessId = me32.th32ProcessID;
					pTask->dwInheritedFromProcessId = pe32.th32ParentProcessID;
					//
					// next process
					//
					pTask++;
					dwNumTasks++;
				} 
			} 
			while (pProcess32Next(hProcessSnap, &pe32)); 
		}
		else 
			return 0;    // could not walk the list of processes 
		
		// Do not forget to clean up the snapshot object. 
		CloseHandle (hProcessSnap); 
	}

	return dwNumTasks;
}

DWORD FillTaskList(PTASK_LIST pTask, DWORD dwNumTasks)
{
    TASK_LIST_ENUM te;
	DWORD num;

	num = GetTaskList(pTask, dwNumTasks);
    //
    // enumerate all windows and try to get the window
    // titles for each task
    //
    te.tlist = pTask;
    te.numtasks = num;
    GetWindowTitles(&te);

	return num;
}


BOOL IsTaskActive(LPCSTR lpTask)
{
	BOOL bRet = FALSE;	
	TASK_LIST tlist[256];
	int num, i;

	num = FillTaskList(tlist, 256);
	for (i=0; i<num; i++)
	{
		if (!_stricmp (tlist[i].ProcessName, lpTask))
			bRet = TRUE;
	}

	return bRet;
}

void ShutDownTask(LPCSTR lpTask)
{
	TASK_LIST tlist[256];
	int num, i;
	HANDLE hProcess;

	num = FillTaskList(tlist, 256);
	for (i=0; i<num; i++)
	{
		if (!_stricmp (tlist[i].ProcessName, lpTask))
		{
			hProcess = OpenProcess(PROCESS_TERMINATE, FALSE, tlist[i].dwProcessId);
			if (hProcess!=NULL)
				TerminateProcess(hProcess, 0);
		}
	}
}
