#ifndef _TLIST_H_INCLUDED_
#define _TLIST_H_INCLUDED_

#define TITLE_SIZE          64
#define PROCESS_SIZE        16

//
// task list structure
//
typedef struct _TASK_LIST
{
    DWORD       dwProcessId;
    DWORD       dwInheritedFromProcessId;
    BOOL        flags;
    HANDLE      hwnd;
    CHAR        ProcessName[PROCESS_SIZE];
    CHAR        WindowTitle[TITLE_SIZE];
} TASK_LIST, *PTASK_LIST;

extern DWORD FillTaskList(PTASK_LIST pTask, DWORD dwNumTasks);
extern BOOL IsTaskActive(LPCSTR lpTask);
extern void ShutDownTask(LPCSTR lpTask);


#endif _TLIST_H_INCLUDED_