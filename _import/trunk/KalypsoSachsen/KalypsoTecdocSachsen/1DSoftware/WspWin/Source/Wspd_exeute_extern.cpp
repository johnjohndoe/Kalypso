/*
	Handler for dialog DLG_222 ("Projektbezeichnung")
*/

#include <windows.h>
#include <string>
#include "xvt.h"
#include "wspwin.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

PROCESS_INFORMATION pi;
long m_timerID;

/* 
	Information about the dialog
*/
#define DLG_RES_ID DLG_EXECUTE_EXTERN
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL

/*
	Handler for dialog DLG_execute_extern
*/
long XVT_CALLCONV1 DLG_EXECUTE_EXTERN_eh XVT_CALLCONV2( WINDOW xdWindow, EVENT* xdEvent )
{
	switch( xdEvent->type ) 
	{
	case E_CREATE:
		/*
			Dialog has been created; first event sent to newly-created
			dialog.
		*/
		{
			std::string* cmdLine = (std::string*)( xvt_vobj_get_data( xdWindow ) );
			char buffer[MAX_PATH];
			strncpy( buffer, cmdLine->c_str(), cmdLine->length() );
			buffer[cmdLine->length()] = '\0';

			STARTUPINFO sui;

			::GetStartupInfo( &sui );
			sui.lpReserved = NULL;
			sui.lpTitle = "";
			sui.dwFlags |= STARTF_USESHOWWINDOW;
			sui.wShowWindow = SW_SHOWNORMAL;

			BOOL bCreate = ::CreateProcess( NULL, buffer, NULL, NULL, TRUE, NORMAL_PRIORITY_CLASS, NULL, NULL, &sui, &pi );
			DWORD lastError = GetLastError();

			// we dont need the thread handle, so close it
			CloseHandle( pi.hThread );

			if( bCreate )
			{
				// einen Timer einrichten, der den Thread überwacht
				m_timerID = xvt_timer_create( xdWindow, 500 );
				if( m_timerID == XVT_TIMER_ERROR )
				{
					CloseHandle( pi.hProcess );
					xvt_vobj_destroy( xdWindow );
				}
			}
			else
			{
				LPVOID lpMsgBuf;
				::FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
					NULL, lastError, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR) &lpMsgBuf,0, NULL );


				xvt_dm_post_error( "%s\n\n%s", cmdLine->c_str(), lpMsgBuf );

				::LocalFree( lpMsgBuf );

				xvt_vobj_destroy( xdWindow );
			}
		}
		break;

	case E_TIMER:
		{
			if( xdEvent->v.timer.id == m_timerID )
			{
				DWORD m_result;
				if( GetExitCodeProcess( pi.hProcess, &m_result ) && m_result != STILL_ACTIVE )
				{
					xvt_timer_destroy( m_timerID );
					CloseHandle( pi.hProcess );
					xvt_vobj_destroy( xdWindow );
				}
			}
		}
		break;
	
	case E_DESTROY:
		break;
	
	case E_CLOSE:
		/*
			Request to close dialog; user operated "close" menu item on
			dialog system menu, or operated "close" control on dialog
			frame. Dialog not closed unless xvt_vobj_destroy is called.
		*/
		{
		// xvt_vobj_destroy(xdWindow);
		}
		break;
	
	default:
		break;
	}
	return 0L;
}
