// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#define VC_EXTRALEAN		// Exclude rarely-used stuff from Windows headers

#include <afxwin.h>         // MFC core and standard components
#include <afxext.h>         // MFC extensions
#include <afxole.h>         // MFC OLE classes
#include <afxodlgs.h>       // MFC OLE dialog classes
#include <afxcmn.h>			// MFC support for Windows 95 Common Controls

#include <afxtempl.h>

// c++ Header
#include <limits>
#include <vector>
#include <algorithm>


#include <locale.h>
#include <fstream.h>
#include <iomanip.h>
#include <io.h>

// ein paar nützliche Makros

#define LINE_SIZE	400


// Macros for MessageBox types.
#define MB_ERROR	MB_OK | MB_ICONSTOP
#define MB_WARN		MB_OK | MB_ICONEXCLAMATION
#define MB_QUERY	MB_YESNO | MB_ICONQUESTION
#define MB_INFO		MB_OK | MB_ICONINFORMATION
