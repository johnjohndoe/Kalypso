// Draw.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"

#include "commonMfc\include\contextHelp.h"
#include "commonMfc\include\version.h"

#include "optdlg.h"
#include "mainfrm.h"

#include "draw.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


#define N_PATTERNS				24

HPALETTE _AfxInitPalette(VOID)
{

	NPLOGPALETTE pPal; //Local Palette
	HPALETTE pal;


	//Allocates space for QcQp Palette 'pPal'
	VERIFY( (UINT) (pPal = (NPLOGPALETTE)::LocalAlloc (LPTR,
				(sizeof (LOGPALETTE) +
				(sizeof (PALETTEENTRY) * (NCOLORS))))));

	//Initialize 'pPal' fields
	pPal->palNumEntries = NCOLORS;
	pPal->palVersion = 0x300;

	//Inits Every color of our palette
	pPal->palPalEntry[BLACK].peRed        =   0;
	pPal->palPalEntry[BLACK].peGreen      =   0;
	pPal->palPalEntry[BLACK].peBlue       =   0;
	pPal->palPalEntry[BLACK].peFlags      =   (BYTE) 0;

	pPal->palPalEntry[WHITE].peRed        =   255;
	pPal->palPalEntry[WHITE].peGreen      =   255;
	pPal->palPalEntry[WHITE].peBlue       =   255;
	pPal->palPalEntry[WHITE].peFlags      =   (BYTE) 0;

	pPal->palPalEntry[RED].peRed          =   255;
	pPal->palPalEntry[RED].peGreen        =   0;
	pPal->palPalEntry[RED].peBlue         =   0;
	pPal->palPalEntry[RED].peFlags        =   (BYTE) 0;

	pPal->palPalEntry[GREEN].peRed        =   0;
	pPal->palPalEntry[GREEN].peGreen      =   255;
	pPal->palPalEntry[GREEN].peBlue       =   0;
	pPal->palPalEntry[GREEN].peFlags      =   (BYTE) 0;

	pPal->palPalEntry[BLUE].peRed         =   0;
	pPal->palPalEntry[BLUE].peGreen       =   0;
	pPal->palPalEntry[BLUE].peBlue        =   255;
	pPal->palPalEntry[BLUE].peFlags       =   (BYTE) 0;

	pPal->palPalEntry[YELLOW].peRed       =   255;
	pPal->palPalEntry[YELLOW].peGreen     =   255;
	pPal->palPalEntry[YELLOW].peBlue      =   0;
	pPal->palPalEntry[YELLOW].peFlags     =   (BYTE) 0;

	pPal->palPalEntry[MAGENTA].peRed      =   255;
	pPal->palPalEntry[MAGENTA].peGreen    =   0;
	pPal->palPalEntry[MAGENTA].peBlue     =   255;
	pPal->palPalEntry[MAGENTA].peFlags    =   (BYTE) 0;

	pPal->palPalEntry[CYAN].peRed         =   0;
	pPal->palPalEntry[CYAN].peGreen       =   255;
	pPal->palPalEntry[CYAN].peBlue        =   255;
	pPal->palPalEntry[CYAN].peFlags       =   (BYTE) 0;

	pPal->palPalEntry[GRAY].peRed         =   128;
	pPal->palPalEntry[GRAY].peGreen       =   128;
	pPal->palPalEntry[GRAY].peBlue        =   128;
	pPal->palPalEntry[GRAY].peFlags       =   (BYTE) 0;

	pPal->palPalEntry[LIGHTGRAY].peRed    =   192;
	pPal->palPalEntry[LIGHTGRAY].peGreen  =   192;
	pPal->palPalEntry[LIGHTGRAY].peBlue   =   192;
	pPal->palPalEntry[LIGHTGRAY].peFlags  =   (BYTE) 0;

	pPal->palPalEntry[DARKRED].peRed      =   128;
	pPal->palPalEntry[DARKRED].peGreen    =   0;
	pPal->palPalEntry[DARKRED].peBlue     =   0;
	pPal->palPalEntry[DARKRED].peFlags    =   (BYTE) 0;

	pPal->palPalEntry[DARKGREEN].peRed    =   0;
	pPal->palPalEntry[DARKGREEN].peGreen  =   128;
	pPal->palPalEntry[DARKGREEN].peBlue   =   0;
	pPal->palPalEntry[DARKGREEN].peFlags  =   (BYTE) 0;

	pPal->palPalEntry[DARKBLUE].peRed     =   0;
	pPal->palPalEntry[DARKBLUE].peGreen   =   0;
	pPal->palPalEntry[DARKBLUE].peBlue    =   128;
	pPal->palPalEntry[DARKBLUE].peFlags   =   (BYTE) 0;

	pPal->palPalEntry[LIGHTBROWN].peRed   =   128;
	pPal->palPalEntry[LIGHTBROWN].peGreen =   128;
	pPal->palPalEntry[LIGHTBROWN].peBlue  =   0;
	pPal->palPalEntry[LIGHTBROWN].peFlags =   (BYTE) 0;

	pPal->palPalEntry[DARKMAGENTA].peRed  =   128;
	pPal->palPalEntry[DARKMAGENTA].peGreen=   0;
	pPal->palPalEntry[DARKMAGENTA].peBlue =   128;
	pPal->palPalEntry[DARKMAGENTA].peFlags=   (BYTE) 0;

	pPal->palPalEntry[DARKCYAN].peRed     =   0;
	pPal->palPalEntry[DARKCYAN].peGreen   =   128;
	pPal->palPalEntry[DARKCYAN].peBlue    =   128;
	pPal->palPalEntry[DARKCYAN].peFlags   =   (BYTE) 0;

	//Creates the logical palette
	pal = ::CreatePalette((LPLOGPALETTE)pPal);
	VERIFY(pal);

	//Free allocated memory
	VERIFY(::LocalFree((HLOCAL)pPal) == NULL);

	return pal;
}

/////////////////////////////////////////////////////////////////////////////
// CDrawApp

int CDrawApp::m_nZoomFactorChangedMsg = RegisterWindowMessage(_T("DrawZoomFactorChanged"));
// Text for licence

BEGIN_MESSAGE_MAP(CDrawApp, CWinApp)
	//{{AFX_MSG_MAP(CDrawApp)
  ON_COMMAND(ID_FILE_PRINT_SETUP, OnFilePrintSetup)
	ON_COMMAND(ID_EXTRAS_OPTIONS, OnExtrasOptions)
	ON_COMMAND(ID_APP_EXIT, OnAppExit)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDrawApp construction

CDrawApp::CDrawApp()
{
#ifdef _DEBUG
	m_bTraceUndo = FALSE;
#endif
	m_hWndZoomBox = NULL;

  m_nSelectType = SELTYPE_WHOLE;

  m_pVersion = NULL;
  m_pContextHelp = NULL;
}

/////////////////////////////////////////////////////////////////////////////
// CDrawApp initialization

BOOL CDrawApp::InitInstance()
{
  // die Versionsnummer aus der exe auslesen
  m_pVersion = new CVersion( m_hInstance );

  // eine neue Instanz der ContextHilfe erzeugen
  m_pContextHelp = new CContextHelp();

	char szDefaultLanguage[64];
	LANGID langidDefault;

  SetRegistryKey(_T("BCE"));

  // Initialize OLE libraries
  if (!AfxOleInit())
	{
		AfxMessageBox(IDP_OLE_INIT_FAILED);
		return FALSE;
	}

	// Obtain the locale from the system and set the current locale to the
	// obtained locale.
	langidDefault = GetSystemDefaultLangID();
	GetLocaleInfo(langidDefault, LOCALE_SENGLANGUAGE, szDefaultLanguage, 64);
	setlocale(LC_ALL, szDefaultLanguage);

	// Init palette
	m_hPal = _AfxInitPalette();

	// Init brushes
	CreateStandardBrushes();

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

#ifdef _AFXDLL
	Enable3dControls();			// Call this when using MFC in a shared DLL
#else
	Enable3dControlsStatic();	// Call this when linking to MFC statically
#endif

	LoadStdProfileSettings();  // Load standard INI file options (including MRU)

	m_execDirectory = m_pszHelpFilePath;
	m_execDirectory = m_execDirectory.Left(m_execDirectory.ReverseFind('\\')+1);

	LoadINISettings();

	// get path of executable (no extension) and update registry
	TCHAR szBuff[_MAX_PATH];
	if (::GetModuleFileName(m_hInstance, szBuff, _MAX_PATH))
	{
#ifndef _MAC
		LPTSTR lpszExt = _tcsrchr(szBuff, '.');
		ASSERT(lpszExt != NULL);
		ASSERT(*lpszExt == '.');
		*lpszExt = 0;       // no suffix
#endif

		WriteProfileString("Info", "FilePath", szBuff);
	}

  //delete m_pVersion;
  //m_pVersion = NULL;

	return TRUE;
}

int CDrawApp::ExitInstance() 
{
  // die VersionInfo vergessen
  delete m_pVersion;
  m_pVersion = NULL;

  // die ContextHilfe zerstören
  delete m_pContextHelp;
  m_pContextHelp = NULL;

	SaveINISettings();
	DestroyStandardBrushes();
	
	return CWinApp::ExitInstance();
}

void CDrawApp::LoadINISettings()
{
	CString strSection = "Settings";
	BYTE* pb = NULL;
	UINT nLen = 0;

	m_nSelectType = GetProfileInt(strSection, "Select Type", SELTYPE_WHOLE);
	m_nSelectTol = GetProfileInt(strSection, "Select Tolerance", 10);
	m_bMaximized = GetProfileInt(strSection, "Maximized", (int)FALSE);
	if (GetProfileBinary(strSection, "FrameRect", &pb, &nLen))
	{
		ASSERT(nLen == sizeof(CRect));
		memcpy(&m_rectInitialFrame, pb, sizeof(CRect));
		delete pb;
	}
	else
		m_rectInitialFrame.SetRect(0,0,0,0);


  UINT pbLen = GetProfileInt( strSection, "PrinterLength", 0 );
  if( pbLen != 0 && GetProfileBinary( strSection, "PrinterSettings", &pb, &pbLen ) )
  {
    m_printerSettings.LoadBinary( pb );
    delete pb;
  }
  else // ansonsten Standard setzen
  {
    m_printerSettings.CopyDefaultMfcPrinter();
    m_printerSettings.SetMargins( CRect( 10, 10, 10, 10 ), MM_FACTOR );
  }

#ifdef _DEBUG
	m_bTraceUndo = GetProfileInt(strSection, "Trace Undo", (int)FALSE);
#endif
}

void CDrawApp::SaveINISettings()
{
	CString strSection = "Settings";

	WriteProfileInt(strSection, "Select Type", m_nSelectType);
	WriteProfileInt(strSection, "Select Tolerance", m_nSelectTol);
	WriteProfileInt(strSection, "Maximized", m_bMaximized);
	WriteProfileBinary(strSection, "FrameRect", (BYTE*)&m_rectInitialFrame, sizeof(CRect));

  BYTE* lpByte = NULL;
  DWORD lpByteLen = m_printerSettings.SaveBinary( &lpByte );
  WriteProfileInt( strSection, "PrinterLength", lpByteLen );
  WriteProfileBinary( strSection, "PrinterSettings", lpByte, lpByteLen );
  delete[] lpByte;

#ifdef _DEBUG
	WriteProfileInt(strSection, "Trace Undo", m_bTraceUndo);
#endif
}

void CDrawApp::OnFilePrintSetup() 
{	
  // only purpose is to change standard printer settings
  m_printerSettings.PageSetup();
}


void CDrawApp::CreateStandardBrushes()
{
  {
	  // solid brush
	  CBrush brush( RGB(0, 0, 0) );
	  LPLOGBRUSH lpLogBrush = new LOGBRUSH;
	  brush.GetLogBrush( lpLogBrush );
	  m_logbrushes.Add( lpLogBrush );
  }

	// hatched brushes
	for( int i = 0; i < 6; i++ )
	{
		CBrush brush( i, RGB(0, 0, 0) );
		LPLOGBRUSH lpLogBrush = new LOGBRUSH;
		brush.GetLogBrush( lpLogBrush );
		m_logbrushes.Add( lpLogBrush );
	}

	// patterned brushes
	for( int n = 0; n < N_PATTERNS; n++ )
	{
   	CBitmap* pBitmap = new CBitmap;
		pBitmap->LoadBitmap( GetPatternID( n ) );
		m_patterns.Add( pBitmap );

		CBrush brush( pBitmap );
		LPLOGBRUSH lpLogBrush = new LOGBRUSH;
		brush.GetLogBrush( lpLogBrush );
		m_logbrushes.Add( lpLogBrush );
	}
}

void CDrawApp::DestroyStandardBrushes()
{
	for( int p = 0; p < m_patterns.GetSize(); p++ )
		delete m_patterns[p];
	m_patterns.RemoveAll();
	
  for( int n = 0; n < m_logbrushes.GetSize(); n++ )
		delete m_logbrushes[n];
	m_logbrushes.RemoveAll();
}

int CDrawApp::GetPatternID(int n)
{
	int nID;
	
	switch(n)
	{
		case 0:
			nID = IDB_PATTRN01;
			break;

		case 1:
			nID = IDB_PATTRN02;
			break;

		case 2:
			nID = IDB_PATTRN03;
			break;

		case 3:
			nID = IDB_PATTRN04;
			break;

		case 4:
			nID = IDB_PATTRN05;
			break;

		case 5:
			nID = IDB_PATTRN06;
			break;

		case 6:
			nID = IDB_PATTRN07;
			break;

		case 7:
			nID = IDB_PATTRN08;
			break;

		case 8:
			nID = IDB_PATTRN09;
			break;

		case 9:
			nID = IDB_PATTRN10;
			break;

		case 10:
			nID = IDB_PATTRN11;
			break;

		case 11:
			nID = IDB_PATTRN12;
			break;

		case 12:
			nID = IDB_PATTRN13;
			break;

		case 13:
			nID = IDB_PATTRN14;
			break;

		case 14:
			nID = IDB_PATTRN15;
			break;

		case 15:
			nID = IDB_PATTRN16;
			break;

		case 16:
			nID = IDB_PATTRN17;
			break;

		case 17:
			nID = IDB_PATTRN18;
			break;

		case 18:
			nID = IDB_PATTRN19;
			break;

		case 19:
			nID = IDB_PATTRN20;
			break;

		case 20:
			nID = IDB_PATTRN21;
			break;

		case 21:
			nID = IDB_PATTRN22;
			break;

		case 22:
			nID = IDB_PATTRN23;
			break;

		case 23:
			nID = IDB_PATTRN24;
			break;

		default:
			nID = IDB_PATTRN01;
			break;
	}

	return nID;
}

int CDrawApp::GetBrushIndex(LPLOGBRUSH lpLogBrush)
{
	LPLOGBRUSH lpLB;
	int i;

	for (i=0; i<m_logbrushes.GetSize(); i++)
	{
		lpLB = m_logbrushes[i];
		if (*lpLB==*lpLogBrush)
			return i;
	}

	return 0;
}

LPLOGBRUSH CDrawApp::GetLogBrush(int index)
{
	if (index>=0 && index<m_logbrushes.GetSize())
		return m_logbrushes[index];
	else
		return NULL;
}

void CDrawApp::LoadStempelFiles()
{
	CString path, str;
	CFile file;
	CFileStatus rStatus;
	char buffer[1000]; // hoffentlich ist der Name nie länger als 1000 Zeichen

	path = m_execDirectory + "stempel.dat";
	if (file.GetStatus(path, rStatus))
	{
    ifstream ifs;
		ifs.open(path, ios::in);
		if (!ifs.fail())
		{
			while (!ifs.eof())
			{
				ifs.getline(buffer, 1000, '\n');
				str = buffer;
				str.TrimLeft();
				str.TrimRight();
				path = str;
				if (path.Find('\\')==-1)
					path = m_execDirectory + path;
				if (str.IsEmpty() || !file.GetStatus(path, rStatus))
					str.Empty();
				if (!str.IsEmpty())
					m_strStempelFiles.SetAtGrow(m_strStempelFiles.GetSize(), str);
			}
			ifs.close();
			SaveStempelFiles();		// update file just read in
		}
	}
}

BOOL CDrawApp::SaveStempelFiles()
{
	CString path, str;
	int i;

	path = m_execDirectory + "stempel.dat";
	ofstream ofs;
	ofs.open(path, ios::out);
	if (!ofs.fail())
	{
		for (i=0; i<m_strStempelFiles.GetSize(); i++)
		{
			str = m_strStempelFiles.GetAt(i);
			ofs << str << endl;
		}
		ofs.close();
	}
	else
		return FALSE;
	return TRUE;
}

void CDrawApp::FlushStempelFiles()
{
	m_strStempelFiles.RemoveAll();
}

BOOL CDrawApp::CreateStatusBarProgress(CString& str, int nLower, int nUpper)
{
	if (m_pMainWnd!=NULL)
		return ((CMainFrame*)m_pMainWnd)->CreateStatusBarProgress(str, nLower, nUpper);
	return FALSE;
}

void CDrawApp::IncStatusBarProgress()
{
	if (m_pMainWnd!=NULL)
		((CMainFrame*)m_pMainWnd)->IncStatusBarProgress();
}

void CDrawApp::DestroyStatusBarProgress()
{
	if (m_pMainWnd!=NULL)
		((CMainFrame*)m_pMainWnd)->DestroyStatusBarProgress();
}

/////////////////////////////////////////////////////////////////////////////
// CDrawApp commands

void CDrawApp::OnExtrasOptions() 
{
	COptionDialog dlg(IDS_OPTIONS);

	dlg.DoModal();
}

void CDrawApp::WinHelp( DWORD dwData, UINT nCmd )
// ruft die Hilfe auf
{
  if( m_pContextHelp != NULL )
    m_pContextHelp->ShowHelp( dwData, nCmd );
} // WinHelp

