// PrinterSettings.cpp: Implementierung der Klasse CPrinterSettings.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "PrinterSettings.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

IMPLEMENT_SERIAL( CPrinterSettings, CObject, VERSIONABLE_SCHEMA | 1 )
// History
//    1 : with PageMargins

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CPrinterSettings::CPrinterSettings()
{
	m_hDevMode = m_hDevNames = NULL;
	m_hSaveDevMode = NULL;
	m_hSaveDevNames = NULL;
	m_strPrinterName = PRINTERNAME_UNDEFINED;
	m_strdirname = PRINTERSETTINGS_DIRECTORYNAME;
  m_bHasMargins = FALSE;
  m_marginFactor = 1;

  m_pPaperSizeCache = NULL;
}

CPrinterSettings::CPrinterSettings( const CString& dirname )
{
	CPrinterSettings();
	
  m_strdirname = dirname;
}

CPrinterSettings::~CPrinterSettings()
{
	if( m_hDevMode )
    GlobalFree( m_hDevMode );
	if( m_hDevNames )
    GlobalFree( m_hDevNames );

	// if possible MFC-Printer will go back to m_hSaveDevMode
	RestorePrinter();
  ResetPageSizeCache();
}

void CPrinterSettings::operator=( const CPrinterSettings* src )
{
	VERIFY(src != NULL);
	m_hDevMode = CopyHandle(src->m_hDevMode);
	m_hDevNames = CopyHandle(src->m_hDevNames);
	m_strPrinterName = src->m_strPrinterName;
	m_strdirname = src->m_strdirname;
	m_strfilename = src->m_strfilename;
  m_margins = src->m_margins;
  m_marginFactor = src->m_marginFactor;
  m_bHasMargins =  src->m_bHasMargins;

  ResetPageSizeCache();
}

void CPrinterSettings::operator=( const CPrinterSettings& src )
{
	*this = &src;
}

//
// check to see if our printer (e.g. m_strprintername)
// is still available
//
BOOL CPrinterSettings::IsPrinterAvailable( LPCTSTR pszDeviceName )
{
	HANDLE hPrinter;
	if( OpenPrinter( (char *)pszDeviceName, &hPrinter, NULL ) == FALSE )
  {
    ResetPageSizeCache();
		return FALSE;
  }

	ClosePrinter( hPrinter );
	return TRUE;
}

//
// Check to see if our printer is still valid
//
BOOL CPrinterSettings::IsPrinterValid()
{
  ResetPageSizeCache();

  // wir versuchen, einen DeviceContext zu erzeugen
  CDC dc;
  CreatePrinterDC( dc );

  return dc.GetSafeHdc() != NULL;
} // IsPrinterValid

//
// Set the MFC-Printer
// with default settings ( previous settings are freed )
//
// This code is from a microsoft sample:
// HOWTO: Implementing an Application-Defined Default Printer 
// Article ID: Q193103 
//
BOOL CPrinterSettings::SetPrinterDevice( LPCTSTR pszDeviceName )
{
	HANDLE hPrinter;

	if (OpenPrinter((char *) pszDeviceName, &hPrinter, NULL) == FALSE)
		return FALSE;

	DWORD dwBytesReturned, dwBytesNeeded;

	GetPrinter(hPrinter, 2, NULL, 0, &dwBytesNeeded);

	PRINTER_INFO_2* p2 = (PRINTER_INFO_2*)GlobalAlloc(GPTR,dwBytesNeeded);

	if (GetPrinter(hPrinter, 2, (LPBYTE)p2, dwBytesNeeded,&dwBytesReturned) == 0) {
		GlobalFree(p2);
		ClosePrinter(hPrinter);
		return FALSE;
	}
	ClosePrinter(hPrinter);

	// Allocate a global handle for DEVMODE and copy DEVMODE data.
	HGLOBAL  hDevMode = GlobalAlloc(GHND, sizeof(*p2->pDevMode));
	DEVMODE* pDevMode = (DEVMODE*) GlobalLock(hDevMode);

	CopyMemory(pDevMode, p2->pDevMode, sizeof(*p2->pDevMode));
	GlobalUnlock(hDevMode);

	// Compute size of DEVNAMES structure you'll need.
	DWORD drvNameLen = strlen(p2->pDriverName);  // driver name
	DWORD ptrNameLen = strlen(p2->pPrinterName); // printer name
	DWORD porNameLen = strlen(p2->pPortName);    // port name
	DWORD devNameSize = sizeof(DEVNAMES) + ptrNameLen + porNameLen + 	drvNameLen + 3;

	// Allocate a global handle big enough to hold DEVNAMES.
	HGLOBAL   hDevNames = GlobalAlloc(GHND, devNameSize);
	DEVNAMES* pDevNames = (DEVNAMES*)GlobalLock(hDevNames);

	// Copy the DEVNAMES information from PRINTER_INFO_2 structure.
	pDevNames->wDriverOffset = sizeof(DEVNAMES);
	CopyMemory((char*)pDevNames + pDevNames->wDriverOffset,p2->pDriverName, drvNameLen);

	pDevNames->wDeviceOffset = (USHORT) (sizeof(DEVNAMES) + drvNameLen + 1);
	CopyMemory((char*)pDevNames + pDevNames->wDeviceOffset,p2->pPrinterName, ptrNameLen);

	pDevNames->wOutputOffset = (USHORT) (sizeof(DEVNAMES) + drvNameLen + ptrNameLen + 2);
	CopyMemory((char*)pDevNames + pDevNames->wOutputOffset,p2->pPortName, porNameLen);

	pDevNames->wDefault = 0;
	GlobalUnlock(hDevNames);
	GlobalFree(p2);             // free PRINTER_INFO_2.

	m_hDevMode = hDevMode;
	m_hDevNames = hDevNames;

  ResetPageSizeCache();

	return TRUE;
}


//
// Show the printer common dialog
// Accept and store all settings
//
// This code is predominantly from a microsoft sample:
// HOWTO: Implementing an Application-Defined Default Printer 
// Article ID: Q193103 
//
CString CPrinterSettings::PrinterSetup( LPCTSTR strTitle /* = NULL */, CWnd* pWnd /* = NULL */ )
{
	// Ask the user which printer to use.
	CPrintDialog pd( TRUE,
    PD_ALLPAGES | PD_USEDEVMODECOPIES | PD_NOPAGENUMS | PD_HIDEPRINTTOFILE | PD_NOSELECTION, pWnd );

  // if we have a title, set it
  // but how to do it???

	// Make sure we don't accidentally create a device context
	pd.m_pd.Flags &= ~PD_RETURNDC;   // Reset flag set by constructor.

	// Force the CPrintDialog to use our device mode & name.
	if(m_hDevMode)
		pd.m_pd.hDevMode  = CopyHandle(m_hDevMode);
	if(m_hDevNames)
		pd.m_pd.hDevNames = CopyHandle(m_hDevNames);

	// Display the dialog box and let the user make their selection.

	if( pd.DoModal() == IDOK )
  {
		// The user clicked OK
		// (and POSSIBLY changed printers).
		// In any case, the CPrintDialog logic made a copy of the original
		// DEVMODE/DEVNAMES that we passed it and applied the user's
		// changes to that copy and discarded the original copy we passed
		// it. (NOTE: If the user had clicked CANCEL instead, the original
		// values we passed would have been returned unchanged).
		if( m_hDevMode )
			GlobalFree( m_hDevMode );                      // Free old copies.
		if( m_hDevNames )
			GlobalFree( m_hDevNames );                     // Free old copies.
		if( pd.m_pd.hDevMode )
			m_hDevMode  = CopyHandle( pd.m_pd.hDevMode );  // Save new copies.
		if( pd.m_pd.hDevNames )
			m_hDevNames = CopyHandle( pd.m_pd.hDevNames ); // Save new copies.
	}
	// Regardless of whether the user clicked OK or CANCEL,
	// we need to ALWAYS do a GlobalFree of CPrintDialog's
	// m_pd.hDevMode and m_pd.hDevNames upon return from
	// DoModal in order to prevent a resource leak.
	GlobalFree( pd.m_pd.hDevMode );   // Because DoModal was called,
	GlobalFree( pd.m_pd.hDevNames );  // we need to free these.

  ResetPageSizeCache();

	return DevmodePrinterName();
}

//
// show the page setup common dialog
//
// Returns TRUE, if Settings have changed
BOOL CPrinterSettings::PageSetup( LPCTSTR strTitle /* = NULL */, CWnd* pWnd /* = NULL */ )
{
  BOOL bChanged = FALSE;

  CPageSetupDialog psd;

  // if we have a title, set it
  // but how to do it???

  if( m_hDevMode )
    psd.m_psd.hDevMode  = CopyHandle( m_hDevMode );
  if( m_hDevNames )
    psd.m_psd.hDevNames = CopyHandle( m_hDevNames );

	psd.m_psd.Flags |= PSD_MINMARGINS | PSD_INHUNDREDTHSOFMILLIMETERS;
	psd.m_psd.rtMargin.left = MulDiv( m_margins.left, 100, m_marginFactor );
	psd.m_psd.rtMargin.top = MulDiv( m_margins.top, 100, m_marginFactor );
	psd.m_psd.rtMargin.right = MulDiv( m_margins.right, 100, m_marginFactor );
	psd.m_psd.rtMargin.bottom = MulDiv( m_margins.bottom, 100, m_marginFactor );

	if( psd.DoModal() == IDOK )
	{
    if( m_hDevMode )
      GlobalFree( m_hDevMode );                      // Free old copies.
    if( m_hDevNames )
      GlobalFree( m_hDevNames );                     // Free old copies.
		if( psd.m_psd.hDevMode )
			m_hDevMode  = CopyHandle( psd.m_psd.hDevMode );  // Save new copies.
		if( psd.m_psd.hDevNames )
			m_hDevNames = CopyHandle( psd.m_psd.hDevNames ); // Save new copies.

		m_margins.left = MulDiv( psd.m_psd.rtMargin.left, m_marginFactor, 100 );
		m_margins.top = MulDiv( psd.m_psd.rtMargin.top, m_marginFactor, 100 );
		m_margins.right = MulDiv( psd.m_psd.rtMargin.right, m_marginFactor, 100 );
		m_margins.bottom = MulDiv( psd.m_psd.rtMargin.bottom, m_marginFactor, 100 );

    bChanged = TRUE; // TODO: only TRUE if relly something changed
	}

	// Regardless of whether the user clicked OK or CANCEL,
	// we need to ALWAYS do a GlobalFree of CPrintDialog's
	// m_pd.hDevMode and m_pd.hDevNames upon return from
	// DoModal in order to prevent a resource leak.
	GlobalFree( psd.m_psd.hDevMode );   // Because DoModal was called,
	GlobalFree( psd.m_psd.hDevNames );  // we need to free these.

  ResetPageSizeCache();
	
  return bChanged;
};

//
// This code is from a microsoft sample:
// HOWTO: Implementing an Application-Defined Default Printer 
// Article ID: Q193103 
//
HANDLE CPrinterSettings::CopyHandle( HANDLE h )
{
	// Return a handle to a copy of the data
	// that the passed handle was for.
	if( !h )
    return NULL;

	DWORD dwLen = GlobalSize( h );
	HANDLE hCopy = GlobalAlloc( GHND, dwLen );
	if( hCopy )
  {
		BYTE* lpCopy = (BYTE*)GlobalLock( hCopy );
		BYTE* lp = (BYTE*)GlobalLock( h );
		CopyMemory( lpCopy, lp,dwLen );
		GlobalUnlock( hCopy );
		GlobalUnlock( h );
	}
	return hCopy;
}

//
// retrieve our printername
//
CString CPrinterSettings::DevmodePrinterName()
{
	if( !m_hDevMode ) 
		m_strPrinterName = PRINTERNAME_UNDEFINED;
	else 
  {
    DEVMODE* pDevMode = (DEVMODE*)GlobalLock(m_hDevMode);
    m_strPrinterName = (LPCTSTR) (pDevMode->dmDeviceName);
    GlobalUnlock(m_hDevMode);
  }
  
  return m_strPrinterName;
}


void CPrinterSettings::CopyDefaultMfcPrinter()
{
	PRINTDLG pd;
	// Get MFC's printer
	if( AfxGetApp()->GetPrinterDeviceDefaults(&pd) ) 
	{
		// Make a copy
		m_hDevNames = CopyHandle(pd.hDevNames);
		m_hDevMode = CopyHandle(pd.hDevMode);
		DevmodePrinterName();
	}
	else
	{
		// kein Default Printer gefunden -> alles auf NULL setzen, sonst
		// gibts später exceptions oder der user muss lange warten
		GlobalFree( m_hDevNames );
		m_hDevNames = NULL;

		GlobalFree( m_hDevMode );
		m_hDevMode = NULL;
	}

  ResetPageSizeCache();
}

void CPrinterSettings::SetThisPrinter()
{
	PRINTDLG pd;
	// Save MFC's printer and select ours instead.
	AfxGetApp()->GetPrinterDeviceDefaults(&pd);
	m_hSaveDevNames = pd.hDevNames;
	m_hSaveDevMode = pd.hDevMode;
  AfxGetApp()->SelectPrinter( CopyHandle( m_hDevNames ), CopyHandle( m_hDevMode ), FALSE );
}

void CPrinterSettings::RestorePrinter()
{
	// Restore previous MFC printer if possible
	if(m_hSaveDevNames && m_hSaveDevMode)
		AfxGetApp()->SelectPrinter(m_hSaveDevNames,m_hSaveDevMode,FALSE);
	m_hSaveDevNames = NULL;
	m_hSaveDevMode = NULL;

  ResetPageSizeCache();
}

//
// Save our settings to file
//
int CPrinterSettings::Save( LPCTSTR strFileName )
{
	SetFileName(strFileName);
	ASSERT( !m_strfilename.IsEmpty());
	ASSERT( !m_strdirname.IsEmpty());
	ASSERT(m_hDevMode);
	ASSERT(m_hDevNames);
	DWORD dret = NO_ERROR;
	// create special dir for printer settings
	BOOL ret = CreateDirectory(m_strdirname,NULL);
	if(!ret) {
		dret = GetLastError();
		if(dret != ERROR_ALREADY_EXISTS) {
			TRACE("error at CreateDirectory: %d\n",dret);
			return dret;
		}
	}
	// ask for the length of current directoryname 
	// (e.g. "f:\code guru\sample\" )
	dret = GetCurrentDirectory(0,NULL);
	if(dret > 0 ) {
		CString od;
		// just big enough
		LPTSTR pst = od.GetBuffer(dret);
		// to hold the string
		dret = GetCurrentDirectory(dret,pst);
		// change to our directory
		SetCurrentDirectory(m_strdirname);
		// create a new empty file
		HANDLE hDatei = 
			CreateFile(
				m_strfilename,
				GENERIC_READ | GENERIC_WRITE,
				0,
				NULL,
				CREATE_ALWAYS,
				FILE_ATTRIBUTE_NORMAL,
				NULL);
		if(NULL == hDatei) {
			TRACE("error at CreateFile: %d\n",GetLastError());
			return GetLastError();
		}
		//
		// Devmode
		//
		DWORD   dwLen = GlobalSize(m_hDevMode);
		BYTE* lp     = (BYTE*)GlobalLock(m_hDevMode);
		// leading length
		VERIFY(WriteFile(hDatei,&dwLen,sizeof(dwLen),&dret,NULL));
		// structure data
		VERIFY(WriteFile(hDatei,lp,dwLen,&dret,NULL));
		if(dret != dwLen) {
			TRACE("printersettings: problem writing DevMode %d <-> %d\n",dwLen,dret);
		}
		GlobalUnlock(m_hDevMode);
		//
		// Devnames
		//
		dwLen = GlobalSize(m_hDevNames);
		lp     = (BYTE*)GlobalLock(m_hDevNames);
		// leading length
		VERIFY(WriteFile(hDatei,&dwLen,sizeof(dwLen),&dret,NULL));
		// structure data
		VERIFY(WriteFile(hDatei,lp,dwLen,&dret,NULL));
		if(dret != dwLen) {
			TRACE("printersettings: problem writing DevNames %d <-> %d\n",dwLen,dret);
		}
		GlobalUnlock(m_hDevNames);

    // Margins
    VERIFY( WriteFile( hDatei, &m_bHasMargins, sizeof( m_bHasMargins ), &dret, NULL ) );
    VERIFY( WriteFile( hDatei, &m_margins, sizeof( m_margins ), &dret, NULL ) );
    VERIFY( WriteFile( hDatei, &m_marginFactor, sizeof( m_marginFactor ), &dret, NULL ) );

		CloseHandle(hDatei);
		// restore to previous directory
		SetCurrentDirectory(pst);
		od.ReleaseBuffer();
	}
	return NO_ERROR;
}

//
// load our settings from file
//
int CPrinterSettings::Load( LPCTSTR strFileName )
{
	SetFileName(strFileName);
	ASSERT( !m_strfilename.IsEmpty());
	ASSERT( !m_strdirname.IsEmpty());
	if(m_hDevMode) GlobalFree(m_hDevMode);
	if(m_hDevNames) GlobalFree(m_hDevNames);
	m_hDevMode = m_hDevNames = NULL;
	DWORD dret = NO_ERROR;
	
	// use a special directory for our printer settings
	BOOL ret = CreateDirectory(m_strdirname,NULL);
	if(!ret) {
		dret = GetLastError();
		if(dret != ERROR_ALREADY_EXISTS) {
			TRACE("error at CreateDirectory: %d\n",dret);
			return dret;
		}
	}
	// where are we
	dret = GetCurrentDirectory(0,NULL);
	if(dret > 0 ) {
		CString od;
		od.~od();
		LPTSTR pst = od.GetBuffer(dret + 1);
		dret = GetCurrentDirectory(dret,pst);
		// and where will we go
		SetCurrentDirectory(m_strdirname);
		// open file, ensure it is present
		HANDLE hDatei = 
			CreateFile(
				m_strfilename,
				GENERIC_READ,
				0,
				NULL,
				OPEN_EXISTING,
				FILE_ATTRIBUTE_NORMAL,
				NULL);
		// if the file could not be found
		// use MFC's fefault printer
		if(INVALID_HANDLE_VALUE == hDatei) {
			dret = GetLastError();
			CopyDefaultMfcPrinter();
			SetCurrentDirectory(pst);
			DevmodePrinterName();
			return dret;
		}
		//
		// DEVMODE
		//
		// 
		DWORD   dwLen = 0;
		// read leading length
		VERIFY(ReadFile(hDatei,&dwLen,sizeof(dwLen),&dret,NULL));
		// if desired do more testing for proper size here
		ASSERT(dwLen < 0xFFFF);
		ASSERT(dwLen > 0);
		m_hDevMode = GlobalAlloc(GHND, dwLen);
		if(NULL == m_hDevMode) {
			dret = GetLastError();
			TRACE("error at GlobalAlloc m_hDevMode: %d\n",dret);
			CloseHandle(hDatei);
      return dret;
    }
    BYTE* lpCopy = (BYTE*)GlobalLock(m_hDevMode);
    // read the whole caboodle
    VERIFY(ReadFile(hDatei,lpCopy,dwLen,&dret,NULL));
    GlobalUnlock(m_hDevMode);
    //
    // DEVNAMES
    //
    // read leading length
    VERIFY(ReadFile(hDatei,&dwLen,sizeof(dwLen),&dret,NULL));
    // is it believable
    ASSERT(dwLen < 0xFFFF);
    ASSERT(dwLen > 0);
    m_hDevNames = GlobalAlloc(GHND, dwLen);
    if(NULL == m_hDevNames) {
      dret = GetLastError();
      TRACE("error at GlobalAlloc m_hDevNames: %d\n",dret);
      CloseHandle(hDatei);
      return dret;
    }
    lpCopy = (BYTE*)GlobalLock(m_hDevNames);
    // read DevNames structure
    VERIFY(ReadFile(hDatei,lpCopy,dwLen,&dret,NULL));
    GlobalUnlock(m_hDevNames);

    // read Margins
    VERIFY( ReadFile( hDatei, &m_bHasMargins, sizeof( m_bHasMargins ), &dret, NULL ) );
    VERIFY( ReadFile( hDatei, &m_margins, sizeof( m_margins ), &dret, NULL ) );
    VERIFY( ReadFile( hDatei, &m_marginFactor, sizeof( m_marginFactor ), &dret, NULL ) );

    CloseHandle(hDatei);
    // restore former dir
    SetCurrentDirectory(pst);
  }
  DevmodePrinterName();

  ResetPageSizeCache();

  return NO_ERROR;
}

void CPrinterSettings::Serialize( CArchive& ar )
// Serialisierung a la CObject
{
  if ( ar.IsStoring() )
  {
	WriteHandle( ar, m_hDevMode );
	WriteHandle( ar, m_hDevNames );

    //
    // Margins
    //
    ar << m_bHasMargins;
    ar << m_margins;
    ar << m_marginFactor;

  } // IsStoring
  else
  {
    UINT nVersion = ar.GetObjectSchema();
    ar.SetObjectSchema( nVersion );

    if( m_hDevMode )
      GlobalFree( m_hDevMode );
    if( m_hDevNames ) 
      GlobalFree( m_hDevNames );
    m_hDevMode = m_hDevNames = NULL;
	
    //
    // DEVMODE
    //
    // 
	m_hDevMode = ReadHandle( ar );
	m_hDevNames = ReadHandle( ar );

    if( !IsPrinterValid() )
    {
		CFile* file = ar.GetFile();
		CString filePath = file->GetFilePath();
		LPTSTR lpFilePath = filePath.GetBuffer( filePath.GetLength() );
		TRACE( "Druckereinstellungen der gespeicherten Datei %s nicht mehr gültig\nEs wird der Standarddrucker verwendet.\n", lpFilePath );
		filePath.ReleaseBuffer();
		CopyDefaultMfcPrinter();
    }

    //
    // Margins
    //
    if( nVersion > 0 )
    {
      ar >> m_bHasMargins;
      ar >> m_margins;
      ar >> m_marginFactor;
    };

    ResetPageSizeCache();
  }  // loading
} // Serialize

void CPrinterSettings::WriteHandle( CArchive& ar, HANDLE handle )
{
	DWORD len = handle ? GlobalSize( handle ) : 0;
	ar << len;
	
	if( len > 0 )
	{
		BYTE* lp = (BYTE*)GlobalLock( handle );
		ar.Write( lp, len );
		
		GlobalUnlock( handle );
	}
}

/* static */
HANDLE CPrinterSettings::ReadHandle( CArchive& ar )
{
    DWORD len = 0;
	
    ar >> len;
	
	if( len == 0 )
		return NULL;

	HANDLE handle = NULL;
    handle = GlobalAlloc( GHND, len );
    if( handle == NULL )
      TRACE("error at GlobalAlloc m_hDevMode\n" );
	else
	{
		BYTE* lpCopy = (BYTE*)GlobalLock( handle );
		// read the whole caboodle
		ar.Read( lpCopy, len );
		
		GlobalUnlock( handle );
	}

	return handle;
}



//
// Loads content from a binary buffer
// see SaveBinary
//
void CPrinterSettings::LoadBinary( const BYTE* pb )
{
  // free old Data
  if( m_hDevMode )
    GlobalFree( m_hDevMode );
  if( m_hDevNames ) 
    GlobalFree( m_hDevNames );
  m_hDevMode = m_hDevNames = NULL;

  const BYTE* srcBuf = pb;

  DWORD dwSize = sizeof( DWORD );

  //
  // DEVMODE
  //
  // 
  DWORD devModeLen;
  memcpy( &devModeLen, srcBuf, dwSize );
  srcBuf += dwSize;

  m_hDevMode = GlobalAlloc( GHND, devModeLen );
  if( m_hDevMode == NULL )
  {
    TRACE("error at GlobalAlloc m_hDevMode\n" );
    return;
  }

  BYTE* devModeBuf = (BYTE*)GlobalLock( m_hDevMode );
  memcpy( devModeBuf, srcBuf, devModeLen );
  GlobalUnlock( m_hDevMode );
  srcBuf += devModeLen;

  //
  // DEVNAMES
  //
  DWORD devNamesLen;
  memcpy( &devNamesLen, srcBuf, dwSize );
  srcBuf += dwSize;

  m_hDevNames = GlobalAlloc( GHND, devNamesLen );
  if( m_hDevNames == NULL )
  {
    TRACE("error at GlobalAlloc m_hDevNames\n" );
    return;
  }

  BYTE* devNamesBuf = (BYTE*)GlobalLock( m_hDevNames );
  memcpy( devNamesBuf, srcBuf, devNamesLen );
  GlobalUnlock( m_hDevNames );
  srcBuf += devNamesLen;

  //
  // Margins
  //
  memcpy( &m_bHasMargins, srcBuf, sizeof( m_bHasMargins ) );
  srcBuf += sizeof( m_bHasMargins );

  memcpy( &m_margins, srcBuf, sizeof( m_margins ) );
  srcBuf += sizeof( m_margins );

  memcpy( &m_marginFactor, srcBuf, sizeof( m_marginFactor ) );
  srcBuf += sizeof( m_marginFactor );

  ResetPageSizeCache();
};

//
// Writes content to binary buffer
// Buffer must be delete[]d afterwards
//
DWORD CPrinterSettings::SaveBinary( BYTE** ppb ) const
{
  // first how many bytes do we need?
  DWORD dwSize = sizeof( DWORD );
  DWORD devModeLen = m_hDevMode ? GlobalSize( m_hDevMode ) : 0;
  DWORD devNamesLen = m_hDevNames ? GlobalSize( m_hDevNames ) : 0;
  DWORD hasMarginsLen = sizeof( m_bHasMargins );
  DWORD marginsLen = sizeof( m_margins );
  DWORD factorLen = sizeof( m_marginFactor );

  DWORD len = dwSize + devModeLen + dwSize + devNamesLen + hasMarginsLen + hasMarginsLen + marginsLen + factorLen;

  *ppb = new BYTE[len];

  // now write data
  BYTE* destBuffer = *ppb;

  //
  // Devmode
  //
  memcpy( destBuffer, &devModeLen, dwSize );
  destBuffer += dwSize;

  BYTE* lpMode = (BYTE*)GlobalLock( m_hDevMode );
  memcpy( destBuffer, lpMode, devModeLen );
  GlobalUnlock( m_hDevMode );

  destBuffer += devModeLen;

  //
  // Devnames
  //
  memcpy( destBuffer, &devNamesLen, dwSize );
  destBuffer += dwSize;

  BYTE* lpNames = (BYTE*)GlobalLock( m_hDevNames );
  memcpy( destBuffer, lpNames, devNamesLen );
  GlobalUnlock( m_hDevNames );

  destBuffer += devNamesLen;

  //
  // Margins
  //
  memcpy( destBuffer, &m_bHasMargins, hasMarginsLen );
  destBuffer += hasMarginsLen;

  memcpy( destBuffer, &m_margins, marginsLen );
  destBuffer += marginsLen;

  memcpy( destBuffer, &m_marginFactor, factorLen );

  return len;
}; // SaveBinary


/////////////////////////////////////////////////////////////////////////
// Information about the current printer settings
/////////////////////////////////////////////////////////////////////////

BOOL CPrinterSettings::GetPaperWidth( int& width ) const
// Returns current paper width in millimeters
// TODO: bug: if dmPaperSize is set, dmPaperWidth is not valid
{
  CDC dc;
  if( CreatePrinterDC( dc ) && dc.GetSafeHdc() )
	  width = dc.GetDeviceCaps( HORZSIZE );
  else
	  width = 210; // if no printer is set, assume a4 paper

  dc.DeleteDC();
  
  return TRUE;
} // GetPaperWidth

BOOL CPrinterSettings::GetPaperHeight( int& height ) const
// returns current paper height ( = length ) in mittlimeters
// TODO: bug: if dmPaperSize is set, dmPaperLength is not valid
{
  CDC dc;
  if( CreatePrinterDC( dc ) && dc.GetSafeHdc() )
	height = dc.GetDeviceCaps( VERTSIZE );
  else
    height = 297;
  
  dc.DeleteDC();
  
  return TRUE;
}

CSize CPrinterSettings::GetPaperSize() const
// return current paper size in millimeters
{
  int width, height;
  if( GetPaperWidth( width ) && GetPaperHeight( height ) )
    return CSize( width, height );
  else
    return CSize( 0, 0 );
} // GetPapersize


/*!
 * Returns the RealPaper Size in mm
 *
 * @param faktor : factor, by witch the size is multiplied
 * @param bMargins : if true, paper margins will be subtracted
 *
 * @return CSize  : the paper size in mm * factor - margins; ( 0, 0 ) if the printer is invalid
 */
CSize CPrinterSettings::GetRealPaperSize( const UINT faktor, const bool bMargins )
{
	// setting default size (e.g. no printer is present) to 0 causes to much trouble
	// better default to a4 paper
  CSize new_size( faktor * 210, faktor * 297 );

  if( m_pPaperSizeCache )
    new_size = *m_pPaperSizeCache;
  else
  {
    CDC dc;
    if( CreatePrinterDC( dc ) )
    {
      // Get the size of the page in lometric
      dc.SetMapMode( MM_ANISOTROPIC );
      double hppmm_d = (double)( dc.GetDeviceCaps( HORZRES ) ) / (double)( dc.GetDeviceCaps( HORZSIZE ) );
      double vppmm_d = (double)( dc.GetDeviceCaps( VERTRES ) ) / (double)( dc.GetDeviceCaps( VERTSIZE ) );
      int hppmm = int( hppmm_d * 10.0 );
      int vppmm = int( vppmm_d * 10.0 );
      
      dc.SetWindowExt( faktor * 10, faktor * 10 );
      dc.SetViewportExt( hppmm, vppmm );
      
      CRect rectDraw( 0, 0, dc.GetDeviceCaps( HORZRES ), dc.GetDeviceCaps( VERTRES ) );
      dc.DPtoLP( &rectDraw );

      new_size = CSize( rectDraw.Width(), rectDraw.Height() );
    }

	// auch den cahe setzen, wenn kein Drucker verfügbar ist
	// sonst eventuell Endlosschleife
	m_pPaperSizeCache = new CSize( new_size );
  }

  if( bMargins && new_size != CSize( 0, 0 ) )
    new_size -= CSize( ( m_margins.left + m_margins.right ) * 1, ( m_margins.top + m_margins.bottom ) * 1 );
  
  return new_size;
}; // GetPaperSize

BOOL CPrinterSettings::CreatePrinterDC( CDC& dc ) const
// Create a DeviceContext from the current settings
// the HANDLE to the DC must be destroyed ( e.g. by the destruktor of CDC )
{
  if( m_hDevMode != NULL && m_hDevNames != NULL )
  {
    HDC hDC = AfxCreateDC( m_hDevNames, m_hDevMode );
    if( hDC != NULL)
    {
      dc.DeleteDC();
      VERIFY(dc.Attach(hDC));
      return TRUE;
    }
  }
  return FALSE;
} // CreatePrinterDC

void CPrinterSettings::ResetPageSizeCache()
{
  delete m_pPaperSizeCache;
  m_pPaperSizeCache = NULL;
};