// PrinterSettings.h: Interface of CPrinterSettings.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_PRINTIT_H__2B9969C1_0872_11D3_9A02_0000B45414CC__INCLUDED_)
#define AFX_PRINTIT_H__2B9969C1_0872_11D3_9A02_0000B45414CC__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <winspool.h>	// for: ClosePrinter,GetPrinter,PRINTER_INFO_2


#define PRINTERNAME_UNDEFINED			"printer not set..."
#define PRINTERSETTINGS_DIRECTORYNAME	"printer settings"

class CPrinterSettings : public CObject  
{
public:
	CPrinterSettings();
	CPrinterSettings( const CString& dirname );
  CPrinterSettings( const CPrinterSettings& other ) { *this = other; }
	virtual ~CPrinterSettings();

  // Deklarations: for serialization
  DECLARE_SERIAL( CPrinterSettings );

// for use e.g. in CArray
public:
	void operator=(const CPrinterSettings* src);
	void operator=(const CPrinterSettings& src);

// general interface
public:
	// get the settings for the default MFC Printer
	void CopyDefaultMfcPrinter(void);
	// Set the default MFC Printer to this
	void SetThisPrinter(void);
	// Set the default MFC Printer by Name
	BOOL SetPrinterDevice(LPCTSTR pszDeviceName);
	
  // Show the common dialog and get the settings
  CString PrinterSetup( LPCTSTR strTitle = NULL, CWnd* pWnd = NULL );
  BOOL PageSetup( LPCTSTR strTitle = NULL, CWnd* pWnd = NULL );

  // restore previous printer settings (if safed )
	void RestorePrinter(void);
	CString GetFileName( void ) const	{ return m_strfilename; }
	CString SetDirName( const CString neu)  { CString temp = m_strdirname; m_strdirname = neu; return temp; }
	CString GetDirName( void ) const { return m_strdirname; }

  // Persitenz
	int Save(LPCTSTR strFilename);
	int Load(LPCTSTR strFilename);

  DWORD SaveBinary( BYTE** ppb ) const;
  void LoadBinary( const BYTE* pb );

  void Serialize( CArchive& ar );   // Serialization a la CObject

  // check to see if our printer is still available
	BOOL IsPrinterAvailable(LPCTSTR pszDeviceName);

  // check to see if our printer is still valid
  BOOL IsPrinterValid();

	// retrieve the human readable printername :)
	CString GetPrinterName(void) const { return m_strPrinterName; }

  // Create a PrinterDeviceContext from the current settings
  BOOL CreatePrinterDC( CDC& dc ) const;

  // Information about the current Settings
  BOOL GetPaperWidth( int& width ) const;
  BOOL GetPaperHeight( int& height ) const;
  CSize GetPaperSize() const;
  CSize GetRealPaperSize( const UINT faktor, const bool bMargins );
  
  HANDLE GetDevMode() const { return m_hDevMode; };
  HANDLE GetDevNames() const { return m_hDevNames; };

  CRect GetMargins() const { return m_margins; };
  BOOL HasMargins() const { return m_bHasMargins; };
  void SetMargins( const CRect& margins, const UINT factor ) { m_margins = margins; m_marginFactor = factor; m_bHasMargins = TRUE; };

private:	// but not useless:  :o)
	CString DevmodePrinterName(void);
	HANDLE CopyHandle(HANDLE h); 
	CString SetFileName( const CString neu) { CString temp = m_strfilename; m_strfilename = neu; return temp; }
	HANDLE m_hDevMode;
	HANDLE m_hDevNames;
	HANDLE m_hSaveDevMode;
	HANDLE m_hSaveDevNames;
	CString m_strPrinterName;
	CString m_strfilename;
	CString m_strdirname;

  CRect m_margins; // Page Margins
  UINT m_marginFactor;
  BOOL m_bHasMargins;

  CSize* m_pPaperSizeCache; // weil das holen der Seitengrösse sehr lange dauert
  void ResetPageSizeCache();

private:
	static void WriteHandle( CArchive& ar, HANDLE handle );
	static HANDLE ReadHandle( CArchive& ar );

};

#endif // !defined(AFX_PRINTIT_H__2B9969C1_0872_11D3_9A02_0000B45414CC__INCLUDED_)
