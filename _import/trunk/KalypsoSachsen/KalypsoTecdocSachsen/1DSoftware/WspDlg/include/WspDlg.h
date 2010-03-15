// WspDlg.h : Haupt-Header-Datei für die DLL WSPDLG
//

#if !defined(AFX_WSPDLG_H__60510B84_1411_11D3_A4B8_0080ADAC5D6B__INCLUDED_)
#define AFX_WSPDLG_H__60510B84_1411_11D3_A4B8_0080ADAC5D6B__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// Hauptsymbole

class ProgressDialog;
class DatenbankDlg;

/////////////////////////////////////////////////////////////////////////////
// CWspDlgApp
// Siehe WspDlg.cpp für die Implementierung dieser Klasse
//

class CWspDlgApp : public CWinApp
{
public:
  CWspDlgApp();
  
  CWnd parentProgress, parentDatenbank, parentManager;

  ProgressDialog *pDlg;
  DatenbankDlg *datDlg;
  static int m_nProgressCancelMsg;
  static int nUeberfallbeiwertUpdateMsg;
  static int nRauheitswertUpdateMsg;
  static int nBewuchswertUpdateMsg;
  
protected:


// Überladungen
	// Vom Klassenassistenten generierte Überladungen virtueller Funktionen
	//{{AFX_VIRTUAL(CWspDlgApp)
	public:
	virtual int ExitInstance();
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

	//{{AFX_MSG(CWspDlgApp)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

extern CWspDlgApp theApp;

extern BOOL AFXAPI AfxSimpleScanf(LPCTSTR lpszText,
                                  LPCTSTR lpszFormat, va_list pData);
extern BOOL AFXAPI AfxSimpleFloatParse(LPCTSTR lpszText, double& d);

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // !defined(AFX_WSPDLG_H__60510B84_1411_11D3_A4B8_0080ADAC5D6B__INCLUDED_)
