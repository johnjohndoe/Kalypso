// Draw.h : main header file for all drawing applications
//

#ifndef _DRAW_H_INCLUDED_
#define _DRAW_H_INCLUDED_

#define MM_FACTOR				10
#define PI  3.14159265358979323846

#define GETDRAWAPP ((CDrawApp*)AfxGetApp())

HPALETTE _AfxInitPalette(VOID);

#include "commonMfc\include\printerSettings.h"

class CVersion;
class CContextHelp;

/////////////////////////////////////////////////////////////////////////////
// CDrawApp:
// See Draw.cpp for the implementation of this class
//
class CDrawApp : public CWinApp
{
public:
  enum SelectionType { SELTYPE_WHOLE	= 0, SELTYPE_INTERSECT	= 1 };

	CDrawApp();

	HPALETTE m_hPal;
	CTypedPtrArray<CPtrArray, LPLOGBRUSH> m_logbrushes;
	CTypedPtrArray<CPtrArray, CBitmap*> m_patterns;

	CStringArray m_strStempelFiles;
	CString m_strStempelName;	// temporary storage
	CSize m_sizeStempel;		// temporary storage
	int m_nSelectType;		// type of net selection for graphic objects
	int m_nSelectTol;		// selection tolerance (1/10 mm)
	CRect m_rectInitialFrame;
	BOOL m_bMaximized;

#ifdef _DEBUG
	BOOL m_bTraceUndo;
#endif
	static int m_nZoomFactorChangedMsg;
	HWND m_hWndZoomBox;

	CString GetExecDirectory() const { return m_execDirectory; }
	void LoadStempelFiles();
	BOOL SaveStempelFiles();
	void FlushStempelFiles();
	BOOL CreateStatusBarProgress(CString& str, int nLower, int nUpper);
	void IncStatusBarProgress();
	void DestroyStatusBarProgress();
	int GetBrushIndex(LPLOGBRUSH lpLogBrush);
	LPLOGBRUSH GetLogBrush(int index);

  CVersion* GetVersion() { return m_pVersion; };

  CPrinterSettings* GetPrinterSettings() { return &m_printerSettings; };

protected:
	COleTemplateServer m_server;
	CString m_execDirectory;

	virtual void LoadINISettings();
	virtual void SaveINISettings();
	void CreateStandardBrushes();
	void DestroyStandardBrushes();
	int GetPatternID(int n);

  CVersion* m_pVersion;
  CContextHelp* m_pContextHelp;

private:
  CPrinterSettings m_printerSettings;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDrawApp)
public:
  virtual BOOL InitInstance();
  virtual int ExitInstance();
  //}}AFX_VIRTUAL
  void WinHelp( DWORD dwData, UINT nCmd = HELP_CONTEXT );

// Implementation

	//{{AFX_MSG(CDrawApp)
	afx_msg void OnExtrasOptions();
  afx_msg void OnFilePrintSetup();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

#endif _DRAW_H_INCLUDED_