#ifndef AFX_DIRDLG_H__79ED66E2_3835_11D3_A4B8_0080ADAC5D6B__INCLUDED_
#define AFX_DIRDLG_H__79ED66E2_3835_11D3_A4B8_0080ADAC5D6B__INCLUDED_

// dirdlg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld DirectoryDlg 

class DirectoryDlg : public CFileDialog
{
	DECLARE_DYNAMIC(DirectoryDlg)

public:
	 DirectoryDlg( LPCTSTR title, LPCTSTR lpszDirectory = NULL, CWnd* pParentWnd = NULL );
   ~DirectoryDlg();

	CString m_strDir;
  LPTSTR titleBuffer; // Zwischenspeicher für den Fenstertitel

// Overridable callbacks
protected:
	virtual BOOL OnFileNameOK();
	virtual void OnLBSelChangedNotify(UINT nIDBox, UINT iCurSel, UINT nCode);

	// only called back if OFN_EXPLORER is set
	virtual void OnInitDone();
	virtual void OnFileNameChange();
	virtual void OnFolderChange();

protected:
	CEdit m_edit;
	//{{AFX_MSG(DirectoryDlg)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_DIRDLG_H__79ED66E2_3835_11D3_A4B8_0080ADAC5D6B__INCLUDED_
