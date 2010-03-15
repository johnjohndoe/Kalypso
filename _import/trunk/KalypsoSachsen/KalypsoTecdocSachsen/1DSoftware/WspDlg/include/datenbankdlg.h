#ifndef AFX_DATENBANKDLG_H__423E3F32_5F72_11D3_9D00_0090270D4773__INCLUDED_
#define AFX_DATENBANKDLG_H__423E3F32_5F72_11D3_9D00_0090270D4773__INCLUDED_

// DatenbankDlg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld DatenbankDlg 

#include "..\..\commonMfc\commonMfc.h"

class DatenbankDlg : public CDialog
{
// Konstruktion
public:
	int m_DlgTyp;
	CString pFileName;
	CFile m_db_file;
    enum Type { ueberfallbeiwert, rauheit_ks, rauheit_kst, bewuchs };
	DatenbankDlg(CWnd* pParent = NULL, CString start_path = "\0", int dlgtyp = 0);   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(DatenbankDlg)
	enum { IDD = IDD_BEIWERT_DB };
	CListCtrlEx	m_list;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(DatenbankDlg)
	public:
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	virtual BOOL OnChildNotify(UINT message, WPARAM wParam, LPARAM lParam, LRESULT* pLResult);
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	virtual void PostNcDestroy();
	virtual BOOL OnNotify(WPARAM wParam, LPARAM lParam, LRESULT* pResult);
	//}}AFX_VIRTUAL

// Implementierung
protected:
    CWnd* m_pParent;
    CToolTipCtrl m_tooltip;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(DatenbankDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnDestroy();
	afx_msg void OnNeu();
	afx_msg void OnDel();
	afx_msg void OnUpdate();
	virtual void OnOK();
	virtual void OnCancel();
	afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
    afx_msg BOOL OnToolTipNotify( UINT id, NMHDR * pNMHDR, LRESULT * pResult );
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_DATENBANKDLG_H__423E3F32_5F72_11D3_9D00_0090270D4773__INCLUDED_
