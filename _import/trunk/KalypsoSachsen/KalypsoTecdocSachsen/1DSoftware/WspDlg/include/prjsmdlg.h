#ifndef AFX_PRJSMDLG_H__B2B4C8A1_23C8_11D3_A4B8_0080ADAC5D6B__INCLUDED_
#define AFX_PRJSMDLG_H__B2B4C8A1_23C8_11D3_A4B8_0080ADAC5D6B__INCLUDED_

// prjsmdlg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld ProjectSummDialog 

#include "..\..\commonMfc\commonMfc.h"

class ProjectSummDialog : public CDialog
{
// Konstruktion
public:
	ProjectSummDialog(CWnd* pParent = NULL);   // Standardkonstruktor
	~ProjectSummDialog();

// Dialogfelddaten
	//{{AFX_DATA(ProjectSummDialog)
	enum { IDD = IDD_PROJECT_SUMM };
	CTreeCtrl	m_states;
	CListCtrlEx	m_data;
	CListCtrlEx	m_projects;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(ProjectSummDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	Project *m_pProject;
	CTypedPtrMap<CMapPtrToPtr, HANDLE, State*> m_qmap_tree;
	CTypedPtrMap<CMapPtrToPtr, HANDLE, State*> m_lmap_tree;
	CTypedPtrMap<CMapPtrToPtr, HANDLE, State*> m_cmap_tree;
	CTypedPtrMap<CMapPtrToPtr, HANDLE, State*> m_omap_tree;
	HTREEITEM m_hTISel;

	CUIntArray m_aPageStart;    // array of starting pages
	HFONT m_hPrinterFont;       // if NULL, mirror display font
	HFONT m_hMirrorFont;        // font object used when mirroring

	int m_nTabStops;            // tab stops in dialog units
	CString m_strText;

	BOOL CreateText(BOOL bTotal);

	BOOL DoPreparePrinting(CPrintInfo* pInfo);
	void OnBeginPrinting(CDC* pDC, CPrintInfo*);
	BOOL PaginateTo(CDC* pDC, CPrintInfo* pInfo);
	void OnPrepareDC(CDC* pDC, CPrintInfo* pInfo);
	UINT PrintInsideRect(CDC* pDC, RECT& rectLayout,
		UINT nIndexStart, UINT nIndexStop, UINT nPage);
	void OnPrint(CDC* pDC, CPrintInfo* pInfo);
	void OnEndPrinting(CDC*, CPrintInfo*);

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(ProjectSummDialog)
	virtual BOOL OnInitDialog();
	afx_msg void OnProject(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnSelchangedTree1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnColumnclickList1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnColumnclickList2(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnPrintSummary();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_PRJSMDLG_H__B2B4C8A1_23C8_11D3_A4B8_0080ADAC5D6B__INCLUDED_
