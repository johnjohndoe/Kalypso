#ifndef AFX_OVRVWBAR_H__0AD24A41_4A4C_11D3_A4B9_0080ADAC5D6B__INCLUDED_
#define AFX_OVRVWBAR_H__0AD24A41_4A4C_11D3_A4B9_0080ADAC5D6B__INCLUDED_

#pragma warning(disable:4786)
#pragma warning(disable:4503)

// ovrvwbar.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Fenster COverviewBar 

class COverviewBar : public CDialogBar
{
	DECLARE_DYNAMIC(COverviewBar)

// Konstruktion
public:
	COverviewBar();

// Attribute
public:
	CMoMap m_map;
	CListCtrl m_maps;

protected:
  //CMoMapLayer m_layer;
  //CMoLabelPlacer m_labelPlacer;
// Operationen
public:
	void UpdateOverviewMap();

// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(COverviewBar)
	public:
	virtual BOOL DestroyWindow();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);
	//}}AFX_VIRTUAL

// Implementierung
public:
	virtual ~COverviewBar();

	// Generierte Nachrichtenzuordnungsfunktionen
protected:
	virtual void DoPaint(CDC* pDC);

	//{{AFX_MSG(COverviewBar)
	afx_msg void OnDblclkList1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnClose();
	afx_msg void OnDestroy();
	//}}AFX_MSG
	afx_msg LRESULT HandleInitDialog(WPARAM wParam, LPARAM lParam);
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_OVRVWBAR_H__0AD24A41_4A4C_11D3_A4B9_0080ADAC5D6B__INCLUDED_
