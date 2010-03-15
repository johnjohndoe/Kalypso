#ifndef AFX_TINCUTDLG_H__3EBD3642_F850_11D4_BDD1_00104BB3E525__INCLUDED_
#define AFX_TINCUTDLG_H__3EBD3642_F850_11D4_BDD1_00104BB3E525__INCLUDED_

// tincutdlg.h : Header-Datei
//

#include "resource.h"

class CLayerArray;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CDgmDlg 

class CDgmDlg : public CDialog
{
// Konstruktion
public:
	CDgmDlg(CLayerArray* layers, CWnd* pParent = NULL);

// Dialogfelddaten
	//{{AFX_DATA(CDgmDlg)
	enum { IDD = IDD_DGM };
	CComboBox	m_hmoCombo;
  CString m_path;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CDgmDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
  public:
    CString GetHmoPath() { return m_path; };
    CMapLayer* GetLayer() { return m_layer; };
protected:
  CLayerArray* m_layers;
  CMapLayer* m_layer;

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CDgmDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSelchangeHmoCombo();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_TINCUTDLG_H__3EBD3642_F850_11D4_BDD1_00104BB3E525__INCLUDED_
