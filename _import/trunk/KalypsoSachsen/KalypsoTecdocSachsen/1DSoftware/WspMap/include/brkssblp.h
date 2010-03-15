#ifndef AFX_BRKSSBLP_H__42CC0ED4_813A_11D3_BDAC_00104BB3E537__INCLUDED_
#define AFX_BRKSSBLP_H__42CC0ED4_813A_11D3_BDAC_00104BB3E537__INCLUDED_

#include "resource.h"
#include "colorbtnex.h"

class CMapRenderer;
class CMapLayerPropertyDlg;
class CMapLayer;
class CMapDoc;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CBreaksSymbolPage 

class CBreaksSymbolPage : public CPropertyPage
{
public:
	CBreaksSymbolPage( CMapLayerPropertyDlg* pParent, CMapLayer* pLayer, long type );
	~CBreaksSymbolPage();

// Dialogfelddaten
	//{{AFX_DATA(CBreaksSymbolPage)
	enum { IDD = IDD_BREAKS_SYMBOL_PAGE };
	CColorButtonEx m_endColor;
	CColorButtonEx m_startColor;
	CComboBox	m_endSize;
	CComboBox	m_startSize;
	CSpinButtonCtrl	m_spin;
	CComboBox	m_textField;
	int m_nRenderType;
	CString	m_name;
	short	m_breakCount;
	CMoLegend	m_legend;
	CMoMap m_map;
	BOOL	m_removeOutline;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CBreaksSymbolPage)
	public:
	virtual BOOL OnSetActive();
	virtual BOOL OnWizardFinish();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	CMapLayerPropertyDlg* m_pParent;
	CMapLayer* m_pLayer;
	long m_nShapeType;
	CMapRenderer* m_pRenderer;
	CMapLayer* m_pMapLayer;

	void UpdateLegend();

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CBreaksSymbolPage)
	afx_msg void OnChangeRenderType();
	virtual BOOL OnInitDialog();
	afx_msg void OnChangeName();
	afx_msg void OnStartColor();
	afx_msg void OnEndColor();
	afx_msg void OnKillfocusCombo1();
	afx_msg void OnSelchangeCombo1();
	afx_msg void OnChangeCombo2();
	afx_msg void OnChangeCombo3();
	afx_msg void OnChangeEdit2();
	afx_msg void OnCheck1();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_BRKSSBLP_H__42CC0ED4_813A_11D3_BDAC_00104BB3E537__INCLUDED_
