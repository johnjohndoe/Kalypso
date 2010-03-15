#ifndef AFX_VALSBLP_H__42CC0ED3_813A_11D3_BDAC_00104BB3E537__INCLUDED_
#define AFX_VALSBLP_H__42CC0ED3_813A_11D3_BDAC_00104BB3E537__INCLUDED_

class CMapRenderer;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CValueSymbolPage 

class CValueSymbolPage : public CPropertyPage
{
public:
	CValueSymbolPage( CMapLayerPropertyDlg* pParent, CMapLayer* pLayer, long type );
	~CValueSymbolPage();

// Dialogfelddaten
	//{{AFX_DATA(CValueSymbolPage)
	enum { IDD = IDD_VALUE_SYMBOL_PAGE };
	CComboBox	m_textField;
	int		m_nRenderType;
	CString	m_name;
	CMoLegend	m_legend;
	BOOL	m_removeOutline;
	CMoMap	m_map;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CValueSymbolPage)
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
	CMapLayer* m_pMapLayer;
	CMapRenderer* m_pRenderer;

	void UpdateLegend();

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CValueSymbolPage)
	afx_msg void OnChangeRenderType();
	virtual BOOL OnInitDialog();
	afx_msg void OnChangeName();
	afx_msg void OnKillfocusCombo1();
	afx_msg void OnSelchangeCombo1();
	afx_msg void OnCheck1();
	DECLARE_EVENTSINK_MAP()
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	afx_msg void OnRenderClickLegend1(short FAR* LayerIndex, short FAR* BreakIndex, VARIANT FAR* val1, VARIANT FAR* val2);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_VALSBLP_H__42CC0ED3_813A_11D3_BDAC_00104BB3E537__INCLUDED_
