#ifndef AFX_STDLABP_H__42CC0ED5_813A_11D3_BDAC_00104BB3E537__INCLUDED_
#define AFX_STDLABP_H__42CC0ED5_813A_11D3_BDAC_00104BB3E537__INCLUDED_

class CMapRenderer;

class CStandardLabelPage : public CPropertyPage
{
public:
	CStandardLabelPage( CMapLayerPropertyDlg* pParent, CMapLayer* pLayer );
	~CStandardLabelPage();

// Dialogfelddaten
	//{{AFX_DATA(CStandardLabelPage)
	enum { IDD = IDD_STNDRD_LABEL_PAGE };
	CSliderCtrl	m_slider;
	CComboBox	m_fittedField;
	CComboBox	m_YOffsetField;
	CComboBox	m_XOffsetField;
	CComboBox	m_vertAlign;
	CComboBox	m_horzAlign;
	CComboBox	m_textField;
	int		m_nRenderType;
	CString	m_name;
	BOOL	m_drawBackground;
	BOOL	m_allowDuplicates;
	BOOL	m_splinedText;
	BOOL	m_flip;
	short	m_rotation;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CStandardLabelPage)
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
	CMapRenderer* m_pRenderer;
	LOGFONT m_logfont;
	COLORREF m_colorText;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CStandardLabelPage)
	afx_msg void OnChangeRenderType();
	virtual BOOL OnInitDialog();
	afx_msg void OnFont();
	afx_msg void OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	afx_msg void OnKillfocusCombo1();
	afx_msg void OnKillfocusCombo4();
	afx_msg void OnKillfocusCombo5();
	afx_msg void OnKillfocusCombo6();
	afx_msg void OnChangeName();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_STDLABP_H__42CC0ED5_813A_11D3_BDAC_00104BB3E537__INCLUDED_
