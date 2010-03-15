#ifndef AFX_ADVLABP_H__42CC0ED6_813A_11D3_BDAC_00104BB3E537__INCLUDED_
#define AFX_ADVLABP_H__42CC0ED6_813A_11D3_BDAC_00104BB3E537__INCLUDED_

// advlabp.h : Header-Datei
//

#include "colorbtnex.h"

class CMapLayerPropertyDlg;
class CMapLayer;
class CMapDoc;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CAdvancedLabelPage 

class CAdvancedLabelPage : public CPropertyPage
{
// Konstruktion
public:
	CAdvancedLabelPage( CMapLayerPropertyDlg* pParent, CMapLayer* pLayer );
	~CAdvancedLabelPage();

// Dialogfelddaten
	//{{AFX_DATA(CAdvancedLabelPage)
	enum { IDD = IDD_ADVNCD_LABEL_PAGE };
	CSliderCtrl	m_scale;
	CSliderCtrl	m_size;
	CColorButtonEx m_maskColor;
	CSliderCtrl	m_distance;
	CComboBox	m_textField;
	int		m_nRenderType;
	int		m_position;
	BOOL	m_drawBackground;
	BOOL	m_allowDuplicates;
	BOOL	m_bScale;
	BOOL	m_bMask;
	CString	m_name;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CAdvancedLabelPage)
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
	//{{AFX_MSG(CAdvancedLabelPage)
	afx_msg void OnChangeRenderType();
	virtual BOOL OnInitDialog();
	afx_msg void OnFont();
	afx_msg void OnMaskColor();
	afx_msg void OnScale();
	afx_msg void OnChangeName();
	afx_msg void OnKillfocusCombo1();
	afx_msg void OnRadioPosition();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_ADVLABP_H__42CC0ED6_813A_11D3_BDAC_00104BB3E537__INCLUDED_
