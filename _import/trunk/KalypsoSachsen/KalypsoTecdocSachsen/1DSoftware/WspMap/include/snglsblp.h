#ifndef AFX_SNGLSBLP_H__42CC0ED2_813A_11D3_BDAC_00104BB3E537__INCLUDED_
#define AFX_SNGLSBLP_H__42CC0ED2_813A_11D3_BDAC_00104BB3E537__INCLUDED_

// snglsblp.h : Header-Datei
//

#include "colorbtnex.h"
#include "commonMfc/include/symbbox.h"
#include "commonMfc/include/lineBox.h"
#include "commonMfc/include/brushBox.h"

class CMapLayerPropertyDlg;
class CMapLayer;
class CMapDoc;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CSingleSymbolPage 

class CSingleSymbolPage : public CPropertyPage
{
public:
	CSingleSymbolPage( CMapLayerPropertyDlg* pParent, CMapLayer* pLayer, long type );
	~CSingleSymbolPage();

// Dialogfelddaten
	//{{AFX_DATA(CSingleSymbolPage)
	enum { IDD = IDD_SINGLE_SYMBOL_PAGE };
	CSpinButtonCtrl	m_spin;
	CColorButtonEx m_border;
	CColorButtonEx m_color;
	int		m_nRenderType;
	CString	m_name;
	short	m_size;
	BOOL	m_bOutline;
	//}}AFX_DATA
	CSymbolComboBox	m_ptStyle;
	CLineComboBox m_lnStyle;
	CBrushComboBox m_flStyle;

// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CSingleSymbolPage)
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

  CTypedPtrArray<CPtrArray, CBitmap*> m_patterns;
  CTypedPtrArray<CPtrArray, LPLOGBRUSH> m_logbrushes;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CSingleSymbolPage)
	afx_msg void OnChangeRenderType();
	afx_msg void OnColor();
	afx_msg void OnBorder();
	virtual BOOL OnInitDialog();
	afx_msg void OnChangeName();
	afx_msg void OnSingleSymbolCheck();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_SNGLSBLP_H__42CC0ED2_813A_11D3_BDAC_00104BB3E537__INCLUDED_
