// GenPage.h : header file
//

#ifndef _GENERAL_PAGE_H_INCLUDED_
#define _GENERAL_PAGE_H_INCLUDED_

/////////////////////////////////////////////////////////////////////////////
// CGeneralPage dialog

#include "propdlg.h"

class CGeneralPage : public CPropertyPage
{
  ////////////////////////////////
  // Construction / Destruction //
  ////////////////////////////////
public:
	CGeneralPage( CPropertyDialog *pParent, CPlotterDoc* pDoc, BOOL bTemplate = FALSE );

// Dialog Data
	//{{AFX_DATA(CGeneralPage)
	enum { IDD = IDD_GENERAL };
	CEdit	m_editTo;
	CEdit	m_editFrom;
	CEdit	m_editHeight;
	CButton	m_checkAutoTo;
	CButton	m_checkAutoFrom;
	CComboBox	m_comboStamps;
	CComboBox	m_comboDB;
	CButton	m_checkDB;
	CComboBox	m_comboYScale;
	CComboBox	m_comboXScale;
	CComboBox	m_templCombo;
	CButton	m_checkAutoHeight;
	double	m_height;
	CString	m_heightDefault;
	CString	m_hintText;
	CString	m_staticProps;
	CString	m_staticStamp;
	CString	m_staticFrom;
	CString	m_staticTo;
	CString	m_staticXScale;
	CString	m_staticYScale;
	double	m_ende;
	double	m_anfang;
	//}}AFX_DATA
  CString m_staticScale;
  CString m_staticRange;
  CString m_staticHeight;

// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CGeneralPage)
	public:
	virtual void OnOK();
	virtual BOOL OnApply();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementierung
public:
	void Update( CPlotterDoc* pDataDoc );
	void ApplyTemplate( CTemplate* pTemp );

  BOOL m_bNewStempel;

// Implementation
protected:
	CPropertyDialog* m_pParent;
	CPlotterDoc* m_pDoc;
	CPlotterView* m_pView;
	
  BOOL bOK;
	
  BOOL m_bNewTemplate;
	
  BOOL m_bTemplate;
	
  CMap<int, int, int, int> m_templatestempels;

  void UpdateControls();
	
  // Generated message map functions
	//{{AFX_MSG(CGeneralPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnChangeTemplate();
	afx_msg void OnChangeStempel();
  afx_msg void OnChange();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

}; // class CGenPage

#endif // _GENERAL_PAGE_H_INCLUDED_
