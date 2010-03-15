// GraphDlg.h : header file
//

#ifndef _GRAPHDLG_H_INCLUDED_
#define _GRAPHDLG_H_INCLUDED_

/////////////////////////////////////////////////////////////////////////////
// CGraphicDialog

class CGraphicDialog : public CPropertySheet
{
	DECLARE_DYNAMIC(CGraphicDialog)

// Construction
public:
  CGraphicDialog( UINT nIDCaption, CDrawObjList* pSelection, CDrawView* pView, UINT iSelectPage = 0 );
  CGraphicDialog( LPCTSTR pszCaption, CDrawObjList* pSelection, CDrawView* pView, UINT iSelectPage = 0 );

  void CreatePages( CDrawObjList* pObs, CDrawView* pView );

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CGraphicDialog)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CGraphicDialog();

	// Generated message map functions
protected:
  CTypedPtrArray<CPtrArray, CPropertyPage*> m_pages;

	//{{AFX_MSG(CGraphicDialog)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

#endif // _GRAPHDLG_H_INCLUDED_