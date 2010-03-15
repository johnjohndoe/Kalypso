// PropDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CPropertyDialog

#ifndef _PROPERTYDIALOG_H_INCLUDED_
#define _PROPERTYDIALOG_H_INCLUDED_

class CMarginsPage;
class CFormatPage;
class CDataPage;
class	CLinePage;
class CTextPage;
class CSolidPage;
class CPlotterView;
class CPlotterDoc;
class CTemplate;
class CGeneralPage;
class	CMarginsPage;
class	CFormatPage;
class	CDataPage;
class	CLinePage;
class	CTextPage;
class	CSolidPage;

class CPropertyDialog : public CPropertySheet
{
	DECLARE_DYNAMIC(CPropertyDialog)

// Construction
public:
	CPropertyDialog( CPlotterDoc* pDoc, UINT nIDCaption, UINT iSelectPage = 0, BOOL bTemplate = FALSE, CWnd* pParent = NULL );
	CPropertyDialog( CPlotterDoc* pDoc, LPCTSTR pszCaption, UINT iSelectPage = 0, BOOL bTempate = FALSE, CWnd* pParent = NULL );
protected:
  Init( CPlotterDoc* pDoc, UINT iSelectPage, BOOL bTemplate );

// Attributes
public:
	int m_nFormat;
	int m_nActivePages;
	BOOL m_bUpdatePagesNeeded;
	BOOL m_bUndo;

// Operations
public:
	void UpdatePages();
	void ApplyTemplate(CTemplate* pTemp);
	void ApplyPages(BOOL bUndoable);
	void AttemptUpdateDrawing();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPropertyDialog)
	protected:
	virtual BOOL OnCommand(WPARAM wParam, LPARAM lParam);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CPropertyDialog();

protected:
	CGeneralPage* page1;
	CMarginsPage* page2;
	CFormatPage* page3;
	CDataPage* page4;
	CLinePage* page5;
	CTextPage* page6;
	CSolidPage* page7;
	CPlotterDoc* m_pDoc;

	// Generated message map functions
protected:
	//{{AFX_MSG(CPropertyDialog)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////


#endif _PROPERTYDIALOG_H_INCLUDED_