// PlotterView.h : interface of the CPlotterView class
//
/////////////////////////////////////////////////////////////////////////////

#include "drawvw.h"

class CPlotterView : public CDrawView
{
protected: // create from serialization only
  //CPlotterView() {} : CDrawView();
	DECLARE_DYNCREATE(CPlotterView)

// Operations
public:
	// text editing
	virtual void BeginTextEdit( CDrawRect* pDrawRect );
	virtual void EndTextEdit();

  virtual void Clear();
  virtual void Undo();
  virtual void Props();

  void FillBrowseCombo( const BOOL bEmpty );

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPlotterView)
	public:
	virtual void OnInitialUpdate();
	//}}AFX_VIRTUAL

// Generated message map functions
protected:
	//{{AFX_MSG(CPlotterView)
	//}}AFX_MSG

  afx_msg void OnObjectIsProfil();
  afx_msg void OnUpdateObjectIsProfil( CCmdUI* pCmdUI );
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
