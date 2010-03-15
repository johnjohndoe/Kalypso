// StplView.h : interface of the CStempelView class
//
/////////////////////////////////////////////////////////////////////////////

#include "drawvw.h"

class CStempelView : public CDrawView
{
protected: // create from serialization only
	CStempelView();
	DECLARE_DYNCREATE(CStempelView)

// Implementation
protected:
  virtual void Props();

public:
  virtual ~CStempelView() {};

// Generated message map functions
protected:
	//{{AFX_MSG(CStempelView)
	afx_msg void OnExtrasGuide();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
