// PlotFrm.h : interface of the CPlotterFrame class
//
/////////////////////////////////////////////////////////////////////////////

#include "browsetoolbar.h"

#include "mainfrm.h"

class State;
class SectionArray;
class CrossSection;
class CProfilOverview;

class CPlotterFrame : public CMainFrame
{
	DECLARE_DYNAMIC(CPlotterFrame)

// Attributes
public:
  CComboBox* GetBrowseCombo() { return m_browseToolbar.GetComboBox(); }

protected:
  CBrowseToolBar m_browseToolbar;
  CTypedPtrArray<CObArray, CProfilOverview*> m_profilOverviews;

// Operations
public:
	void CloseProject();
  void SetBrowseCombo( State* state, CrossSection* activeCs );
  void OpenCrossSection( CrossSection* cs );

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPlotterFrame)
	//}}AFX_VIRTUAL

// Implementation
public:

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	virtual void OnUpdateFrameTitle(BOOL bAddToTitle);

protected:
  void OpenFiles( BOOL bAutoFileName );
  void CreatePlot( State* pState, const SectionArray& secArray, BOOL bAutoFileName );

// Generated message map functions
protected:
	//{{AFX_MSG(CPlotterFrame)
	afx_msg void OnFileOpen();
	afx_msg void OnExtrasPref();
	afx_msg void OnFileNew();
	afx_msg void OnFileOpenprojekt();
	afx_msg void OnFilePrintAll();
	afx_msg void OnExtrasStempel();
	afx_msg void OnFileCloseprojekt();
	afx_msg void OnUpdateFileCloseprojekt(CCmdUI* pCmdUI);
	afx_msg void OnFileDelete();
	afx_msg void OnUpdateFileDelete(CCmdUI* pCmdUI);
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnClose();
	//}}AFX_MSG
  afx_msg void OnUpdateEditBrowse(CCmdUI* pCmdUI);
  afx_msg BOOL OnEditBrowse( UINT nID );
  afx_msg void OnUpdateShowOverview( CCmdUI* pCmdUI );
  afx_msg void OnShowOverview();
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
