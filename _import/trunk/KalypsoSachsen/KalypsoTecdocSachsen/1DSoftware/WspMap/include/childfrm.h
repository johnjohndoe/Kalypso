// ChildFrm.h : Schnittstelle der Klasse CChildFrame
//
/////////////////////////////////////////////////////////////////////////////
#pragma warning(disable:4786)
#pragma warning(disable:4503)

#if !defined(AFX_CHILDFRM_H__27A882C9_3E7F_11D3_A4B9_0080ADAC5D6B__INCLUDED_)
#define AFX_CHILDFRM_H__27A882C9_3E7F_11D3_A4B9_0080ADAC5D6B__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "..\..\commonMfc\commonMfc.h"

#include "lgndbar.h"
#include "profileditor.h"
#include "scalebar.h"
#include "profilauswahl.h"
#include "profilauswahl.h"

class CChildFrame : public CMDIChildWnd
{
	DECLARE_DYNCREATE(CChildFrame)
public:
	CChildFrame();

// Attribute
public:
	CLegend m_legend;
  CCoolDialogBar m_wndLegendBar;
  CProfilEditor m_wndProfilEditor;
	CScaleBar m_wndScaleBar;
  CProfilAuswahl m_wndProfilAuswahl;

// Operationen
public:
 
// Überladungen
  virtual void OnUpdateFrameTitle( BOOL bAddToTitle );
	// Vom Klassenassistenten generierte Überladungen virtueller Funktionen
	//{{AFX_VIRTUAL(CChildFrame)
	//}}AFX_VIRTUAL

// Implementierung
public:
	virtual ~CChildFrame();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

// Generierte Message-Map-Funktionen
protected:
	//{{AFX_MSG(CChildFrame)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnClose();
	//}}AFX_MSG
  afx_msg BOOL OnCoolBarCheck( UINT nID );
  afx_msg void OnUpdateControlBarMenu( CCmdUI* pCmdUI );
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // !defined(AFX_CHILDFRM_H__27A882C9_3E7F_11D3_A4B9_0080ADAC5D6B__INCLUDED_)
