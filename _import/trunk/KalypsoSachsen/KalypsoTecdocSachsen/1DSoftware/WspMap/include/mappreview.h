// mappreview.h: Schnittstelle für die Klasse CMapPreview.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAPPREVIEW_H__E5278F33_3FF7_11D6_B2BB_00104BB3E525__INCLUDED_)
#define AFX_MAPPREVIEW_H__E5278F33_3FF7_11D6_B2BB_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

// Vorwärtsdeklarationen
class CPrintRect;

#include "mapLayout.h"

/////////////////////////////////////////////////////////////////////////
// Klasse CMapPreview
/////////////////////////////////////////////////////////////////////////
// Dies ist der Frame der Druckvorschau für den Mapper
// Er behandelt alles, was das MFC-Framework betrifft ( z.B. Messagehandling,
// Scrolling, Umrechnung von MausEvents in Papierkoordinaten ),
// leitet aber sonst alles direkt weiter an das Kartenlayout ( CMapLayout )
/////////////////////////////////////////////////////////////////////////
class CMapPreview : public CPreviewView, public CMapLayoutListener
{
	enum TrackerState { normal, selected, active, NW, N, NE, E, SE, S, SW, W };

public:
	CMapPreview();
	virtual ~CMapPreview();

  // Attribute
  BOOL InitializePreview( CMapLayout* pMapLayout, CMoMap* pMoMap );

  // Operationen
  void ClosePreview() { OnPreviewClose(); };  // benötigt, um die View von aussen zu schliessen
  void SizeToFit();
  void ResyncScrollSizes();
  void InvalidateLogRect( const CRect& rect );

  virtual BOOL Create( CWnd* pToolbarParent, LPCTSTR lpszClassName,
    LPCTSTR lpszWindowName, DWORD dwStyle,
    const RECT& rect,
    CWnd* pParentWnd, UINT nID,
    CCreateContext* pContext = NULL);

  // Implementation des CPrintRectListener Interfaces
  void Update( const CRect& rect, const BOOL bPages );

protected:
  DECLARE_DYNCREATE(CMapPreview)
    
  // Members
  CMapLayout* m_mapLayout;  // Referenz auf das MapLayout

  UINT m_curTool;
  CPoint m_trackPoint;
  CPoint m_lastPoint;
  TrackerState m_state;
  CRect m_trackRect;

  CPrintRect* m_selectedRect;
  LPCTSTR m_cursorID;

  // Operations
  void ToLogCrds( CPoint& point );
  CClientDC* GetDC();

  // Overridables
  virtual void OnPrepareDC(CDC* pDC, CPrintInfo* pInfo);
  
  afx_msg BOOL OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message);
  afx_msg void OnSize(UINT nType, int cx, int cy);
  afx_msg void OnDraw(CDC* pDC);
  afx_msg void OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
  afx_msg void OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
  afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
  afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
  afx_msg void OnLButtonDblClk( UINT nFlags, CPoint point );
  afx_msg void OnRButtonDown( UINT nFlags, CPoint point );
  afx_msg void OnMouseMove( UINT nFlags, CPoint point );
  afx_msg BOOL OnCommand( UINT nID ); // alle Kommandos der Knöpfe
  afx_msg void OnUpdateCommand( CCmdUI* pCmdUI );

  DECLARE_MESSAGE_MAP()

  friend class CMapView;
};

#endif // !defined(AFX_MAPPREVIEW_H__E5278F33_3FF7_11D6_B2BB_00104BB3E525__INCLUDED_)
