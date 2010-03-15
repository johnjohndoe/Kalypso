// plotterchildframe.h: Schnittstelle f�r die Klasse CPlotterChildFrame.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_PLOTTERCHILDFRAME_H__EBDEED53_B425_11D6_B311_00104BB3E525__INCLUDED_)
#define AFX_PLOTTERCHILDFRAME_H__EBDEED53_B425_11D6_B311_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "..\..\DRAW\INCLUDE\splitfrm.h"

// Klasse CPlotterChildFrame
// Von CSplitFrame abgeleitet, speziell f�r den Plotter,
// um 'OnMDIActivate' zu �berschreiben
// n�tig, um die BrwoseCombo immer richtig gef�llt zu haben
class CPlotterChildFrame : public CSplitFrame  
{
  DECLARE_DYNCREATE(CPlotterChildFrame)

protected:
	CPlotterChildFrame();

public:
  afx_msg void OnMDIActivate( BOOL bActivate, CWnd* pActivateWnd, CWnd* pDeactivateWnd );

  DECLARE_MESSAGE_MAP()
};

#endif // !defined(AFX_PLOTTERCHILDFRAME_H__EBDEED53_B425_11D6_B311_00104BB3E525__INCLUDED_)
