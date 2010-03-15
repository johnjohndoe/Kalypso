// PlotArranger.h: Schnittstelle für die Klasse CPlotArranger.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_PLOTARRANGER_H__177D3D91_8228_11D7_B3D7_00104BB3E525__INCLUDED_)
#define AFX_PLOTARRANGER_H__177D3D91_8228_11D7_B3D7_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "..\..\commonMfc\include\rect.h"

class CPlotArranger  
{
public:
  enum Align { NW, NE, SW, SE }; // Mögliche Ausrichtungen des Stempels und des Profils

public:
  CPlotArranger( const CIntIRect& profilMargins, const CSize& stampMargins, const CSize& profilSize, const CSize& stampSize, const CSize& drawingSize, const Align& profilAlign, const BOOL bAttachStampToProfil, const BOOL bHorizontal );

  const CIntPoint& GetStampPoint() { return m_stampPoint; };
  const CIntPoint& GetProfilPoint() { return m_profilPoint; };

  static CSize ComputeDrawingSize( const CIntIRect& profilMargins, const CSize& stampMargins, const CSize& profilSize, const CSize& stampSize, const Align& stampAlign, const BOOL bHorizontal );
	
private:
  CIntPoint m_stampPoint;
  CIntPoint m_profilPoint;
};

#endif // !defined(AFX_PLOTARRANGER_H__177D3D91_8228_11D7_B3D7_00104BB3E525__INCLUDED_)
