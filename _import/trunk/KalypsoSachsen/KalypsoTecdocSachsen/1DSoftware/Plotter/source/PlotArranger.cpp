// PlotArranger.cpp: Implementierung der Klasse CPlotArranger.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "PlotArranger.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif


//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CPlotArranger::	CPlotArranger( const CIntIRect& profilMargins, const CSize& stampMargins, const CSize& profilSize, const CSize& stampSize, const CSize& drawingSize, const Align& profilAlign, const BOOL bAttachStampToProfil, const BOOL bHorizontal )
{
  // zuerst das Profil anordnen
  int profilX = 0;
  int profilY = 0;
  switch( profilAlign )
  {
  case NW:
    profilX = profilMargins.left;
    profilY = drawingSize.cy - profilSize.cy - profilMargins.top;
    break;
    
  case SW:
    profilX = profilMargins.left;
    profilY = profilMargins.bottom;
    break;
    
  case NE:
    profilX = drawingSize.cx - profilSize.cx - profilMargins.right;
    profilY = drawingSize.cy - profilSize.cy - profilMargins.top;
    break;
    
  case SE:
    profilX = drawingSize.cx - profilSize.cx - profilMargins.right;
    profilY = profilMargins.bottom;
    break;
  }
  
  // Stempel anordnen
  int stampX = 0;
  int stampY = 0;
  if( bAttachStampToProfil )
  {
    if( bHorizontal )
    {
      switch( profilAlign )
      {
      case NW:
        stampX = profilX + profilSize.cx + profilMargins.right;
        stampY = drawingSize.cy - stampMargins.cy - stampSize.cy;
        break;
        
      case SW:
        stampX = profilX + profilSize.cx + profilMargins.right;
        stampY = stampMargins.cy;
        break;
        
      case NE:
        stampX = profilX - profilMargins.left - stampSize.cx;
        stampY = drawingSize.cy - stampMargins.cy - stampSize.cy;
        break;
        
      case SE:
        stampX = profilX - profilMargins.left - stampSize.cx;
        stampY = stampMargins.cy;
        break;
      }
    }
    else
    {
      switch( profilAlign )
      {
      case NW:
        stampX = stampMargins.cx;
        stampY = profilY - profilMargins.bottom - stampSize.cy;
        break;
        
      case SW:
        stampX = stampMargins.cx;
        stampY = profilY + profilSize.cy + profilMargins.top;
        break;
        
      case NE:
        stampX = drawingSize.cx - stampMargins.cx - stampSize.cx;
        stampY = profilY - profilMargins.bottom - stampSize.cy;
        break;
        
      case SE:
        stampX = drawingSize.cx - stampMargins.cx - stampSize.cx;
        stampY = profilY + profilSize.cy + profilMargins.top;
        break;
      }
    }
  }
  else
  {
    if( bHorizontal )
    {
      switch( profilAlign )
      {
      case NW:
        stampX = drawingSize.cx - stampMargins.cx - stampSize.cx;
        stampY = drawingSize.cy - stampMargins.cy - stampSize.cy;
        break;
        
      case SW:
        stampX = drawingSize.cx - stampMargins.cx - stampSize.cx;
        stampY = stampMargins.cy;
        break;
        
      case NE:
        stampX = stampMargins.cx;
        stampY = drawingSize.cy - stampMargins.cy - stampSize.cy;
        break;
        
      case SE:
        stampX = stampMargins.cx;
        stampY = stampMargins.cy;
        break;
      }
    }
    else
    {
      switch( profilAlign )
      {
      case NW:
        stampX = stampMargins.cx;
        stampY = stampMargins.cy;
        break;
        
      case SW:
        stampX = stampMargins.cx;
        stampY = drawingSize.cy - stampMargins.cy - stampSize.cy;
        break;
        
      case NE:
        stampX = drawingSize.cx - stampMargins.cx - stampSize.cx;
        stampY = stampMargins.cy;
        break;
        
      case SE:
        stampX = drawingSize.cx - stampMargins.cx - stampSize.cx;
        stampY = drawingSize.cy - stampMargins.cy - stampSize.cy;
        break;
      }
    }
  }

  m_profilPoint = CIntPoint( profilX, profilY );
  m_stampPoint = CIntPoint( stampX, stampY );
}

/* static */
CSize CPlotArranger::ComputeDrawingSize( const CIntIRect& profilMargins, const CSize& stampMargins, const CSize& profilSize, const CSize& stampSize, const Align& stampAlign, const BOOL bHorizontal )
{
  if( bHorizontal )
    return CSize( profilMargins.left + profilMargins.right + profilSize.cx + stampMargins.cx + stampSize.cx,
    max( profilMargins.bottom + profilMargins.top + profilSize.cy, stampMargins.cy + stampSize.cy ) );
  else
    return CSize( max( profilMargins.left + profilMargins.right + profilSize.cx, stampMargins.cx + stampSize.cx ), 
    profilMargins.bottom + profilMargins.top + profilSize.cy + stampMargins.cy + stampSize.cy );
}