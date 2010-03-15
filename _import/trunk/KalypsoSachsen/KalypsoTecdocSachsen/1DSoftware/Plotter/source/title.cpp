// Title.cpp: Implementierung der Klasse CTitle.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "draw.h"
#include "drawobj.h"
#include "drawvw.h"
#include "plotdoc.h"

#include "Title.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

////////////////
// Konstanten //
////////////////

/* static */ UINT CTitle::TITLE_GAP = 2 * MM_FACTOR;

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CTitle::CTitle()
{
  m_titel = NULL;
}

void CTitle::Update( const CIntPoint& basePoint, CPlotterDoc* pDoc )
// Parameter:
//        const CPoint& basePoint: der Basispunkt des Titels: der Mittelpunkt der oberen Kante
{
  if( pDoc == NULL || m_titel == NULL || !m_titel->IsText() )
    return;
  
  CString str = m_titelFormatText;
  if( pDoc->FormatText( str ) )
    m_titel->SetText( str );
  
  CSize sizeOutput = m_titel->GetOutputTextSize( pDoc->GetView() );
  
  CDoubleIRect rect( basePoint.x - sizeOutput.cx / 2, basePoint.y - TITLE_GAP, basePoint.x + sizeOutput.cx / 2, basePoint.y - sizeOutput.cy - TITLE_GAP );
  m_titel->m_dPosition = rect;
} // Update
