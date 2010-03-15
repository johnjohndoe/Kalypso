// stempel.cpp: Implementierung der Klasse CStempel.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "drawdoc.h"
#include "draw.h"

#include "stempel.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

/* static */
const CSize CStempel::nullSize = CSize( 0, 0 );

// dieser Faktor ist nur für den Herrn Jung (Lippelängsschnitt) und sollte iegentlich immer 1 sein
const int CStempel::X_SCALE_FAKTOR = 1;

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CStempel::CStempel()
{
  m_margins = CSize( 10 * MM_FACTOR, 10 * MM_FACTOR );
  m_bAlignToProfil = FALSE; // Standard: Stempel klebt am Seitenrand
  m_bHorizontal = TRUE; // Standard: Profil und Stempel horizontal angeordnet
  m_aktZoomFaktor = m_zoomFaktor = 100; // Standard ist keine Skalierung des Stempels
}


//////////////////////////////////////////////////////////////////////
// Operationen
//////////////////////////////////////////////////////////////////////

void CStempel::AddObject( CDrawObj* pObj )
{
  AddTailObject( pObj );
}

BOOL CStempel::RemoveObject( CDrawObj* pObj )
{
  // Find and remove from document
  POSITION pos = FindObject( pObj );
  if( pos != NULL )
  {
    RemoveObjectAt( pos );
    return TRUE;
  }
  else
    return FALSE;
} // Remove

void CStempel::Update( const CIntPoint& basePoint, const double realXScale, const double realYScale )
{
  // den Text der Scale neu setzen
  POSITION pos = GetHeadPosition();
  while( pos != NULL )
  {
    CDrawObj* pObj = GetNextObject( pos );
    if( pObj->IsText() && ((CDrawRect*)pObj)->GetStempelTextType() == STPL_TEXT_MS )
    {
      CString str;
      str.Format("1 : %.0f / %.0f", realXScale * X_SCALE_FAKTOR, realYScale );
      ((CDrawRect*)pObj)->SetText( str );
    }
  } // while pos

  // vorsichthalber mal UpdateScale nochmal hier aufrufen
  // sollte aber schon vor der Platzierung der Zeichenelemente erfolgt sein
  UpdateScale();

  // Set Stempel Position
  CIntPoint oldPos = GetRectPos();
  CIntPoint offset =  basePoint - oldPos;

  SetRectPos( basePoint );
  SetRectSize( GetExtent() );

  // alle Stempel-Objekte verschieben
  pos = GetHeadPosition();
  while( pos != NULL )
  {
    CDrawObj* pObj = GetNextObject( pos );
    pObj->m_position.OffsetRect( offset );
  } // while pos
} // UpdateStempel

CSize CStempel::UpdateScale()
// Skaliert den Stempel auf die gewünschte Grösse
// der aktuelle Platz wird beibehalten
{
  // falls sich gewünschter und aktueller ZoomFaktor unterscheiden, jetzt die
  // Skalierung durchführen
  if( m_zoomFaktor != m_aktZoomFaktor )
  {
    // was ist der Streckungsfaktor?
    double faktor = (double)m_zoomFaktor / (double)m_aktZoomFaktor; // um diesen Faktor wird alles gestreckt 1.0 = nix tun

    // das sollte nie passieren, aber man weiss ja nie
    if( faktor == 0.0 )
      return CSize( abs( GetRect().Width() ), abs( GetRect().Height() ) );

    CIntIRect stampRect( m_rect );
    stampRect.NormalizeRect();

    // jedes einzelne Objekt skalieren
    POSITION pos = GetHeadPosition();
    while( pos != NULL )
    {
      CDrawObj* pObj = GetNextObject( pos );

      // jetzt noch das Objekt entsprechend zum Basispunkt verschieben
      CIntIRect rect( pObj->m_position );
      rect.NormalizeRect();

      // jetzt alles neu ausrechnen
      double left = stampRect.left + ( rect.left - stampRect.left ) * faktor;
      double bottom = stampRect.bottom + ( rect.bottom - stampRect.bottom ) * faktor;
      double right = left + ( rect.right - rect.left ) * faktor;
      double top = bottom + ( rect.top - rect.bottom ) * faktor;

      rect = CIntIRect( left, top, right, bottom );

      pObj->MoveTo( rect );

      // zusätzlich noch den Text skalieren
      pObj->ScaleText( faktor );
    } // while

    // aktuellen Faktor jetzt neu setzen
    m_aktZoomFaktor = m_zoomFaktor;
  } // if m_zoomFaktor != m_aktZoomeFaktor
  
  // immer den Extent neu setzen
  CSize extent = GetExtent();
  CIntPoint basePoint = GetRectPos();
  CIntIRect rect( basePoint.x, basePoint.y + extent.cy, basePoint.x + extent.cx, basePoint.y );
  SetRect( rect );

  return CSize( abs( rect.Width() ), abs( rect.Height() ) );
} // UpdateScale

//////////////////////////////////////////////////////////////////////
// Attribute 
//////////////////////////////////////////////////////////////////////

const CSize& CStempel::GetMarginsAdjusted()
{
  if( GetObjectCount() == 0 )
     return nullSize;
  else
     return m_margins;
}
