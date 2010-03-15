#include "stdAfx.h"

#include "printTextPage.h"

#include "PrintRectText.h"


#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

IMPLEMENT_SERIAL( CPrintRectText, CPrintRect, VERSIONABLE_SCHEMA | 2 )

// Konstruktion/Destruktion //


CPrintRectText::CPrintRectText() : CPrintRect(), IPrintRectFont()
{
  m_text = TEXT( "" );
};

void CPrintRectText::Paint( CDC* pDC )
{
  // die Höhe des Fonts transformieren: TODO: wie gehts wirklich??
  LOGFONT logFont = GetFontLogfont();
  // jetzt Punkt in Millimeter umrechnen; 1pt = 0.376 mm -> 1/10pt = 0.0376mm
  logFont.lfHeight = -(long)( GetFontPtSize() * 0.0376 ); // gibt blöde Rundungsfehler -> vielleicht doch lieber in MM_HIMETRIC arbeiten??

  // den Font und die Farbe setzen
  CFont font;
  if( !font.CreateFontIndirect( &logFont ) )
    return;

  int saveDC = pDC->SaveDC();

  pDC->SelectObject( &font ); // den neuen Font setzen
  pDC->SetTextColor( GetFontColor() ); // und die Farbe
  pDC->SetBkMode( TRANSPARENT ); // den Hintergrund nicht zeichnen

  CRect bounds = GetBounds();

  pDC->DrawText( m_text, &bounds, GetFontAlign() ); // und endlich den Text ausgeben
  
  pDC->RestoreDC( saveDC ); // den Context wiedeherstellen

  CPrintRect::Paint( pDC ); // und die Oberklasse aufrufen
} //Paint

void CPrintRectText::SetText( const CString& text)
{
  m_text = text;

  // und neu zeichnen
  NotifyListeners( GetBounds() );
}

CPropertyPage* CPrintRectText::CreatePropPage( UINT captionID )
// erzeugt die Seite für die Einstellungen dieses Objekts
{
  return new CPrintTextPage( captionID, this );
}

void CPrintRectText::Serialize( CArchive& ar )
{
  // zuerst die allgemeinen Daten laden/schreiben
  CPrintRect::Serialize( ar );
  
  if( ar.IsStoring() )
  {
    ar << m_text;

    WriteFont( ar );
  }
  else
  {
    UINT nVersion = ar.GetObjectSchema();

    if( nVersion > 0 )
    {
      ar >> m_text;

      ReadFont( ar );
    }
    else
    {
      UINT align;
      COLORREF color;
      UINT ptSize;
      LOGFONT logfont;
      
      ar >> align;
      ar >> m_text;
      
      ar.Read( &logfont,sizeof(LOGFONT) );
      ar >> color;
      ar >> ptSize;
      
      SetFontLogfont( logfont );
      SetFontColor( color );
      SetFontAlign( align );
      SetFontPtSize( ptSize );
    }; // if nVersion > 0
  }; 
  
}; // Serialize