// PrintRectImage.cpp: Implementierung der Klasse CPrintRectImage.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "..\..\commonMfc\commonMfc.h"
#include "resource.h"

#include "PrintImagePage.h"

#include "PrintRectImage.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

// CPrintRect ist serialisierbar
IMPLEMENT_SERIAL( CPrintRectImage, CPrintRect, VERSIONABLE_SCHEMA | 2 )

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CPrintRectImage::CPrintRectImage() : CPrintRect()
{
  m_dibSection = NULL;
  m_bFixSize = FALSE;
}

CPrintRectImage::~CPrintRectImage()
{
  delete m_dibSection;
  m_dibSection = NULL; // Paranoia!
}

void CPrintRectImage::AdjustBounds( CDC* pDC, CRect& bounds )
{
  if( m_dibSection != NULL )
  {
    CSize imageSize = m_dibSection->GetSize();
    
    if( m_bFixSize )
    {
      // falls die Grösse aufs original fixiert ist, dies jetzt tun
      bounds.bottom = bounds.top + imageSize.cy;
      bounds.right = bounds.left + imageSize.cx;
    }
    else
    {
      // sonst anhand der Breite ausrichten
      long height = MulDiv( bounds.Width(), imageSize.cy, imageSize.cx ); // Höhe = ( m_bounds.Width / m_size.cx ) * m_size.cx
      bounds.bottom = bounds.top + height;
    } // if m_bFixSize
  };
} // AdjustBounds

void CPrintRectImage::Paint( CDC* pDC )
{
  CRect bounds = GetBounds();
  
  if( m_dibSection != NULL )
    m_dibSection->Stretch( pDC, bounds.TopLeft(), bounds.Size(), FALSE );
  
  CPrintRect::Paint( pDC );
} // Paint

void CPrintRectImage::Serialize( CArchive& ar )
{
  CPrintRect::Serialize( ar );
  
  if( ar.IsStoring() )
  {
    // Geht leider doch nicht, da ColorTable nicht mitserialisiert wird
    //ar.WriteObject( m_dibSection );
    ar << m_fileName;
    ar << m_bFixSize;
  }
  else
  {
    UINT nVersion = ar.GetObjectSchema();
    
    // den Dateinamen wiederherstellen
    delete m_dibSection;
    m_dibSection = NULL;
    
    // m_dibSection = (CDIBSectionLite*)ar.ReadObject( RUNTIME_CLASS(CDIBSectionLite) );
    
    ar >> m_fileName;
    if( nVersion > 1 )
      ar >> m_bFixSize;
    else
      m_bFixSize = FALSE;

    LoadBitmap( m_fileName );
  }; 
}; // Serialize


BOOL CPrintRectImage::LoadBitmap( const CString& fileName )
// lädt die Bitmap aus einer Datei
// Parameter:
//        const CString& fileName:
{
  // eine neue DibSection erstellen und versuchen zu laden
  CDIBSectionLite* newSection = new CDIBSectionLite();
  
  if( !newSection->Load( fileName ) )
  {
    delete newSection;
    newSection = NULL;
    return FALSE;
  }; // if !Load
  
  // die alte DibSection durch die neue ersetzen
  delete m_dibSection;
  m_dibSection = newSection;
  m_fileName = fileName;

  // und die Bounds neu setzen, um das anpassen der Grösse zu forcieren
  SetBounds( GetBounds(), NULL );
  
  return TRUE;
} // LoadBitmap

CPropertyPage* CPrintRectImage::CreatePropPage( UINT captionID )
{
  return new CPrintImagePage( captionID, this );
}

void CPrintRectImage::SetFixSize( const BOOL fixSize ) 
{
  m_bFixSize = fixSize;

  // die Grösse neu ausrechnen
  SetBounds( GetBounds(), NULL );

  NotifyListeners( GetBounds() );
};