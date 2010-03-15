// PrintRectLegend.cpp: Implementierung der Klasse CPrintRectLegend.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "resource.h"

#include "..\..\commonMfc\commonMfc.h"

#include "mapHelper.h"

#include "PrintRectLegend.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

IMPLEMENT_SERIAL( CPrintRectLegend, CPrintRect, VERSIONABLE_SCHEMA | 2 )

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CPrintRectLegend::CPrintRectLegend() : CPrintRect()
{
  m_bFixSize = FALSE;
}

CPrintRectLegend::~CPrintRectLegend()
{
  DeleteContents();
}

void CPrintRectLegend::DeleteContents()
{
  for( int i = 0; i < m_dibSections.GetSize(); i++ )
    delete m_dibSections[i];
  m_dibSections.RemoveAll();
}

void CPrintRectLegend::AdjustBounds( CDC* pDC, CRect& bounds )
{
  // erstmal die Gesamtgrösse rausfinden
  CSize size = GetSize();

  if( m_bFixSize )
  {
    // die Originalgrösse einstellen
    bounds.right = bounds.left + size.cx;
    bounds.bottom = bounds.top + size.cy;
  }
  else
  {
    // sonst, zumindest das Seitenverhältnis beibehalten
    // zur Zeit ist immer die Breite ausschlaggebend
    long height = MulDiv( bounds.Width(), size.cy, size.cx ); // Höhe = ( m_bounds.Width / size.cx ) * size.cx
    bounds.bottom = bounds.top + height;
  }
};

void CPrintRectLegend::Paint( CDC* pDC )
// gibt die Bitmap aus
{
  // wir müssen den DeviceContext wieder auf MM_TEXT umstellen, damit die Liniendicke Pixel und
  // nicht Millimeter bedeutet
  CRect bounds = GetBounds();
  
  // die Gesamtgrösse ausrechnen
  CSize size = GetSize();
  double scale = (double)bounds.Width() / (double)size.cx;
  
  // einfach die Bitmap ausgeben
  for( int i = 0; i < m_dibSections.GetSize(); i++ )
  {
    CDIBSectionLite* dibSection = m_dibSections[i];
    ASSERT( dibSection );

    // die ausgabemasse ausrechnen
    long height = (long)( scale * dibSection->GetHeight() ); // Höhe dieses Stücks in Logischen Massen
    bounds.bottom = bounds.top + height; // entpsrechend die Bounds anpassen
    dibSection->Stretch( pDC, bounds.TopLeft(), bounds.Size(), TRUE );
    bounds.top = bounds.bottom; // das nächste soll direkt unten anschliessen
  }; // for i
    
  CPrintRect::Paint( pDC );
} // Paint


CPropertyPage* CPrintRectLegend::CreatePropPage( UINT captionID )
{
  return 0; //new CPrintLegendPage( captionID, this );
}

void CPrintRectLegend::SetMap( CMoMap* pMoMap )
{
  /*
  CPrintRect::SetMap( pMoMap, pMoLegend );

  // erstmal den alten Inhalt löschen
  DeleteContents();

  // alle Daten der Legende auslesen:
  ULONG lBackColor = pMoLegend->GetBackColor();

  // die Legende initialisieren
  pMoLegend->RemoveAll();

  pMoLegend->SetBackColor( moWhite );
  BOOL bShowCheck = FALSE;
  pMoLegend->LoadLegend( &bShowCheck );

  // die Legende hat leider keine vernünftige Zeichenfunktion
  // deswegen benutzten wird den Bitmap export
  // die einzelnen Legenden einträge als Bitmap in eine temp. Datei speichern
  // und dann zu einer grossen Bitmap zusammensetzen

  CString tmpPath = "C:\\"; // Pfad auf ein Temporäres Verzeichnis, im zweifelsfalls die Root von C

  // den Wert der Umgebungsvariable 'Temp' rausfinden
  char buffer[MAX_PATH];
  buffer[0] = '\0';
  int nRet = GetEnvironmentVariable( "TEMP", buffer, sizeof(buffer) );
  if( nRet > 0 && nRet < sizeof(buffer) )
    tmpPath = buffer;


  // jeden Layer als kleine Bmp rausschreiben, und den Handle darauf merken
  for( short i = 0; i < pMoMap->GetLayers().GetCount(); i++ )
  {
    // nur ausgeben, wenn sichtbar
    if( !pMoLegend->GetLayerVisible( &i ) )
      continue;
    
    // jetzt in Datei exportieren ( einzige export Methode von CMoLegend )
    CString tmpName;
    
    tmpName.Format( "%s\\legende%d.bmp", tmpPath, i );
    
    CComBSTR bString( tmpName );
    if( !pMoLegend->ExportToBmp( &bString, &i ) )
      continue;

    CDIBSectionLite* dibSection = new CDIBSectionLite();
    if( dibSection->Load( tmpName ) )
      m_dibSections.Add( dibSection );
    else
      delete dibSection;
    // und die dibSection gleich vergessen
    dibSection = NULL;

    // sofort die neue Datei wieder löschen
    try
    {
      CFile::Remove( tmpName );
    }
    catch( CFileException* e )
    {
      e->Delete();
    }; // die Exception wird einfach ignoriert

  }; // for i

  // die Legende wieder so herstellen wie sie vorher war
  pMoLegend->RemoveAll();
  pMoLegend->SetBackColor( lBackColor );
  bShowCheck = TRUE; // in der anderen View sollen die Controlkästchen an sein
  pMoLegend->LoadLegend( &bShowCheck );

  // zuletzt nochmal die Bounds neu setzen, um ein ausrechnen der Seitenverhätlnisse zu erzwingen
  SetBounds( GetBounds(), NULL );
  */
}; // Rechteck mit der Karte neu initialisieren

void CPrintRectLegend::Serialize( CArchive& ar )
{
  // zuerst die allgmeinen Daten laden/schreiben
  CPrintRect::Serialize( ar );

  if( ar.IsStoring() )
    ar << m_bFixSize;
  else
  {
    UINT nVersion = ar.GetObjectSchema();
    
    if( nVersion > 1 )
      ar >> m_bFixSize;
    else
      m_bFixSize = FALSE;
  }; 
}; // Serialize

CSize CPrintRectLegend::GetSize() const
// gibt die gesamtgrösser der Legende zurück
{
  CSize size( 0, 0 );
  for( int i = 0; i < m_dibSections.GetSize(); i++ )
  {
    CDIBSectionLite* dibSection = m_dibSections[i];
    ASSERT( dibSection );
  
    size.cx = max( size.cx, dibSection->GetWidth() );
    size.cy += dibSection->GetHeight();
  }; // for i

  return size;
}; // GetSize

void CPrintRectLegend::SetFixSize( const BOOL fixSize )
{
  m_bFixSize = fixSize;

  // die Grösse neu ausrechnen
  SetBounds( GetBounds(), NULL );

  NotifyListeners( GetBounds() );
};
