#include "stdAfx.h"

#include "resource.h"

#include "printBorderPage.h"

#include "PrintRect.h"
#include "..\..\commonMfc\commonMfc.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

// CPrintRect ist serialisierbar
IMPLEMENT_SERIAL( CPrintRect, CObject, VERSIONABLE_SCHEMA | 2 )

CPrintRect::CPrintRect()
{
	m_pMoMap = NULL;
	m_borderWidth = 1;
	m_borderStyle = PS_SOLID;
  m_borderColor = RGB( 0, 0, 0 ); // Default Schwarz
	m_bounds = CRect( 0, 0, 1, 1 );
}

void CPrintRect::SetBounds( const CRect& bounds, CDC* pDC )
// setzt die Grösse des Druckrechtecks neu
// und gibt diese zurück
// Unterklassen von CPrintRect können das Rechtecks ggfls. noch modifizieren
// ( z.B. falls Grössenverhätlnisse gewahrt bleiben müssen )
// Parameter:
//        const CRect& bounds: das gewünschte Ausmass des Rechtecks
//        CDC* pDC: manche Druckrechte brauchen den DC für AdjustBounds
// Rückgabewert:
//        CRect: die tats. gesetzten Ausmase des Rechtecks
{
  // erstmal die Listeners informieren, dass die alten Bounds nicht mehr gültig sind
  NotifyListeners( m_bounds );

  // die bounds anpassen und setzen
  m_bounds = bounds;
  AdjustBounds( pDC, m_bounds );

  // und dann über die neuen informieren
  NotifyListeners( m_bounds );
}

void CPrintRect::SetBorderWidth( UINT width )
{
  if( m_borderWidth != width )
  {
    m_borderWidth = width;

    // es muss neu gezeichnet werden
    NotifyListeners( m_bounds );
  }
} // SetWidth

void CPrintRect::SetMap( CMoMap* pMoMap )
{
  ASSERT( pMoMap );
  
  m_pMoMap = pMoMap;
}; // SetMap

void CPrintRect::Paint( CDC* pDC )
// zeichnet nur de Rand um das DruckRechteck
{
	// den Zustand sichern
  int saveDC = pDC->SaveDC(); // den DeviceContext sichern

  // die Farben und Formen festlegen
  HBRUSH hBrush = (HBRUSH)::GetStockObject( NULL_BRUSH );
  ::SelectObject( pDC->GetSafeHdc(), hBrush );

  HPEN   hPen = (HPEN)::CreatePen( GetBorderStyle(), GetBorderWidth(), GetBorderColor() );
  ::SelectObject( pDC->GetSafeHdc(), hPen );

  //zeichne den Verschieberahmen
  CPrintRect::PaintTracker(pDC,m_state);
  
  // wir müssen den DeviceContext wieder auf MM_TEXT umstellen, damit die Liniendicke Pixel und
  // nicht Millimeter bedeutet
  CRect bounds = m_bounds;
  pDC->LPtoDP( bounds ); // aus logischen Device Krds machen

  pDC->SetMapMode( MM_TEXT ); // MappingMode umstellen

  pDC->DPtoLP( bounds ); // wieder in logische ( jetzt aber Pixel )

	pDC->Rectangle( bounds ); // und zeichnen

  // und den alten Context wiederherstellen
  pDC->RestoreDC( saveDC );
} // Paint

void CPrintRect::PaintTracker(CDC* pDC, TrackerState state)
{
	CRect rect, clipRect;
	
	ASSERT_VALID(this);

	switch (state)
	{
	case normal:
		break;

	case selected:
		{
			//Anzahl der handle punkte
			int nHandleCount = 8;
			for (int nHandle = 1; nHandle <= nHandleCount; nHandle += 1)
			{
				CRect handleRect = GetHandleRect( nHandle, pDC );

				pDC->PatBlt( handleRect.left, handleRect.top, handleRect.Width(), handleRect.Height(), DSTINVERT  ); // BLACKNESS ); // DST_INVERT
			}
			
		}
		break;
	}
}

CPoint CPrintRect::GetHandle(int nHandle)
{
	ASSERT_VALID(this);
	int x, y, xCenter, yCenter;
	CRect rect = GetBounds();
	

	// this gets the center regardless of left/right and top/bottom ordering
	xCenter = rect.left + rect.Width() / 2;
	yCenter = rect.top + rect.Height() / 2;

	switch (nHandle)
	{
	default:
		ASSERT(FALSE);

	case 1:
		x = rect.left;
		y = rect.top;
		break;

	case 2:
		x = xCenter;
		y = rect.top;
		break;

	case 3:
		x = rect.right;
		y = rect.top;
		break;

	case 4:
		x = rect.right;
		y = yCenter;
		break;

	case 5:
		x = rect.right;
		y = rect.bottom;
		break;

	case 6:
		x = xCenter;
		y = rect.bottom;
		break;

	case 7:
		x = rect.left;
		y = rect.bottom;
		break;

	case 8:
		x = rect.left;
		y = yCenter;
		break;
	}

	return CPoint(x, y);
}

// return rectange of handle in logical coords
CRect CPrintRect::GetHandleRect(int nHandleID, CDC* pDC)
{
	ASSERT_VALID(this);
	ASSERT(pDC != NULL);

	// get the center of the handle in logical coords
	CPoint point = GetHandle(nHandleID);
	
	// wir wollen in Pixels rechnen
	pDC->LPtoDP( &point );

	int saveDC = pDC->SaveDC();
	pDC->SetMapMode( MM_TEXT );

	pDC->DPtoLP( &point );

	int width = HANDLE_WIDTH;
	CRect rect( point.x - width, point.y - width, point.x + width, point.y + width );

	pDC->LPtoDP( rect );
	pDC->RestoreDC( saveDC );

	pDC->DPtoLP( rect );

	return rect;
}

int CPrintRect::DoPropertyDialog( CWnd* pParent /* = NULL */ )
// zeigt einen Eigenschaftsdialog für dieses Objects an
// Rückgabewert:
//          wie CDialog::DoModal; IDOK wenn OK gedrückt wurde, sonst IDCANCEL
{
  // eine PropertySheet mit zwei Propertypages erzeugen
  CPropertySheet propSheet( IDS_PRINT_PROP_CAPTION, pParent );
  
  // die Einstellungen für das spezielle Objekt
  CPropertyPage* pPropertiesPage = CreatePropPage( IDS_PRINT_PROP_PAGE );
  if( pPropertiesPage != NULL )
    propSheet.AddPage( pPropertiesPage );
  
  // die Einstellungen für den Rand
  CPrintBorderPage printBorder( this );
  propSheet.AddPage( &printBorder );
  

  // den Dialog anzeigen
  int nRet = propSheet.DoModal();

  // die Seite wieder löschen
  delete pPropertiesPage;
  
  return nRet;
} // DoPropertyDialog

void CPrintRect::Serialize( CArchive& ar )
// Serialisierung: 
// Achtung: jede Änderung in den Serialisierungen der Subklassen braucht eine Änderung
// der Versionsnummer dieser Klasse ( und nur diese Nummer ist jeweils gültig )
{
  if( ar.IsStoring() )
  {
    ar << m_bounds;
    ar << GetBorderStyle();
    ar << GetBorderWidth();
    ar << GetBorderColor();
  }
  else
  {
    int nVersion = ar.GetObjectSchema();
    ar.SetObjectSchema( nVersion ); // damit der nächste Aufruf der Subklasse genau diese Nummer bekommt

    ar >> m_bounds;
    ar >> m_borderStyle;
    ar >> m_borderWidth;
    ar >> m_borderColor;
  }; 
}; // Serialize

void CPrintRect::AddListener( CPrintRectListener* pListener )
// fügt einen neuen Listener in die Liste ein
{
  ASSERT( pListener != NULL );

  m_listeners.Add( pListener );
} // AddListener

void CPrintRect::RemoveListener( CPrintRectListener* pListener )
// löäscht einen Listener aus der Liste
{
  for( int i = 0; i < m_listeners.GetSize(); i++ )
  {
    if( m_listeners[i] == pListener )
    {
      m_listeners.RemoveAt( i );
      return;
    }; // if ...
  }; // for i
}; // RemoveListener

void CPrintRect::NotifyListeners( const CRect& rect )
// benachrichtigt alle Listener in der Liste
{
  for( int i = 0; i < m_listeners.GetSize(); i++ )
    m_listeners[i]->Update( rect );
} // NotifyListeners
  

