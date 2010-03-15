#include "stdAfx.h"

#include "resource.h"

#include "PrintRect.h"

#include "PrintRectOle.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

// CPrintRect ist serialisierbar
IMPLEMENT_SERIAL( CPrintRectOle, CPrintRect, VERSIONABLE_SCHEMA | 2 )

CPrintRectOle::CPrintRectOle()
{
  m_pOleItem = NULL;
}

CPrintRectOle::~CPrintRectOle()
{
  delete m_pOleItem;
}

void CPrintRectOle::Paint( CDC* pDC )
{
  if( m_pOleItem != NULL )
    m_pOleItem->Draw( pDC, GetBounds(), DVASPECT_CONTENT );

  CPrintRect::Paint( pDC );
}

void CPrintRectOle::Serialize( CArchive& ar )
{
  CPrintRect::Serialize( ar );

  if( ar.IsLoading() )
  {
    delete m_pOleItem;
    m_pOleItem = new COleClientItem( &m_oleDoc );
  };

  m_pOleItem->Serialize( ar );
}; // Serialize


BOOL CPrintRectOle::CreateOleObject()
// erzeugt und lädt das OLE Object
// Parameter:
//        CDC* pDC: ein Device Context, wird weitergegeben an UpdateExtent
{
  // Invoke the standard Insert Object dialog box to obtain information
  //  for new CDrawItem object.
  COleInsertDialog dlg( IOF_SELECTCREATENEW, NULL );

  //dlg.m_io.cClsidExclude = 1;
  //dlg.m_io.lpClsidExclude = &clsid;
  
  if( dlg.DoModal() != IDOK )
    return FALSE;
  
  COleClientItem* pOleItem = new COleClientItem( &m_oleDoc );
  
  // Now create the OLE object/item
  TRY
  {
    if( !dlg.CreateItem( pOleItem ) )
      AfxThrowMemoryException();
    
    pOleItem->UpdateItemType();
    
    // try to get initial presentation data
    pOleItem->UpdateLink();
    
    // if insert new object -- initially show the object
    if (dlg.GetSelectionType() == COleInsertDialog::createNewItem)
      pOleItem->DoVerb(OLEIVERB_SHOW, NULL );
  }
  CATCH_ALL(e)
  {
    // clean up item
    pOleItem->Delete();
    AfxMessageBox("Ole Objekt konnte nicht erzeugt werden");
  }
  END_CATCH_ALL

  m_pOleItem = pOleItem;
    
  return TRUE;
} // 

void CPrintRectOle::UpdateExtent( CDC* pDC )
// teilt dem OleContainer die verfügbare Grösse mit
// damit die Ansicht komplett ist
// Parameter:
//        CDC* pDC: der Device Context anhand dessen die Coordinaten umgerechnet werden
{
  ASSERT( pDC && m_pOleItem );

  // erstmal m_bounds in device Koordinaten umrechnen
  CSize size = GetBounds().Size();
  pDC->LPtoHIMETRIC( &size ); // schön das es sone funktion gibt
  m_pOleItem->SetExtent( size );
} // UpdateExtent
