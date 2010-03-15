// template.cpp - implementation for templates
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "plotdocdata.h"
#include "template.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif


/////////////////////////////////////////////////////////////////////////////
// CTemplateData - all serializable data from CTemplate

IMPLEMENT_SERIAL( CTemplateData, CObject, VERSIONABLE_SCHEMA | 9 )

CTemplateData::CTemplateData()
{
  m_pCTitel = NULL;
  m_pLTitel = NULL;
}

CTemplateData::~CTemplateData()
{
  delete m_pCTitel;
  delete m_pLTitel;
}

void CTemplateData::Serialize(CArchive& ar)
{
  CString str;
  
  CObject::Serialize(ar);
  if (ar.IsStoring())
  {
    ar << m_name;
    ar << m_pCTitel;
    ar << m_pLTitel;
  }
  else
  {
    int nVersion = ar.GetObjectSchema();
    ar.SetObjectSchema(nVersion);
    switch (nVersion)
    {
    case 0:
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
    case 8:
    case 9:
      {
        ar >> m_name;
        if (nVersion<3)
        {
          ar >> str;
          ((CPlotterDoc*)ar.m_pDocument)->GetStempel()->SetFileName( str );
        }
        if (nVersion >=2 )
        {
          ar >> m_pCTitel;
          ar >> m_pLTitel;
        }
        else
        {
          CDoubleIRect rect(0, 1, 0, 1);
          /***** add text to cross section title *****/
          m_pCTitel = new CDrawRect(rect, (CDrawDoc*)ar.m_pDocument);
          m_pCTitel->SetShape(CDrawRect::text);
          m_pCTitel->SetTextOrientation(0);
          m_pCTitel->SetHorzJust( CDrawRect::center );
          m_pCTitel->SetVertJust( CDrawRect::center );
          m_pCTitel->SetTextType( CDrawRect::normal );
          m_pCTitel->SetFlags( CDrawObj::editable );
          m_pCTitel->SetTextHeight(-27);
          str.LoadString(IDS_DEFAULT_CTITLE);
          m_pCTitel->SetText(str);
          /***** add text to length section title *****/
          m_pLTitel = new CDrawRect(rect, (CDrawDoc*)ar.m_pDocument);
          m_pLTitel->SetShape(CDrawRect::text);
          m_pLTitel->SetTextOrientation(0);
          m_pLTitel->SetHorzJust(CDrawRect::center);
          m_pLTitel->SetVertJust(CDrawRect::center);
          m_pLTitel->SetTextType(CDrawRect::normal);
          m_pLTitel->SetFlags(CDrawObj::editable);
          m_pLTitel->SetTextHeight(-27);
          str.LoadString(IDS_DEFAULT_LTITLE);
          m_pLTitel->SetText(str);
        }
        if( nVersion < 4 )
        {
          CDoubleIRect rect(0, 1, 0, 1);
          CDrawRect *pDrawRect = new CDrawRect(rect, (CDrawDoc*)ar.m_pDocument);
          pDrawRect->SetShape(CDrawRect::text);
          pDrawRect->SetTextOrientation(0);
          pDrawRect->SetHorzJust(CDrawRect::center);
          pDrawRect->SetVertJust(CDrawRect::center);
          pDrawRect->SetTextType(CDrawRect::normal);
          pDrawRect->SetStempelTextType(STPL_TEXT_ZU);
          pDrawRect->SetFlags(CDrawObj::editable);
          ((CPlotterDoc*)ar.m_pDocument)->AddObject( pDrawRect, CPlotterDoc::stamp, 0 );
        }
        if( nVersion < 5 )
        {
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_RECHTSWERT_2);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_HOCHWERT_2);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_RE_BOESCHUNG_RE);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_HO_BOESCHUNG_RE);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_RE_BOESCHUNG_LI);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_HO_BOESCHUNG_LI);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_RE_BOESCHUNG_RE_2);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_HO_BOESCHUNG_RE_2);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_RE_BOESCHUNG_LI_2);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_HO_BOESCHUNG_LI_2);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_DEICH_RECHTS);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_DEICH_LINKS);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_DEICH_RECHTS_2);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_DEICH_LINKS_2);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_RE_DEICH_RE);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_HO_DEICH_RE);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_RE_DEICH_LI);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_HO_DEICH_LI);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_RE_DEICH_RE_2);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_HO_DEICH_RE_2);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_RE_DEICH_LI_2);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_HO_DEICH_LI_2);
        }
        if( nVersion < 6 )
        {
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_SCHLEPPSPANN);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_AUSUFERUNG_LI);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_AUSUFERUNG_RE);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_ENERGIEHOEHE);
        }
        if( nVersion < 7 )
        {
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_BOESCHUNGSKANTEN);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_WSP_DIFFERENZ);
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet(DST_MODELLGRENZEN);
        }
        if( nVersion < 8 )
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet( DST_LP_TEXT );

        if( nVersion < 9 )
          ((CTemplate*)ar.m_pDocument)->AddDataBlockObjectSet( DST_POL_GRENZEN );
        
        // die Titel-Objekte Titel alle noch offsetable setzen
        m_pCTitel->SetFlags( CDrawObj::offsetable );
        m_pLTitel->SetFlags( CDrawObj::offsetable );
      }
      break;
      
    default:
      AfxThrowArchiveException(CArchiveException::badSchema);
      break;
    }
  }
}

#ifdef _DEBUG
void CTemplateData::AssertValid()
{
}
#endif

/////////////////////////////////////////////////////////////////////////////
// CTemplate

IMPLEMENT_DYNCREATE(CTemplate, CPlotterDoc)

CTemplate::CTemplate() : CPlotterDoc()
{
  m_pTData = new CTemplateData;
}

CTemplate::~CTemplate()
{
  delete m_pTData;
}

CTemplate::CTemplate( const CString& name ) : CPlotterDoc()
{
  m_pTData = new CTemplateData;
  m_pTData->m_name = name;
}

CDrawObj* CTemplate::GetTemplateObj( int tmpl_type, int dat_type )
// Gibt das Vorlagenobjekt zu einem bestimmten Typ zurück
{
  // Stempel-Text
  if( tmpl_type > STPL_TEXT_NONE && tmpl_type < N_STPLTEXTS )
  {
    POSITION pos = GetStempel()->GetHeadPosition();
    while( pos != NULL )
    {
      CDrawObj* pObj = GetStempel()->GetNextObject( pos );
      if( pObj->IsText() && ((CDrawRect*)pObj)->GetStempelTextType() == tmpl_type )
      {
        pObj->SetFlags( CDrawObj::hideable | CDrawObj::moveable ); // TODO: das muss eigentlich nicht sein, Problem: alte Serialisierungen
        return pObj;
      }
    }
  }

  switch( tmpl_type )
  {
  case TMPL_PROFIL_POLYLINE:
    {
      POSITION pos = m_pPData->m_profil.GetHeadPosition();
      while( pos != NULL )
      {
        CDrawObj* pObj = m_pPData->m_profil.GetNextObject( pos );
        if( pObj->GetType() == dat_type )
        {
          if( !pObj->IsKindOf( RUNTIME_CLASS(CDrawPoly)) || ((CDrawPoly*)pObj)->GetShape() == CDrawPoly::polyline )
          {
            pObj->SetFlags( CDrawObj::hideable );
            return pObj;
          }
        }
      }
    }
    break;
    
  case TMPL_PROFIL_POLYGON:
    {
      POSITION pos = GetProfil()->GetHeadPosition();
      while( pos != NULL )
      {
        CDrawObj* pObj = GetProfil()->GetNextObject( pos );
        if( pObj->GetType() == dat_type )
        {
          if( pObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)) && ((CDrawPoly*)pObj)->GetShape()==CDrawPoly::polygon )
          {
            pObj->SetFlags( CDrawObj::hideable );
            return pObj;
          }
        }
      }
    }
    break;
    
  case TMPL_TABELLE_LINE:
    {
      for( int i = 0; i < GetTable()->GetSize(); i++ )
      {
        if( GetTable()->GetAt( i )->GetHeadObject()->GetType() == dat_type )
        {
          POSITION pos = GetTable()->GetAt( i )->GetHeadPosition();
          while( pos != NULL )
          {
            CDrawObj* pObj = GetTable()->GetAt( i )->GetNextObject( pos );
            if( pObj->IsLine() )
              return pObj;
          }
        }
      }
    }
    break;
    
  case TMPL_TABELLE_KEY1TEXT:
    {
      for( int i = 0; i < GetTableKey1()->GetSize(); i++ )
      {
        if( GetTableKey1()->GetAt( i )->GetHeadObject()->GetType() == dat_type)
        {
          POSITION pos = GetTableKey1()->GetAt( i )->GetHeadPosition();
          while( pos != NULL )
          {
            CDrawObj* pObj = GetTableKey1()->GetAt( i )->GetNextObject( pos );
            if( pObj->IsText() && ((CDrawRect*)pObj)->GetTextType()==CDrawRect::normal )
              return pObj;
          }
        }
      }
    }
    break;
    
  case TMPL_TABELLE_KEY2TEXT:
    {
      for( int i = 0; i < GetTableKey2()->GetSize(); i++ )
      {
        if( GetTableKey2()->GetAt( i )->GetHeadObject()->GetType()==dat_type)
        {
          POSITION pos = GetTableKey2()->GetAt( i )->GetHeadPosition();
          while( pos != NULL )
          {
            CDrawObj* pObj = GetTableKey2()->GetAt( i )->GetNextObject( pos );
            if( pObj->IsText() && ((CDrawRect*)pObj)->GetTextType()==CDrawRect::normal )
              return pObj;
          }
        }
      }
    }
    break;
    
  case TMPL_TABELLE_NORMTEXT:
    {
      for( int i = 0; i < GetTable()->GetSize(); i++ )
      {
        if( GetTable()->GetAt( i )->GetHeadObject()->GetType()==dat_type)
        {
          POSITION pos = GetTable()->GetAt( i )->GetHeadPosition();
          while( pos != NULL )
          {
            CDrawObj* pObj = GetTable()->GetAt( i )->GetNextObject( pos );
            if( pObj->IsText() && ((CDrawRect*)pObj)->GetTextType()==CDrawRect::normal )
            {
              pObj->SetFlags( CDrawObj::hideable );
              return pObj;
            }
          }
        }
      }
    }
    break;
    
  case TMPL_TABELLE_XCOORD:
    {
      for( int i = 0; i < GetTable()->GetSize(); i++ )
      {
        if( GetTable()->GetAt( i )->GetHeadObject()->GetType()==dat_type)
        {
          POSITION pos = GetTable()->GetAt( i )->GetHeadPosition();
          while( pos != NULL )
          {
            CDrawObj* pObj = GetTable()->GetAt( i )->GetNextObject( pos );
            if (pObj->IsText() && ((CDrawRect*)pObj)->GetTextType()==CDrawRect::xcoord)
            {
              pObj->SetFlags( CDrawObj::hideable );
              return pObj;
            }
          }
        }
      }
    }
    break;
    
  case TMPL_TABELLE_YCOORD:
    {
      for( int i = 0; i < GetTable()->GetSize(); i++ )
      {
        if( GetTable()->GetAt( i )->GetHeadObject()->GetType() == dat_type )
        {
          POSITION pos = GetTable()->GetAt( i )->GetHeadPosition();
          while( pos != NULL )
          {
            CDrawObj* pObj = GetTable()->GetAt( i )->GetNextObject( pos );
            if (pObj->IsText() && ((CDrawRect*)pObj)->GetTextType()==CDrawRect::ycoord)
            {
              pObj->SetFlags( CDrawObj::hideable );
              return pObj;
            }
          }
        }
      }
    }
    break;
    
  case TMPL_RAHMEN_LINE:
    {
      POSITION pos = m_pPData->m_rahmen.GetHeadPosition();
      while( pos != NULL )
      {
        CDrawObj* pObj = m_pPData->m_rahmen.GetNextObject( pos );
        if( pObj->IsLine() )
          return pObj;
      }
    }
    break;
    
  case TMPL_RAHMEN_TEXT:
    {
      POSITION pos = m_pPData->m_rahmen.GetHeadPosition();
      while( pos != NULL )
      {
        CDrawObj* pObj = m_pPData->m_rahmen.GetNextObject(pos);
        if( pObj->IsText() )
          return pObj;
      }
    }
    break;
    
  case TMPL_TITEL:
    {
      CDrawObj* pObj = GetTitle()->GetTitle();
      pObj->SetFlags( CDrawObj::hideable | CDrawObj::offsetable );
      return pObj;
    }
    break;
    
  case TMPL_HEIGHT:
    {
      CDrawObj* pObj = GetHeight();
      pObj->SetFlags( CDrawObj::hideable | CDrawObj::offsetable );
      return pObj;
    }
    break;
    
  case TMPL_CTITEL:
    {
      CDrawObj* pObj = m_pTData->m_pCTitel;
      pObj->SetFlags( CDrawObj::hideable | CDrawObj::offsetable );
      return pObj;
    }
    break;
    
  case TMPL_LTITEL:
    {
      CDrawObj* pObj = m_pTData->m_pLTitel;
      pObj->SetFlags( CDrawObj::hideable | CDrawObj::offsetable );
      return pObj;
    }
    break;
    
  case TMPL_COMMENT:
    {
      CDrawObj* pObj = GetComment();
      pObj->SetFlags( CDrawObj::hideable | CDrawObj::offsetable );
      return pObj;
    }
    break;
  }
  
  return NULL;
} // GetTemplateObj

void CTemplate::Serialize(CArchive& ar)
{
  CPlotterDoc::Serialize( ar );
  if( ar.IsStoring() )
    ar << m_pTData;
  else
  {
    delete m_pTData;
    m_pTData = NULL;  // prepare for exception
    ar >> m_pTData;
  }
}

#ifdef _DEBUG
void CTemplate::AssertValid()
{
}
#endif

////////////////////////////////////////////////////////////////////////////

void CTemplate::CreateDrawing( BOOL bLoaded /*=FALSE*/, BOOL )
// Erzeugt die Vorlagenobjekte für das Standardtemplate
{
  CDoubleIRect rect(0, 1, 0, 1);
  
  GetSections()->SetAtGrow( 0, NULL );
  GetStates()->SetAtGrow( 0, NULL );
  if( !bLoaded )
  {
    // create the drawing objects
    for( int i = 1; i < N_DSTYPES; i++ )
    {
      if( i != DST_STATION )
        AddDataBlockObjectSet( i );
    }
    // Station ganz ans Ende stellen
    AddDataBlockObjectSet( DST_STATION );
    
    CDrawRect* pDrawRect;

    /***** add 1 text item to height *****/
    pDrawRect = new CDrawRect( rect, this );
    pDrawRect->SetShape( CDrawRect::text );
    pDrawRect->SetTextOrientation( 0 );
    pDrawRect->SetHorzJust( CDrawRect::left );
    pDrawRect->SetVertJust( CDrawRect::center );
    pDrawRect->SetTextType( CDrawRect::normal );
    pDrawRect->SetFlags( CDrawObj::editable | CDrawObj::offsetable | CDrawObj::hideable );
    m_pPData->m_heightFormatText = CString( MAKEINTRESOURCE( IDS_DEFAULT_HEIGHT ) );
    AddObject( pDrawRect, height, 0 );
    
    /***** add line and text to rahmen *****/
    // add line
    pDrawRect = new CDrawRect( rect, this );
    pDrawRect->SetShape( CDrawRect::line );
    AddObject( pDrawRect, border, 0 );
    
    // add text
    /***** add text to title *****/
    pDrawRect = new CDrawRect(rect, this);
    pDrawRect->SetShape(CDrawRect::text);
    pDrawRect->SetTextOrientation(0);
    pDrawRect->SetHorzJust(CDrawRect::center);
    pDrawRect->SetVertJust(CDrawRect::center);
    pDrawRect->SetTextType(CDrawRect::normal);
    pDrawRect->SetFlags( CDrawObj::editable | CDrawObj::offsetable | CDrawObj::hideable );
    pDrawRect->SetTextHeight(-27);
    GetTitle()->SetFormatText( CString( MAKEINTRESOURCE( IDS_DEFAULT_TITLE ) ) );
    AddObject( pDrawRect, title, 0 );
    
    /***** add text to cross section title *****/
    pDrawRect = new CDrawRect( rect, this );
    pDrawRect->SetShape( CDrawRect::text );
    pDrawRect->SetTextOrientation( 0 );
    pDrawRect->SetHorzJust( CDrawRect::center );
    pDrawRect->SetVertJust( CDrawRect::center );
    pDrawRect->SetTextType( CDrawRect::normal );
    pDrawRect->SetFlags( CDrawObj::editable | CDrawObj::offsetable | CDrawObj::hideable );
    pDrawRect->SetTextHeight( -27 );
    pDrawRect->SetText( CString(MAKEINTRESOURCE(IDS_DEFAULT_CTITLE)) );
    m_pTData->m_pCTitel = pDrawRect;
    
    /***** add text to length section title *****/
    pDrawRect = new CDrawRect( rect, this );
    pDrawRect->SetShape( CDrawRect::text );
    pDrawRect->SetTextOrientation( 0 );
    pDrawRect->SetHorzJust( CDrawRect::center );
    pDrawRect->SetVertJust( CDrawRect::center );
    pDrawRect->SetTextType( CDrawRect::normal );
    pDrawRect->SetFlags( CDrawObj::editable | CDrawObj::offsetable | CDrawObj::hideable );
    pDrawRect->SetTextHeight( -27 );
    pDrawRect->SetText( CString( MAKEINTRESOURCE( IDS_DEFAULT_LTITLE ) ) );
    m_pTData->m_pLTitel = pDrawRect;

    /***** add text to comment *****/
    pDrawRect = new CDrawRect( rect, this );
    pDrawRect->SetShape( CDrawRect::text );
    pDrawRect->SetTextOrientation( 0 );
    pDrawRect->SetHorzJust( CDrawRect::center );
    pDrawRect->SetVertJust( CDrawRect::center );
    pDrawRect->SetTextType( CDrawRect::normal );
    pDrawRect->SetFlags( CDrawObj::editable | CDrawObj::offsetable | CDrawObj::hideable );
    pDrawRect->SetTextHeight( -27 );
    m_pPData->m_commentFormatText.LoadString( IDS_DEFAULT_COMMENT );
    pDrawRect->SetText( m_pPData->m_commentFormatText );
    AddObject( pDrawRect, comment, 0 );
    
    /***** add line and text to stempel *****/
    // add line
    pDrawRect = new CDrawRect( rect, this );
    pDrawRect->SetShape( CDrawRect::line );
    AddObject( pDrawRect, CPlotterDoc::stamp, 0 );
    
    // add text
    for( i = 0; i < N_STPLTEXTS; i++ )
    {
      pDrawRect = new CDrawRect( rect, this );
      pDrawRect->SetShape( CDrawRect::text );
      pDrawRect->SetTextOrientation( 0 );
      pDrawRect->SetHorzJust( CDrawRect::center );
      pDrawRect->SetVertJust( CDrawRect::center );
      pDrawRect->SetTextType( CDrawRect::normal );
      pDrawRect->SetStempelTextType( i );
      pDrawRect->SetFlags( CDrawObj::editable | CDrawObj::hideable );
      AddObject( pDrawRect, CPlotterDoc::stamp, 0 );
    } // for i
  } // if !bLoaded
} // CreateDrawing

void CTemplate::AddDataBlockObjectSet( int type )
{
  BOOL bRect, bNorm, bXCoord, bYCoord, bPolyline, bPolygon, bInvisible, bTable, bTableInvisible;
  
  bRect = bXCoord = bYCoord = bPolyline = bPolygon = bTable = TRUE;
  bNorm = bInvisible = bTableInvisible = FALSE;
  
  switch( type )
  {
  default:
    break;
    
  case DST_GELAENDEHOEHE:   // polylines are drawn, only y coord
  case DST_UK_BRUECKE:
  case DST_OK_BRUECKE:
  case DST_GELAENDE2:
  case DST_OK_GELAENDE:
  case DST_KASTEN:
  case DST_WSP_HOEHE:
  case DST_WASSERSP1:
  case DST_WASSERSP100:
  case DST_WASSERSP5:
  case DST_SOHLHOEHE:
  case DST_WASSERSPIEGEL:
  case DST_BOESCHUNG_LINKS:
  case DST_BOESCHUNG_RECHTS:
  case DST_BVHOEHE:
  case DST_SOHLHOEHE_2:
  case DST_WASSERSPIEGEL_2:
  case DST_BOESCHUNG_LINKS_2:
  case DST_BOESCHUNG_RECHTS_2:
  case DST_WSP_FIXIERUNG:
  case DST_WSP_MESSUNG:
  case DST_DEICH_RECHTS:
  case DST_DEICH_LINKS:
  case DST_DEICH_RECHTS_2:
  case DST_DEICH_LINKS_2:
  case DST_AUSUFERUNG_LI:
  case DST_AUSUFERUNG_RE:
  case DST_ENERGIEHOEHE:
    bRect = FALSE;
    bXCoord = FALSE;
    break;
    
  case DST_OK_WEHRS:    // polylines are drawn, both coords
    bRect = FALSE;
    break;
    
  case DST_TRAPEZ:
    bRect = FALSE;
    bNorm = TRUE;
    break;
    
  case DST_KREISSEGM:   // rectangles are drawn, only y coord
  case DST_BORDVOLL:
  case DST_TRENN_WEHR:
  case DST_DKUK:
  case DST_DKOK:
    bPolyline = FALSE;
    bPolygon = FALSE;
    bXCoord = FALSE;
    break;
    
  case DST_STATION: // rectangles are drawn, only x coord
    bPolyline = FALSE;
    bPolygon = FALSE;
    bYCoord = FALSE;
    break;
    
  case DST_TRENNFLAECHEN:   // rectangles are drawn, both coords
  case DST_DURCHST_BEREICH:
  case DST_BOESCHUNGSKANTEN:
  case DST_MODELLGRENZEN:
    bPolyline = FALSE;
    bPolygon = FALSE;
    break;
    
  case DST_MAUL:
  case DST_EIPROFIL:
  case DST_KREIS:
  case DST_ARMCO84:
  case DST_ARMCO71:
    bPolyline = FALSE;
    bPolygon = FALSE;
    bNorm = TRUE;
    break;
    
  case DST_BAUWERK:   // rectangles are drawn, no coords available
  case DST_LP_TEXT:
    bPolyline = FALSE;
    bPolygon = FALSE;
    bXCoord = FALSE;
    bYCoord = FALSE;
    bTable = FALSE;
    bTableInvisible = TRUE;
    break;
    
  case DST_RAUHIGKEIT_KST:  // only in table, only y coord
  case DST_RAUHIGKEIT:
  case DST_AXM:
  case DST_AYM:
  case DST_DPM:
  case DST_FLAECHE:
  case DST_SVA_WERT:
  case DST_LWA_FELDER:
  case DST_GAUSS:
  case DST_RECHTSWERT:
  case DST_HOCHWERT:
  case DST_PUNKT_NR:
  case DST_NWRINNE:
  case DST_LAENGE:
  case DST_WSP_DIFFERENZ:
  case DST_WSP_BREITE:
  case DST_PROFILART:
  case DST_VZKENNG:
  case DST_PROFILKENNG:
  case DST_GEFAELLE:
  case DST_VMITTEL:
  case DST_RECHTSWERT_2:
  case DST_HOCHWERT_2:
  case DST_RE_BOESCHUNG_RE:
  case DST_HO_BOESCHUNG_RE:
  case DST_RE_BOESCHUNG_LI:
  case DST_HO_BOESCHUNG_LI:
  case DST_RE_BOESCHUNG_RE_2:
  case DST_HO_BOESCHUNG_RE_2:
  case DST_RE_BOESCHUNG_LI_2:
  case DST_HO_BOESCHUNG_LI_2:
  case DST_RE_DEICH_RE:
  case DST_HO_DEICH_RE:
  case DST_RE_DEICH_LI:
  case DST_HO_DEICH_LI:
  case DST_RE_DEICH_RE_2:
  case DST_HO_DEICH_RE_2:
  case DST_RE_DEICH_LI_2:
  case DST_HO_DEICH_LI_2:
  case DST_SCHLEPPSPANN:
    bPolyline = FALSE;
    bPolygon = FALSE;
    bRect = FALSE;
    bXCoord = FALSE;
    break;

  case DST_POL_GRENZEN:
    bPolyline = FALSE;
    bPolygon = FALSE;
    bRect = FALSE;
    bYCoord = FALSE;
    bNorm = TRUE;
    break;

  case DST_ABFLUSS:
    bRect = FALSE;
    bXCoord = FALSE;
    bInvisible=TRUE;
    break;
  }

  CDoubleIRect rect( 0, 1, 0, 1 );

  if( bPolyline )
  {
    /***** add polyline to profil *****/
    CDrawPoly* pDrawPoly = new CDrawPoly( rect, this );
    pDrawPoly->SetShape( CDrawPoly::polyline );
    pDrawPoly->SetType( type );
    pDrawPoly->SetFlags( CDrawObj::editable | CDrawObj::hideable );
    pDrawPoly->SetSectionIndex( 0 );
    if( bInvisible )
      pDrawPoly->SetFlags( CDrawObj::invisible, FALSE );
    AddObject( pDrawPoly, profil, 0 );
  }
  if( bPolygon )
  {
    /***** add polygon to profil *****/
    CDrawPoly* pDrawPoly = new CDrawPoly( rect, this );
    pDrawPoly->SetShape( CDrawPoly::polygon );
    pDrawPoly->SetType( type );
    pDrawPoly->SetSectionIndex( 0 );
    pDrawPoly->SetLineColor( RGB( 255, 255, 255 ) );
    pDrawPoly->SetFlags( CDrawObj::invisible, FALSE );
    pDrawPoly->SetFlags( CDrawObj::editable | CDrawObj::hideable );
    AddObject( pDrawPoly, profil, 0 );
  }

  if( bRect )
  {
    /***** add rect to profil *****/
    CDrawRect* pDrawRect = new CDrawRect( rect, this );
    pDrawRect->SetShape( CDrawRect::line );
    pDrawRect->SetType( type );
    pDrawRect->SetSectionIndex( 0 );
    pDrawRect->SetFlags( CDrawObj::editable | CDrawObj::hideable );

    switch( type )
    {
    case DST_STATION:
      pDrawRect->SetLineStyle( PS_DOT );
      break;
      
    case DST_LP_TEXT:
      pDrawRect->SetShape( CDrawRect::text );
      pDrawRect->SetTextOrientation( 270 );
      pDrawRect->SetVertJust( CDrawRect::right );
    };
    if( bInvisible )
      pDrawRect->SetFlags( CDrawObj::invisible, FALSE );

    AddObject( pDrawRect, profil, 0 );
  }
  
  /***** add 1 text item to table key1 *****/

  DataBlock DBlock( NULL );
  DBlock.SetType( type );

  // normal text
  CDrawRect* pDrawRect = new CDrawRect( rect, this );
  pDrawRect->SetShape( CDrawRect::text );
  pDrawRect->SetText( DBlock.GetDesc( 0 ) );
  pDrawRect->SetTextOrientation( 0 );
  pDrawRect->SetHorzJust( CDrawRect::center );
  pDrawRect->SetVertJust( CDrawRect::center );
  pDrawRect->SetTextType( CDrawRect::normal );
  pDrawRect->SetFlags( CDrawObj::editable );
  if ( bTableInvisible )
    pDrawRect->SetFlags( CDrawObj::invisible, FALSE );
  pDrawRect->SetType( type );
  pDrawRect->SetSectionIndex( 0 );
  AddObject( pDrawRect, tableKey1, GetTableKey1()->CreateNewIndex() );

  /***** add 1 text item to table key2 *****/
  pDrawRect = new CDrawRect( rect, this );
  pDrawRect->SetShape( CDrawRect::text );
  pDrawRect->SetText( DBlock.GetDesc( 1 ) );
  pDrawRect->SetTextOrientation( 0 );
  pDrawRect->SetHorzJust( CDrawRect::center );
  pDrawRect->SetVertJust( CDrawRect::center );
  pDrawRect->SetTextType( CDrawRect::normal );
  pDrawRect->SetFlags( CDrawObj::editable );
  if ( bTableInvisible )
    pDrawRect->SetFlags( CDrawObj::invisible, FALSE );
  pDrawRect->SetType( type );
  pDrawRect->SetSectionIndex( 0 );
  AddObject( pDrawRect, tableKey2, GetTableKey2()->CreateNewIndex() );
  

  CDrawObjList objList;
  /***** add 2/3 text items and 1 line to table *****/
  if( bNorm )
  {
    // normal text
    CDrawRect* pDrawRect = new CDrawRect( rect, this );
    pDrawRect->SetShape( CDrawRect::text );
    pDrawRect->SetTextOrientation( 0 );
    pDrawRect->SetHorzJust( CDrawRect::center );
    pDrawRect->SetVertJust( CDrawRect::center );
    pDrawRect->SetTextType( CDrawRect::normal );
    pDrawRect->SetFlags( CDrawObj::editable | CDrawObj::hideable );
    if ( bTableInvisible )
      pDrawRect->SetFlags( CDrawObj::invisible, FALSE );
    pDrawRect->SetType( type );
    pDrawRect->SetSectionIndex( 0 );
    objList.AddTailObject( pDrawRect );
  }
  if( bTable )   // coords available
  { 
    // x coord text
    CDrawRect* pDrawRect = new CDrawRect( rect, this );
    pDrawRect->SetShape( CDrawRect::text );
    pDrawRect->SetTextOrientation( 270 );
    pDrawRect->SetHorzJust( CDrawRect::right );
    pDrawRect->SetVertJust( CDrawRect::left );  // top
    pDrawRect->SetTextType( CDrawRect::xcoord );
    pDrawRect->SetFlags( CDrawObj::editable | CDrawObj::hideable );
    if( !bXCoord || bTableInvisible )
      pDrawRect->SetFlags( CDrawObj::invisible, FALSE );
    pDrawRect->SetType( type );
    pDrawRect->SetSectionIndex( 0 );
    objList.AddTailObject( pDrawRect );
    
    // y coord text
    pDrawRect = new CDrawRect( rect, this );
    pDrawRect->SetShape( CDrawRect::text );
    pDrawRect->SetTextOrientation( 270 );
    pDrawRect->SetHorzJust( CDrawRect::left );
    pDrawRect->SetVertJust( CDrawRect::left );  // top
    pDrawRect->SetTextType( CDrawRect::ycoord );
    pDrawRect->SetFlags( CDrawObj::editable | CDrawObj::hideable );
    if( !bYCoord || bTableInvisible )
      pDrawRect->SetFlags( CDrawObj::invisible, FALSE );
    pDrawRect->SetType( type );
    pDrawRect->SetSectionIndex( 0 );
    objList.AddTailObject( pDrawRect );
  }
  
  // line
  pDrawRect = new CDrawRect( rect, this );
  pDrawRect->SetShape( CDrawRect::line );
  if ( bTableInvisible )
    pDrawRect->SetFlags( CDrawObj::invisible, FALSE );
  pDrawRect->SetType( type );
  pDrawRect->SetSectionIndex( 0 );
  objList.AddTailObject( pDrawRect );
  
  AddObjects( &objList, table, GetTable()->CreateNewIndex() );
} // AddDataBlockObjectSet

