/////////////////////////////////////////////////////////////////////////////
// CPlotterDocData - all serializable data from CPlotterDoc


#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "plotter.h"
#include "profil.h"

#include "plotdocdata.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


IMPLEMENT_SERIAL( CPlotterDocData, CObject, VERSIONABLE_SCHEMA | 12 )

CPlotterDocData::CPlotterDocData()
{
  m_comment = NULL;
  m_height = NULL;
  m_sizeTableMargins = CSize(0, 0);

  m_rectBorderGaps.SetRect( 10 * MM_FACTOR, 10 * MM_FACTOR, 10 * MM_FACTOR, 10 * MM_FACTOR );
};

void CPlotterDocData::DeleteContents()
{
  m_user.RemoveAllObjects();
  m_stempel.RemoveAllObjects();
  m_profil.RemoveAllObjects();
  
  m_table.DeleteAllIndices( TRUE, FALSE );
  m_tableKey1.DeleteAllIndices( TRUE, FALSE );
  m_tableKey2.DeleteAllIndices( TRUE, FALSE );
  m_rahmen.RemoveAllObjects();

  m_title.SetTitle( NULL );
  m_comment = NULL;
  m_height = NULL;

  m_States.RemoveAll();
  m_Sections.RemoveAll();
}

void CPlotterDocData::Serialize( CArchive& ar )
{
  BOOL bFindLowPoint = FALSE;
  CString str;
  
  CObject::Serialize(ar);
  if( ar.IsStoring() )
  {
    ar.WriteCount(m_States.GetSize());
    ar.WriteCount(m_Sections.GetSize());
    ar << m_sizePage;
    ar << m_eigenschaften;

    ar << m_stempel.GetFileName();

    CDoubleIRect profilRect( m_profil.GetVisibleRect() );
    CDoublePoint sizeProfile( profilRect.Width(), profilRect.Height() );
    ar << sizeProfile; // Ersatz für m_sizeProfile

    ar << m_profil.GetTotalRect();
    ar << m_profil.GetFrom();
    ar << m_profil.GetTo();
    ar << (WORD)m_profil.GetAutoAnfang();
    ar << (WORD)m_profil.GetAutoEnde();
    ar << m_sizeTableMargins;
    ar << m_scale;
    ar << m_realScale;
    ar << m_title.GetTitle();
    ar << m_comment;
    ar << m_height;
    ar << m_profil.GetLowPoint();
    ar << (WORD)m_profil.GetAutoHeight();
    ar << m_profil.GetHeight();
    ar << m_heightFormatText;
    ar << m_commentFormatText;
    ar << m_title.GetFormatText();
    ar << m_rectBorderGaps;
    ar << m_stempel.GetMargins();
    ar << m_stempel.GetHorizontal();
    ar << (WORD)m_table.GetTableFormat();
    ar << (WORD)m_profil.GetFormat();
    ar << (int)m_profil.GetAlign();
    ar << m_stempel.GetAlignToProfil();
    ar << m_stempel.GetZoomFaktor();
    ar << m_profil.GetRangeDbType();
  }
  else
  {
    int nVersion = ar.GetObjectSchema();
    ar.SetObjectSchema( nVersion );
    switch( nVersion )
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
    case 10:
    case 11:
    case 12:
      {
        WORD wTemp;
        double dTemp;
        
        // dieser Quatch ist nötig für die EigenschaftsdialogSeiten: die schauen immer auf die Anzahl von
        // m_Sections um zu wissen wieviele verschiedene Datenquellen es gibt
        DWORD nOldSize = ar.ReadCount();  // was soll dieser ganze Quatsch??? Belger 13.11.2001 !!!
        m_States.SetSize( nOldSize );
        for( int i = 0; i < m_States.GetSize(); i++ )
          m_States[i] = NULL;
        
        nOldSize = ar.ReadCount();
        m_Sections.SetSize( nOldSize );
        for( i = 0; i < m_Sections.GetSize(); i++ )
          m_Sections[i] = NULL;
        
        ar >> m_sizePage;
        
        if( nVersion < 11 )
        {
          CRect margins;
          ar >> margins;

          ((CDrawDoc*)ar.m_pDocument)->GetPrinterSettings()->SetMargins( margins, MM_FACTOR );
        }

        ar >> m_eigenschaften;
        if(m_eigenschaften.IsEmpty()) //Dick 18.01.99
          m_eigenschaften.LoadString(IDS_NONE); 
        if( nVersion >= 5 )
        {
          CString fileName;
          ar >> fileName;
          m_stempel.SetFileName( fileName );
        } // if nVersion >= 5

        CDoublePoint sizeProfile; // Ersatz für m_sizeProfile
        ar >> sizeProfile;
        CDoubleIRect profilRect( 0, sizeProfile.y, sizeProfile.x, 0 );
        m_profil.SetVisibleRect( profilRect );

        CDoubleIRect rectProfile;
        ar >> rectProfile;
        m_profil.SetTotalRect( rectProfile );

        ar >> dTemp;
        m_profil.SetFrom( dTemp );

        ar >> dTemp;
        m_profil.SetTo( dTemp );

        if (nVersion>=3)
        {
          ar >> wTemp; m_profil.SetAutoAnfang( (BOOL)wTemp );
          ar >> wTemp; m_profil.SetAutoEnde( (BOOL)wTemp );
        }
        else
        {
          if( m_profil.GetFrom() >= m_profil.GetTo() )
          {
            m_profil.SetAutoAnfang( TRUE );
            m_profil.SetAutoEnde( TRUE );
          }
          else
          {
            m_profil.SetAutoAnfang( FALSE );
            m_profil.SetAutoEnde( FALSE );
          }
        }
        ar >> m_sizeTableMargins;
        ar >> m_scale;
        ar >> m_realScale;

        CDrawRect* pTitle;
        ar >> pTitle;
        if( pTitle != NULL )
          pTitle->UnsetFlags( CDrawObj::user );
        m_title.SetTitle( pTitle );
        
        if( nVersion > 8 )
          ar >> m_comment;
        if( !m_comment )
        {
          m_comment = new CDrawRect( CIntIRect(), (CDrawDoc*)ar.m_pDocument );
          m_comment->SetShape( CDrawRect::text );
          m_comment->SetTextType( CDrawRect::normal );
          m_comment->SetFlags( CDrawObj::editable );
          m_comment->SetTextHeight( -27 );
          m_comment->SetText( CString( MAKEINTRESOURCE( IDS_DEFAULT_COMMENT ) ) );
        };
        
        m_comment->UnsetFlags( CDrawObj::user );
        
        ar >> m_height;
        if( !m_height )
        {
          m_height = new CDrawRect( CIntIRect(), (CDrawDoc*)ar.m_pDocument );
          m_height->SetShape( CDrawRect::text );
          m_height->SetTextType( CDrawRect::normal );
          m_height->SetFlags( CDrawObj::editable );
          m_height->SetTextHeight( -27 );
          m_height->SetText( CString( MAKEINTRESOURCE( IDS_DEFAULT_HEIGHT ) ) );
        };

        if( m_height )
        {
          m_height->SetFlags(CDrawObj::editable);
          m_height->UnsetFlags(CDrawObj::user);
        }
        if (nVersion==0)
          bFindLowPoint = TRUE;
        else
        {
          ar >> dTemp;
          m_profil.SetLowPoint( dTemp );
        }

        if (nVersion>=2)
        {
          ar >> wTemp; m_profil.SetAutoHeight( (BOOL)wTemp );

          ar >> dTemp;
          m_profil.SetHeight( dTemp );
        }
        else
        {
          m_profil.SetAutoHeight( TRUE );
          m_profil.SetHeight( 0 );
        }

        if( nVersion >= 3 && nVersion < 11 )
        {
          short dummy;
          ar >> dummy; //m_dmOrientation;
          ar >> dummy; //m_dmPaperSize;
          ar >> dummy; //m_dmPaperWidth;
          ar >> dummy; //m_dmPaperLength;
        }

        if( nVersion >= 4 )
        {
          ar >> m_heightFormatText;
          if( nVersion > 8 )
            ar >> m_commentFormatText;
          else
            m_commentFormatText.LoadString( IDS_DEFAULT_COMMENT );

          CString strTmpFormat;
          ar >> strTmpFormat;
          m_title.SetFormatText( strTmpFormat );
        }
        else
        {
          m_heightFormatText.LoadString( IDS_DEFAULT_HEIGHT );
          CString tmpStr;
          m_title.GetTitle()->GetText( tmpStr );
          m_title.SetFormatText( tmpStr );
        }

        // Border Gaps
        if( nVersion > 5 )
          ar >> m_rectBorderGaps;
        else
          m_rectBorderGaps = CIntIRect( 10 * MM_FACTOR, 10 * MM_FACTOR, 10 * MM_FACTOR, 10 * MM_FACTOR );

        if( nVersion > 11 )
        {
          CSize stampMargins;
          ar >> stampMargins;
          m_stempel.SetMargins( stampMargins );
        }
        else if( nVersion > 9 )
        {
          UINT dist;
          ar >> dist;
          m_stempel.SetMargins( CSize( dist, m_rectBorderGaps.right ) );
        }
        else if( nVersion > 5 )
        {
          CPoint tmpPoint;
          ar >> tmpPoint;

          m_stempel.SetMargins( tmpPoint );
        }
        else
          m_stempel.SetMargins( CSize( 10 * MM_FACTOR, 10 * MM_FACTOR ) );

        if( nVersion > 11 )
        {
          BOOL bHorizontal;
          ar >> bHorizontal;
          m_stempel.SetHorizontal( bHorizontal );
        }
        else
          m_stempel.SetHorizontal( TRUE );

        if (nVersion==7)
        {
          ar >> wTemp;
          if( wTemp == 1 )
            m_table.SetTableFormat( CTable::allValues );
          else
            m_table.SetTableFormat( CTable::allLines );
        }
        else if (nVersion>=8)
        {
          ar >> wTemp;
          m_table.SetTableFormat( (CTable::TableFormat)wTemp );
          ar >> wTemp;
          m_profil.SetFormat( (CProfil::Format)wTemp );
        }

        if( nVersion > 9 )
        {
          int temp;
          ar >> temp;
          m_profil.SetAlign( (CPlotArranger::Align)temp );

          BOOL bAlignToProfil;
          ar >> bAlignToProfil;
          m_stempel.SetAlignToProfil( bAlignToProfil );

          ar >> temp;
          m_stempel.SetSerialZoomFaktor( (UINT)temp );

          ar >> temp;
          m_profil.SetRangeDbType( temp );
        }
        else
        {
          m_profil.SetAlign( CPlotArranger::SW );
          m_stempel.SetAlignToProfil( FALSE ); // Default wie früher
        };

        // die Titel/Height/Comment-Objekte alle noch offsetable setzen
        CDrawObj* title = m_title.GetTitle();
        if( title != NULL )
          title->SetFlags( CDrawObj::offsetable );
        if( m_comment != NULL )
          m_comment->SetFlags( CDrawObj::offsetable );
        if( m_height != NULL )
          m_height->SetFlags( CDrawObj::offsetable );
      } // case 0...10
      break;
      
      default:
        AfxThrowArchiveException(CArchiveException::badSchema);
        break;
    } // switch nVersion
  } // if isStoring
  m_user.Serialize(ar);
  m_stempel.Serialize(ar);
  m_profil.Serialize(ar);
  m_table.Serialize(ar);
  m_tableKey1.Serialize(ar);
  m_tableKey2.Serialize(ar);
  m_rahmen.Serialize(ar);

  // hoffentlich haben die dirty tricks ma ein Ende, aber hier noch einer:
  // den LayerTyp aller Objekte setzen
  if( !ar.IsStoring() )
  {
    m_user.SetLayer( CPlotterDoc::user );
    m_stempel.SetLayer( CPlotterDoc::stamp );
    m_profil.SetLayer( CPlotterDoc::profil );
    m_table.SetLayer( CPlotterDoc::table );
    m_tableKey1.SetLayer( CPlotterDoc::tableKey1 );
    m_tableKey2.SetLayer( CPlotterDoc::tableKey2 );
    m_rahmen.SetLayer( CPlotterDoc::border );
    if( m_title.GetTitle() != NULL )
      m_title.GetTitle()->SetLayer( CPlotterDoc::title );
    if( m_comment != NULL )
      m_comment->SetLayer( CPlotterDoc::comment );
    if( m_height != NULL )
      m_height->SetLayer( CPlotterDoc::height );

    m_table.InitIndex();
    m_tableKey1.InitIndex();
    m_tableKey2.InitIndex();
  };

  // falls gewünscht, jetzt den LowPoint neu finden
  if( bFindLowPoint )
  {
    double dLowPoint = 0;
    POSITION pos1 = m_profil.GetHeadPosition();
    while (pos1!=NULL)
    {
      CDrawObj* pPObj = m_profil.GetNextObject(pos1);
      if (pPObj->GetType()==DST_GELAENDEHOEHE)
      {
        if (pPObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)))
        {
          for( int i = 0; i < ((CDrawPoly*)pPObj)->GetNumPoints(); i++ )
          {
            if( i == 0 )
              dLowPoint = ((CDrawPoly*)pPObj)->GetPoint(i).x;
            else
              dLowPoint = min( dLowPoint, ((CDrawPoly*)pPObj)->GetPoint(i).x );
          }
          break;
        }
      }
    }
    m_profil.SetLowPoint( dLowPoint );
  }

  // falls geladen wird, die Daten noch ein bisserl bereinigen
  if( !ar.IsStoring() )
  {
    // Serialisierung:
    // es werden eigentlich ein Haufen Objekte doppelt serialisiert, allerdings erkennt dies das Archiv
    // und erzeugt diese auch nur einmal wieder
    // aus irgendwelchen Gründen stimmen aber die Grosse Liste aus CDrawDoc nicht immer
    // mit der Summe der einzelnen Objekte überein; das also nochmal abgleichen
    
    // zuerst eine neue Liste aller Objekte erzeugen
    CDrawObjList objList;
    objList.AddTailObjects( &m_user );
    objList.AddTailObjects( &m_profil );
    objList.AddTailObjects( &m_table );
    objList.AddTailObjects( &m_tableKey1 );
    objList.AddTailObjects( &m_tableKey2 );
    if( m_title.GetTitle() != NULL )
      objList.AddTailObject( m_title.GetTitle() );
    if( m_height != NULL )
      objList.AddTailObject( m_height );
    if( m_comment != NULL )
      objList.AddTailObject( m_comment );
    objList.AddTailObjects( &m_stempel );
    objList.AddTailObjects( &m_rahmen );
    
    // die Grosse CDrawDoc Liste holen
    CPlotterDoc* pDoc = (CPlotterDoc*)ar.m_pDocument;
    CDrawObjList* pObjects = pDoc->GetObjects();
    
    // pObject und pObjs müssen eigentlich gleich sein
    
    // jetzt einfach die alte Liste durch die neue ersetzen
    pObjects->RemoveAllObjects();
    pObjects->AddTailObjects( &objList );
    
    // now reconstruct connections when loading
    for( int i = 0; i < m_tableKey2.GetSize(); i++ )
    {
      POSITION pos1 = m_tableKey2.GetAt( i )->GetHeadPosition();
      while( pos1 != NULL )
      {
        CDrawObj* pTObj = m_tableKey2.GetAt( i )->GetNextObject( pos1 );
        if( pTObj->IsConnected() )
        {
          pTObj->RemoveConnections( FALSE ); // nicht nur vergessen sondern auch zerstören, da die Objekte doppelt gelesen wurden
          POSITION pos2 = m_profil.GetHeadPosition();
          while( pos2 != NULL )
          {
            CDrawObj* pPObj = m_profil.GetNextObject( pos2 );
            if( pPObj->IsConnected() )
            {
              CDrawObjList* pConnections = pPObj->GetConnections();
              CDrawObjList TempList;
              
              POSITION pos3 = pConnections->GetHeadPosition();
              while( pos3 != NULL )
              {
                CDrawObj* pCon = pConnections->GetNextObject( pos3 );
                if( pCon != NULL && pPObj->m_pThisDrawObj != NULL && pCon == pTObj->m_pThisDrawObj )
                {	// connect legend to profil object
                  pTObj->AddConnection( pPObj );
                  pPObj->AddConnection( pTObj );
                  // store profil objects
                  TempList.AddTailObject( pPObj );
                  pPObj->m_pThisDrawObj = NULL;
                }
              }
              // connect stored profil objects together
              pos3 = TempList.GetHeadPosition();
              while( pos3 != NULL )
              {
                CDrawObj *pObj1 = TempList.GetNextObject( pos3 );
                POSITION pos4 = TempList.GetHeadPosition();
                while( pos4 != NULL )
                {
                  CDrawObj* pObj2 = TempList.GetNextObject( pos4 );
                  if( pObj1 != pObj2 )
                    pObj1->AddConnection( pObj2 );
                }
              }
            }
          }
          break;	// only one connected item in each table row
        }
      }
    }
  }
}; // Serialize


#ifdef _DEBUG
void CPlotterDocData::AssertValid()
{
}
#endif