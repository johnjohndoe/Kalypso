// drawdoc.cpp : implementation of the CDrawDoc class
//

#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "bce\include\wspfeatures.h"
#include "wspprj\include\giostr.h"
#include "commonMfc\include\version.h"

#include "drawvw.h"
#include "drawLayer.h"
#include "draw.h"
#include "summinfo.h"
#include "svritem.h"
#include "summpage.h"
#include "statpage.h"
#include "dxfzeich.h"

#include "drawdoc.h"


#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

CLIPFORMAT NEAR CDrawDoc::m_cfPrivate = NULL;

/////////////////////////////////////////////////////////////////////////////
// CDrawDocData - all serializable data from CDrawDoc

IMPLEMENT_SERIAL(CDrawDocData, CObject, VERSIONABLE_SCHEMA | 6 )

CDrawDocData::CDrawDocData()
{
	m_paperColor = RGB(255, 255, 255);
	m_sizeGrid = CSize(5*MM_FACTOR, 5*MM_FACTOR);
	m_bSnapToGrid = TRUE;
	m_nXValueFormat = topLeft;
	m_nYValueFormat = topRight;
	m_sizeDrawing = CSize( 200, 200 );
  m_pages = CSize( 1, 1 ); // eine Seite!

  m_printerSettings = GETDRAWAPP->GetPrinterSettings();
#if !defined(_MAC)
#if _MFC_VER>=0x0421
	m_pSummInfo = NULL;
#endif
#endif
}

CDrawDocData::~CDrawDocData()
{
#if !defined(_MAC)
#if _MFC_VER>=0x0421
	delete m_pSummInfo;
#endif
#endif
}

void CDrawDocData::DeleteContents()
{
	m_objects.DeleteContents( FALSE );

  POSITION layerPos = m_drawLayers.GetStartPosition();
  while( layerPos )
  {
    CString layerName; 
    CDrawLayer* pLayer = NULL;
    m_drawLayers.GetNextAssoc( layerPos, layerName, pLayer );
    delete pLayer;
  } // while layerPos
  m_drawLayers.RemoveAll();
}

void CDrawDocData::FormatValue(CDrawRect* pObj, BOOL bTwoValuesInRow)
{
	pObj->m_dPosition.top = 1;
	pObj->m_dPosition.bottom = 0;

	if (!bTwoValuesInRow)
		return;

	if ((m_nXValueFormat==topLeft && m_nYValueFormat==bottomLeft) ||
		(m_nXValueFormat==topRight && m_nYValueFormat==bottomRight))
	{
		switch (pObj->GetTextType())
		{
			case CDrawRect::xcoord:
				pObj->m_dPosition.top = 1;
				pObj->m_dPosition.bottom = 0.5;
				break;

			case CDrawRect::ycoord:
				pObj->m_dPosition.top = 0.5;
				pObj->m_dPosition.bottom = 0;
				break;
		}
	}
	else if ((m_nXValueFormat==bottomLeft && m_nYValueFormat==topLeft) ||
		(m_nXValueFormat==bottomRight && m_nYValueFormat==topRight))
	{
		switch (pObj->GetTextType())
		{
			case CDrawRect::xcoord:
				pObj->m_dPosition.top = 0.5;
				pObj->m_dPosition.bottom = 0;
				break;

			case CDrawRect::ycoord:
				pObj->m_dPosition.top = 1;
				pObj->m_dPosition.bottom = 0.5;
				break;
		}
	}
}

void CDrawDocData::Serialize(CArchive& ar)
{
	CObject::Serialize( ar );
	if( ar.IsStoring() )
	{
		ar << m_paperColor;
		ar << m_sizeDrawing;
		ar << m_sizeGrid;
		ar << (WORD)m_bSnapToGrid;
		ar << (WORD)m_nXValueFormat;
		ar << (WORD)m_nYValueFormat;
    ar << m_pages;

    m_printerSettings.Serialize( ar );

    // die DrawLayer speichern, muss vor den Objekten erfolgen
    ar << m_drawLayers.GetCount();
    POSITION layerPos = m_drawLayers.GetStartPosition();
    while( layerPos )
    {
      CString layerName;
      CDrawLayer* pLayer;
      m_drawLayers.GetNextAssoc( layerPos, layerName, pLayer );
      
      pLayer->Serialize( ar );
    } // while layerPos

    // die DrawObjekte
    ar << (WORD)0;  // Versionsnummer, eigentlich veraltet, bleibt aus Kompatibilitätsgründen
    ar << m_objects.GetRect();
    
    ar << m_objects.GetObjectCount();
    POSITION pos = m_objects.GetHeadPosition();
    while( pos )
    {
      CDrawObj* pObj = m_objects.GetNextObject( pos );
      ASSERT( pObj != NULL );
      CString layerName = pObj->GetDrawLayer() == NULL ? CString() : pObj->GetDrawLayer()->GetName();
      
      ar << pObj << layerName;
    }

		if( ar.m_pDocument )
			m_pSummInfo->WriteToStorage( ((CDrawDoc*)ar.m_pDocument)->m_lpRootStg );
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
      {
        WORD wTemp;
        
        ar >> m_paperColor;
        ar >> m_sizeDrawing;
        ar >> m_sizeGrid;
        ar >> wTemp; m_bSnapToGrid = (BOOL)wTemp;

        if( nVersion >= 3 )
        {
          ar >> wTemp;
          m_nXValueFormat = (ValueFormat)wTemp;
          ar >> wTemp;
          m_nYValueFormat = (ValueFormat)wTemp;
        }

        if( nVersion > 3 )
        {
          ar >> m_pages;
          m_printerSettings.Serialize( ar );
        }

        if( nVersion > 5 )
        {
          // die DrawLayers
          int layerCount;
          ar >> layerCount;
          
          for( int layerN = 0; layerN < layerCount; layerN++ )
          {
            CDrawLayer* pLayer = new CDrawLayer();
            pLayer->Serialize( ar );
            
            m_drawLayers[pLayer->GetName()] = pLayer;
          }
        } // if nVersion > 5

        if( nVersion > 4 )
        {
          WORD oldVersion; // veraltet: Version der Serialisierung
          ar >> oldVersion;
          
          CIntIRect objectsRect;
          ar >> objectsRect;
          m_objects.SetRect( objectsRect );
          
          int objCount = 0;
          ar >> objCount;
          while( objCount-- > 0 )
          {
            CDrawObj* pObj = NULL;
            ar >> pObj;
            m_objects.AddTailObject( pObj );

            if( nVersion > 5 )
            {
              CString layerName;
              ar >> layerName;

              // den Laer setzen
              CDrawLayer* pLayer = NULL;
              m_drawLayers.Lookup( layerName, pLayer );
              pObj->SetDrawLayer( pLayer );
            }
          }; // for i
        }
        else
          m_objects.Serialize( ar );
        
        delete m_pSummInfo;	
        m_pSummInfo = new CSummInfo;
        m_pSummInfo->StartEditTimeCount();
        if( nVersion >= 2 )
          m_pSummInfo->ReadFromStorage( ((CDrawDoc*)ar.m_pDocument)->m_lpRootStg );
      }
      break;
      
    default:
      AfxThrowArchiveException( CArchiveException::badSchema );
      break;
    }
  }
}

#ifdef _DEBUG
void CDrawDocData::AssertValid()
{
}
#endif

/////////////////////////////////////////////////////////////////////////////
// CDrawDoc

IMPLEMENT_DYNCREATE( CDrawDoc, COleServerDoc )

BEGIN_MESSAGE_MAP(CDrawDoc, COleServerDoc)
//{{AFX_MSG_MAP(CDrawDoc)
ON_COMMAND(ID_EDIT_COPY, OnEditCopy)
//}}AFX_MSG_MAP


  // Enable default OLE container implementation
#if _MFC_VER>=0x0421
	ON_COMMAND(ID_FILE_SUMMARYINFO, OnFileSummaryinfo)
#endif
  ON_UPDATE_COMMAND_UI(ID_EDIT_PASTE, COleServerDoc::OnUpdatePasteMenu)
  ON_UPDATE_COMMAND_UI(ID_EDIT_PASTE_LINK, COleServerDoc::OnUpdatePasteLinkMenu)
	ON_UPDATE_COMMAND_UI(ID_OLE_EDIT_CONVERT, COleServerDoc::OnUpdateObjectVerbMenu)
	ON_COMMAND(ID_OLE_EDIT_CONVERT, COleServerDoc::OnEditConvert)
	ON_UPDATE_COMMAND_UI(ID_OLE_EDIT_LINKS, COleServerDoc::OnUpdateEditLinksMenu)
	ON_COMMAND(ID_OLE_EDIT_LINKS, COleServerDoc::OnEditLinks)
	ON_UPDATE_COMMAND_UI(ID_OLE_VERB_FIRST, COleServerDoc::OnUpdateObjectVerbMenu)
#if !defined(_MAC)
  // MAPI support
	ON_COMMAND(ID_FILE_SEND_MAIL, OnFileSendMail)
	ON_UPDATE_COMMAND_UI(ID_FILE_SEND_MAIL, OnUpdateFileSendMail)
#endif
  ON_COMMAND(ID_FILE_DXF_EXPORT, OnFileDxfExport)
  ON_UPDATE_COMMAND_UI(ID_FILE_DXF_EXPORT, OnUpdateFileDxfExport)

  ON_COMMAND(ID_FILE_PAGE_SETUP, OnFilePageSetup)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Static Members
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// CDrawDoc construction/destruction

CDrawDoc::CDrawDoc()
{
	EnableCompoundFile();

	m_pData = new CDrawDocData;
	m_nMapMode = MM_ANISOTROPIC;

	if( m_cfPrivate == NULL )
	{
		m_cfPrivate = (CLIPFORMAT)
#ifdef _MAC
		::RegisterClipboardFormat(_T("WSPPL"));
#else
		::RegisterClipboardFormat(_T("WSPWIN Plot"));
#endif //_MAC
	}
	AfxOleLockApp();
}

CDrawDoc::~CDrawDoc()
{
	if (m_pData!=NULL)
	{
		m_pData->DeleteContents();
		delete m_pData;
	}
	AfxOleUnlockApp();
}

void CDrawDoc::DeleteContents()
{
	COleServerDoc::DeleteContents();
	if (m_pData!=NULL)
		m_pData->DeleteContents();
	POSITION pos;
	pos = GetFirstViewPosition();
	while (pos != NULL)
		((CDrawView*)GetNextView(pos))->DeleteContents();
}

BOOL CDrawDoc::OnNewDocument()
{
	if (!COleServerDoc::OnNewDocument())
		return FALSE;

	// reinitialization code 
	// (SDI documents will reuse this document)
#if !defined(_MAC)
#if _MFC_VER>=0x0421
	CString str;

	if(m_pData->m_pSummInfo != NULL)
		delete m_pData->m_pSummInfo;
	m_pData->m_pSummInfo = new CSummInfo;
	// Title, Subject, Author, Keywords default to empty string
	// Comments, Template, SavedBy default to empty string
	// LastSave, LastPrint, EditTime, RevNum default to 0
	m_pData->m_pSummInfo->StartEditTimeCount();
	m_pData->m_pSummInfo->RecordCreateDate();
	m_pData->m_pSummInfo->SetNumPages(1);
	// NumWords, NumChars default to 0
	m_pData->m_pSummInfo->SetAppname( _T("WSPWIN Plotter") );
	// Security defaults to 0 
	str.LoadString(IDS_NONE);
	m_pData->m_pSummInfo->SetTemplate(str);
#endif
#endif
	return TRUE;
}

BOOL CDrawDoc::OnOpenDocument(LPCTSTR lpszPathName)
{
	return COleServerDoc::OnOpenDocument(lpszPathName);
}

BOOL CDrawDoc::OnSaveDocument(LPCTSTR lpszPathName)
{
#if !defined(_MAC)
#if _MFC_VER>=0x0421
	m_pData->m_pSummInfo->RecordSaveDate();
	m_pData->m_pSummInfo->IncrRevNum();
	m_pData->m_pSummInfo->SetLastAuthor(m_pData->m_pSummInfo->GetAuthor());
	m_pData->m_pSummInfo->AddCountToEditTime();
	m_pData->m_pSummInfo->StartEditTimeCount();
#endif
#endif
	return COleServerDoc::OnSaveDocument(lpszPathName);
}

/////////////////////////////////////////////////////////////////////////////
// CDrawDoc serialization

void CDrawDoc::Serialize(CArchive& ar)
{
	// first empty all undo buffers (otherwise OLE objects
	// from the undo buffer will be saved by COleServerDoc!)
	POSITION pos = GetFirstViewPosition();
	while(pos!=NULL)
	{
		CDrawView *pView = (CDrawView*)GetNextView(pos);
		if (!pView->IsKindOf(RUNTIME_CLASS(CPreviewView)))
			pView->EmptyUndoBuffer();
	}

	if( ar.IsStoring() )
		ar << m_pData;
	else
	{
		m_pData->DeleteContents();
		delete m_pData;
		m_pData = NULL;		// prepare for exception
		ar >> m_pData;
	}

	// By calling the base class COleServerDoc, we enable serialization
	//  of the container document's COleClientItem objects automatically.
	COleServerDoc::Serialize( ar );
}


/////////////////////////////////////////////////////////////////////////////
// CDrawDoc implementation

COleServerItem* CDrawDoc::OnGetEmbeddedItem()
{
	CEmbeddedItem* pItem = new CEmbeddedItem( this );
	ASSERT_VALID( pItem );
	return pItem;
}

/////////////////////////////////////////////////////////////////////////////
// CTestDoc Implementierung des ActiveX-Dokument-Servers

void CDrawDoc::Draw( CDC* pDC, CDrawView* pView )
{
  // Unterscheide zwischen Layerweisem Zeichnen und Zeichnen aller Objekte
  // dies sollte vorläufig sein und später auf immer layerweise wechseln
  POSITION layerPos = m_pData->m_drawLayers.GetStartPosition();
  while( true )
  {
    bool bBreak = false; // jetzt abbrechen?
    CDrawLayer* pLayer = NULL;
    
    // solange Layer da sind, diese zeichnen, ansonsten wird der Default-Layer (NULL) gezeichnet
    if( layerPos )
    {
      CString layerName;
      m_pData->m_drawLayers.GetNextAssoc( layerPos, layerName, pLayer );
    }
    else
      bBreak = true; // wird sind nach dem letzten Layer, also fertig
    
    // nur 'echte' Layer zeichnen, die auch sichtbar sind
    // der Standardlayer, NULL, wird immer gezeichnet
    if( !pLayer || pLayer->IsVisible() )
    {
      // um die z-Order zu erhalten jetzt wieder alle Objekte durchgehen,
      // sollte durch die z-Order im Layer ersetzt werden
      POSITION pos = m_pData->m_objects.GetHeadPosition();
      while( pos )
      {
        CDrawObj* pObj = m_pData->m_objects.GetNextObject( pos );
        if( pObj->GetDrawLayer() == pLayer )
        {
          pObj->Draw( pDC );
          if( pView->m_bActive && !pDC->IsPrinting() && pView->IsSelected( pObj ) )
            pObj->DrawTracker( pDC, CDrawObj::selected );
        }
      } // while pos
    }

    if( bBreak )
      break;
  }; // while layerPos
}

CDrawView* CDrawDoc::GetView() const
{
	POSITION pos = GetFirstViewPosition();
	if (pos == NULL)
		return NULL;

	CDrawView* pView = (CDrawView*)GetNextView(pos);
	ASSERT_VALID(pView);
	ASSERT_KINDOF(CDrawView, pView);
	return pView;
}

/* virtual */ 
void CDrawDoc::AddObject( CDrawObj* pObj, const int type, const int index )
// Fügt Objekt zur Zeichnung hinzu
// Parameter:
//        CDrawObj* pObj: dieses Objekt wird hinzugefügt, falls es nicht schon vorhanden ist
//        int type: wird in Überschreibung CPlotterDoc benutzt
{
  // das gleiche Objekt sollte nicht zweimal hinzugefügt werden
  ASSERT( m_pData->m_objects.FindObject( pObj ) == NULL );

  m_pData->m_objects.AddTailObject( pObj );

  pObj->m_pDocument = this;
  pObj->SetLayer( type );
  pObj->SetIndex( index );
  SetModifiedFlag();
} // AddObject

/* virtual */ 
void CDrawDoc::AddObjects( CDrawObjList* pObjList, const int type, const int index )
// fügt alle Objekte aus einer Liste zur Zeichnung hinzu, sonst wie AddObject
{
  POSITION pos = pObjList->GetHeadPosition();
  while( pos != NULL )
  {
    CDrawObj* pObj = pObjList->GetNextObject( pos );
    AddObject( pObj, type, index );
  }
} // AddObjects

int CDrawDoc::RemoveObject( CDrawObj* pObj, int* index /* = NULL */ )
// Löscht ein Objekt aus der Zeichnung
// Parameter:
//        CDrawObj* pObj: dieses Objekt wird gelöscht, falls es in der Zeichnung ist
//        int* index: wird nur in CPlotterDoc benutzt
// Rückgabewert:
//        int: der 'Typ' des Objekts; hier: -1, falls das Objekt nicht gefunden wurde, sonst 0
{
	// Find and remove from document
  int type = -1;

	POSITION pos = m_pData->m_objects.FindObject( pObj );
	if( pos != NULL )
  {
    type = 0;
		m_pData->m_objects.RemoveObjectAt( pos );
  }
	// set document modified flag
	SetModifiedFlag();

	// call remove for each view so that the view can remove from m_selection
	pos = GetFirstViewPosition();
	while (pos != NULL)
		((CDrawView*)GetNextView(pos))->Remove( pObj );

  return type; 
} // Removeobject

BOOL CDrawDoc::ReplaceObject( CDrawObj* pOrgObj, CDrawObj *pNewObj )
// ersetzt en Objekt durch ein anderes
// Rückgabewert:
//        TRUE, falls das alte Objekt gefunden wurde, sonst FALSE
{
  // das Objekt aus der Zeichnung nehmen
  int index;
	int type = RemoveObject( pOrgObj, &index );
	
  // das Objekt auf jeden Fall zerstören
  pOrgObj->Remove();

  // das neue Objekt hinzufügen
	AddObject( pNewObj, type, index );

	return type != -1;
} // RemoveObjekt

CDrawLayer* CDrawDoc::GetDrawLayer( const CString& name ) const
{
  POSITION layerPos = m_pData->m_drawLayers.GetStartPosition();
  while( layerPos )
  {
    CString layerName;
    CDrawLayer* pLayer = NULL;
    m_pData->m_drawLayers.GetNextAssoc( layerPos, layerName, pLayer );

    if( pLayer->GetName().Compare( name ) == 0 )
      return pLayer;
  }

  return NULL;
}

CDrawLayer* CDrawDoc::CreateDrawLayer( const CString& name )
{
  CDrawLayer* pLayer = GetDrawLayer( name );

  if( !pLayer )
  {
    // neuen Layer erzeugen
    pLayer = new CDrawLayer( name );
    m_pData->m_drawLayers[name] = pLayer;
  }

  return pLayer;
}

/*!
 * Das Objekt wird an das Ende der Zeichenliste gestellt, so
 * dass es als letztes gezeichnet wird. Dadurch ist es in der Zeichnung
 *  'ganz oben' also immer sichtbar.
 *
 * @param pObj : das Objekt, welches ganz nach oben in der Zeichnung kommt
 */
/* virtual */ void CDrawDoc::MoveObjectToFront( CDrawObj* pObj )
{
  CDrawObjList* pObjects = GetObjects();
  POSITION pos = pObjects->FindObject( pObj );
  if( pos != NULL )
  {
    pObjects->RemoveObjectAt( pos );
    pObjects->AddTailObject( pObj );
  };
}; // MoveObjectToFront

void CDrawDoc::InvalAll()
{
	CWaitCursor wait;

  CDrawApp* drawApp = GETDRAWAPP;
	CString str( MAKEINTRESOURCE( IDS_RENEWING_OBJECTS ) );
	BOOL bInc = drawApp->CreateStatusBarProgress( str, 0, m_pData->m_objects.GetObjectCount() );
	
  POSITION pos = m_pData->m_objects.GetHeadPosition();
	while( pos!=NULL )
	{
		CDrawObj* pDrawObj = m_pData->m_objects.GetNextObject( pos );
		pDrawObj->Invalidate( TRUE );
		if (pDrawObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)))
			((CDrawPoly*)pDrawObj)->RecalcBounds();
		drawApp->IncStatusBarProgress();
	}
  if (bInc)
    drawApp->DestroyStatusBarProgress();
} // InvallAll

// point is in logical coordinates
CDrawObj* CDrawDoc::ObjectAt( const CPoint& point )
{
	CIntIRect rect( point.x, point.y, point.x + 1, point.y + 1 );
  rect.NormalizeRect();

	POSITION pos = m_pData->m_objects.GetTailPosition();
	while( pos != NULL )
	{
		CDrawObj* pObj = m_pData->m_objects.GetPrevObject( pos );
		if( !pObj->IsInvisible() && pObj->Intersects( rect ) )
			return pObj;
	}

	return NULL;
} // ObjectAt

double CDrawDoc::GetLogicalXScale( const CDrawObj* /*pObj*/) const
{
	return 1;
}

double CDrawDoc::GetLogicalYScale( const CDrawObj* /*pObj*/) const
{
	return 1;
}

void CDrawDoc::MetersToLogical( const CDoubleIRect& input, CIntIRect& output, const CDrawObj* pObj ) const
{
	output.left = (int)(input.left / GetLogicalXScale( pObj ) );
	output.top = (int)(input.top / GetLogicalYScale( pObj ) );
	output.right = (int)(input.right / GetLogicalXScale( pObj ) );
	output.bottom = (int)(input.bottom / GetLogicalYScale( pObj ) );
}

void CDrawDoc::LogicalToMeters( const CIntIRect& input, CDoubleIRect& output, const CDrawObj* pObj ) const
{
	output.left = input.left * GetLogicalXScale(pObj);
	output.top = input.top * GetLogicalYScale(pObj);
	output.right = input.right * GetLogicalXScale(pObj);
	output.bottom = input.bottom * GetLogicalYScale(pObj);
}

void CDrawDoc::MetersToLogical( const CDoublePoint& input, CIntPoint& output, const CDrawObj* pObj ) const
{
	output.x = (int)(input.x / GetLogicalXScale(pObj));
	output.y = (int)(input.y / GetLogicalYScale(pObj));
}

void CDrawDoc::LogicalToMeters( const CIntPoint& input, CDoublePoint& output, const CDrawObj* pObj ) const
{
	output.x = input.x * GetLogicalXScale(pObj);
	output.y = input.y * GetLogicalYScale(pObj);
}

void CDrawDoc::GetDefaultStempelText(int n, CString& str)
{
	int nID;

	if (n==STPL_TEXT_NONE)
	{
		str.Empty();
		return;
	}
	switch(n)
	{
		default:
			ASSERT(FALSE);
			break;

		case STPL_TEXT_AG1:
			nID = IDS_STPL_AG1;
			break;

		case STPL_TEXT_AG2:
			nID = IDS_STPL_AG2;
			break;

		case STPL_TEXT_PB1:
			nID = IDS_STPL_PB1;
			break;

		case STPL_TEXT_PB2:
			nID = IDS_STPL_PB2;
			break;

		case STPL_TEXT_PB3:
			nID = IDS_STPL_PB3;
			break;

		case STPL_TEXT_BB1:
			nID = IDS_STPL_BB1;
			break;

		case STPL_TEXT_BB2:
			nID = IDS_STPL_BB2;
			break;

		case STPL_TEXT_BB3:
			nID = IDS_STPL_BB3;
			break;

		case STPL_TEXT_PN:
			nID = IDS_STPL_PN;
			break;

		case STPL_TEXT_DAT:
			nID = IDS_STPL_DAT;
			break;

		case STPL_TEXT_BN:
			nID = IDS_STPL_BN;
			break;

		case STPL_TEXT_MS:
			nID = IDS_STPL_MS;
			break;

		case STPL_TEXT_UD:
			nID = IDS_STPL_UD;
			break;

		case STPL_TEXT_ZU:
			nID = IDS_STPL_ZU;
			break;
	}
	str.LoadString(nID);
}

/////////////////////////////////////////////////////////////////////////////
// CDrawDoc diagnostics

#ifdef _DEBUG
void CDrawDoc::AssertValid() const
{
	COleServerDoc::AssertValid();
}

void CDrawDoc::Dump(CDumpContext& dc) const
{
	COleServerDoc::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CDrawDoc commands


void CDrawDoc::OnSetItemRects(LPCRECT lpPosRect, LPCRECT lpClipRect) 
{
	// call base class to change the size of the window
	COleServerDoc::OnSetItemRects(lpPosRect, lpClipRect);

	// notify first view that scroll info should change
	POSITION pos = GetFirstViewPosition();
	CDrawView* pView = (CDrawView*)GetNextView(pos);
	pView->ResyncScrollSizes();
}

void CDrawDoc::OnEditCopy() 
{
	CEmbeddedItem* pItem = GetEmbeddedItem();
	pItem->CopyToClipboard(TRUE);
}

/* virtual */ BOOL CDrawDoc::HasType( const CDrawObj* pObj, const int type, int* index /* = NULL */ ) const
{
  return type == 0;
};

/* virtual */ BOOL CDrawDoc::HasType( const CDrawObjList* pObj, const int type, int* index /* = NULL */ ) const
{
  return type == 0;
};


#if _MFC_VER>=0x0421
void CDrawDoc::OnFileSummaryinfo() 
{
#if !defined(_MAC)
	ASSERT_VALID(this);

	CString str;
	str.FormatMessage(IDS_DOC_PROPS, GetTitle());
	
	CPropertySheet sheet(str);
  CSummPage summ;
  CStatPage stat;
	sheet.AddPage( &summ );
	sheet.AddPage( &stat );
	sheet.m_psh.dwFlags |= PSH_NOAPPLYNOW;

	summ.m_strAppname = m_pData->m_pSummInfo->GetAppname();
	summ.m_strTitle   = m_pData->m_pSummInfo->GetTitle();
	summ.m_strSubj    = m_pData->m_pSummInfo->GetSubject();
	summ.m_strAuthor  = m_pData->m_pSummInfo->GetAuthor();
	summ.m_strKeywd   = m_pData->m_pSummInfo->GetKeywords();
	summ.m_strCmt     = m_pData->m_pSummInfo->GetComments();
	summ.m_strTempl   = m_pData->m_pSummInfo->GetTemplate();

	stat.m_strSavedBy    = m_pData->m_pSummInfo->GetLastAuthor();
	stat.m_strRevNum     = m_pData->m_pSummInfo->GetRevNum();
	stat.m_strEditTime   = m_pData->m_pSummInfo->GetEditTime();
	stat.m_strLastPrint  = m_pData->m_pSummInfo->GetLastPrintDate();
	stat.m_strCreateDate = m_pData->m_pSummInfo->GetCreateDate();
	stat.m_strLastSave   = m_pData->m_pSummInfo->GetLastSaveDate();
	stat.m_strNumPages   = m_pData->m_pSummInfo->GetNumPages();

	if (sheet.DoModal() != IDOK)
		return;

	m_pData->m_pSummInfo->SetAuthor(summ.m_strAuthor);
	m_pData->m_pSummInfo->SetKeywords(summ.m_strKeywd);
	m_pData->m_pSummInfo->SetSubject(summ.m_strSubj);
	m_pData->m_pSummInfo->SetComments(summ.m_strCmt);
	m_pData->m_pSummInfo->SetTemplate(summ.m_strTempl);
	m_pData->m_pSummInfo->SetTitle(summ.m_strTitle);
	
	SetModifiedFlag();		
#endif
}
#endif

BOOL CDrawDoc::OnFilePageSetup()
{
  if( GetPrinterSettings()->PageSetup() )
  {
    UpdateDrawing();
    return TRUE;
  }
  return FALSE;
}

void CDrawDoc::SetDrawingSize( const CSize& drawSize )
{
  CSize newDrawSize = AdaptDrawingSize( drawSize );

  // hat sich die Zeichnungsgrösse geändert?
  if( m_pData->m_sizeDrawing != newDrawSize /* || GetPages() != newPages */ )
  {
    m_pData->m_sizeDrawing = newDrawSize;
    UpdateAllViews( NULL, HINT_UPDATE_WINDOW, NULL );
  };
}; // SetDrawingSize

/** 
 * Default behaviour is to change nothing.
 *
 *	@return just returns the original size
 */
CSize CDrawDoc::AdaptDrawingSize( const CSize& size )
{
	return CSize( size );
}


void CDrawDoc::SetPages( const CSize& pages ) 
{
  m_pData->m_pages = pages; 

#if _MFC_VER>=0x0421
  m_pData->m_pSummInfo->SetNumPages( GetPageCount() );
#endif
};

CSize CDrawDoc::GetPageSize() const
{
  return GetPrinterSettings()->GetRealPaperSize( MM_FACTOR, true );
};

void CDrawDoc::OnUpdateFileDxfExport(CCmdUI* pCmdUI) 
{
//kim  pCmdUI->Enable( BCE::RegKey::BitKey::Instance()->isOn( BCE::RegKey::BitKey::plot_noDemo ) );
		pCmdUI->Enable(true);
}

void CDrawDoc::OnFileDxfExport()
{

  if (!WSPFeatures::Instance()->isEnabled("PLOTTER","plot_nodemo"))
	{
		std::string TheText = std::string(WSPFeatures::Instance()->GetDataStr("HEAD", "DEMO_INFO"));
		TheText.append("\n\n");
		TheText.append(WSPFeatures::Instance()->GetDataStr("PLOTTER", "plot_nodemo"));
		AfxMessageBox(TheText.c_str() ,MB_ICONINFORMATION,0);
		return;
	}

  CString str;
  str.LoadString( IDS_DXFFILES_ALLFILES );

  CFileDialog dlg( FALSE, "dxf", NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT, str, NULL );
  if( dlg.DoModal() == IDOK )
  {
    CWaitCursor wait;

    CSize sizeDrawing = GetDrawingSize();
    CString str;
    
    CDXFZeichnung zn( this );
    zn.SetSize( sizeDrawing.cx, sizeDrawing.cy, 0, 0 );
    
    str.LoadString( IDS_STAMP );
    CDrawObjList* pObjs = GetObjects();
    POSITION pos = pObjs->GetHeadPosition();
    while( pos )
    {
      CDrawObj* pObj = pObjs->GetNextObject( pos );
      if( !pObj->IsInvisible() )
        pObj->AddToDXF( &zn, "alles" );
    }
    
    // Saves a file with DXF format.
    gofstream ofs;
    ofs.open( dlg.GetPathName(), ios::out );
    if( ofs.fail() )
    {
      //			AfxFormatString1(rString, IDS_ERROR_FILEWRITE, file);
      //			AfxMessageBox(rString, MB_ERROR);
    }
    else
    {
      ofs << zn;
      ofs.close();
    }
  }
} // OnFileDxfExport


void CDrawDoc::GetDrawLayers( CDrawLayerArray* pLayers, const CString& namePrefix )
{
  POSITION layerPos = m_pData->m_drawLayers.GetStartPosition();
  while( layerPos )
  {
    CString layerName;
    CDrawLayer* pLayer = NULL;
    m_pData->m_drawLayers.GetNextAssoc( layerPos, layerName, pLayer );

    if( namePrefix.Compare( pLayer->GetName().Left( namePrefix.GetLength() ) ) == 0 )
      pLayers->Add( pLayer );
  }
};
