/*! Time-stamp: <@(#)multidoc.cpp   02.12.02 - 18:07:28   Belger>
 *********************************************************************
 *  @file   : multidoc.cpp
 *
 *  Author  : Belger                              Date: 02.12.02
 *
 *  Purpose : Implementation of methods for class CMultiDoc.
 *
 *********************************************************************
 */

#include "stdafx.h"

#include "commonMfc\include\boxPager.h"
#include "commonMfc\include\helper.h"
#include "log4cpp\category.hh"

#include "wspprj\include\section.h"

#include "profil.h"

#include "drawlayer.h"
#include "title.h"
#include "plotdoc.h"
#include "plotfrm.h"
#include "plotview.h"
#include "plotter.h"
#include "stempel.h"
#include "cntritem.h"

#include "multiprops.h"
#include "multidoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/*static */
#ifdef _DEBUG
log4cpp::Category& CMultiDoc::m_logCat = log4cpp::Category::getInstance( "CMultiDoc" );
#endif _DEBUG

/////////////////////////////////
// Konstruktion // Destruktion //
/////////////////////////////////

IMPLEMENT_SERIAL( CMultiDoc, CDrawDoc, VERSIONABLE_SCHEMA | 0 )

CString CMultiDoc::DEFAULT_TITEL = "Überschrift";

/*!
 * Der Standardkonstruktor.
 *
 */
CMultiDoc::CMultiDoc() : CDrawDoc()
{
  m_gaps = CSize( 100, 100 ); // Standard: 1cm
  m_pData->m_bSnapToGrid = FALSE;
}


BEGIN_MESSAGE_MAP( CMultiDoc, CDrawDoc )
 	{ WM_COMMAND, CN_COMMAND, (WORD)ID_MULTI_PROPS, (WORD)ID_MULTI_PROPS, AfxSig_bv, (AFX_PMSG)&OnProperties },
END_MESSAGE_MAP()

BOOL CMultiDoc::OnProperties()
{
  CMultiProps dlg( this );
  return dlg.DoModal();
};

/*!
 * Legt fest, welche Profile angezeigt werden sollen.
 *
 * @param sections : Die Liste der anzuzeigenden Sections
 */
void CMultiDoc::SetSections( const SectionArray& sections )
{
  // für jede Section ein CPlotterDoc erzeugen
  bool bStempel = false; // ob der Stempel schon hinzugefügt wurde
  bool bRahmen = false; // dito für den Rahmen
  bool bTitel = false; // dito für den Titel

  for( int i = 0; i < sections.GetSize(); i++ )
  {
    Section* section = sections[i];

    CPlotterDoc* pDoc = (CPlotterDoc*)RUNTIME_CLASS( CPlotterDoc )->CreateObject();
    if( !pDoc )
      return;

    CMultiDocTemplate* pPlotterDocTemplate = GETPLOTTERAPP->GetPlotterDocTemplate();
    pPlotterDocTemplate->AddDocument( pDoc );
    
    // m_bEmbedded: nur so wie die Flags geschalten sind klappts
    // falls am Anfang m_bEmbedded FALSE ist klappt CreateNewFrame nicht
    // falls es vor OnNewDocument TRUE klappt später
    // das Clonen von OleObjekten nicht
    pDoc->m_bEmbedded = TRUE;
    pDoc->m_bAutoDelete = FALSE;

    CFrameWnd* pFrame = pPlotterDocTemplate->CreateNewFrame( pDoc, NULL );

    pDoc->m_bEmbedded = FALSE;
    pDoc->OnNewDocument();
    pDoc->m_bEmbedded = TRUE;

    // den Frame aktivieren
    pFrame->InitialUpdateFrame( pDoc, FALSE );
    CView* pView = pFrame->GetActiveView();
    if( pView )
      pView->OnInitialUpdate();

    // das Profil ins Dokument laden und formatieren
    pDoc->InsertData( NULL, section, FALSE, FALSE );
    pDoc->UpdateDrawing();

    // jetzt dem DocTemplate das Document wieder entziehen, sonst wird
    // später versucht es automatisch zu speichern etc.
    pPlotterDocTemplate->RemoveDocument( pDoc );

    // dirty Trick:
    // da wir die Connections gar nicht brauchen und sie bei der Serialisierung eh nur Probleme
    // machen, löschen wir einfach alle (dadurch wirds dann auch noch schneller)
    pDoc->GetObjects()->RemoveConnections( FALSE );
    
    // den Frame wieder zerstören
    delete pFrame;
    pFrame = NULL;

    // jetzt die Objekte in dieses Dokument übernehmen
    CDrawObjList objList;
    pDoc->GetProfilAndTableObjects( &objList );

    CString layerName;
    layerName.Format( "Diagram_%d", i );
    CDrawLayer* pLayer = CreateDrawLayer( layerName );
    CloneObjectsToLayer( objList, pLayer );

    // für jedes Profil auch noch den Titel dranbasteln
    CDrawObj* pProfilTitle = pDoc->GetTitle()->GetTitle();
    if( pProfilTitle )
    {
      CDrawObj* pTitle = pProfilTitle->Clone( this, TRUE );

      // den Titel zentral über das Profil setzen
      CIntIRect profilExtent( pLayer->CalcRect() );
      CIntIRect titleRect( pTitle->m_position );
      
      int titleWidth = pTitle->m_position.Width();
      int profilWidth = profilExtent.Width();
      int titleX = profilExtent.left + profilWidth / 2 - titleWidth / 2; // damits über dem Profil zentriert ist
      pTitle->m_position = CIntIRect( titleX, profilExtent.top + 2 * titleRect.Height(), titleX + titleWidth, profilExtent.top + titleRect.Height() );

      AddObject( pTitle, 0, 0 );
      pTitle->SetDrawLayer( pLayer );
    }
    
    // den Stempel hinzufügen, falls nicht schon geschehen
    if( !bStempel )
    {
      CDrawObjList* pList = pDoc->GetStempel();

      if( pList != NULL && pList->GetObjectCount() > 0 )
      {
        CDrawLayer* pSplLayer = CreateDrawLayer( "Stempel" );
        pSplLayer->SetFlags( CDrawLayer::visible_flag );
        CloneObjectsToLayer( *pList, pSplLayer );
        bStempel = true;
      }
    }; // if plotDocs

    // einen Rahmen, falls noch nicht vorhanden
    if( !bRahmen )
    {
      CDrawObjList* pRahmenList = pDoc->GetRahmen();
      CDrawLayer* pRahmenLayer = CreateDrawLayer( "Rahmen" );
      pRahmenLayer->SetFlags( CDrawLayer::visible_flag );
    
      POSITION pos = pRahmenList->GetHeadPosition();
      while( pos )
      {
        CDrawObj* pObj = pRahmenList->GetNextObject( pos );
        CDrawObj* pRahmen = pObj->Clone( this, TRUE );
      
        AddObject( pRahmen, 0, 0 );
        pRahmen->SetDrawLayer( pRahmenLayer );
      }; // while pos

      bRahmen = true;
    } // if !bRahmen

    // den Dokumenttitel, falls noch nicht vorhanden
    if( !bTitel )
    {
      CDrawRect* pTitelObj = pDoc->GetTitle()->GetTitle();
      if( pTitelObj )
      {
        CDrawRect* pTitel = (CDrawRect*)pTitelObj->Clone( this, TRUE ); // ein Kopie eines beliebigen Titels anfertigen
        pTitel->SetText( DEFAULT_TITEL );
        
        AddObject( pTitel, 0, 0 );

        CDrawLayer* pTitelLayer = CreateDrawLayer( "Titel" );
        pTitelLayer->SetFlags( CDrawLayer::visible_flag );;

        pTitel->SetDrawLayer( pTitelLayer );

        bTitel = true;
      } // if pTitelObj
    }; // if !bTitel

    // auch das PlotDoc wird nicht mehr benötigt
    delete pDoc; 
  }; // for i

  UpdateDrawing();
}; // SetSections

void CMultiDoc::CloneObjectsToLayer( const CDrawObjList& objList, CDrawLayer* pLayer )
{
  // alle Objekte durchgehen, kopieren und dann zum angegebenen Layer hinzufügen
  POSITION pos = objList.GetHeadPosition();
  while( pos )
  {
    CDrawObj* pObj = objList.GetNextObject( pos );
    
    CDrawObj* pCloneObj = pObj->Clone( this, TRUE );
    pCloneObj->UnsetFlags( CDrawObj::offsetable ); // sonst werden bestimmte objekte nicht verschoben
    
    AddObject( pCloneObj, 0, 0 );
    pCloneObj->SetDrawLayer( pLayer );
  }; // while po
}

/*!
 * Erneuert die komplette Zeichnung. Kopiert die Objekte aus den CPlotterDoc's und
 * Ordnet diese via einem CBoxPager auf den Seiten an.
 *
 */
void CMultiDoc::UpdateDrawing()
{
  // Warte Cursor
  CWaitCursor wait;

  // die Seitengrösse könnte sich geändert haben
  CSize pageSize = GetPageSize();

  // jetzt über alle Profile iterieren
  // - per CBoxPager auf den Seiten anordnen
  // - alle anzuzeigenden Objekte rausholen
  //CArray<CDrawObjList*, CDrawObjList*> objListArray;
  
  
  CDrawObj* pTitelObj = GetTitleObject(); // wird erst gesucht und später positioniert
  int titleHeight = pTitelObj ? pTitelObj->m_position.Height() : 0; // wird für die Positionierung des Gesamtitels benötigt

  // die Profile auf den Seiten anordnen
  CSize boxPageSize( pageSize.cx, pageSize.cy - titleHeight * 3 ); // Platz für den Seitentitel lassen
  CBoxPager boxPager( CBoxPager::horizontal, boxPageSize, m_gaps );

  CDrawLayerArray drawLayers;
  GetDrawLayers( &drawLayers, "Diagram_" );
  for( int layerI = 0; layerI < drawLayers.GetSize(); layerI++ )
  {
    CIntIRect profilExtent( drawLayers[layerI]->CalcRect() );

    boxPager.AddBox( CSize( profilExtent.Width(), profilExtent.Height() ) );
  }; // for secID

  // jetzt die Profile in die Seiten einpassen
  bool bFailBoxes = false; // Falg, ob es Boxes gab, die nicht auf die Seiten passen
  for( int boxID = 0; boxID < /*objListArray*/drawLayers.GetSize(); boxID++ )
  {
    CDrawLayer* pLayer = drawLayers[boxID];  //objListArray.GetAt( boxID );

    CPoint boxPoint = boxPager.GetBoxPoint( boxID );
    CSize boxSize = boxPager.GetBoxSize( boxID );
    int boxPage = boxPager.GetBoxPage( boxID );

    CIntIRect targetRect( 0, boxSize.cy, boxSize.cy, 0 );

    if( boxPage == -1 )
    {
      bFailBoxes = true;

      //pLayer->SetFlags( CDrawLayer::visible_flag ); // TODO: wird in UpdateDrawing gesetzt!

      pLayer->UnsetFlags( CDrawLayer::visible_flag );

    }
    else
    {
      targetRect.left = boxPoint.x + pageSize.cx * boxPage;
      targetRect.right = targetRect.left + boxSize.cx;
      targetRect.bottom = boxPoint.y;
      targetRect.top = boxPoint.y + boxSize.cy;
      pLayer->SetFlags( CDrawLayer::visible_flag );
    };

    MoveIntoExtent( *pLayer, targetRect );
  }; //  for i

  int pageCount = boxPager.GetPageCount();

  // den Stempel zum BoxPager hinzufügen
  CDrawLayer* pStempelLayer = GetDrawLayer( "Stempel" );
  if( pStempelLayer )
  {
    CIntIRect stempelRect( pStempelLayer->CalcRect() );

    CSize stempelSize( stempelRect.Width(), stempelRect.Height() );

    if( stempelSize.cx < pageSize.cx && stempelSize.cy < pageSize.cy )
    {
      CRect stempelRect( pageSize.cx * pageCount - stempelSize.cx - m_gaps.cx, stempelSize.cy + m_gaps.cy, pageSize.cx * pageCount - m_gaps.cx, m_gaps.cy );

      if( !boxPager.FitsRect( stempelRect ) )
      {
        pageCount++;
        stempelRect += CSize( pageSize.cx, 0 );
      }
      
      MoveIntoExtent( *pStempelLayer, stempelRect );
    }
  }; // if plotDocs

  // die Zeichnungsgrösse ausrechnen
  CSize drawSize( pageSize.cx * pageCount, pageSize.cy );
  SetDrawingSize( drawSize );
  drawSize = GetDrawingSize(); // könnte sich geändert haben (auf ganze Seiten aufgerundet )

  // den Rahmen positionieren
  CDrawLayer* pRahmenLayer = GetDrawLayer( "Rahmen" );
  if( pRahmenLayer )
  {
    POSITION pos = pRahmenLayer->GetHeadPosition();
    while( pos )
    {
      CDrawObj* pObj = pRahmenLayer->GetNextObject( pos );
            
      CIntIRect rahmRect( 0, drawSize.cy, drawSize.cx, 0 );
      CDoubleIRect dRahmRect( rahmRect );
      pObj->m_dPosition = dRahmRect;
      pObj->m_position = rahmRect;
    }; // while pos
  } // if pRahmenLayer

  // wenn es einen Titel gibt, diesen jetzt positionieren
  if( pTitelObj )
  {
    CIntIRect titleRect( 0, drawSize.cy - titleHeight, drawSize.cx, drawSize.cy - titleHeight * 2 );
    pTitelObj->m_position = titleRect;
    pTitelObj->m_dPosition = CDoubleIRect( titleRect );
  }


  // alle Objekte invalidieren
  InvalAll();

  UpdateAllViews( NULL, 0, NULL );

  if( bFailBoxes )
    AfxMessageBox( IDS_MULTI_DOC_BAD_BOXES );
}; // UpdateDrawing


/*!
 * Kopiert eine Liste von Objekten in dieses Dokument und verschiebt diese so,
 * dass sie in ein vorgegebenes Rechteck passen.
 *
 * @param objList : Die Liste der zu kopierenden (via CDrawObj::Clone) Objekte.
 * @param iRect : In dieses Rechteck werden die objekte verschoben.
 *
 */
void CMultiDoc::MoveIntoExtent( const CDrawObjList& objList, const CIntIRect& iRect )
{
  CIntIRect objsRect( objList.CalcRect() );

  CIntPoint profilBase( objsRect.left, objsRect.bottom );
  CIntPoint rectBase( iRect.left, iRect.bottom );

  // für später brauchen wir noch die aktuelle View
  CDrawView* pView = GetView();
  
  // in die Umrandung das Profil einfüllen
  POSITION pos = objList.GetHeadPosition();
  while( pos )
  {
    CDrawObj* pObj = objList.GetNextObject( pos );
    pObj->UnsetFlags( CDrawObj::offsetable ); // sonst werden bestimmte objekte nicht verschoben
    
    // das Object verschieben!
    CIntIRect objRect( pObj->m_position );
    
    CIntPoint newBasePoint( objRect.left - profilBase.x + rectBase.x, objRect.bottom - profilBase.y + rectBase.y );
    CIntIRect newPosRect( newBasePoint.x, newBasePoint.y, newBasePoint.x + objRect.Width(), newBasePoint.y + objRect.Height() );
    
    newPosRect.NormalizeRect();
    pObj->MoveTo( newPosRect, pView );
  }; // while po
}; // CloneIntoExtent

void CMultiDoc::Serialize( CArchive& ar )
{
  // SetCompound( FALSE ); // damit die OleItems 'Falt' deserialisiert werden

  CDrawDoc::Serialize( ar );

	if( ar.IsStoring() )
  {
    ar << (int)0; // Serialisierungsversion: ich vertraue dem MFC Schema nicht

    ar << m_gaps;
  }
	else
	{
    int nVersion = 0;
    ar >> nVersion;

    ar >> m_gaps;

    // nach dem laden immer die Zeichnung neu erzeugen, es könnte sich die Blattgrösse geändert haben
    UpdateDrawing();
	}
}; // Serialize



/*!
 * Gibt das Objekt zurück, welches den Titel beinhaltet.
 *
 * @return CDrawRect* : Das einzige Objekt des Layers 'Titel'
 */
CDrawRect* CMultiDoc::GetTitleObject() const
{
  // den Titel finden
  CDrawLayer* pTitelLayer = GetDrawLayer( "Titel" );
  if( pTitelLayer )
  {
    POSITION pos = pTitelLayer->GetHeadPosition();
    if( pos )
    {
      CDrawObj* pObj = pTitelLayer->GetNextObject( pos );
      if( pObj->GetRuntimeClass() == RUNTIME_CLASS( CDrawRect ) )
        return (CDrawRect*)pObj; // ein Titel reicht
    }
  }; // if pTitelLayer

  return NULL;
}



/*!
 * Gibt den Text des Titels zurück.
 *
 * @return CString  : Der Titeltext, der Text des Titelobjekts (@see GetTitleObject())
 */
CString CMultiDoc::GetPageTitle() const 
{
  CDrawRect* pTitelObj = GetTitleObject();

  CString titel;
  pTitelObj->GetText( titel );
  
  return titel; 
};


/*!
 * Setzt den Text des Titels, die Zeichnung muss noch per UpdateDrawing()
 * neu formatiert werden.
 *
 * @param pageTitle : der neue Text des Titels
 *
 */
void CMultiDoc::SetPageTitle( const CString& pageTitle ) 
{
  CDrawRect* pTitelObj = GetTitleObject();

  if( pTitelObj )
    pTitelObj->SetText( pageTitle );
};