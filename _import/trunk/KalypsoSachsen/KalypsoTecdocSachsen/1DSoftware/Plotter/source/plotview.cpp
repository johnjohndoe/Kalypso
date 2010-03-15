// PlotterView.cpp : implementation of the CPlotterView class
//

#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"
#include "bce\include\wspfeatures.h"

#include "plotdoc.h"
#include "plotdocdata.h"
#include "profdoc.h"
#include "propdlg.h"
#include "plotter.h"
#include "plotfrm.h"

#include "plotview.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPlotterView

IMPLEMENT_DYNCREATE(CPlotterView, CDrawView)

BEGIN_MESSAGE_MAP(CPlotterView, CDrawView)
	//{{AFX_MSG_MAP(CPlotterView)
//	ON_UPDATE_COMMAND_UI(ID_FILE_PRINT_ALL, OnUpdateFilePrintAll)
	//}}AFX_MSG_MAP
	// Standard printing commands (CDrawView)
  ON_COMMAND( ID_OBJECT_ISPROFIL, OnObjectIsProfil )
  ON_UPDATE_COMMAND_UI( ID_OBJECT_ISPROFIL, OnUpdateObjectIsProfil )
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CPlotterView drawing

void CPlotterView::OnInitialUpdate() 
{
	CWaitCursor wait;
	GetDocument()->UpdateDrawing();
	if (!GetDocument()->IsEmbedded())
	{
		CFrameWnd *pWnd = GetParentFrame();
		if (pWnd!=NULL)
			pWnd->ShowWindow(SW_SHOWMAXIMIZED);
	}
	else
		ResizeParentToFit();
	ResyncScrollSizes();

	CPlotterDoc *pDoc = (CPlotterDoc*)GetDocument();
	if (pDoc!=NULL && pDoc->IsKindOf(RUNTIME_CLASS(CProfDatDoc)))
		GETPLOTTERAPP->AddToRecentFileList(((CProfDatDoc*)pDoc)->m_originalFile);
}

/////////////////////////////////////////////////////////////////////////////
// OLE Client support and commands

/////////////////////////////////////////////////////////////////////////////
// CPlotterView message handlers

void CPlotterView::Props() 
{
	CPropertyDialog dlg( (CPlotterDoc*)GetDocument(), IDS_PROPERTIES );
	dlg.DoModal();
} // Props

void CPlotterView::Undo() 
// gleich wie CDrawView::Undo, checkt nur vorher, ob die Zeichnung neu
// formatiert werden muss
{
	BOOL bUpdate = FALSE;

	if( m_undoBuffer.GetSize() > 0 )
	{
		POSITION pos = m_undoBuffer.GetAt( 0 )->GetHeadPosition();
		while( pos != NULL )
		{
			CDrawObj* pObj = m_undoBuffer.GetAt( 0 )->GetNextObject( pos );
			if( !pObj->IsUser() )
			{
				bUpdate = TRUE;
				break;
			}
		}
	}
	CDrawView::Undo();

	if( bUpdate )
		GetDocument()->UpdateDrawing();
} // Undo

void CPlotterView::BeginTextEdit(CDrawRect* pDrawRect)
{
	CPlotterDoc* pDoc = (CPlotterDoc*)GetDocument();

	CDrawView::BeginTextEdit(pDrawRect);
	if( m_pDrawRect!=NULL )
	{
		// special case for title and height text - edit format text
		// and not the actual text
		if( m_pDrawRect == pDoc->GetTitle()->GetTitle() )
		{
			m_pDrawRect->SetText( pDoc->GetTitle()->GetFormatText() );
			m_pDrawRect->Invalidate();
			InvalObj(m_pDrawRect);
		}
    else if( m_pDrawRect == pDoc->GetComment())
		{
			m_pDrawRect->SetText( pDoc->m_pPData->m_commentFormatText );
			m_pDrawRect->Invalidate();
			InvalObj(m_pDrawRect);
		}
		else if (m_pDrawRect==pDoc->GetHeight())
		{
			m_pDrawRect->SetText(pDoc->m_pPData->m_heightFormatText);
			m_pDrawRect->Invalidate();
			InvalObj(m_pDrawRect);
		}
	}
}

void CPlotterView::EndTextEdit()
{
	CPlotterDoc *pDoc = (CPlotterDoc*)GetDocument();
	BOOL bUpdate = FALSE;

	if (m_bEditing)
	{
		if (m_pDrawRect!=NULL)
		{
			// special case for title and height text - format the text
			if( m_pDrawRect == pDoc->GetTitle()->GetTitle() )
			{
				m_pDrawRect->GetText( pDoc->GetTitle()->GetFormatText() );
				bUpdate = TRUE;
			}
      else if( m_pDrawRect == pDoc->GetComment() )
			{
				m_pDrawRect->GetText( pDoc->m_pPData->m_commentFormatText );
				bUpdate = TRUE;
			}
			else if (m_pDrawRect==pDoc->GetHeight())
			{
				m_pDrawRect->GetText(pDoc->m_pPData->m_heightFormatText);
				bUpdate = TRUE;
			}
		}
	}
	CDrawView::EndTextEdit();
	if( bUpdate )
		pDoc->UpdateDrawing();
}

void CPlotterView::FillBrowseCombo( const BOOL bEmpty )
// Füllt die Brwose-Combo entsprechend dem Inhalt dieses Fensters
// Parameter:
//      const BOOL bEmpty: falls TRUE, wird die combo geleert
{
  CPlotterDoc* pDoc = (CPlotterDoc*)GetDocument();
  CPlotterFrame* plotFrame = (CPlotterFrame*)GETPLOTTERAPP->GetMainWnd();
  CrossSection* cs = NULL;
  State* state = NULL;
  if ( bEmpty == FALSE && pDoc->GetSections()->GetSize() > 0 )
  {
    Section* section = pDoc->GetMSection( 0 );
    if ( section && section->GetClassType() == CLASS_TYPE_CSECTION )
      cs = (CrossSection*)section;
  };

  // jetzt den Zustand suchen, nur falls es ein Querprofil ist, sonst soll die Brwosecombo leer bleiben
  if ( cs && pDoc->GetStates()->GetSize() > 0 )
    state = pDoc->GetMState( 0 );

  if ( plotFrame )
    plotFrame->SetBrowseCombo( state, cs );
} // FillBrwoseCombo

/* virtual */ 
void CPlotterView::Clear()
// wie CDrawView::Clear, stellt nur fest, ob die Zeichnung neu formatiert werden muss
{
  CPlotterDoc* pDoc = (CPlotterDoc*)GetDocument();
  if( !pDoc )
    return;

  // die ganze Selektion durchgehen, und schaun, ob eins gelöscht wird, welches ein
  // Update nötig macht
  // das ist der Fall
  // - falls etwas aus dem Profil gelöscht wird
  // - falls ein Text aus der Tabelle gelöscht wird
  // - ein Stempelobjekt gelöscht wird
  BOOL bUpdate = FALSE;
  POSITION pos = m_selection.GetHeadPosition();
  while( pos != NULL )
  {
    CDrawObj* pObj = m_selection.GetNextObject( pos );

    if( pObj->IsUser() || pObj->IsHideable() )
    {
      // wird gelöscht werden
      // jetzt feststellen, ob ein Update nötig ist: das ist der Fall bei Objekten aus dem Profil und aus dem Stempel
      if( pDoc->HasType( pObj, CPlotterDoc::profil ) || pDoc->HasType( pObj, CPlotterDoc::stamp ) || 
        ( pDoc->HasType( pObj, CPlotterDoc::table ) && pObj->IsText() ) )
      {
        bUpdate = TRUE;
        break;
      };
    }
  } // while pos

  CDrawView::Clear();

  if( bUpdate )
    pDoc->UpdateDrawing();
} // Clear

void CPlotterView::OnObjectIsProfil()
// setzt oder löscht die Fixierung des Objektes am Profil
{
  CPlotterDoc* pDoc = (CPlotterDoc*)GetDocument();
  if( pDoc == NULL )
    return;

  // zuerst alle Objekte raussuchen, die überhaupt in betracht kommen
  CDrawObjList profilObs;
  CDrawObjList userObs;

  POSITION pos = m_selection.GetHeadPosition();
  while ( pos != NULL )
  {
    CDrawObj* pObj = m_selection.GetNextObject( pos );

    if( pDoc->HasType( pObj, CPlotterDoc::profil ) && pObj->GetType() == DST_UNKNOWN )
      profilObs.AddTailObject( pObj );
    else if( pDoc->HasType( pObj, CPlotterDoc::user ) )
      userObs.AddTailObject( pObj );
  } // while pos

  // falls gar nichts zu tun ist, jetzt abbrechen
  if( profilObs.GetObjectCount() == 0 && userObs.GetObjectCount() == 0 )
    return;

  // jetzt die Typen der jeweiligen Objekte ändern
  pDoc->ChangeType( profilObs, CPlotterDoc::user );
  pDoc->ChangeType( userObs, CPlotterDoc::profil );

  // und die Zeichnung neu formatieren
  pDoc->UpdateDrawing();
} // OnObjectIsProfil

void CPlotterView::OnUpdateObjectIsProfil( CCmdUI* pCmdUI )
{
  CPlotterDoc* pDoc = (CPlotterDoc*)GetDocument();
  
  BOOL bObs = FALSE; // es sind überhaupt Objekte da, die es betrifft
  BOOL bIsProfil = FALSE; // das Object ist schon fixiert
  
  POSITION pos = m_selection.GetHeadPosition();
  while( pos != NULL )
  {
    CDrawObj* pObj = m_selection.GetNextObject( pos );

    if( pObj != NULL )
    {
      if( pDoc->HasType( pObj, CPlotterDoc::profil ) && pObj->GetType() == DST_UNKNOWN )
        bIsProfil = TRUE;
      else if( pDoc->HasType( pObj, CPlotterDoc::user ) )
        bObs = TRUE;
    } // if pObj
  } // while pos

  // nur erlauben, falls es überhaupt gültige Objekte gibt
  pCmdUI->Enable( bIsProfil || bObs );

  // den Status setzen
  int check = 0;
  
  if( bObs && bIsProfil )
    check = 2;
  else if( bIsProfil )
    check = 1;
  pCmdUI->SetCheck( check );
} // OnUpdateObjectIsProfil
