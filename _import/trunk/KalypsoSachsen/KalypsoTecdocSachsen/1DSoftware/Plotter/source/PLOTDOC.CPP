// PlotterDoc.cpp : implementation of the CPlotterDoc class
//

#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"
#include "..\..\wspprj\include\textBlock.h"

#include "bce\include\wspfeatures.h"

#include "plotview.h"
#include "drawvw.h"
#include "template.h"
#include "stpldoc.h"
#include "opendlg.h"
#include "plotter.h"
#include "summinfo.h"
#include "dxfzeich.h"
#include "plotdocdata.h"
#include "profdoc.h"

#include "plotdoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif



/////////////////////////////////////////////////////////////////////////////
// CPlotterDoc

IMPLEMENT_DYNCREATE(CPlotterDoc, CDrawDoc)

BEGIN_MESSAGE_MAP(CPlotterDoc, CDrawDoc)
//{{AFX_MSG_MAP(CPlotterDoc)
ON_UPDATE_COMMAND_UI(ID_FILE_SAVE, OnUpdateFileSave)
ON_UPDATE_COMMAND_UI(ID_FILE_SAVE_AS, OnUpdateFileSaveAs)
ON_COMMAND(ID_FILE_INSERT, OnFileInsert)
ON_UPDATE_COMMAND_UI(ID_FILE_INSERT, OnUpdateFileInsert)
ON_UPDATE_COMMAND_UI(ID_FILE_PRINT_SETUP, OnUpdateFilePrintSetup)
ON_COMMAND(ID_FILE_DXF_EXPORT, OnFileDxfExport)
//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPlotterDoc construction/destruction

CPlotterDoc::CPlotterDoc() : CDrawDoc()
{
  m_pPData = new CPlotterDocData;
  
  m_pData->m_bSnapToGrid = FALSE;
  m_bAlignLowPoint = FALSE;
  m_dAlignValue = 0;
}

CPlotterDoc::~CPlotterDoc()
{
  if (m_pPData!=NULL)
  {
    m_pPData->DeleteContents();
    delete m_pPData;
  }
}

void CPlotterDoc::DeleteContents()
{
  if( m_pPData != NULL )
    m_pPData->DeleteContents();
  
  CDrawDoc::DeleteContents();
}

///////////////
// Attribute //
///////////////
CDrawObjList* CPlotterDoc::GetUser() { return &(m_pPData->m_user); };
CDrawObjList* CPlotterDoc::GetUser() const { return &(m_pPData->m_user); };

CStempel* CPlotterDoc::GetStempel() { return &(m_pPData->m_stempel); }
CStempel* CPlotterDoc::GetStempel() const { return &(m_pPData->m_stempel); }

CProfil* CPlotterDoc::GetProfil() { return &(m_pPData->m_profil); }
CProfil* CPlotterDoc::GetProfil() const { return &(m_pPData->m_profil); }

CTable* CPlotterDoc::GetTable() { return &(m_pPData->m_table); }
CTable* CPlotterDoc::GetTable() const { return &(m_pPData->m_table); }

CTableKey1* CPlotterDoc::GetTableKey1() { return &(m_pPData->m_tableKey1); }
CTableKey1* CPlotterDoc::GetTableKey1() const { return &(m_pPData->m_tableKey1); }

CTableKey2* CPlotterDoc::GetTableKey2() { return &(m_pPData->m_tableKey2); }
CTableKey2* CPlotterDoc::GetTableKey2() const { return &(m_pPData->m_tableKey2); }

CDrawObjList* CPlotterDoc::GetRahmen() { return &(m_pPData->m_rahmen); }
CDrawObjList* CPlotterDoc::GetRahmen() const { return &(m_pPData->m_rahmen); }

CTitle* CPlotterDoc::GetTitle() { return &(m_pPData->m_title); }
CTitle* CPlotterDoc::GetTitle() const { return &(m_pPData->m_title); }

CDrawRect* CPlotterDoc::GetComment() { return m_pPData->m_comment; };
CDrawRect* CPlotterDoc::GetComment() const { return m_pPData->m_comment; };

CDrawRect* CPlotterDoc::GetHeight() { return m_pPData->m_height; }
CDrawRect* CPlotterDoc::GetHeight() const { return m_pPData->m_height; }

void CPlotterDoc::GetScale(CDoublePoint& scale) { scale = m_pPData->m_scale; }
void CPlotterDoc::GetRealScale(CDoublePoint& scale) { scale = m_pPData->m_realScale; }
double CPlotterDoc::GetRangeFrom() { return GetProfil()->GetFrom(); }
void CPlotterDoc::SetRangeFrom( double x ) { GetProfil()->SetFrom( x ); };
double CPlotterDoc::GetRangeTo() { return GetProfil()->GetTo(); }
void CPlotterDoc::SetRangeTo( double x ) { GetProfil()->SetTo( x ); };

CTable::TableFormat CPlotterDoc::GetTableFormat() { return GetTable()->GetTableFormat(); }
void CPlotterDoc::SetTableFormat( CTable::TableFormat n ) { GetTable()->SetTableFormat( n ); }
int CPlotterDoc::GetProfilFormat() { return GetProfil()->GetFormat(); }
void CPlotterDoc::SetProfilFormat( int n ) { GetProfil()->SetFormat( (CProfil::Format)n ); }

const CIntIRect& CPlotterDoc::GetBorderGaps() const { return m_pPData->m_rectBorderGaps; };
void CPlotterDoc::SetBorderGaps( const CIntIRect& borderGaps ) { m_pPData->m_rectBorderGaps = borderGaps; };


CTypedPtrArray<CObArray, State*>* CPlotterDoc::GetStates() const { return &m_pPData->m_States; };
CTypedPtrArray<CObArray, Section*>* CPlotterDoc::GetSections() const { return &m_pPData->m_Sections; };

BOOL CPlotterDoc::OnOpenDocument(LPCTSTR lpszPathName) 
{
  CFile file;
  CFileStatus rStatus;
  CString str;
  CDoublePoint scale;
  
  BOOL bLoaded = FALSE;
  BOOL bGetStatOK = FALSE;
  
  if( lpszPathName != NULL )
    bGetStatOK =  file.GetStatus( lpszPathName, rStatus );
  else
    bGetStatOK = FALSE;

  // if lpszPathName==NULL we have probably got a call from an OLE Container!
  if( lpszPathName == NULL || ( bGetStatOK && rStatus.m_size != 0 ) )
  {
    bLoaded = TRUE;
    if( !COleDocument::OnOpenDocument( lpszPathName ) )
      return FALSE;
  }
  else
  {
    bLoaded = FALSE;
    if (!COleDocument::OnNewDocument())
      return FALSE;
#if !defined(_MAC)
#if _MFC_VER>=0x0421
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
#endif
#endif
  }

  CPlotterApp* plotApp = GETPLOTTERAPP;
  
  if( !plotApp->GetProject() )
    return TRUE;		// only update pointers when project exists
  
  if (plotApp->m_pState==NULL)
  {
    if (bLoaded)
      return TRUE;	// only update pointers when state exists
    else
      return FALSE;
  }

  State* state = plotApp->m_pState; // der Zustand des zu ladenden Profils
  Section* section = NULL;
  if( plotApp->m_nCount >= 0 && plotApp->m_nCount < plotApp->m_Sections.GetSize() )
    section = plotApp->m_Sections[plotApp->m_nCount]; // die zu ladende Section

  InsertData( state, section, bLoaded, FALSE );
  
  return TRUE;
}; // OnOpenDocument

BOOL CPlotterDoc::InsertData( State* state, Section* section, BOOL bLoaded, BOOL bInserted, BOOL bAlign /*=FALSE*/ )
// fügt ein Profil zum Dokument hinzu und merkt sich state und section
// Parameter:
//        State* state: Zustand, unter welchem section zugefügt wird. Darf NULL sein
//        Section* section: die hinzuzufügende Section
//        BOOL bLoaded: TRUE, falls das Profil bereits als Zeichenobjekt geladen ist ( d.h. ein bestehendes Dokument wurde geöffnet )
//        BOOL bInserted: falls TRUE wird Profil nur hnzugefügt ( d.h Darstellung an vorhandenes angepasst ),
//                        falls FALSE ist es das erste und entsprechend behandelt
//        BOOL bAlign = FALSE: ?
// Bemerkung: es wird das letzte in m_pPData->m_section stehende Profil hinzugefügt
{
  // Voraussetzungen prüfen und das Profil laden falls noch nicht geschehen
  if( !section || !section->LoadProfil() ) 
    return FALSE;

  Profil* profil = section->GetProfil();
  if( !profil )
    return FALSE;

  // alles ok, also in der Liste der Profile merken ( wird schon in SetTitle benötigt )
  int addNr = bInserted ? GetStates()->GetSize() : 0; // falls es das erste Profil ist ( !bInserted ) immer an Stelle 0, sonst hnten dran
  GetStates()->SetAtGrow( addNr, state );
  GetSections()->SetAtGrow( addNr, section );

  AlignProfil( profil, bAlign ); // ??

  if( !bInserted )
  {
    m_pPData->m_realScale.x = profil->GetXMaszstab();
    m_pPData->m_realScale.y = profil->GetYMaszstab();
    SetTitle("");
  }; // if !bInserted 

  CWaitCursor wait;
  
  // CreateDrawing liest die Profildaten aus der 'globalen' variable m_pProfil!!! ( wer macht eigentlich so einen scheiss! )
  m_pProfil = profil;
  CreateDrawing( bLoaded, bInserted );
  m_pProfil = NULL;

  return TRUE;
}; // InsertData

void CPlotterDoc::AlignProfil(Profil* prof, BOOL bAlign)
// setzt die Variable m_pPData->m_dLowpoint und m_dAlignValue
{
  DataBlock *db;
  Coord *crd;
  double ymin, xofymin;
  
  ASSERT(prof!=NULL);
  
  ymin = 1e36;
  // find ymin and xofymin
  db = prof->GetFirstDataBlock();
  while (db!=NULL)
  {
    crd = db->GetFirstCoord();
    while (crd!=NULL)
    {
      if (db->GetType()==DST_GELAENDEHOEHE)
      {
        if (crd->dy<ymin)
        {
          ymin = crd->dy;
          xofymin = crd->dx;
        }
      }
      crd = db->GetNextCoord();
    }
    db = prof->GetNextDataBlock();
  }
  if (!bAlign)
    GetProfil()->SetLowPoint( xofymin );
  else
  {
    if( m_bAlignLowPoint )
      m_dAlignValue = GetProfil()->GetLowPoint() - xofymin;
    // shift all coords
    db = prof->GetFirstDataBlock();
    while (db!=NULL)
    {
      crd = db->GetFirstCoord();
      while (crd!=NULL)
      {
        crd->dx += m_dAlignValue;
        crd = db->GetNextCoord();
      }
      db = prof->GetNextDataBlock();
    }
  }
}; // AlignProfil

/////////////////////////////////////////////////////////////////////////////
// CPlotterDoc serialization

void CPlotterDoc::Serialize(CArchive& ar)
{
  CDrawDoc::Serialize(ar);
  if (ar.IsStoring())
    ar << m_pPData;
  else
  {
    m_pPData->DeleteContents();
    delete m_pPData;
    m_pPData = NULL;	// prepare for exception
    ar >> m_pPData;
  }
}

/////////////////////////////////////////////////////////////////////////////
// CPlotterDoc implementation

BOOL CPlotterDoc::DoFileSave()
{
   if (!WSPFeatures::Instance()->isEnabled("PLOTTER","plot_nodemo"))
	{
		std::string TheText = std::string(WSPFeatures::Instance()->GetDataStr("HEAD", "DEMO_INFO"));
		TheText.append("\n\n");
		TheText.append(WSPFeatures::Instance()->GetDataStr("PLOTTER", "plot_nodemo"));
		AfxMessageBox(TheText.c_str() ,MB_ICONINFORMATION,0);
		return false;
	}

  CFileStatus rStatus;
  BOOL bExists = FALSE;
  
  // falls ein Projekt offen ist, im Projekt die Plotter Unterverzeichnisse anlegen, falls nicht schon geschehen
  Project* proj = GETPLOTTERAPP->GetProject();
  if ( proj )
  {
    CString path, dir;
    path = proj->GetDir();
    dir = path + "\\Plot_qp";
    path += "\\Plot_qp\\*.*";
    if (!CFile::GetStatus(path, rStatus))
      CreateDirectory(dir, NULL);
    path = proj->GetDir();
    dir = path + "\\Plot_ls";
    path += "\\Plot_ls\\*.*";
    if (!CFile::GetStatus(path, rStatus))
      CreateDirectory(dir, NULL);
  }
  
  if (!m_strPathName.IsEmpty())
    bExists = (BOOL)CFile::GetStatus(m_strPathName, rStatus);
  DWORD dwAttrib = GetFileAttributes(m_strPathName);
  if (bExists && dwAttrib & FILE_ATTRIBUTE_READONLY)
  {
    // we do not have read-write access
    if (!DoSave(NULL))
    {
      TRACE0("Warning: File save with new name failed.\n");
      return FALSE;
    }
  }
  else
  {
    if (!DoSave(m_strPathName))
    {
      TRACE0("Warning: File save failed.\n");
      return FALSE;
    }
  }
  return TRUE;
}

BOOL CPlotterDoc::OnSaveDocument(LPCTSTR lpszPathName)
{
  CFile file;
  CFileStatus rStatus;

  if (!m_strPathName.IsEmpty())
  {
    if (!file.GetStatus(m_strPathName, rStatus))
      m_strPathName.Empty();
  }
  return CDrawDoc::OnSaveDocument(lpszPathName);
}

void CPlotterDoc::AddObject( CDrawObj* pObj, const int type, const int index )
// Überschreibung von CDrawDoc::AddObject: der Typ wird hier berücksichtigt
// Fügt das Objekt zur Zeichnung hinzu und zum jeweiligen Objekt, welches einen
// Zeichnungsteil repräsentiert ( z.B. Stempel oder Profil )
// Parameter:
//        int type: gibt an, zu welchem Teil der Zeichnung das objekt gehört
{
  // zur Zeichnung hinzufügen
  CDrawDoc::AddObject( pObj, type, index );

  if( type == user )
  {
    // falls es typ 'User' ist, nochmals die Flags richtig setzen,
    // denn es ist möglcherweise ein kopiertes Objekt 
    pObj->SetFlags( CDrawObj::user | CDrawObj::moveable, TRUE );

    // ausserdem noch sicherstellen, das die logischen und die metrischen Kooridnaten gleich sind
    ResetLogicalCoords( pObj );
  }
  else
    pObj->UnsetFlags( CDrawObj::user, TRUE );

  switch( type )
  {
  case user:
    GetUser()->AddTailObject( pObj );
    break;

  case profil:
    GetProfil()->AddObject( pObj );
    break;

  case table:
    GetTable()->AddObject( index, pObj );
    break;

  case tableKey1:
    GetTableKey1()->AddObject( index, pObj );
    break;

  case tableKey2:
    GetTableKey2()->AddObject( index, pObj );
    break;

  case title:
    GetTitle()->SetTitle( (CDrawRect*)pObj );
    break;

  case height:
    SetHeight( (CDrawRect*)pObj );
    break;

  case comment:
    SetComment( (CDrawRect*)pObj );
    break;

  case stamp:
    GetStempel()->AddObject( pObj );
    break;

  case border:
    GetRahmen()->AddTailObject( pObj );
    break;
  } // switch type
} // AddObject

int CPlotterDoc::RemoveObject( CDrawObj* pObj, int* index /* = NULL */ )
// überschreibt CDrawDoc::RemoveObject
// Parameter:
//        int* index: falls ungleich NULL, wird diesem Parameter der Index zugewiesen, unter welchen das
//                Objekt in den Tabellen steckte ( -> gilt nur für CPlotterDoc )
// Rückgabewert:
//        int: der typ des Objekts gibt an, zu welchem Zeichnungselement das Objekt gehörte
{
  int type;

  // jetzt rausfinden, zu welchem Teil der Zeichnung das Objekt gehört
  if( GetUser()->RemoveObject( pObj ) )
    type = user;
  else if( GetProfil()->RemoveObject( pObj ) )
    type = profil;
  else if( GetTable()->RemoveObject( pObj, index ) )
    type = table;
  else if( GetTableKey1()->RemoveObject( pObj, index ) )
    type = tableKey1;
  else if( GetTableKey2()->RemoveObject( pObj, index ) )
    type = tableKey2;
  else if( GetTitle()->GetTitle() == pObj )
  {
    GetTitle()->SetTitle( NULL );
    type = title;
  }
  else if( GetComment() == pObj )
  {
    SetComment( NULL );
    type = comment;
  }
  else if( GetHeight() == pObj )
  {
    SetHeight( NULL );
    type = height;
  }
  else if( GetStempel()->RemoveObject( pObj ) )
    type = stamp;
  else if( GetRahmen()->RemoveObject( pObj ) )
    type = border;

  // jetzt das Objekt auch aus der Zeichnung löschen
  CDrawDoc::RemoveObject( pObj, index );

  return type;
}

UINT CPlotterDoc::CalcTableHeightsAndWidths( CView* pView, UINT& wmax1, UINT& wmax2 )
// Rechnet die benötigten Höhen und Breiten der Tabelle aus
// Setzt die Variablen:
//      m_pPData->m_sizeTableMargins
//      m_tableHeights
// Parameter:
//        CView* pView: wird benötigt, um die echten Textgrössen auszurechnen
//        CUIntArray& tableHeights: hier werden die gefundenen Höhen abgelegt
//        UINT& wmax1: die gefundenen Breite der TableKey1
//        UINT& wmax2: die gefundene Breite der TableKey2
{
  UINT tableSize = GetTable()->GetSize();
  
  // alte TableHeights löschen
  m_tableHeights.RemoveAll();
  m_tableHeights.SetSize( tableSize );

  CUIntArray tableWidths1, tableWidths2; // Hilfsvariablen für die Breite von Key1, Key2
  tableWidths1.SetSize( tableSize );
  tableWidths2.SetSize( tableSize );

  // find max heights and widths for table rows
  for( int i = tableSize - 1; i >= 0; i-- )
  {
    m_tableHeights.SetAtGrow(i, 0);
    tableWidths1.SetAtGrow(i, 0);
    tableWidths2.SetAtGrow(i, 0);
    
    CDrawObjList* pTableRow = GetTable()->GetAt( i );
    
    BOOL bXValuesVisible = FALSE;
    BOOL bYValuesVisible = FALSE;
    
    // zuerst rausfinden, ob diese Zeile x und/oder y Werte hat
    POSITION pos = pTableRow->GetHeadPosition();
    while( pos != NULL )
    {
      CDrawObj* pObj = pTableRow->GetNextObject(pos);
      if( pObj->IsText() && ((CDrawRect*)pObj)->GetTextType()==CDrawRect::xcoord)
      {
        if (!pObj->IsInvisible())
          bXValuesVisible = TRUE;
        break;
      }
    }
    pos = pTableRow->GetHeadPosition();
    while( pos!=NULL )
    {
      CDrawObj* pObj = pTableRow->GetNextObject(pos);
      if (pObj->IsText() && ((CDrawRect*)pObj)->GetTextType()==CDrawRect::ycoord)
      {
        if (!pObj->IsInvisible())
          bYValuesVisible = TRUE;
        break;
      }
    } // while pos

    int wmax = 0;
    // Jetzt die Grössen der Table rausfinden
    pos = pTableRow->GetHeadPosition();
    while( pos != NULL )
    {
      CDrawObj* pObj = pTableRow->GetNextObject(pos);
      if (!pObj->IsInvisible() && pObj->IsText())
      {
        if (((CDrawRect*)pObj)->GetTextType()==CDrawRect::xcoord)
        {
          if (pObj->m_dPosition.right < GetRangeFrom() || pObj->m_dPosition.right > GetRangeTo() )
            continue;
        }
        if (((CDrawRect*)pObj)->GetTextType()==CDrawRect::ycoord)
        {
          if( pObj->m_dPosition.left < GetRangeFrom() || pObj->m_dPosition.left > GetRangeTo() )
            continue;
        }
        m_pData->FormatValue((CDrawRect*)pObj, bXValuesVisible && bYValuesVisible);
        CSize sizeOutput = ((CDrawRect*)pObj)->GetOutputTextSize( pView );
        if( ((CDrawRect*)pObj)->GetTextType() == CDrawRect::ycoord )
        {
          if( pObj->m_dPosition.left == GetRangeTo() || pObj->m_dPosition.left == GetRangeFrom() )
            wmax = max( wmax, sizeOutput.cx );
        }
        double factor = pObj->m_dPosition.top - pObj->m_dPosition.bottom;
        m_tableHeights[i] = max( m_tableHeights[i], (int)(sizeOutput.cy/factor) );
      }
    }
    m_pPData->m_sizeTableMargins.cy = 2 * wmax; // macht hier keinen Sinn??? -> Breite einer Zelle???

    // die Grössen der TableKey1
    if( i < GetTableKey1()->GetSize() )
    {
      POSITION pos = GetTableKey1()->GetAt(i)->GetHeadPosition();
      while (pos!=NULL)
      {
        CDrawObj* pObj = GetTableKey1()->GetAt(i)->GetNextObject(pos);
        if (!pObj->IsInvisible() && pObj->IsText())
        {
          CSize sizeOutput = ((CDrawRect*)pObj)->GetOutputTextSize(pView);
          double factor = pObj->m_dPosition.top - pObj->m_dPosition.bottom;
          m_tableHeights[i] = max( m_tableHeights[i], (int)(sizeOutput.cy/factor));
          tableWidths1[i] = max( tableWidths1[i], sizeOutput.cx);
        }
      }
    }

    // zuletzt die Grössen der TableKey2
    if( i < GetTableKey2()->GetSize() )
    {
      POSITION pos = GetTableKey2()->GetAt( i )->GetHeadPosition();
      while (pos!=NULL)
      {
        CDrawObj* pObj = GetTableKey2()->GetAt( i )->GetNextObject(pos);
        if (!pObj->IsInvisible() && pObj->IsText())
        {
          CSize sizeOutput = ((CDrawRect*)pObj)->GetOutputTextSize(pView);
          double factor = pObj->m_dPosition.top - pObj->m_dPosition.bottom;
          m_tableHeights[i] = max( m_tableHeights[i], (int)(sizeOutput.cy/factor));
          tableWidths2[i] = max( tableWidths2[i], sizeOutput.cx);
        }
      }
    }
    GETPLOTTERAPP->IncStatusBarProgress();
  }

  wmax1 = 0;
  for( i = 0; i < tableWidths1.GetSize(); i++ )
    wmax1 = max( wmax1, tableWidths1[i] );
  
  wmax2 = 0;
  for( i = 0; i < tableWidths2.GetSize(); i++ )
    wmax2 = max( wmax2, tableWidths2[i] );

  m_pPData->m_sizeTableMargins.cx = wmax1 + wmax2;


  // noch die Gesamthöhe ausrechnen
  UINT tableHeight = 0;
  for( i = 0; i < m_tableHeights.GetSize(); i++ )
    tableHeight += m_tableHeights[i];

  return tableHeight;
} // CalculateTableHeightsAndWidths

void CPlotterDoc::UpdateRahmen(CPlotterView* /*pView*/)
{
  CSize sizeDrawing = GetDrawingSize();
  CIntIRect rahmenRect( 0 * MM_FACTOR, sizeDrawing.cy - 0 * MM_FACTOR, sizeDrawing.cx - 0 * MM_FACTOR, 0 * MM_FACTOR );

  POSITION pos = m_pPData->m_rahmen.GetHeadPosition();
  while( pos )
  {
    CDrawObj* pObj = m_pPData->m_rahmen.GetNextObject( pos );
    if( !pObj->IsText() )
    {
      pObj->m_position = rahmenRect;
      // store the new position
      LogicalToMeters( pObj->m_position, pObj->m_dPosition, pObj );
      pObj->Invalidate();
    }
  }
}

void CPlotterDoc::SetComment(CDrawRect* pObj)
{
  m_pPData->m_comment = pObj;
}

void CPlotterDoc::UpdateComment( const CIntPoint& basePoint, CView *pView )
// Positioniert den Kommentar neu in der Zeichnung
// Parameter:
//        const CPoint& basePoint: Basispunkt des Kommentar: Mitte/Oben des Rechtecks
//        CView* pView: Referenz auf die View um die Grösse des Textes auszurechnen
{
  if( m_pPData->m_comment && m_pPData->m_comment->IsText() )
  {
    CString str = m_pPData->m_commentFormatText;
    if( FormatText( str ) )
      m_pPData->m_comment->SetText( str );
    
    CSize sizeOutput = ((CDrawRect*)m_pPData->m_comment)->GetOutputTextSize( pView );

    // jetzt das Rechteck bestimmen
    CDoubleIRect rect( basePoint.x - sizeOutput.cx / 2, basePoint.y, basePoint.x + sizeOutput.cx / 2, basePoint.y - sizeOutput.cy );

    m_pPData->m_comment->m_dPosition = rect;
  }
}

void CPlotterDoc::SetHeight( CDrawRect* pObj )
{
  m_pPData->m_height = pObj;
}

void CPlotterDoc::UpdateHeight( CPlotterView *pView )
{
  CIntIRect rect( m_pPData->m_tableKey1.GetRect() );
  if( m_pPData->m_height != NULL && m_pPData->m_height->IsText() )
  {
    CString str = m_pPData->m_heightFormatText;
    if( FormatText( str ) )
      m_pPData->m_height->SetText( str );
    CSize sizeOutput = ((CDrawRect*)m_pPData->m_height)->GetOutputTextSize( pView );
    CDoubleIRect rectObj( rect.left, rect.top + sizeOutput.cy, max( rect.right, rect.left + sizeOutput.cx ), rect.top );
    m_pPData->m_height->m_dPosition = rectObj;
    MetersToLogical( m_pPData->m_height->m_dPosition, m_pPData->m_height->m_position, m_pPData->m_height );
  }
}

void CPlotterDoc::SetScale(CDoublePoint& scale)
{
  m_pPData->m_scale = scale;
  m_pPData->m_realScale = scale;
}

double CPlotterDoc::GetRealXScale() const
{
  return m_pPData->m_realScale.x;
} // GetRealXScale

double CPlotterDoc::GetRealYScale() const
{
  return m_pPData->m_realScale.y;
} // GetRealYScale

double CPlotterDoc::GetLogicalXScale( const CDrawObj* pObj ) const
{
  BOOL bScale = FALSE;
  
  // 1. scale from metres to millimeters (1000) if the object is in the Profilbereich
  // 2. scale from millimeters to hundredths of millimeters (MM_FACTOR)
  // 3. scale using drawing scale (real scale) if the object is in the Profilbereich
  double scale = 1;

  if( pObj == NULL )
    return scale;

  if( HasType( pObj, profil ) || HasType( pObj, table ) )
  {
    scale /= MM_FACTOR;
    scale *= GetRealXScale() / 1000;
  }
  
  return scale;
}

double CPlotterDoc::GetLogicalYScale( const CDrawObj* pObj ) const
{
  // 1. scale from metres to millimeters (1000) if the object is in the Profilbereich
  // 2. scale from millimeters to hundredths of millimeters (MM_FACTOR)
  // 3. scale using drawing scale (real scale) if the object is in the Profilbereich
  double scale = 1;

  if( pObj == NULL )
    return scale;

  int index;
  if( HasType( pObj, profil ) )
  {
    scale /= MM_FACTOR;
    scale *= GetRealYScale() / 1000;
  }
  else if( HasType( pObj, tableKey1, &index ) ||
    HasType( pObj, tableKey2, &index ) ||
    HasType( pObj, table, &index ) )
  {
    int vppmm = abs(m_tableHeights[index]);
    if( vppmm != 0 )
      scale /= vppmm;
  }
  return scale;
}

void CPlotterDoc::MetersToLogical( const CDoubleIRect& input, CIntIRect& output, const CDrawObj* pObj ) const
{
  CIntPoint lOffset = GetLogicalOffset( pObj );
  CDoublePoint mOffset = GetMetersOffset( pObj );
  
  double f_left = ((input.left-mOffset.x) / GetLogicalXScale(pObj)) + lOffset.x;
  double f_top = ((input.top-mOffset.y) / GetLogicalYScale(pObj)) + lOffset.y;
  double f_right = ((input.right-mOffset.x) / GetLogicalXScale(pObj)) + lOffset.x;
  double f_bottom = ((input.bottom-mOffset.y) / GetLogicalYScale(pObj)) + lOffset.y;

  char convert[MAX_PATH];
  sprintf( convert,"%.0lf",f_left );
  sscanf( convert,"%lf",&f_left );
  sprintf( convert,"%.0lf",f_top );
  sscanf( convert,"%lf",&f_top );
  sprintf( convert,"%.0lf",f_right );
  sscanf( convert,"%lf",&f_right );
  sprintf( convert,"%.0lf",f_bottom );
  sscanf( convert,"%lf",&f_bottom );
  
  output.left = (int)(f_left);
  output.top = (int)(f_top);
  output.right = (int)(f_right);
  output.bottom = (int)(f_bottom);

  /*
  CSize drawSize = GetDrawingSize();
  CPoint offset( 0, 0 );
  if( output.left < 0 )
    offset.x = -output.left;
  if( output.right > drawSize.cx )
    offset.x = drawSize.cx - output.right;
  if( output.bottom < 0 )
    offset.y = -output.bottom;
  if( output.top > drawSize.cy )
    offset.y = drawSize.cy - output.top;

  output += offset;
  */
}; // MetersToLogical

void CPlotterDoc::LogicalToMeters( const CIntIRect& input, CDoubleIRect& output, const CDrawObj* pObj ) const
// rechnet ein Rechteck in logischen Koordinaten (d.h. ProfilKoordinaten) in ein Rechteck in 
// Ausgabekoordinaten ( d.h. mm um ), abhängig davon, wo sich das Objekt befindet ( Tabelle, 
// Profil oder sonst )
{
  CIntPoint lOffset = GetLogicalOffset( pObj );
  CDoublePoint mOffset = GetMetersOffset( pObj );
  
  output.left = ( input.left - lOffset.x ) * GetLogicalXScale( pObj ) + mOffset.x;
  output.top = ( input.top - lOffset.y ) * GetLogicalYScale( pObj ) + mOffset.y;
  output.right = ( input.right - lOffset.x ) * GetLogicalXScale( pObj ) + mOffset.x;
  output.bottom = ( input.bottom - lOffset.y ) * GetLogicalYScale( pObj ) + mOffset.y;
}

void CPlotterDoc::MetersToLogical( const CDoublePoint& input, CIntPoint& output, const CDrawObj* pObj ) const
{
  CIntPoint lOffset = GetLogicalOffset( pObj );
  CDoublePoint mOffset = GetMetersOffset( pObj );
  
  double f_x = ((input.x-mOffset.x) / GetLogicalXScale(pObj)) + lOffset.x;
  double f_y = ((input.y-mOffset.y) / GetLogicalYScale(pObj)) + lOffset.y;
  
  char convert[MAX_PATH];
  sprintf( convert,"%.0lf",f_x );
  sscanf( convert,"%lf",&f_x );
  strcpy( convert,"0.0" );
  sprintf( convert,"%.0lf",f_y );
  sscanf( convert,"%lf",&f_y );
  
  output.x = int( f_x );
  output.y = int( f_y );

/*  
  CSize drawSize = GetDrawingSize();
  CIntPoint offset( 0, 0 );
  
  if( output.x < 0 )
    offset.x = -output.x;
  if( output.x > drawSize.cx )
    offset.x = drawSize.cx - output.x;
  if( output.y < 0 )
    offset.y = -output.y;
  if( output.y > drawSize.cy )
    offset.y = drawSize.cy - output.y;

  output += offset;
*/
}

void CPlotterDoc::LogicalToMeters( const CIntPoint& input, CDoublePoint& output, const CDrawObj* pObj ) const
{
  CIntPoint lOffset = GetLogicalOffset( pObj );
  CDoublePoint mOffset = GetMetersOffset( pObj );
  
  output.x = ( input.x - lOffset.x ) * GetLogicalXScale( pObj ) + mOffset.x;
  output.y = ( input.y - lOffset.y ) * GetLogicalYScale( pObj ) + mOffset.y;
}

CIntPoint CPlotterDoc::GetLogicalOffset( const CDrawObj* pObj ) const
{
  if( pObj == NULL )
    return CIntPoint( 0, 0 );

  CIntPoint offset = pObj->GetOffset();

  if( HasType( pObj, profil ) )
    return offset + GetProfil()->GetRectPos();

  if( HasType( pObj, stamp ) )
    return offset + GetStempel()->GetRectPos();

  int index;
  if( HasType( pObj, table, &index ) )
    return offset + GetTable()->GetAt( index )->GetRectPos();

  if( HasType( pObj, tableKey1, &index ) )
    return offset + GetTableKey1()->GetAt( index )->GetRectPos();

  if( HasType( pObj, tableKey2, &index ) )
    return offset + GetTableKey2()->GetAt( index )->GetRectPos();
  
  return offset;
}

CDoublePoint CPlotterDoc::GetMetersOffset( const CDrawObj* pObj ) const
{
  CDoublePoint offset(0, 0);

  if( pObj == NULL )
    return offset;

  CDoubleIRect profilRect( GetProfil()->GetVisibleRect() );

  if( HasType( pObj, profil ) )
    offset = CDoublePoint( profilRect.left, profilRect.bottom );
  else if( HasType( pObj, table ) )
    offset.x = profilRect.left;
  
  return offset;
}

#define FMT_BEZUGSHOEHE	  0
#define FMT_GEWAESSER	    1
#define FMT_ZUSTAND	      2
#define FMT_VZK           3
#define FMT_PK				    4
#define FMT_STATION	      5
#define FMT_ANFANG        6
#define FMT_ENDE          7
#define FMT_NAME          8
#define FMT_URDATEI	      9
#define FMT_COMMENT      10

#define N_FMTS				11

BOOL CPlotterDoc::FormatText(CString& str)
{
  State* state = GetMState( 0 );
  Section* section = GetMSection( 0 );

  CString temp, fmt, text;
  int i, j;
  
  for (i=0; i<N_FMTS; i++)
  {
    switch(i)
    {
    case FMT_BEZUGSHOEHE:
      fmt.LoadString(IDS_FMT_BEZUGSHOEHE);
      break;
      
    case FMT_GEWAESSER:
      fmt.LoadString(IDS_FMT_GEWAESSER);
      break;
      
    case FMT_ZUSTAND:
      fmt.LoadString(IDS_FMT_ZUSTAND);
      break;
      
    case FMT_VZK:
      fmt.LoadString(IDS_FMT_VZK);
      break;
      
    case FMT_PK:
      fmt.LoadString(IDS_FMT_PK);
      break;
      
    case FMT_STATION:
      fmt.LoadString(IDS_FMT_STATION);
      break;
      
    case FMT_ANFANG:
      fmt.LoadString(IDS_FMT_ANFANG);
      break;
      
    case FMT_ENDE:
      fmt.LoadString(IDS_FMT_ENDE);
      break;
      
    case FMT_NAME:
      fmt.LoadString(IDS_FMT_NAME);
      break;
      
    case FMT_URDATEI:
      fmt.LoadString(IDS_FMT_URDATEI);
      break;

    case FMT_COMMENT:
      fmt.LoadString( IDS_FMT_COMMENT );
      break;
    }
    temp = str;
    j = temp.Find(fmt);
    
    text.Empty();
    if( j != -1 )
    {
      temp = temp.Left(j) + "%s" + temp.Right(temp.GetLength()-j-fmt.GetLength());

      switch( i )
      {
      case FMT_BEZUGSHOEHE:
        text.Format( "%.2f", GetProfil()->GetHeight() );
        break;
        
      case FMT_GEWAESSER:
        if( state )
          text = GetMState( 0 )->GetWaterName();
        else if( section && section->GetClassType()==CLASS_TYPE_CSECTION )
          text = ((CrossSection*)section)->GetWaterName();
        break;
        
      case FMT_ZUSTAND:
        if( state )
          text = GetMState( 0 )->GetName();
        else if( section && section->GetClassType()==CLASS_TYPE_CSECTION )
          text = ((CrossSection*)section)->GetStateName();
        break;
        
      case FMT_VZK:
        if( section )
          section->GetVZK( text );
        break;
        
      case FMT_PK:
        if( section && section->GetClassType()==CLASS_TYPE_CSECTION )
          text = ((CrossSection*)section)->GetPK();
        break;
        
      case FMT_STATION:
        if( section && section->GetClassType()==CLASS_TYPE_CSECTION )
          text.Format("%.4f", ((CrossSection*)section)->GetStation() );
        break;

      case FMT_COMMENT:
        if( section && section->GetClassType() == CLASS_TYPE_CSECTION )
        {
          CrossSection* cs = (CrossSection*)section;
          cs->LoadProfil();
          Profil* profil = cs->GetProfil();
          if( profil )
            text = profil->GetComment();

          if( text.IsEmpty() )
            text = " "; // sonst "(unbekannt)"
        } // if m_section.GetSize() > 0
        break;
        
      case FMT_ANFANG:
        if( section && section->GetClassType()==CLASS_TYPE_LSECTION )
          text.Format( "%.4f", ((LengthSection*)section)->GetStartStation() );
        break;
        
      case FMT_ENDE:
        if( section && section->GetClassType()==CLASS_TYPE_LSECTION )
          text.Format( "%.4f", ((LengthSection*)section)->GetEndStation() );
        break;
        
      case FMT_NAME:
        if( section && section->GetClassType()==CLASS_TYPE_LSECTION )
          text = ((LengthSection*)section)->GetName();
        break;
        
      case FMT_URDATEI:
        text = GetMSection( 0 )->GetFileName();
        break;
      };
      if( text.IsEmpty() )
        text.LoadString( IDS_UNKNOWN );
      str.Format( temp, text );
      temp = str;
    }
  }
  
  return TRUE;
}; // FormatText

void CPlotterDoc::CreateDrawing(BOOL bLoaded /*=FALSE*/, BOOL bInserted /*==FALSE*/)
{
/********************************************************
Creates the drawing objects. Add the objects coordinates in
the following manner:
1. Profil
x and y coords are in meters.
2. Table Row
x coord is in meters, y coord is between zero (bottom of row)
and one (top of row). The height of the rows will be
calculated later. When creating text set the x coords (left
and right) to the corresponding point in the profil. The width
will be calculated later depending on the type of text (xcoord
or ycoord or normal).
  ********************************************************/
  CDoublePoint pt1, pt2;

  CMap<double, double, double, double> heights;		// maps stationierung to highest point
  CMap<double, double, double, double> shoehe_heights;		// maps stationierung to highest point ??? eher zu sohlhoehe
  CList<double, double> stations; 


  ASSERT(!(bLoaded && bInserted));
  
  int secIndex = GetSections()->GetSize() - 1;
  GETPLOTTERAPP->LoadTemplates();
  
  // die Vorlage bestimmen
  BOOL bDefaultTemplate = FALSE;
  CTemplate *pTemp = NULL;
  if( !bLoaded )
  {
    int i = GETPLOTTERAPP->m_nTemplate;
    if( i >= 0 && i < GETPLOTTERAPP->m_templates.GetSize() )
    {
      pTemp = GETPLOTTERAPP->m_templates.GetAt(i);
      m_pPData->m_eigenschaften = pTemp->GetName();
    }
    if( pTemp == NULL )
    {
      pTemp = new CTemplate;
      pTemp->CreateDrawing();
      bDefaultTemplate = TRUE;
      m_pPData->m_eigenschaften.LoadString( IDS_NONE );
    }
#if _MFC_VER>=0x0421
    if( m_pData->m_pSummInfo )
      m_pData->m_pSummInfo->SetTemplate(m_pPData->m_eigenschaften);
#endif
    //m_pPData->m_rectMargin = GETPLOTTERAPP->m_rectMargin;
  }

  CDoubleIRect pbrect;	// profilbereich rect
  if( bInserted )
    pbrect = GetProfil()->GetTotalRect();
  else
    pbrect.SetRect(1e36, -1e36, -1e36, 1e36);
  
  // calculate profil extents
  GETPLOTTERAPP->CreateStatusBarProgress( CString(MAKEINTRESOURCE(IDS_CREATING_DRAWING)), 0, m_pProfil->GetNumDataBlocks() * 2 + 1 );

  DataBlock* pDBGel = NULL; // der GELAENDEHOEHE Block wird für später gemerkt
  DataBlock* pDBlock = m_pProfil->GetFirstDataBlock();
  while( pDBlock != NULL )
  {
    int dbType = pDBlock->GetType();
    if( dbType == DST_GELAENDEHOEHE || dbType == DST_SOHLHOEHE )
      pDBGel = pDBlock;

    switch( dbType )
    {
    case DST_GELAENDEHOEHE:		// polylines are drawn
    case DST_UK_BRUECKE:
    case DST_OK_BRUECKE:
    case DST_OK_WEHRS:
    case DST_GELAENDE2:
    case DST_OK_GELAENDE:
    case DST_KASTEN:
    case DST_WSP_HOEHE:
    case DST_WASSERSP1:
    case DST_WASSERSP100:
    case DST_WASSERSP5:
    case DST_KREISSEGM:
    case DST_SOHLHOEHE:
    case DST_WASSERSPIEGEL:
    case DST_BOESCHUNG_LINKS:
    case DST_BOESCHUNG_RECHTS:
    case DST_DKUK:
    case DST_DKOK:
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
      {
        Coord* pCrd = pDBlock->GetFirstCoord();
        while (pCrd!=NULL)
        {
          pt1 = CDoublePoint( pCrd->dx, pCrd->dy );;
          pbrect.CompareAndExpand(pt1);
          
          // now set stations
          double height;
          if( heights.Lookup( pt1.x, height ) )
          {
            if (pt1.y>height)
            {
              heights.SetAt(pt1.x, pt1.y);
              if( pDBlock->GetType() == DST_SOHLHOEHE || pDBlock->GetType() == DST_GELAENDEHOEHE )
                shoehe_heights.SetAt(pt1.x, pt1.y);
            }
          }
          else
          {
            BOOL bInsertedStation = FALSE;
            heights.SetAt( pt1.x, pt1.y );
            if( pDBlock->GetType() == DST_SOHLHOEHE || pDBlock->GetType() == DST_GELAENDEHOEHE )
              shoehe_heights.SetAt( pt1.x, pt1.y );
            POSITION pos1 = stations.GetHeadPosition();
            while( pos1 != NULL && !bInsertedStation )
            {
              POSITION pos2 = pos1;
              if( stations.GetNext( pos1 ) >= pt1.x )
              {
                stations.InsertBefore( pos2, pt1.x );
                bInsertedStation = TRUE;
              }
            }
            if( !bInsertedStation )
              stations.AddTail( pt1.x );
          }
          pCrd = pDBlock->GetNextCoord();
        }
      }
      break;

      case DST_ABFLUSS:
      break;

    case DST_KREIS:
      {
        CDoubleIRect rect( pDBlock->GetDVar(2)-pDBlock->GetDVar(0)/2,
          pDBlock->GetDVar(3)+pDBlock->GetDVar(0),
          pDBlock->GetDVar(2)+pDBlock->GetDVar(0)/2,
          pDBlock->GetDVar(3) );
        pbrect.CompareAndExpand( rect );
      }
      break;
      
    case DST_MAUL:
      {
        CDoubleIRect rect( pDBlock->GetDVar(3)-pDBlock->GetDVar(0)/2,
          pDBlock->GetDVar(4)+3*pDBlock->GetDVar(0)/4,
          pDBlock->GetDVar(3)+pDBlock->GetDVar(0)/2,
          pDBlock->GetDVar(4) );
        pbrect.CompareAndExpand( rect );
      }
      break;
      
    case DST_EIPROFIL:
      {
        CDoubleIRect rect( pDBlock->GetDVar(3)-pDBlock->GetDVar(0)/2,
          pDBlock->GetDVar(4)+3*pDBlock->GetDVar(0)/2,
          pDBlock->GetDVar(3)+pDBlock->GetDVar(0)/2,
          pDBlock->GetDVar(4) );
        pbrect.CompareAndExpand( rect );
      }
      break;
      
    case DST_TRAPEZ:
      {
        CDoubleIRect rect( pDBlock->GetDVar(4)-pDBlock->GetDVar(0)/2-pDBlock->GetDVar(1)/pDBlock->GetDVar(2),
          pDBlock->GetDVar(5)+pDBlock->GetDVar(1),
          pDBlock->GetDVar(4)+pDBlock->GetDVar(0)/2+pDBlock->GetDVar(1)/pDBlock->GetDVar(2),
          pDBlock->GetDVar(5) );
        pbrect.CompareAndExpand( rect );
      }
      break;

    // case DST_LP_TEXT: ändert die Aussmasse des Profils nicht
    case DST_BAUWERK:
      // store current DBlock pos
      {
        POSITION pos1 = m_pProfil->GetDataBlockPos();
        if( pDBlock->GetNVar(0) <= m_pProfil->GetNumDataBlocks() )
        {
          DataBlock *pDBTemp;
          int count = 1;
          
          pDBTemp = m_pProfil->GetFirstDataBlock();
          while (pDBTemp!=NULL)
          {
            if (count==pDBlock->GetNVar(0))
            {
              for( int i = 0; i < pDBlock->GetNVar( 1 ); i++ )
              {
                count = 1;
                Coord* pCrd = pDBTemp->GetFirstCoord();
                while (pCrd!=NULL)
                {
                  if (count==pDBlock->GetNVar(i+2))//Dick 10.02.00 1->2
                  {
                    // main rect
                    CDoubleIRect rect( pCrd->dx-pDBlock->GetDVar(i*3+1)/2, pCrd->dy - (pDBlock->GetDVar(i*3+2)>0.0?-(pDBlock->GetDVar(i*3)/2):pDBlock->GetDVar(i*3)/2), pCrd->dx+pDBlock->GetDVar(i*3+1)/2,pCrd->dy + pDBlock->GetDVar(i*3)-pDBlock->GetDVar(i*3+2) );
                    pbrect.CompareAndExpand(rect);
                    // foundations
                    if (pDBlock->GetDVar(i*3+2)==0)
                      rect.SetRect(pCrd->dx-pDBlock->GetDVar(i*3+1)/2, pCrd->dy+pDBlock->GetDVar(i*3)/6, pCrd->dx+pDBlock->GetDVar(i*3+1)/2, pCrd->dy-pDBlock->GetDVar(i*3)/6);
                    else
                      rect.SetRect(pCrd->dx-pDBlock->GetDVar(i*3+1)/2, pCrd->dy - (pDBlock->GetDVar(i*3+2)>0.0?pDBlock->GetDVar(i*3)/2:-(pDBlock->GetDVar(i*3)/2)), pCrd->dx+pDBlock->GetDVar(i*3+1)/2, pCrd->dy + (pDBlock->GetDVar(i*3+2)>0.0?pDBlock->GetDVar(i*3+2):-pDBlock->GetDVar(i*3+2)));
                    pbrect.CompareAndExpand(rect);
                    break;
                  }
                  pCrd = pDBTemp->GetNextCoord();
                  count++;
                }
              }
              break;
            }
            pDBTemp = m_pProfil->GetNextDataBlock();
            count++;
          }
        }
        // restore current DBlock pos
        m_pProfil->SetDataBlockPos(pos1);
      }
      break;
      
    default:
      break;
    }
    pDBlock = m_pProfil->GetNextDataBlock();
    GETPLOTTERAPP->IncStatusBarProgress();
  }; // while pDBlock

  GetProfil()->SetTotalRect( pbrect );
  if( !bInserted && !bLoaded )
  {
    SetProfilFormat( pTemp->GetProfilFormat() );
    SetTableFormat( pTemp->GetTableFormat() );
    SetXValueFormat( pTemp->GetXValueFormat() );
    SetYValueFormat( pTemp->GetYValueFormat() );
    SetBorderGaps( pTemp->GetBorderGaps() );
    GetProfil()->SetRangeDbType( pTemp->GetProfil()->GetRangeDbType() );
    SetRangeFrom( pTemp->GetRangeFrom() );
    SetRangeTo( pTemp->GetRangeTo() );
    GetProfil()->SetAutoAnfang( pTemp->GetProfil()->GetAutoAnfang() );
    GetProfil()->SetAutoEnde( pTemp->GetProfil()->GetAutoEnde() );
    GetProfil()->SetAutoHeight( pTemp->GetProfil()->GetAutoHeight() );
    GetProfil()->SetHeight( pTemp->GetProfil()->GetHeight() );
    GetProfil()->SetAlign( pTemp->GetProfil()->GetAlign() );
    SetScale( pTemp->m_pPData->m_scale );
  }
  if( GetRangeFrom() == GetRangeTo() )
  {
    SetRangeFrom( GetProfil()->GetTotalRect().left );
    SetRangeTo( GetProfil()->GetTotalRect().right );
  }
  // offset bottom of profil size to the next lowest whole number
  double bottom = floor(pbrect.bottom);
  if( bottom == pbrect.bottom )
    pbrect.bottom -= 1;
  else
    pbrect.bottom = bottom;
  // pbbl is the bottom left coord of the profil rect
  CDoublePoint pbbl( pbrect.left, pbrect.bottom );

  // set origin of profil rect as bottom left
  // set size of profil (meters)
  GetProfil()->SetVisibleRect( pbrect );
  
  if (!bLoaded)
  {
    CTime startTime = CTime::GetCurrentTime();

    /********* create the drawing objects ***********/
    BOOL bLast = FALSE;
    
    DataBlock* pDBlock = m_pProfil->GetFirstDataBlock();
    DataBlock* pGHBlock = NULL; // merkt sich den Geländeblock ( oder Sohlhöhe )
    while (pDBlock!=NULL || !bLast)
    {
      int dbIndex = m_pProfil->GetDataBlockIndex(pDBlock);
      
      int type;
      if( pDBlock != NULL )
        type = pDBlock->GetType();
      else
      {
        type = DST_STATION;
        bLast = TRUE;
        dbIndex = -1;
      }
      if( type <= 0 )
      {
        pDBlock = m_pProfil->GetNextDataBlock();
        continue;
      }

      CDrawObjList ProfilObjs;

      // first the Profile
      switch(type)
      {
      case DST_GELAENDEHOEHE:		// polylines are drawn
      case DST_SOHLHOEHE:
      case DST_UK_BRUECKE:
      case DST_OK_BRUECKE:
      case DST_OK_WEHRS:
      case DST_GELAENDE2:
      case DST_KASTEN:
      case DST_OK_GELAENDE:
      case DST_WSP_FIXIERUNG:
      case DST_WASSERSP1:
      case DST_WASSERSP100:
      case DST_WASSERSP5:
      case DST_WASSERSPIEGEL:
      case DST_BOESCHUNG_LINKS:
      case DST_BOESCHUNG_RECHTS:
      case DST_BVHOEHE:
      case DST_SOHLHOEHE_2:
      case DST_WASSERSPIEGEL_2:
      case DST_BOESCHUNG_LINKS_2:
      case DST_BOESCHUNG_RECHTS_2:
      case DST_DEICH_RECHTS:
      case DST_DEICH_LINKS:
      case DST_DEICH_RECHTS_2:
      case DST_DEICH_LINKS_2:
      case DST_AUSUFERUNG_LI:
      case DST_AUSUFERUNG_RE:
      case DST_ENERGIEHOEHE:
      case DST_ABFLUSS:
        {
          CDrawPoly* pDrawPoly;
          CMap<double, double, double, double> intersections;
          
          // filling
          if( type == DST_GELAENDEHOEHE || type == DST_SOHLHOEHE )
          {
            pGHBlock = pDBlock; // den Datenblock merken
            pDrawPoly = (CDrawPoly*)pTemp->GetTemplateObj( TMPL_PROFIL_POLYGON, type );
            if( pDrawPoly )
            {
              pDrawPoly = (CDrawPoly*)pDrawPoly->Clone(this);
              pDrawPoly->SetSectionIndex(secIndex);
              pDrawPoly->SetDataBlockIndex(dbIndex);
              pDrawPoly->m_dPosition = pbrect;
              pDrawPoly->SetType(type);

              Coord* pCrd = pDBlock->GetFirstCoord();
              pDrawPoly->AddPoint( CDoublePoint( pCrd->dx, pbrect.bottom ) );
              
              while( pCrd )
              {
                pDrawPoly->AddPoint( CDoublePoint( pCrd->dx, pCrd->dy ) );

                Coord* pLastCrd = pCrd;
                pCrd = pDBlock->GetNextCoord();

                if( pCrd == NULL )
                  pDrawPoly->AddPoint( CDoublePoint( pLastCrd->dx, pbrect.bottom ) );
              }

              AddObject( pDrawPoly, profil, 0 );
            }
          }
          else if( pGHBlock )
          {
            double xfirst, xlast;
            int index;
            
            Coord* pCrd = pDBlock->GetFirstCoord();
            
            if( pCrd )
              xfirst = pCrd->dx;

            pDrawPoly = (CDrawPoly*)pTemp->GetTemplateObj(TMPL_PROFIL_POLYGON, type);
            if( pDrawPoly )
            {
              pDrawPoly = (CDrawPoly*)pDrawPoly->Clone(this);
              pDrawPoly->SetSectionIndex(secIndex);
              pDrawPoly->SetDataBlockIndex(dbIndex);
              pDrawPoly->m_dPosition = pbrect;
              pDrawPoly->SetType(type);
              
              // add coords for the profil
              while( pCrd )
              {
                Coord* pLastCrd = pCrd;
                
                pDrawPoly->AddPoint( CDoublePoint( pCrd->dx, pCrd->dy ) );
                pCrd = pDBlock->GetNextCoord();
                
                if( pCrd == NULL )
                  xlast = pLastCrd->dx;
              }

              // add coords for Gelaendehoehe
              index = pGHBlock->GetCoordIndex(xlast);
              if( index == -1 )
              {
                double prevy, prevx = 1e36;
                
                for( int i = 0; i < pGHBlock->GetNumCoords(); i++ )
                {
                  Coord* pCrd = pGHBlock->GetCoordAt(i);
                  if (prevx<=xlast && pCrd->dx>=xlast)
                  {
                    pt1 = CDoublePoint( xlast, prevy-(prevy-pCrd->dy)*(xlast-prevx)/(pCrd->dx-prevx) );
                    pDrawPoly->AddPoint( pt1 );
                    index = pGHBlock->GetCoordIndex(prevx);
                  }
                  prevx = pCrd->dx;
                  prevy = pCrd->dy;
                }
              }
              else
              {
                Coord* pCrd = pGHBlock->GetCoordAt(index);
                pt1 = CDoublePoint( pCrd->dx, pCrd->dy );
                pDrawPoly->AddPoint( pt1 );
              }
              // for each interval of profil insert coords for Gelaedehoehe
              for( int i = pDBlock->GetNumCoords()-1; i>0; i-- )
              {
                Coord* pC1 = pDBlock->GetCoordAt( i - 1 );
                Coord* pC2 = pDBlock->GetCoordAt( i );
                for( int j = pGHBlock->GetNumCoords() - 1; j >= 0; j-- )
                {
                  Coord* pCrd = pGHBlock->GetCoordAt( j );
                  if( ( pCrd->dx >= pC1->dx && pCrd->dx <= pC2->dx ) || ( pCrd->dx <= pC1->dx && pCrd->dx >= pC2->dx ) )
                  {	// the coord falls in this interval
                    double testy1 = pC2->dy-(pC2->dy-pC1->dy)*(pCrd->dx-pC2->dx)/(pC1->dx-pC2->dx); // height of profil at the coord for GH
                    if( testy1 >= pCrd->dy )
                    {	// this point can be inserted
                      pt1 = CDoublePoint( pCrd->dx, pCrd->dy );
                      pDrawPoly->AddPoint(pt1);
                    }
                    else
                    {	// Gelaendehoehe is above profil
                      Coord *pCrdBefore, *pCrdAfter;
                      BOOL bBeforeInI, bAfterInI;
                      
                      pCrdBefore = pGHBlock->GetCoordAt(j-1);
                      pCrdAfter = pGHBlock->GetCoordAt(j+1);
                      if (pCrdBefore!=NULL && pCrdBefore->dx>=pC1->dx && pCrdBefore->dx<=pC2->dx)
                        bBeforeInI = TRUE;
                      else
                        bBeforeInI = FALSE;
                      
                      if (pCrdAfter!=NULL && pCrdAfter->dx>=pC1->dx && pCrdAfter->dx<=pC2->dx)
                        bAfterInI = TRUE;
                      else
                        bAfterInI = FALSE;
                      if (!bAfterInI)
                      {	// coord after doesn't lie in interval
                        pt1 = CDoublePoint( pC2->dx, pC2->dy );
                        pDrawPoly->AddPoint(pt1);
                      }
                      else
                      {	// coord after lies in interval
                        double testy2;
                        
                        testy2 = pC1->dy-(pC1->dy-pC2->dy)*(pCrdAfter->dx-pC1->dx)/(pC2->dx-pC1->dx);
                        if (testy2>=pCrdAfter->dy)
                        {	// line between coord and coord-after intersects profil
                          double lambda, xacydc, yacxdc, ybaxdc, xbaydc;
                          
                          xacydc = (pCrd->dx-pC1->dx)*(pC2->dy-pC1->dy);
                          yacxdc = (pCrd->dy-pC1->dy)*(pC2->dx-pC1->dx);
                          ybaxdc = (pCrdAfter->dy-pCrd->dy)*(pC2->dx-pC1->dx);
                          xbaydc = (pCrdAfter->dx-pCrd->dx)*(pC2->dy-pC1->dy);
                          lambda = (xacydc-yacxdc)/(ybaxdc-xbaydc);
                          pt1.x = pCrd->dx+lambda*(pCrdAfter->dx-pCrd->dx);
                          pt1.y = pCrd->dy+lambda*(pCrdAfter->dy-pCrd->dy);
                          pDrawPoly->AddPoint(pt1);
                          intersections.SetAt(pt1.x, pt1.y);
                        }
                      }
                      if (bAfterInI || bBeforeInI)
                      {	// atleast one lies in interval
                        pt1 = CDoublePoint( pCrd->dx, testy1 );
                        pDrawPoly->AddPoint(pt1);
                      }
                      if (!bBeforeInI)
                      {	// coord before doesn't lie in interval
                        pt1 = CDoublePoint( pC1->dx, pC1->dy );
                        pDrawPoly->AddPoint(pt1);
                      }
                      else
                      {	// coord before lies in interval
                        double testy2;
                        
                        testy2 = pC1->dy-(pC1->dy-pC2->dy)*(pCrdBefore->dx-pC1->dx)/(pC2->dx-pC1->dx);
                        if (testy2>=pCrdBefore->dy)
                        {	// line between coord and coord-before intersects profil
                          double lambda, xacydc, yacxdc, ybaxdc, xbaydc;
                          
                          xacydc = (pCrdBefore->dx-pC1->dx)*(pC2->dy-pC1->dy);
                          yacxdc = (pCrdBefore->dy-pC1->dy)*(pC2->dx-pC1->dx);
                          ybaxdc = (pCrd->dy-pCrdBefore->dy)*(pC2->dx-pC1->dx);
                          xbaydc = (pCrd->dx-pCrdBefore->dx)*(pC2->dy-pC1->dy);
                          lambda = (xacydc-yacxdc)/(ybaxdc-xbaydc);
                          pt1.x = pCrdBefore->dx+lambda*(pCrd->dx-pCrdBefore->dx);
                          pt1.y = pCrdBefore->dy+lambda*(pCrd->dy-pCrdBefore->dy);
                          pDrawPoly->AddPoint(pt1);
                          intersections.SetAt(pt1.x, pt1.y);
                        }
                      }
                    }
                  }
                }
              }
              if (pt1.x>xfirst)
              {
                double prevy, prevx = 1e36;
                
                for (i=0; i<pGHBlock->GetNumCoords(); i++)
                {
                  pCrd = pGHBlock->GetCoordAt(i);
                  if (prevx<=xfirst && pCrd->dx>=xfirst)
                  {
                    pt1.x = xfirst;
                    pt1.y = prevy-(prevy-pCrd->dy)*(xfirst-prevx)/(pCrd->dx-prevx);
                    pDrawPoly->AddPoint(pt1);
                  }
                  prevx = pCrd->dx;
                  prevy = pCrd->dy;
                }
              }
              AddObject( pDrawPoly, profil, 0 );
              }
            }
            // Normal polyline
            if( ( type == DST_WASSERSPIEGEL || type == DST_WASSERSP1 ||
              type==DST_WASSERSP100 || type==DST_WASSERSP5 || type==DST_WASSERSPIEGEL_2 ) && pGHBlock!=NULL )
            {
              Coord* pCrd = pDBlock->GetFirstCoord();
              while (pCrd!=NULL)
              {
                double dummy;
                
                if (!intersections.Lookup(pCrd->dx, dummy))	// 15.09.99
                  intersections.SetAt(pCrd->dx, pCrd->dy);
                pCrd = pDBlock->GetNextCoord();
              }
              double xprev = -1e36;
              double x, y;
              CList<double, double> xlist;
              POSITION pos;
              BOOL bNewPoly = TRUE;
              BOOL bLastWasIntersection = FALSE;
              
              while (xlist.GetCount()<intersections.GetCount())
              {
                double xlast = 1e36;
                pos = intersections.GetStartPosition();
                while (pos!=NULL)
                {
                  intersections.GetNextAssoc(pos, x, y);
                  if (x>xprev && x<xlast)
                    xlast = x;
                }
                xlist.AddTail(xlast);
                xprev = xlast;
              }
              pos = xlist.GetHeadPosition();
              while (pos!=NULL)
              {
                x = xlist.GetNext(pos);
                if (intersections.Lookup(x, y))
                {
                  pt1.x = x;
                  pt1.y = y;
                  int numcoord=pGHBlock->GetNumCoords();
                  for( int j = 0; j < numcoord - 1; j++ )
                  {
                    Coord *pC1, *pC2;
                    pC1 = pGHBlock->GetCoordAt(j);
                    pC2 = pGHBlock->GetCoordAt(j+1);
                    if ((x>=pC1->dx && (x<pC2->dx && j!=numcoord-2 || x<=pC2->dx && j==numcoord-2 )) 
                      || (x<=pC1->dx && (x>pC2->dx && j!=numcoord-2 || x>=pC2->dx && j==numcoord-2 )))
                    {	// the coord falls in this interval
                      double testy1;	// height of profil at the coord for GH
                      
                      testy1 = pC2->dy-(pC2->dy-pC1->dy)*(x-pC2->dx)/(pC1->dx-pC2->dx);
                      if (y>testy1 || fabs(testy1-y)<1e-4 || x==pC1->dx || x==pC2->dx)
                      {
                        if (bLastWasIntersection && !bNewPoly)
                        {
                          // store for connection to legend
                          ProfilObjs.AddTailObject( pDrawPoly );
                          AddObject( pDrawPoly, profil, 0 );
                          bNewPoly = TRUE;
                        }
                        
                        if (bNewPoly)
                        {
                          pDrawPoly = (CDrawPoly*)pTemp->GetTemplateObj(TMPL_PROFIL_POLYLINE, type)->Clone(this);
                          pDrawPoly->SetSectionIndex(secIndex);
                          pDrawPoly->SetDataBlockIndex(dbIndex);
                          pDrawPoly->m_dPosition = pbrect;
                          pDrawPoly->SetType(type);
                          bNewPoly = FALSE;
                        }
                        if(y<pC2->dy && y>=pC1->dy)
                        {
                          bLastWasIntersection = TRUE;
                        }
                        else                             
                          bLastWasIntersection = FALSE;
                        pDrawPoly->AddPoint(pt1);
                        break;
                      }
                      else if (!bNewPoly)
                      {
                        // store for connection to legend
                        ProfilObjs.AddTailObject( pDrawPoly );
                        AddObject( pDrawPoly, profil, 0 );
                        bNewPoly = TRUE;
                        break;
                      }
                      
                    }
                  }
                }
              }
              if (!bNewPoly)
              {
                // store for connection to legend
                ProfilObjs.AddTailObject( pDrawPoly );
                AddObject( pDrawPoly, profil, 0 );
                bNewPoly = TRUE;
              }
            }
              else if(pDrawPoly) // Dick 
              {
                Coord* pCrd = pDBlock->GetFirstCoord();
                pDrawPoly = (CDrawPoly*)pTemp->GetTemplateObj(TMPL_PROFIL_POLYLINE, type)->Clone(this);
                pDrawPoly->SetSectionIndex(secIndex);
                pDrawPoly->SetDataBlockIndex(dbIndex);
                pDrawPoly->m_dPosition = pbrect;
                pDrawPoly->SetType(type);
                while (pCrd!=NULL)
                {
                  pt1 = CDoublePoint( pCrd->dx, pCrd->dy );
                  pDrawPoly->AddPoint(pt1);
                  pCrd = pDBlock->GetNextCoord();
                }
                // store for connection to legend
                ProfilObjs.AddTailObject( pDrawPoly );
                AddObject( pDrawPoly, profil, 0 );
              }
          }
          break;
          
        case DST_TRAPEZ:
          {
            // trapezium polyline
            CDrawPoly* pDrawPoly = (CDrawPoly*)pTemp->GetTemplateObj(TMPL_PROFIL_POLYLINE, type)->Clone(this);
            pDrawPoly->SetSectionIndex(secIndex);
            pDrawPoly->SetDataBlockIndex(dbIndex);
            pDrawPoly->m_dPosition = pbrect;
            pDrawPoly->SetType(type);
            pt1 = CDoublePoint(pDBlock->GetDVar(4), pDBlock->GetDVar(5));
            pDrawPoly->AddPoint(pt1);
            pt1 = CDoublePoint(pDBlock->GetDVar(4)+pDBlock->GetDVar(0)/2, pDBlock->GetDVar(5));
            pDrawPoly->AddPoint(pt1);
            pt1 = CDoublePoint(pDBlock->GetDVar(4)+pDBlock->GetDVar(0)/2+pDBlock->GetDVar(1)/pDBlock->GetDVar(2), pDBlock->GetDVar(5)+pDBlock->GetDVar(1));
            pDrawPoly->AddPoint(pt1);
            pt1 = CDoublePoint(pDBlock->GetDVar(4)-pDBlock->GetDVar(0)/2-pDBlock->GetDVar(1)/pDBlock->GetDVar(2), pDBlock->GetDVar(5)+pDBlock->GetDVar(1));
            pDrawPoly->AddPoint(pt1);
            pt1 = CDoublePoint(pDBlock->GetDVar(4)-pDBlock->GetDVar(0)/2, pDBlock->GetDVar(5));
            pDrawPoly->AddPoint(pt1);
            pt1 = CDoublePoint(pDBlock->GetDVar(4), pDBlock->GetDVar(5));
            pDrawPoly->AddPoint(pt1);
            // store for connection to legend
            ProfilObjs.AddTailObject( pDrawPoly );
            AddObject( pDrawPoly, profil, 0 );
          }
          break;

      case DST_WSP_HOEHE:
      case DST_WSP_MESSUNG:
        {
          // jeweils 2 Koordinaten bilden ein Stück Wasserspiegel

          CDrawObjList FillObjs; // alle Füllungen merken, werden später 'connected'
          Coord* pCoord = pDBlock->GetFirstCoord();
          while( pCoord != NULL )
          {
            // pro Abschnitt gibts eine neue Polylinie: eine horizontale Linie

            // die zweite Koordinate des Abschnitts holen: sollte immer existieren, falls nicht: abbrechen
            Coord* pCoord2 = pDBlock->GetNextCoord();
            if( pCoord2 == NULL )
              break;
  
            //
            // die Linie
            //
            {
              CDrawPoly* pObj = (CDrawPoly*)pTemp->GetTemplateObj( TMPL_PROFIL_POLYLINE, type )->Clone( this);
              pObj->SetSectionIndex( secIndex );
              pObj->SetDataBlockIndex( dbIndex );
              pObj->m_dPosition = pbrect;
              pObj->SetType( type );
              
              CDoublePoint pt = CDoublePoint( pCoord->dx, pCoord->dy );
              CDoublePoint pt2 = CDoublePoint( pCoord2->dx, pCoord2->dy );
              pObj->AddPoint( pt );
              pObj->AddPoint( pt2 );
              
              ProfilObjs.AddTailObject( pObj ); // um mit der TabellenLegende Connected zu werden
              AddObject( pObj, profil, 0 );
            }

            //
            // die Füllung
            //
            if( pDBGel )
            {
              CDrawPoly* pObj = (CDrawPoly*)pTemp->GetTemplateObj( TMPL_PROFIL_POLYGON, type )->Clone( this );
              pObj->SetSectionIndex( secIndex );
              pObj->SetDataBlockIndex( dbIndex );
              pObj->m_dPosition = pbrect;
              pObj->SetType( type );
              
              CDoublePoint pt = CDoublePoint( pCoord->dx, pCoord->dy );
              CDoublePoint pt2 = CDoublePoint( pCoord2->dx, pCoord2->dy );
              
              // der 'Deckel'
              pObj->AddPoint( pt2 );
              pObj->AddPoint( pt );
              
              // der Boden = Gelaende
              Coord* pCrd = pDBGel->GetFirstCoord();
              while( pCrd && pCrd->dx < pt.x )
                pCrd = pDBGel->GetNextCoord();

              while( pCrd && pCrd->dx <= pt2.x )
              {
                pObj->AddPoint( CDoublePoint( pCrd->dx, pCrd->dy ) );

                pCrd = pDBGel->GetNextCoord();
              }

              FillObjs.AddTailObject( pObj ); // um mit der TabellenLegende Connected zu werden
              AddObject( pObj, profil, 0 );
            }

            pCoord = pDBlock->GetNextCoord();
          } // while pCoord

          // connect stored profil objects together
          POSITION fillPos = FillObjs.GetHeadPosition();
          while( fillPos )
          {
            CDrawObj* pFillObj = FillObjs.GetNextObject( fillPos );
            POSITION pos2 = FillObjs.GetHeadPosition();
            while( pos2 )
            {
              CDrawObj* pObj2 = FillObjs.GetNextObject( pos2 );
              if( pFillObj != pObj2 )
                pFillObj->AddConnection( pObj2 );
            }
          }
        } // DST_WSP_HOEHE
        break;


        case DST_TRENNFLAECHEN:		// rectangles are drawn
        case DST_DURCHST_BEREICH:
        case DST_BOESCHUNGSKANTEN:
        case DST_MODELLGRENZEN:
        case DST_BORDVOLL:
        case DST_TRENN_WEHR:
          {
            // two vertical lines
            Coord* pCrd = pDBlock->GetFirstCoord();
            while (pCrd!=NULL)
            {
              pt1 = CDoublePoint( pCrd->dx, pCrd->dy );
              // draw vertical line for this point
              CDoubleIRect rect( pt1.x, pbrect.top, pt1.x, pbbl.y );
              CDrawRect* pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_PROFIL_POLYLINE, type)->Clone(this);
              pDrawRect->SetSectionIndex(secIndex);
              pDrawRect->SetDataBlockIndex(dbIndex);
              pDrawRect->m_dPosition = rect;
              pDrawRect->SetShape(CDrawRect::line);
              pDrawRect->SetType(type);
              // store for connection to legend
              ProfilObjs.AddTailObject( pDrawRect );
              AddObject( pDrawRect, profil, 0 );
              pCrd = pDBlock->GetNextCoord();
            }
          }
          break;
          
        case DST_KREISSEGM:
          {
            // arc
            Coord* pCrd = pDBlock->GetFirstCoord();
            VERIFY(pCrd!=NULL);
            pt1 = CDoublePoint( pCrd->dx, pCrd->dy );
            pCrd = pDBlock->GetNextCoord();
            VERIFY(pCrd!=NULL);
            pt2 = CDoublePoint( pCrd->dx, pCrd->dy );
            pCrd = pDBlock->GetNextCoord();
            VERIFY(pCrd!=NULL);
            CDoublePoint pt3 = CDoublePoint( pCrd->dx, pCrd->dy );
            CDoubleIRect rect( 1e36, -1e36, -1e36, 1e36 );
            rect.CompareAndExpand(pt1);
            rect.CompareAndExpand(pt2);
            rect.CompareAndExpand(pt3);
            CDrawRect* pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_PROFIL_POLYLINE, type)->Clone(this);
            pDrawRect->SetSectionIndex(secIndex);
            pDrawRect->SetDataBlockIndex(dbIndex);
            pDrawRect->m_dPosition = rect;
            pDrawRect->SetShape(CDrawRect::arc);
            pDrawRect->SetType(type);
            pDrawRect->SetArcLeft(pt1);
            pDrawRect->SetArcCenter(pt2);
            pDrawRect->SetArcRight(pt3);
            // store for connection to legend
            ProfilObjs.AddTailObject( pDrawRect );
            AddObject( pDrawRect, profil, 0 );
          }
          break;
          
        case DST_MAUL:
          {
            // mouth
            CDoubleIRect rect( pDBlock->GetDVar(3)-pDBlock->GetDVar(0)/2,
              pDBlock->GetDVar(4)+3*pDBlock->GetDVar(0)/4,
              pDBlock->GetDVar(3)+pDBlock->GetDVar(0)/2,
              pDBlock->GetDVar(4) );
            CDrawRect* pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_PROFIL_POLYLINE, type)->Clone(this);
            pDrawRect->SetSectionIndex(secIndex);
            pDrawRect->SetDataBlockIndex(dbIndex);
            pDrawRect->m_dPosition = rect;
            pDrawRect->SetShape(CDrawRect::mouth);
            pDrawRect->SetType(type);
            // store for connection to legend
            ProfilObjs.AddTailObject( pDrawRect );
            AddObject( pDrawRect, profil, 0 );
          }
          break;
          
        case DST_EIPROFIL:
          {
            // egg
            CDoubleIRect rect( pDBlock->GetDVar(3)-pDBlock->GetDVar(0)/2,
              pDBlock->GetDVar(4)+3*pDBlock->GetDVar(0)/2,
              pDBlock->GetDVar(3)+pDBlock->GetDVar(0)/2,
              pDBlock->GetDVar(4) );
            CDrawRect* pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_PROFIL_POLYLINE, type)->Clone(this);
            pDrawRect->SetSectionIndex(secIndex);
            pDrawRect->SetDataBlockIndex(dbIndex);
            pDrawRect->m_dPosition = rect;
            pDrawRect->SetShape(CDrawRect::egg);
            pDrawRect->SetType(type);
            // store for connection to legend
            ProfilObjs.AddTailObject( pDrawRect );
            AddObject( pDrawRect, profil, 0 );
          }
          break;
          
        case DST_KREIS:
          {
            // ellipse
            CDoubleIRect rect( pDBlock->GetDVar(2)-pDBlock->GetDVar(0)/2,
              pDBlock->GetDVar(3)+pDBlock->GetDVar(0),
              pDBlock->GetDVar(2)+pDBlock->GetDVar(0)/2,
              pDBlock->GetDVar(3) );
            CDrawRect* pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_PROFIL_POLYLINE, type)->Clone(this);
            pDrawRect->SetSectionIndex(secIndex);
            pDrawRect->SetDataBlockIndex(dbIndex);
            pDrawRect->m_dPosition = rect;
            pDrawRect->SetShape(CDrawRect::ellipse);
            pDrawRect->SetType(type);
            // store for connection to legend
            ProfilObjs.AddTailObject( pDrawRect );
            AddObject( pDrawRect, profil, 0 );
          }
          break;
          
        case DST_ARMCO84:
        case DST_ARMCO71:
          break;
          
        case DST_DKUK:
        case DST_DKOK:
          {
            // T shape
            Coord* pCrd = pDBlock->GetFirstCoord();
            while (pCrd!=NULL)
            {
              pt1 = CDoublePoint( pCrd->dx, pCrd->dy );
              if (!shoehe_heights.Lookup(pt1.x, bottom))
                bottom = pbrect.bottom;
              // draw vertical line for this point
              CDoubleIRect rect( pt1.x, pt1.y, pt1.x, bottom );
              CDrawRect* pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_PROFIL_POLYLINE, type)->Clone(this);
              pDrawRect->SetSectionIndex(secIndex);
              pDrawRect->SetDataBlockIndex(dbIndex);
              pDrawRect->m_dPosition = rect;
              pDrawRect->SetShape( CDrawRect::line );
              pDrawRect->SetType(type);
              
              // store for connection to legend
              ProfilObjs.AddTailObject( pDrawRect );
              AddObject( pDrawRect, profil, 0 );
              
              // draw horizontal line for this point
              POSITION pos1 = stations.Find( pt1.x );
              double left = 0;
              if (pos1!=NULL)
              {
                left = stations.GetPrev(pos1);
                if (pos1!=NULL)
                  left = stations.GetPrev(pos1);
                left = pt1.x - (pt1.x-left)/2;
              }
              pos1 = stations.Find(pt1.x);
              double right = 0;
              if (pos1!=NULL)
              {
                right = stations.GetNext(pos1);
                if (pos1!=NULL)
                  right = stations.GetNext(pos1);
                right = pt1.x - (pt1.x-right)/2;
              }
              rect.SetRect(left, pt1.y, right, pt1.y);
              pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_PROFIL_POLYLINE, type)->Clone(this);
              pDrawRect->SetSectionIndex(secIndex);
              pDrawRect->SetDataBlockIndex(dbIndex);
              pDrawRect->m_dPosition = rect;
              pDrawRect->SetShape(CDrawRect::line);
              pDrawRect->SetType(type);
              
              // store for connection to legend
              ProfilObjs.AddTailObject( pDrawRect );
              AddObject( pDrawRect, profil, 0 );
              pCrd = pDBlock->GetNextCoord();
            }
          }
          break;
          
        case DST_STATION:
          // stationierung
          {
            POSITION pos1 = stations.GetHeadPosition();
            while( pos1 )
            {
              pt1.x = stations.GetNext(pos1);
              heights.Lookup(pt1.x, pt1.y);
              CDoubleIRect rect( pt1.x, pt1.y, pt1.x, pbbl.y );
              CDrawRect* pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_PROFIL_POLYLINE, type)->Clone(this);
              pDrawRect->SetSectionIndex(secIndex);
              pDrawRect->SetDataBlockIndex(dbIndex);
              pDrawRect->m_dPosition = rect;
              pDrawRect->SetShape(CDrawRect::line);
              pDrawRect->SetType(type);
              // store for connection to legend
              ProfilObjs.AddTailObject( pDrawRect );
              AddObject( pDrawRect, profil, 0 );
            }
          }
          break;
          
        case DST_LP_TEXT:
          {
            const TextBlock* tb = pDBlock->GetTextBlock();
            DataBlock* dbBezug = tb ?  tb->GetBezugsdatensatz() : 0;
            if( tb && dbBezug )
            {
              for( int i = 0; i < tb->GetTextSatzCount(); i++ )
              {
                const TextBlock::TextSatz& ts = tb->GetTextSatz( i );

                // position des Textes ausrechnen
                int crdRefNum = ts.stationsNummer;
                Coord* crdBezug = dbBezug->GetCoordAt( crdRefNum );
                Coord* crdPrev = dbBezug->GetCoordAt( crdRefNum - 1 );
                Coord* crdNext = dbBezug->GetCoordAt( crdRefNum + 1 );

                if( !crdBezug )
                  continue; // falls keine Coordinate gefunden -> keine Ausgabe möglich
                
                // maximale Höhe des Profil an dieser Stelle ermitteln
                double hoehe;
                if( !heights.Lookup( crdBezug->dx, hoehe ) )
                  hoehe = crdBezug->dy; // im Zweifelsfalle die Sohlhöhe selbst

                // keine Ausdehnung in y-Richtung und x Richtung
                // erst bei Umrechung in logische Koordinaten wird die Höhe und Breite automatisch gesetzt
                CDoubleIRect rect( crdBezug->dx, hoehe, crdBezug->dx, hoehe ); 

                // Drawobject erzeugen und zu Profil hinzufügen
                CDrawRect* pDrawRect = (CDrawRect*)pTemp->GetTemplateObj( TMPL_PROFIL_POLYLINE, DST_LP_TEXT )->Clone( this);
                pDrawRect->SetSectionIndex( secIndex );
                pDrawRect->SetDataBlockIndex( dbIndex );
                pDrawRect->m_dPosition = rect;
                CString text;
                for( int j = 0; j < ts.strings.GetSize(); j++ )
                {
                  if( j != 0 )
                    text += " - ";
                  text += ts.strings[j];
                };
                pDrawRect->SetText( text );
                ProfilObjs.AddTailObject( pDrawRect );
                AddObject( pDrawRect, profil, 0 );
              }; // for i

            }; // if dbBezug 
          }; // LP_TEXT
          break;

        case DST_BAUWERK:
          {
          // store current DBlock pos
            POSITION pos1 = m_pProfil->GetDataBlockPos();
            if (pDBlock->GetNVar(0)<=m_pProfil->GetNumDataBlocks())
            {
              DataBlock *pDBTemp;
              int count = 1;
              
              pDBTemp = m_pProfil->GetFirstDataBlock();
              while (pDBTemp!=NULL)
              {
                if (count==pDBlock->GetNVar(0))
                {
                  for( int i = 0; i<pDBlock->GetNVar(1); i++ )
                  {
                    count = 1;
                    Coord* pCrd = pDBTemp->GetFirstCoord();
                    while (pCrd!=NULL)
                    {
                      if (count==pDBlock->GetNVar(i+2))//Dick 10.02.00 1->2
                      {
                        // foundations
                        CDoubleIRect rect;
                        if (pDBlock->GetDVar(i*3+2)==0)
                          rect.SetRect(pCrd->dx-pDBlock->GetDVar(i*3+1)/2, pCrd->dy+pDBlock->GetDVar(i*3)/6, pCrd->dx+pDBlock->GetDVar(i*3+1)/2, pCrd->dy-pDBlock->GetDVar(i*3)/6);
                        else
                          rect.SetRect(pCrd->dx-pDBlock->GetDVar(i*3+1)/2, pCrd->dy - (pDBlock->GetDVar(i*3+2)>0.0?pDBlock->GetDVar(i*3)/2:-(pDBlock->GetDVar(i*3)/2)), pCrd->dx+pDBlock->GetDVar(i*3+1)/2, pCrd->dy + (pDBlock->GetDVar(i*3+2)>0.0?-pDBlock->GetDVar(i*3+2):-pDBlock->GetDVar(i*3+2)));
                        double test =pCrd->dy + (pDBlock->GetDVar(i*3+2)>0.0?pDBlock->GetDVar(i*3+2):-(pDBlock->GetDVar(i*3+2)));
                        double test2=-(pDBlock->GetDVar(i*3+2));
                        double test3=pDBlock->GetDVar(i*3+2)>0.0?pDBlock->GetDVar(i*3+2):-(pDBlock->GetDVar(i*3+2)); 
                        
                        CDrawRect* pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_PROFIL_POLYLINE, type)->Clone(this);
                        pDrawRect->SetSectionIndex(secIndex);
                        pDrawRect->SetDataBlockIndex(dbIndex);
                        pDrawRect->m_dPosition = rect;
                        pDrawRect->SetShape(CDrawRect::rectangle);
                        pDrawRect->SetType(type);
                        if (pDBlock->GetDVar(i*3+1)==0)
                          pDrawRect->SetHorzAdjust(2*MM_FACTOR);
                        if (pDBlock->GetDVar(i*3)==0 && pDBlock->GetDVar(i*3+2)==0)
                          pDrawRect->SetVertAdjust(4*MM_FACTOR);
                        
                        // das erste Bauwerk mit Farblinie in der Legende verknüpfen
                        // die restlichen nur mit sich selbst
                        if ( i == 0 )
                          ProfilObjs.AddTailObject( pDrawRect );
                        AddObject( pDrawRect, profil, 0 );
                        CDrawRect* pDrawRectTemp = pDrawRect; // für connection zum 2. Teil
                        
                        // main rect
                        rect.SetRect(pCrd->dx-pDBlock->GetDVar(i*3+1)/2, pCrd->dy - (pDBlock->GetDVar(i*3+2)>0.0?(pDBlock->GetDVar(i*3)/2):pDBlock->GetDVar(i*3)/2), pCrd->dx+pDBlock->GetDVar(i*3+1)/2,pCrd->dy + pDBlock->GetDVar(i*3)-pDBlock->GetDVar(i*3+2));
                        
                        pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_PROFIL_POLYLINE, type)->Clone(this);
                        pDrawRect->SetSectionIndex(secIndex);
                        pDrawRect->SetDataBlockIndex(dbIndex);
                        pDrawRect->m_dPosition = rect;
                        pDrawRect->SetShape(CDrawRect::rectangle);
                        pDrawRect->SetType(type);
                        if (pDBlock->GetDVar(i*3)==0)
                          pDrawRect->SetVertAdjust(10*MM_FACTOR);
                        if (pDBlock->GetDVar(i*3+1)==0)
                          pDrawRect->SetHorzAdjust(2*MM_FACTOR);
                        
                        // das erste Bauwerk mit Farblinie in der Legende verknüpfen, 
                        // alle anderen nur mit sich selbst ( damit die einzelnen Bauwerke 
                        // unterschiedlich eingefärbt werden könenn
                        if ( i == 0 )
                          ProfilObjs.AddTailObject( pDrawRect );
                        else
                        {
                          pDrawRect->AddConnection( pDrawRectTemp );
                          pDrawRectTemp->AddConnection( pDrawRect );
                        };
                        AddObject( pDrawRect, profil, 0 );
                        break;
                      }
                      pCrd = pDBTemp->GetNextCoord();
                      count++;
                    }
                  }
                  break;
                }
                pDBTemp = m_pProfil->GetNextDataBlock();
                count++;
              }
            }
            // restore current DBlock pos
            m_pProfil->SetDataBlockPos(pos1);
          }
          break;
          
        default:	// only in table
          break;
      }// 

      ///////////////////
      // now the table //
      ///////////////////
      
      // TableKey1: die Bezeichner der Legende
      CDrawObjList* pObjList = new CDrawObjList;
      CDrawRect* pDrawRect = (CDrawRect*)pTemp->GetTemplateObj( TMPL_TABELLE_KEY1TEXT, type )->Clone( this );
      pDrawRect->SetSectionIndex( secIndex );
      pDrawRect->SetDataBlockIndex( dbIndex );
      pDrawRect->SetShape( CDrawRect::text );
      pDrawRect->SetTextType( CDrawRect::normal );
      pDrawRect->SetType( type );

      // zuerst die darzustellenden Texte finden: kann mehrzeilig sein
      CStringArray tableKey1Strings;

      // zuerst die erste Zeile
      // Bezeichnung aus dem Template übernehmen
      CString str;
      pDrawRect->GetText( str );
      tableKey1Strings.Add( str );

      // noch eine zweite Zeile?
      switch( type )
      {
      case DST_WSP_HOEHE:
      case DST_SOHLHOEHE_2:
      case DST_WASSERSPIEGEL:
      case DST_WASSERSPIEGEL_2:
      case DST_BOESCHUNG_LINKS_2:
      case DST_BOESCHUNG_RECHTS_2:
      case DST_RECHTSWERT_2:
      case DST_HOCHWERT_2:
      case DST_RE_BOESCHUNG_LI_2:
      case DST_HO_BOESCHUNG_LI_2:
      case DST_RE_BOESCHUNG_RE_2:
      case DST_HO_BOESCHUNG_RE_2:
      case DST_RE_DEICH_LI_2:
      case DST_HO_DEICH_LI_2:
      case DST_RE_DEICH_RE_2:
      case DST_HO_DEICH_RE_2:
      case DST_WSP_MESSUNG:
      case DST_WSP_FIXIERUNG:
      case DST_ABFLUSS:
      case DST_POL_GRENZEN:
      case DST_DEICH_RECHTS:
      case DST_DEICH_LINKS:
      case DST_DEICH_RECHTS_2:
      case DST_DEICH_LINKS_2:
        {
          // die zweite Zeile ist der Name der Berechnungsvariante vor dem @ Zeichen 
          CString name( pDBlock->GetName( 1 ) );
          int i = name.ReverseFind( '@' );
          if( i != -1 && i > 99 )
          {
            CString name2 = name.Mid( 99, i - 99 );
            name2.TrimLeft();
            name2.TrimRight();
            tableKey1Strings.Add( name2 );
          }

          // die dritte Zeile ist alles in der Namenszeile was vor der Kennung kommt
          CString comment = name.Left( 99 );
          comment.TrimLeft();
          comment.TrimRight();
          if( !comment.IsEmpty() )
            tableKey1Strings.Add( comment );
        }
        break;
      } // switch type

      // jetzt alle Strings in die Legende
      int tableKey1Count = tableKey1Strings.GetSize();
      ASSERT(tableKey1Count != 0 ); 
      double tableKey1Height = 1.0 / tableKey1Count;

      for( int i = 0; i < tableKey1Count; i++ )
      {
        CDrawRect* pTableKey1Rect = (CDrawRect*)pDrawRect->Clone( this, TRUE );
        
        CDoubleIRect rect( 0, 1 - i * tableKey1Height, 1, 1 - ( i + 1 ) * tableKey1Height );
        pTableKey1Rect->m_dPosition = rect;
        pTableKey1Rect->SetText( tableKey1Strings[i] );
        pTableKey1Rect->SetDataBlockIndex( dbIndex );
        pTableKey1Rect->SetSectionIndex( secIndex );
        pObjList->AddTailObject( pTableKey1Rect );
      }

      delete pDrawRect; // den Hilfsklon löschen
      pDrawRect = NULL;

      // noch die Umrandung der Texte: einfach ein Kasten
      for( i = 0; i < 4; i++ )
      {
        CDoubleIRect rect( 0, 1, 1, 0 );
        pDrawRect = (CDrawRect*)pTemp->GetTemplateObj( TMPL_TABELLE_LINE, type )->Clone( this );
        pDrawRect->SetSectionIndex(secIndex);
        pDrawRect->SetDataBlockIndex(dbIndex);
        pDrawRect->m_dPosition = rect;
        pDrawRect->SetShape(CDrawRect::line);
        pDrawRect->SetType(type);
        pDrawRect->SetHorzJust(i % 2 ? CDrawRect::nojust : (i==0 ? CDrawRect::left : CDrawRect::right));
        pDrawRect->SetVertJust(i % 2 ? (i==1 ? CDrawRect::left : CDrawRect::right) : CDrawRect::nojust);
        pObjList->AddTailObject( pDrawRect );
      }
      
      // Objekte unter einem neuen Index zur Tabelle hinzufügen
      int index1 = GetTableKey1()->CreateNewIndex();
      AddObjects( pObjList, tableKey1, index1 );
      delete pObjList;
      pObjList = NULL;

      // Draw table key2
      CDoubleIRect rect( 0, 0.75, 1, 0.25 );
      pObjList = new CDrawObjList;
      pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_TABELLE_KEY2TEXT, type)->Clone(this);
      pDrawRect->SetSectionIndex(secIndex);
      pDrawRect->SetDataBlockIndex(dbIndex);
      pDrawRect->m_dPosition = rect;
      pDrawRect->SetShape(CDrawRect::text);
//      if( pDBlock )
//      {
//        CString str = pDBlock->GetDesc(1);
//        pDrawRect->SetText(str);
//      };
      pDrawRect->SetTextType(CDrawRect::normal);
      pDrawRect->SetType(type);
      pObjList->AddTailObject( pDrawRect );
      // legend
      if( ProfilObjs.GetObjectCount() > 0 )
      {
        rect.SetRect(0, 0.25, 1, 0);
        pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_TABELLE_LINE, type)->Clone(this);
        pDrawRect->SetSectionIndex(secIndex);
        pDrawRect->SetDataBlockIndex(dbIndex);
        pDrawRect->m_dPosition = rect;
        pDrawRect->SetHorzJust(CDrawRect::nojust);
        pDrawRect->SetVertJust(CDrawRect::center);
        pDrawRect->SetShape(CDrawRect::line);
        pDrawRect->SetType(type);
        if( ProfilObjs.GetHeadObject()->IsInvisible() )
          pDrawRect->SetFlags( CDrawObj::invisible );
        POSITION pos1 = ProfilObjs.GetHeadPosition();
        while( pos1 != NULL )
          pDrawRect->AddConnection(ProfilObjs.GetNextObject(pos1));
        
        // connect stored profil objects together
        // TODO: das hauptperformanceloch beim CreateDrawing ist hier! zur Zeit n^3 !!!
        pos1 = ProfilObjs.GetHeadPosition();
        while( pos1 )
        {
          CDrawObj *pObj1 = ProfilObjs.GetNextObject( pos1 );
          POSITION pos2 = ProfilObjs.GetHeadPosition();
          while( pos2 )
          {
            CDrawObj *pObj2 = ProfilObjs.GetNextObject( pos2 );
            if( pObj1 != pObj2 )
              pObj1->AddConnection( pObj2 );
          }
        }
        pObjList->AddTailObject( pDrawRect );
      }
      // boxing
      rect.SetRect(0, 1, 1, 0);
      for( i = 0; i < 4; i++ )
      {
        pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_TABELLE_LINE, type)->Clone(this);
        pDrawRect->SetSectionIndex(secIndex);
        pDrawRect->SetDataBlockIndex(dbIndex);
        pDrawRect->m_dPosition = rect;
        pDrawRect->SetShape(CDrawRect::line);
        pDrawRect->SetType(type);
        pDrawRect->SetHorzJust(i % 2 ? CDrawRect::nojust : (i==0 ? CDrawRect::left : CDrawRect::right));
        pDrawRect->SetVertJust(i % 2 ? (i==1 ? CDrawRect::left : CDrawRect::right) : CDrawRect::nojust);
        pObjList->AddTailObject( pDrawRect );
      }

      // Objekte unter einem neuen Index zur Tabelle hinzufügen
      int index2 = GetTableKey2()->CreateNewIndex();
      AddObjects( pObjList, tableKey2, index2 );
      delete pObjList;
      pObjList = NULL;
      
      // rest of table
      pObjList = new CDrawObjList;

      double xcoord, ycoord;
      switch( type )
      {
      case DST_TRAPEZ:
        xcoord = pDBlock->GetDVar(4);
        ycoord = pDBlock->GetDVar(5);
        break;
        
      case DST_MAUL:
      case DST_EIPROFIL:
        xcoord = pDBlock->GetDVar(3);
        ycoord = pDBlock->GetDVar(4);
        break;
        
      case DST_KREIS:
        xcoord = pDBlock->GetDVar(2);
        ycoord = pDBlock->GetDVar(3);
        break;
        
      case DST_ARMCO84:
      case DST_ARMCO71:
        xcoord = pDBlock->GetDVar(2);
        ycoord = pDBlock->GetDVar(3);
        break;
        
      default:
        break;
      }

      // Koordinaten in Tabelle einfügen; dauert sehr lange
      switch( type )
      {
      case DST_GELAENDEHOEHE:		// coords in table
      case DST_UK_BRUECKE:
      case DST_OK_BRUECKE:
      case DST_OK_WEHRS:
      case DST_GELAENDE2:
      case DST_FLAECHE:
      case DST_OK_GELAENDE:
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
      case DST_TRENNFLAECHEN:
      case DST_DURCHST_BEREICH:
      case DST_BOESCHUNGSKANTEN:
      case DST_MODELLGRENZEN:
      case DST_BUHNEN:
      case DST_KREISSEGM:
      case DST_KASTEN:
      case DST_BORDVOLL:
      case DST_TRENN_WEHR:
      case DST_DKUK:
      case DST_DKOK:
      case DST_RAUHIGKEIT_KST:
      case DST_RAUHIGKEIT:
      case DST_AXM:
      case DST_AYM:
      case DST_DPM:
      case DST_SVA_WERT:
      case DST_LWA_FELDER:
      case DST_GAUSS:
      case DST_RECHTSWERT:
      case DST_HOCHWERT:
      case DST_PUNKT_NR:
      case DST_NWRINNE:
      case DST_LAENGE:
      case DST_WSP_BREITE:
      case DST_WSP_DIFFERENZ:
      case DST_PROFILART:
      case DST_VZKENNG:
      case DST_PROFILKENNG:
      case DST_GEFAELLE:
      case DST_VMITTEL:
      case DST_WSP_FIXIERUNG:
      case DST_WSP_MESSUNG:
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
      case DST_DEICH_RECHTS:
      case DST_DEICH_LINKS:
      case DST_DEICH_RECHTS_2:
      case DST_DEICH_LINKS_2:
      case DST_RE_DEICH_RE:
      case DST_HO_DEICH_RE:
      case DST_RE_DEICH_LI:
      case DST_HO_DEICH_LI:
      case DST_RE_DEICH_RE_2:
      case DST_HO_DEICH_RE_2:
      case DST_RE_DEICH_LI_2:
      case DST_HO_DEICH_LI_2:
      case DST_SCHLEPPSPANN:
      case DST_AUSUFERUNG_LI:
      case DST_AUSUFERUNG_RE:
      case DST_ENERGIEHOEHE:
      case DST_ABFLUSS: //12.04.2000 Dick
        {
          CTypedPtrArray<CObArray, Coord*> tempCoords;		// array of coordinates in strictly increasing order (used for table to ensure correct formatting)
          
          Coord* pCrd = pDBlock->GetFirstCoord();
          while( pCrd != NULL )
          {
            BOOL bInserted = FALSE;
            for( i = 0; i < tempCoords.GetSize() && !bInserted; i++ )
            {
              if( pCrd->dx < tempCoords[i]->dx )
              {
                tempCoords.InsertAt( i, pCrd );
                bInserted = TRUE;
              }
            }
            if( !bInserted )
              tempCoords.Add( pCrd );
            pCrd = pDBlock->GetNextCoord();
          }

          if( tempCoords.GetSize() > 0 )
            pt2 = CDoublePoint( tempCoords[0]->dx, tempCoords[0]->dy );
          
          CDrawRect* pXObj = (CDrawRect*)pTemp->GetTemplateObj( TMPL_TABELLE_XCOORD, type )->Clone(this);
          pXObj->SetSectionIndex(secIndex);
          pXObj->SetDataBlockIndex(dbIndex);
          pXObj->SetTextType(CDrawRect::xcoord);
          pXObj->SetType(type);
          pXObj->SetShape(CDrawRect::text);

          CDrawRect* pYObj = (CDrawRect*)pTemp->GetTemplateObj( TMPL_TABELLE_YCOORD, type )->Clone(this);
          pYObj->SetSectionIndex(secIndex);
          pYObj->SetDataBlockIndex(dbIndex);
          pYObj->SetShape(CDrawRect::text);
          pYObj->SetTextType(CDrawRect::ycoord);
          pYObj->SetType(type);

          CDrawRect* pLObj = (CDrawRect*)pTemp->GetTemplateObj( TMPL_TABELLE_LINE, type )->Clone(this);
          pLObj->SetFlags( CDrawObj::hideable );
          pLObj->SetSectionIndex(secIndex);
          pLObj->SetDataBlockIndex(dbIndex);
          pLObj->SetShape(CDrawRect::line);
          pLObj->SetType(type);

          for( i = 0; i < tempCoords.GetSize(); i++ )
          {
            pCrd = tempCoords[i];
            pt1 = CDoublePoint( pCrd->dx, pCrd->dy );
            rect.SetRect(pt1.x, 1, pt1.x, 0);
            
            CString str;

            // x coord text
            pDrawRect = (CDrawRect*)pXObj->Clone(this);
			pDrawRect->SetSectionIndex( secIndex );
            pDrawRect->m_dPosition = rect;
            str.Format("%.10f", pCrd->dx);
            pDrawRect->SetText(str);
            pObjList->AddTailObject( pDrawRect );	
            
            // y coord text
            pDrawRect = (CDrawRect*)pYObj->Clone(this);
			pDrawRect->SetSectionIndex( secIndex );
            pDrawRect->m_dPosition = rect;
            str.Format("%.10f", pCrd->dy);
            pDrawRect->SetText(str);
            pObjList->AddTailObject( pDrawRect );
            
            // vertical dividor lines
            if( pt2.x == pt1.x )
              rect.SetRect( pt2.x, 1, pbrect.right, 0 );
            else
              rect.SetRect( pt2.x, 1, pt1.x, 0 );

            pDrawRect = (CDrawRect*)pLObj->Clone(this);
			pDrawRect->SetSectionIndex( secIndex );
            pDrawRect->SetHorzJust( CDrawRect::left );
            pDrawRect->m_dPosition = rect;
            pObjList->AddTailObject( pDrawRect );	// only add left line for the moment

            pt2 = CDoublePoint( pCrd->dx, pCrd->dy );
            if( i + 1 >= tempCoords.GetSize() || pDBlock->GetNumCoords() == 1 )
            {
              pDrawRect = (CDrawRect*)pLObj->Clone( this);
			  pDrawRect->SetSectionIndex( secIndex );
              pDrawRect->SetHorzJust( CDrawRect::right );
              pDrawRect->m_dPosition = rect;
              
              pObjList->AddTailObject( pDrawRect );		// add the right line
            }
          }

          delete pXObj; pXObj = NULL;
          delete pYObj; pYObj = NULL;
          delete pLObj; pLObj = NULL;
        }
        break;	

        case DST_POL_GRENZEN:
        {
          CDrawRect* pTextTemplateObj = (CDrawRect*)pTemp->GetTemplateObj( TMPL_TABELLE_NORMTEXT, type );
          CDrawRect* pKastenTemplateObj = (CDrawRect*)pTemp->GetTemplateObj( TMPL_TABELLE_LINE, type );

          for( int l = 0; l < pDBlock->GetBereichCount(); l++ )
          {
            const DataBlock::Bereich bereich = pDBlock->GetBereich( l );

            Rect<double, 1> rct( bereich.from, 1, bereich.to, 0 );

            CDrawRect* pTextObj = (CDrawRect*)pTextTemplateObj->Clone( this );
            pTextObj->SetFlags( CDrawObj::hideable );
            pTextObj->SetSectionIndex( secIndex );
            pTextObj->SetDataBlockIndex( dbIndex );
            pTextObj->SetShape( CDrawRect::text ); 
            pTextObj->SetType( type );
            pTextObj->SetHorzJust( CDrawRect::center );
            pTextObj->m_dPosition = rct;
            pTextObj->SetText( bereich.text );

            CDrawRect* pKastenObj = (CDrawRect*)pKastenTemplateObj->Clone( this );
            pKastenObj->SetFlags( CDrawObj::hideable | CDrawObj::filled | CDrawObj::brush );
            pKastenObj->SetSectionIndex( secIndex );
            pKastenObj->SetDataBlockIndex( dbIndex );
            pKastenObj->SetShape( CDrawRect::rectangle ); 
            pKastenObj->SetType( type );
            pKastenObj->SetHorzJust( CDrawRect::left );
            pKastenObj->m_dPosition = rct;

            pKastenObj->SetLogBrush( bereich.color, bereich.style, bereich.hatch );
  
            pObjList->AddTailObject( pKastenObj );
            pObjList->AddTailObject( pTextObj );
          }
        }
        break;	


      case DST_STATION:
        {
          if( !stations.IsEmpty() )
            pt2.x = stations.GetHead();
          heights.Lookup( pt2.x, pt2.y );
          int j = 0;

          // erstmal die TemplateObjekte holen
          CDrawRect* pXObj = (CDrawRect*)pTemp->GetTemplateObj( TMPL_TABELLE_XCOORD, type )->Clone( this);
          pXObj->SetSectionIndex(secIndex);
          pXObj->SetDataBlockIndex(dbIndex);
          pXObj->SetShape(CDrawRect::text);
          pXObj->SetTextType(CDrawRect::xcoord);
          pXObj->SetType(type);

          CDrawRect* pYObj = (CDrawRect*)pTemp->GetTemplateObj( TMPL_TABELLE_YCOORD, type )->Clone( this);
          pYObj->SetSectionIndex(secIndex);
          pYObj->SetDataBlockIndex(dbIndex);
          pYObj->SetShape(CDrawRect::text);
          pYObj->SetTextType(CDrawRect::ycoord);
          pYObj->SetType(type);

          CDrawRect* pLObj = (CDrawRect*)pTemp->GetTemplateObj( TMPL_TABELLE_LINE, type )->Clone( this);
          pLObj->SetFlags( CDrawObj::hideable );
          pLObj->SetSectionIndex(secIndex);
          pLObj->SetDataBlockIndex(dbIndex);
          pLObj->SetShape(CDrawRect::line);
          pLObj->SetType(type);
         
          POSITION pos1 = stations.GetHeadPosition();
          while( pos1 != NULL )
          {
            pt1.x = stations.GetNext( pos1 );
            j++;
            heights.Lookup(pt1.x, pt1.y);

            CDoubleIRect rect( pt1.x, 1, pt1.x, 0 );

            CString str;

            // x coord text
            pDrawRect = (CDrawRect*)pXObj->Clone(this);
			pDrawRect->SetSectionIndex( secIndex );
            pDrawRect->m_dPosition = rect;
            str.Format("%.10f", pt1.x);
            pDrawRect->SetText(str);
            pObjList->AddTailObject( pDrawRect );	
            
            // y coord text
            pDrawRect = (CDrawRect*)pYObj->Clone(this);
			pDrawRect->SetSectionIndex( secIndex );
            pDrawRect->m_dPosition = rect;
            str.Format("%.10f", pt1.y);
            pDrawRect->SetText(str);
            pObjList->AddTailObject( pDrawRect );	
            
            // vertical dividor lines
            if( pt2.x == pt1.x )
              rect.SetRect( pt2.x, 1, pbrect.right, 0 );
            else
              rect.SetRect( pt2.x, 1, pt1.x, 0 );

            pDrawRect = (CDrawRect*)pLObj->Clone(this);
			pDrawRect->SetSectionIndex( secIndex );
            pDrawRect->m_dPosition = rect;
            pDrawRect->SetHorzJust( CDrawRect::left );
            pObjList->AddTailObject( pDrawRect );	// only add left line for the moment

            if( j >= stations.GetCount() )
            {
              pDrawRect = (CDrawRect*)pLObj->Clone(this);
			  pDrawRect->SetSectionIndex( secIndex );
              pDrawRect->m_dPosition = rect;
              pDrawRect->SetHorzJust( CDrawRect::right );
              pObjList->AddTailObject( pDrawRect );		// add the right line
            }

            pt2 = pt1;
          }

          delete pXObj; pXObj = NULL;
          delete pYObj; pYObj = NULL;
          delete pLObj; pLObj = NULL;

        }
        break;
        
      case DST_TRAPEZ:
      case DST_MAUL:
      case DST_EIPROFIL:
      case DST_KREIS:
      case DST_ARMCO84:
      case DST_ARMCO71:
        {
          CString str;

          rect.SetRect(xcoord, 1, xcoord, 0);
          // x coord text
          pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_TABELLE_XCOORD, type)->Clone(this);
          pDrawRect->SetSectionIndex(secIndex);
          pDrawRect->SetDataBlockIndex(dbIndex);
          pDrawRect->m_dPosition = rect;
          pDrawRect->SetShape(CDrawRect::text);
          str.Format("%.10f", xcoord);
          pDrawRect->SetText(str);
          pDrawRect->SetTextType(CDrawRect::xcoord);
          pDrawRect->SetType(type);
          pObjList->AddTailObject( pDrawRect );	
          // y coord text
          pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_TABELLE_YCOORD, type)->Clone(this);
          pDrawRect->SetSectionIndex(secIndex);
          pDrawRect->SetDataBlockIndex(dbIndex);
          pDrawRect->m_dPosition = rect;
          pDrawRect->SetShape(CDrawRect::text);
          str.Format("%.10f", ycoord);
          pDrawRect->SetText(str);
          pDrawRect->SetTextType(CDrawRect::ycoord);
          pDrawRect->SetType(type);
          pObjList->AddTailObject( pDrawRect );	
          // vertical dividor line
          rect.SetRect(pbrect.left, 1, xcoord, 0);
          pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_TABELLE_LINE, type)->Clone(this);
          pDrawRect->SetFlags( CDrawObj::hideable );
          pDrawRect->SetSectionIndex(secIndex);
          pDrawRect->SetDataBlockIndex(dbIndex);
          pDrawRect->m_dPosition = rect;
          pDrawRect->SetShape(CDrawRect::line);
          pDrawRect->SetType(type);
          pDrawRect->SetHorzJust(CDrawRect::right);
          pObjList->AddTailObject( pDrawRect );
        }
        switch(type)
        {
        default:
          break;
          
        case DST_TRAPEZ:
          // first line
          {
            CString str;
            CString temp;
            rect.SetRect(xcoord, 1, pbrect.right, 0.66);
            pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_TABELLE_NORMTEXT, type)->Clone(this);
            pDrawRect->SetSectionIndex(secIndex);
            pDrawRect->SetDataBlockIndex(dbIndex);
            pDrawRect->m_dPosition = rect;
            pDrawRect->SetShape(CDrawRect::text);
            temp.Format("%.3f", pDBlock->GetDVar(3));
            str.FormatMessage(IDS_IS_PROMILLE, temp);
            pDrawRect->SetText(str);
            pDrawRect->SetTextType(CDrawRect::normal);
            pDrawRect->SetHorzJust(CDrawRect::center);
            pDrawRect->SetType(type);
            pObjList->AddTailObject(pDrawRect);
            // second line
            rect.SetRect(xcoord, 0.66, pbrect.right, 0.33);
            pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_TABELLE_NORMTEXT, type)->Clone(this);
            pDrawRect->SetSectionIndex(secIndex);
            pDrawRect->SetDataBlockIndex(dbIndex);
            pDrawRect->m_dPosition = rect;
            pDrawRect->SetShape(CDrawRect::text);
            temp.Format("%.3f", pDBlock->GetDVar(2));
            str.FormatMessage(IDS_GRADIENT, temp);
            pDrawRect->SetText(str);
            pDrawRect->SetTextType(CDrawRect::normal);
            pDrawRect->SetHorzJust(CDrawRect::center);
            pDrawRect->SetType(type);
            pObjList->AddTailObject( pDrawRect );	
            // third line
            rect.SetRect(xcoord, 0.33, pbrect.right, 0);
            pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_TABELLE_NORMTEXT, type)->Clone(this);
            pDrawRect->SetSectionIndex(secIndex);
            pDrawRect->SetDataBlockIndex(dbIndex);
            pDrawRect->m_dPosition = rect;
            pDrawRect->SetShape(CDrawRect::text);
            temp.Format("%.3f", pDBlock->GetDVar(1));
            str.FormatMessage(IDS_HEIGHTM, temp);
            pDrawRect->SetText(str);
            pDrawRect->SetTextType(CDrawRect::normal);
            pDrawRect->SetHorzJust(CDrawRect::center);
            pDrawRect->SetType(type);
            pObjList->AddTailObject( pDrawRect );
          }
          break;
          
        case DST_EIPROFIL:
        case DST_MAUL:
          {
            CString temp, str;
            // first line
            rect.SetRect(xcoord, 1, pbrect.right, 0.66);
            pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_TABELLE_NORMTEXT, type)->Clone(this);
            pDrawRect->SetSectionIndex(secIndex);
            pDrawRect->SetDataBlockIndex(dbIndex);
            pDrawRect->m_dPosition = rect;
            pDrawRect->SetShape(CDrawRect::text);
            temp.Format("%.3f", pDBlock->GetDVar(2));
            str.FormatMessage(IDS_IS_PROMILLE, temp);
            pDrawRect->SetText(str);
            pDrawRect->SetTextType(CDrawRect::normal);
            pDrawRect->SetHorzJust(CDrawRect::center);
            pDrawRect->SetType(type);
            pObjList->AddTailObject( pDrawRect );
            // second line
            rect.SetRect(xcoord, 0.66, pbrect.right, 0.33);
            pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_TABELLE_NORMTEXT, type)->Clone(this);
            pDrawRect->SetSectionIndex(secIndex);
            pDrawRect->SetDataBlockIndex(dbIndex);
            pDrawRect->m_dPosition = rect;
            pDrawRect->SetShape(CDrawRect::text);
            str.Format("Dn = %.3f mm", pDBlock->GetDVar(0));
            pDrawRect->SetText(str);
            pDrawRect->SetTextType(CDrawRect::normal);
            pDrawRect->SetHorzJust(CDrawRect::center);
            pDrawRect->SetType(type);
            pObjList->AddTailObject( pDrawRect );
            // third line
            rect.SetRect(xcoord, 0.33, pbrect.right, 0);
            pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_TABELLE_NORMTEXT, type)->Clone(this);
            pDrawRect->SetSectionIndex(secIndex);
            pDrawRect->SetDataBlockIndex(dbIndex);
            pDrawRect->m_dPosition = rect;
            pDrawRect->SetShape(CDrawRect::text);
            temp.Format("%.3f", pDBlock->GetDVar(1));
            str.FormatMessage(IDS_HEIGHTM, temp);
            pDrawRect->SetText(str);
            pDrawRect->SetTextType(CDrawRect::normal);
            pDrawRect->SetHorzJust(CDrawRect::center);
            pDrawRect->SetType(type);
            pObjList->AddTailObject( pDrawRect );
          }
          break;
          
        case DST_KREIS:
          {
            CString temp, str;
            // first line
            rect.SetRect(xcoord, 1, pbrect.right, 0.5);
            pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_TABELLE_NORMTEXT, type)->Clone(this);
            pDrawRect->SetSectionIndex(secIndex);
            pDrawRect->SetDataBlockIndex(dbIndex);
            pDrawRect->m_dPosition = rect;
            pDrawRect->SetShape(CDrawRect::text);
            temp.Format("%.3f", pDBlock->GetDVar(1));
            str.FormatMessage(IDS_IS_PROMILLE, temp);
            pDrawRect->SetText(str);
            pDrawRect->SetTextType(CDrawRect::normal);
            pDrawRect->SetHorzJust(CDrawRect::left);
            pDrawRect->SetType(type);
            pObjList->AddTailObject( pDrawRect );
            // second line
            rect.SetRect(xcoord, 0.5, pbrect.right, 0);
            pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_TABELLE_NORMTEXT, type)->Clone(this);
            pDrawRect->SetSectionIndex(secIndex);
            pDrawRect->SetDataBlockIndex(dbIndex);
            pDrawRect->m_dPosition = rect;
            pDrawRect->SetShape(CDrawRect::text);
            str.Format("Dn = %.3f mm", pDBlock->GetDVar(0));
            pDrawRect->SetText(str);
            pDrawRect->SetTextType(CDrawRect::normal);
            pDrawRect->SetHorzJust(CDrawRect::left);
            pDrawRect->SetType(type);
            pObjList->AddTailObject( pDrawRect );	
          }
          break;
          }
          break;
          
        default:
          break;
      } // switch Type
      // Right of table
      // top and bottom lines for whole of table row
      rect.SetRect(pbrect.left, 1, pbrect.right, 0);
      for( i = 0; i < 2; i++ )
      {
        pDrawRect = (CDrawRect*)pTemp->GetTemplateObj( TMPL_TABELLE_LINE, type )->Clone( this );
        pDrawRect->SetSectionIndex( secIndex );
        pDrawRect->SetDataBlockIndex( dbIndex );
        pDrawRect->m_dPosition = rect;
        pDrawRect->SetShape( CDrawRect::line );
        pDrawRect->SetType( type );
        pDrawRect->SetVertJust( i == 0 ? CDrawRect::left : CDrawRect::right );
        pObjList->AddTailObject( pDrawRect );
      }
      // vertical dividor line (must come after top and bottom lines!)
      rect.SetRect( pbrect.right, 1, pbrect.right, 0 );
      pDrawRect = (CDrawRect*)pTemp->GetTemplateObj( TMPL_TABELLE_LINE, type )->Clone( this );
      pDrawRect->SetSectionIndex( secIndex );
      pDrawRect->SetDataBlockIndex( dbIndex );
      pDrawRect->m_dPosition = rect;
      pDrawRect->SetShape( CDrawRect::line );
      pDrawRect->SetType( type );
      pDrawRect->SetHorzJust( CDrawRect::right );
      pObjList->AddTailObject( pDrawRect );

      // Objekte unter einem neuen Index zur Tabelle hinzufügen
      int index0 = GetTable()->CreateNewIndex();
      AddObjects( pObjList, table, index0 );
      delete pObjList;
      pObjList = NULL;
      
      if( pDBlock )
        pDBlock = m_pProfil->GetNextDataBlock();
      GETPLOTTERAPP->IncStatusBarProgress();

    } // while pDBlock

#ifdef _DEBUG
    CTime stopTime = CTime::GetCurrentTime();
    CTimeSpan durance = stopTime - startTime;
    CString duranceStr = durance.Format( "%D:%H:%M:%S" );
    afxDump << duranceStr;
#endif

    // now reorder table entries according to template
    int count = 0;
    for( int i = 0; i < pTemp->m_pPData->m_table.GetSize(); i++ )
    {
      BOOL bFound = FALSE;
      CList<int, int> rows_to_move;
      
      int type = pTemp->GetTable()->GetAt( i )->GetHeadObject()->GetType();
      for( int j = 0; j < m_pPData->m_table.GetSize(); j++ )
      {
        if( GetTable()->GetAt( j )->GetHeadObject()->GetType() == type )
          rows_to_move.AddTail( j );
      }
      
      POSITION pos = rows_to_move.GetHeadPosition();
      while( pos != NULL )
      {
        int j = rows_to_move.GetNext( pos );
        CDrawObjList* pObjList = GetTable()->GetAt( j );
        GetTable()->RemoveIndex( j );
        GetTable()->InsertIndex( count, pObjList, TRUE );
        
        pObjList = GetTableKey1()->GetAt( j );
        GetTableKey1()->RemoveIndex( j );
        GetTableKey1()->InsertIndex( count, pObjList, TRUE );
        
        pObjList = GetTableKey2()->GetAt( j );
        GetTableKey2()->RemoveIndex( j );
        GetTableKey2()->InsertIndex( count, pObjList, TRUE );
        count++;
      }
    }

    // jetzt noch Überschriften etc. erzeugen ( alles, was nur einmal in der Zeichnung sein darf )
    if( !bInserted )
    {
      // Draw relative height text
      CDoubleIRect rect( -3, 1, 0, 0 );
      CDrawRect* pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_HEIGHT, DST_UNKNOWN)->Clone(this);
      pDrawRect->SetSectionIndex(secIndex);
      pDrawRect->m_dPosition = rect;
      pDrawRect->SetShape(CDrawRect::text);
      m_pPData->m_heightFormatText = pTemp->m_pPData->m_heightFormatText;
      CString heightFormatText = m_pPData->m_heightFormatText;
      FormatText( heightFormatText );
      heightFormatText.TrimLeft();
      heightFormatText.TrimRight();
      if( heightFormatText.IsEmpty() )
        pDrawRect->SetFlags( CDrawObj::invisible );
      AddObject( pDrawRect, CPlotterDoc::height, 0 );

      // DrawTitle
      rect.SetRect(0, m_pPData->m_sizePage.cy-2*MM_FACTOR, m_pPData->m_sizePage.cx, m_pPData->m_sizePage.cy-3*MM_FACTOR);
      if (GetMSection( 0 )->GetClassType()==CLASS_TYPE_CSECTION)
        pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_CTITEL, DST_UNKNOWN)->Clone(this);
      else
        pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_LTITEL, DST_UNKNOWN)->Clone(this);
      CString titleText;
      pDrawRect->GetText( titleText );
      GetTitle()->SetFormatText( titleText );
      // falls der Titel von Beginn an Leer ist, gleich wieder verstecken
      FormatText( titleText );
      titleText.TrimLeft();
      titleText.TrimRight();
      if( titleText.IsEmpty() )
        pDrawRect->SetFlags( CDrawObj::invisible );
      pDrawRect->m_dPosition = rect;
      pDrawRect->SetShape(CDrawRect::text);
      AddObject( pDrawRect, title, 0 );

      // DrawComment
      pDrawRect = (CDrawRect*)pTemp->GetTemplateObj( TMPL_COMMENT, DST_UNKNOWN )->Clone( this);
      if( GetMSection( 0 )->GetClassType() == CLASS_TYPE_LSECTION )
        pDrawRect->SetFlags( CDrawObj::invisible );
      pDrawRect->GetText( m_pPData->m_commentFormatText );
      CString commentFormatText = m_pPData->m_commentFormatText;
      FormatText( commentFormatText );
      commentFormatText.TrimLeft();
      commentFormatText.TrimRight();
      if( commentFormatText.IsEmpty() )
        pDrawRect->SetFlags( CDrawObj::invisible );
      pDrawRect->SetShape( CDrawRect::text );
      AddObject( pDrawRect, comment, 0 );

      // Draw Rahmen
      // outer rectangle
      rect.SetRect(0, m_pPData->m_sizePage.cy, m_pPData->m_sizePage.cx, 0); // macht eigentlich keinen Sinn, da die Seitengrösse noch nicht feststeht
      for (i=0; i<4; i++)
      {
        pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_RAHMEN_LINE, DST_UNKNOWN)->Clone(this);
        pDrawRect->m_dPosition = rect;
        pDrawRect->SetShape(CDrawRect::line);
        pDrawRect->SetHorzJust(i % 2 ? CDrawRect::nojust : (i==0 ? CDrawRect::left : CDrawRect::right));
        pDrawRect->SetVertJust(i % 2 ? (i==1 ? CDrawRect::left : CDrawRect::right) : CDrawRect::nojust);
        AddObject( pDrawRect, border, 0 );
      }
      // inner rectangle
      rect.SetRect(MM_FACTOR, m_pPData->m_sizePage.cy-MM_FACTOR, m_pPData->m_sizePage.cx-MM_FACTOR, MM_FACTOR);
      for (i=0; i<4; i++)
      {	
        pDrawRect = (CDrawRect*)pTemp->GetTemplateObj(TMPL_RAHMEN_LINE, DST_UNKNOWN)->Clone(this);
        pDrawRect->m_dPosition = rect;
        pDrawRect->SetShape(CDrawRect::line);
        pDrawRect->SetHorzJust(i % 2 ? CDrawRect::nojust : (i==0 ? CDrawRect::left : CDrawRect::right));
        pDrawRect->SetVertJust(i % 2 ? (i==1 ? CDrawRect::left : CDrawRect::right) : CDrawRect::nojust);
        AddObject( pDrawRect, border, 0 );
      }

      CreateStempelFromTemplate( pTemp, m_pProfil );
    }
    else
    { // d.h. if bInserted
      // falls Profildaten hinzugefügt wurden, jetzt die Tabelle wieder 'nach vorne' holen, 
      // da sonst ein Strich verschwindet
      CTable* pTable = GetTable();
      if( pTable->GetSize() > 0 )
      {
        CDrawObjList* pObjList = pTable->GetAt( 0 );
        POSITION pos = pObjList->GetHeadPosition();
        while( pos != NULL )
        {
          CDrawObj* pObj = pObjList->GetNextObject( pos );
          MoveObjectToFront( pObj );
        };
      }; // if Size > 0 
      
    } // if !bInserted

    // falls es die Standardvorlage war, das oben erzeugte Objekt wieder löschen
    if( bDefaultTemplate )
    {
      pTemp->DeleteContents();
      delete pTemp;
      pTemp = NULL;
    }
  }
  GETPLOTTERAPP->FlushTemplates();
  GETPLOTTERAPP->DestroyStatusBarProgress();

} // CreateDrawing

void CPlotterDoc::CreateStempelFromTemplate( CTemplate* pTemp, Profil* pProf )
// fügt einen Stempel zur Zeichnung hinzu
{
  // Draw Stempel
  CString stampName;
  stampName = pTemp->GetStempel()->GetFileName();
  if( !stampName.IsEmpty() )
  {
    CStempelDoc* pStpl = CStempelDoc::LoadStempel( stampName );

    if( pStpl != NULL )
    {
      // die Einstellungen aus dem Template übertragen
      GetStempel()->SetSerialZoomFaktor( 100 ); // fest auf hundert setzen, weil wir grade das Original laden
      GetStempel()->SetZoomFaktor( pTemp->GetStempel()->GetZoomFaktor() );
      GetStempel()->SetMargins( pTemp->GetStempel()->GetMargins() );
      GetStempel()->SetHorizontal( pTemp->GetStempel()->GetHorizontal() );
      GetStempel()->SetAlignToProfil( pTemp->GetStempel()->GetAlignToProfil() );

      CDrawObjList* pStplObjs = pStpl->GetObjects();
      POSITION pos = pStplObjs->GetHeadPosition();

      while( pos != NULL )
      {
        CDrawObj* pObj = pStplObjs->GetNextObject( pos );
        
        CDrawObj* pAddObj = NULL; // diese Objekt wird später hinzugefügt

        if( pObj->IsText() )
        {
          int i = ((CDrawRect*)pObj)->GetStempelTextType();
          if( i != STPL_TEXT_NONE )
          {
            // einen Stempeltext clonen
            CDrawRect* pDrawRect = (CDrawRect*)pTemp->GetTemplateObj( i, DST_UNKNOWN );
            CDrawRect* pStplRect = (CDrawRect*)pObj->Clone( pStpl, TRUE );

            CString str;
            pDrawRect->GetText( str );

            if( str.IsEmpty() )
            {	// if template text is empty use text from prf file
              switch( i )
              {	
              default:
                ASSERT(FALSE);
                
              case STPL_TEXT_AG1:
                str = pProf->GetClient( 0 );
                break;
                
              case STPL_TEXT_AG2:
                str = pProf->GetClient( 1 );
                break;
                
              case STPL_TEXT_PB1:
                str = pProf->GetProjectDesc( 0 );
                break;
                
              case STPL_TEXT_PB2:
                str = pProf->GetProjectDesc( 1 );
                break;
                
              case STPL_TEXT_PB3:
                str = pProf->GetProjectDesc( 2 );
                break;
                
              case STPL_TEXT_BB1:
                str = pProf->GetPageDesc( 0 );
                break;
                
              case STPL_TEXT_BB2:
                str = pProf->GetPageDesc( 1 );
                break;
                
              case STPL_TEXT_BB3:
                str = pProf->GetPageDesc( 2 );
                break;
                
              case STPL_TEXT_PN:
                str = pProf->GetProjectNum();
                break;
                
              case STPL_TEXT_DAT:
                {
                  COleDateTime datum = pProf->GetDate();
                  if( datum.GetStatus()!=COleDateTime::valid )
                    datum = COleDateTime::GetCurrentTime();
                  str = datum.Format( "%x" );
                }
                break;
                
              case STPL_TEXT_BN:
                str = pProf->GetPageNum();
                break;
                
              case STPL_TEXT_MS:
                str.Empty();
                break;
                
              case STPL_TEXT_UD:
                str = GetMSection( 0 )->GetFileName();
                break;
                
              case STPL_TEXT_ZU:
                str = pProf->GetDrawingTitle();
                break;
              } // switch i
            }

            pStplRect->SetText( str );
            pAddObj = pStplRect;
          } // if i != STPL_TEXT_NONE
        } // if pObj->IsText

        // falls noch nicht als Text hinzugefügt
        if( pAddObj == NULL )
          pAddObj = pObj->Clone( this, TRUE );

        if( pAddObj != NULL )
        {
          pAddObj->SetFlags( CDrawObj::editable | CDrawObj::hideable );
          pAddObj->UnsetFlags( CDrawObj::moveable );

          // manchmal ( vor allem in alten Serialisierungen )
          // stimmen die Grössen der Objekte nicht
          pAddObj->m_dPosition.NormalizeRect();
          pAddObj->m_position = (CIntIRect)pAddObj->m_dPosition;

          AddObject( pAddObj, stamp, 0 );
        }
      } // while pos

      pStpl->DeleteContents();
      delete pStpl;
      pStpl = NULL;

      // jetzt die Position des Stempels auf 0 setzen
      CSize stampSize = GetStempel()->GetExtent();
      CIntIRect stampRect( 0, stampSize.cy, stampSize.cx, 0 );
      GetStempel()->SetRect( stampRect );
    } // if pStpl != NULL
  } // if( !str.IsEmpty() )
} // CreateStempelFromTemplate

CSize CPlotterDoc::ComputeDrawingSize( CSize& tableSize, CSize& profilSize, const CSize& stampSize, const CDoubleIRect& profRect )
// Parameter:
//        CSize& tableSize: [IN]: alle Breite der Tabelle ausserhalb des Profils; die Höhe der Tabelle in mm
//                          [OUT]: die gesamtGrösse der Tabelle im aktuellen Masstab  
//        CSize& profilSize: [OUT]: die Gesamtgrösse des Profils im aktuellen Maßstab
//        CSize stampSize: die Ausdehnung des Stempels in mm
//        CDoubleIRect profRect: die sichtbare Ausdehnung des Profils in Profilkoordinaten
{
  // zuerst die X-Ausdehnung bestimmen:
  CSize pageSize = GetPageSize();

  int index = NSCALES - 1;
  UINT drawSizeX = 0;
  UINT lastDrawSize = 0;
  UINT scaleX = 0;
  BOOL bFirst = TRUE; // merken, obs der erste Durchlauf war
  UINT minPageCount = 0;
  UINT tableWidth = 0;

  while( TRUE )
  {
    // die festeingestellte Scale oder die Scale, welche gerade getestet wird
    double scale = m_pPData->m_scale.x != 0 ? m_pPData->m_scale.x : dscale[index];

    // anhand dieser Scale die Tabellengrösse ausrechnen
    UINT aktTableWidth = ( 1000 * MM_FACTOR * profRect.Width() / scale ) + tableSize.cx;

    //UINT aktDrawSize = ComputeDrawingSizeX( aktTableWidth, stampSize.cx );
    CSize profilSize( aktTableWidth, 0 ); // Höhe des Profils egal
    UINT aktDrawSize = CPlotArranger::ComputeDrawingSize( GetBorderGaps(), GetStempel()->GetMarginsAdjusted(), profilSize, stampSize, GetProfil()->GetAlign(), GetStempel()->GetHorizontal() ).cx;
    
    UINT pageCount = (UINT)( aktDrawSize / pageSize.cx );

    // bei festeingestellten Maßstab einfach abbrechen
    if( m_pPData->m_scale.x != 0 )
    {
      drawSizeX = aktDrawSize;
      scaleX = scale;
      tableWidth = aktTableWidth;
      break;
    }

    index--;

    // der erste Durchlauf legt fest, wieviele Seiten mindesten gebraucht werden
    if( bFirst )
    {
      bFirst = FALSE;
      minPageCount = pageCount;
    }
    else
    {
      // ists nicht der erste Durchlauf, schauen, obs immer noch auf minPageCount Seiten passt
      // ansonsten sind wir fertig
      if( pageCount > minPageCount || index == 0 )
        break; // und abbrechen: dadurch werden automatisch die zuletzt gewählten Werte genommen
    } // if bFirst

    drawSizeX = aktDrawSize;
    scaleX = scale;
    tableWidth = aktTableWidth;
  } // while true

  // die Tabellengrösse setzen
  tableSize.cx = tableWidth;


  // jetzt das gleiche Spiel für die Y-Richtung
  bFirst = TRUE;
  index = NSCALES - 1;
  UINT drawSizeY = 0;
  minPageCount = 0;
  double scaleY = 0;
  while( TRUE )
  {
    // die festeingestellte Scale oder die Scale, welche gerade getestet wird
    double scale = m_pPData->m_scale.y != 0 ? m_pPData->m_scale.y : dscale[index];

    // anhand der Scale kann die Profilhöhe ermittelt werden
    CSize aktProfilSize = GetProfil()->GetRealSize( scaleX, scale, GetView() );
    CSize aktProfilAndTableSize = aktProfilSize + tableSize;
    
    //UINT aktDrawSize = ComputeDrawingSizeY( tableSize.cy + aktProfilSize.cy, stampSize.cy );
    UINT aktDrawSize = CPlotArranger::ComputeDrawingSize( GetBorderGaps(), GetStempel()->GetMarginsAdjusted(), aktProfilAndTableSize, stampSize, GetProfil()->GetAlign(), GetStempel()->GetHorizontal() ).cy;
    
    UINT pageCount = (UINT)( aktDrawSize / pageSize.cy );

    // bei festeingestellten Maßstab einfach abbrechen
    if( m_pPData->m_scale.y != 0 )
    {
      drawSizeY = aktDrawSize;
      scaleY = scale;
      profilSize = aktProfilSize;
      break;
    }

    index--;

    // der erste Durchlauf legt fest, wieviele Seiten mindesten gebraucht werden
    if( bFirst )
    {
      bFirst = FALSE;
      minPageCount = pageCount;
    }
    else
    {
      // ists nicht der erste Durchlauf, schauen, obs immer noch auf minPageCount Seiten passt
      // ansonsten sind wir fertig
      if( pageCount > minPageCount || index == 0 )
        break; // und abbrechen
    } // if bFirst

    drawSizeY = aktDrawSize;
    scaleY = scale;
    profilSize = aktProfilSize;
  } // while TRUE

  m_pPData->m_realScale = CDoublePoint( scaleX, scaleY );

  return CSize( drawSizeX, drawSizeY );
} // ComputeDrawingSize

void CPlotterDoc::UpdateDrawing()
{
  CDrawView* pView = GetView();

  // Warte Cursor
  CWaitCursor wait;

  // Fortschrittsanzeige initialisieren
  CString str( MAKEINTRESOURCE( IDS_FORMATTING_DRAWING ) );
  GETPLOTTERAPP->CreateStatusBarProgress(str, 0, 7+3*m_pPData->m_table.GetSize()+GetObjects()->GetObjectCount());
  
  // Grössen des Profile neu ausrechnen
  GetProfil()->CalcProfilSizes( pView );

  UINT wmax1, wmax2;
  UINT tableHeight = CalcTableHeightsAndWidths( pView, wmax1, wmax2 ); // Find heights and widths of table rows
  UINT tableMargins = m_pPData->m_sizeTableMargins.cx + m_pPData->m_sizeTableMargins.cy;

  // den Stempel neu skalieren, falls nötig, dadurch seine Grösse neu ausrechnen
  CSize stampSize = GetStempel()->UpdateScale();

  // die Seitengrösse neu ausrechnen, dabei ggfls. den Optimalen Maßstab festlegen
  CSize tableSize( tableMargins, tableHeight );
  CSize profilSize;
  CSize drawSize = ComputeDrawingSize( tableSize, profilSize, stampSize, GetProfil()->GetVisibleRect() );

  // die neue Seitengrösse setzen und die Seiten erzeugen
  SetDrawingSize( drawSize );
  drawSize = GetDrawingSize(); // die drawSize kann und wird durch SetDrawingSize geändert (auf ganze Seiten aufgerundet)

  // statt eines InvallAll reicht ein einfaches MetersToLogical für alle Objekte
  POSITION pos = GetObjects()->GetHeadPosition();
	while( pos != NULL )
	{
		CDrawObj* pDrawObj = GetObjects()->GetNextObject( pos );
    MetersToLogical( pDrawObj->m_dPosition, pDrawObj->m_position, pDrawObj );
	}

  // die Positionen der Elemente bestimmen
  CIntIRect gaps( GetBorderGaps() );
  
  // Profil und Stempel anordnen
  CSize profilTableSize( tableSize.cx, tableSize.cy + profilSize.cy );
  CPlotArranger plotArranger( GetBorderGaps(), GetStempel()->GetMargins(), profilTableSize, stampSize, drawSize, GetProfil()->GetAlign(), GetStempel()->GetAlignToProfil(), GetStempel()->GetHorizontal() );

  // den Basispunkt der Tabelle bestimmen
  CIntPoint tableKey1Point = plotArranger.GetProfilPoint();
  CIntPoint tableKey2Point( tableKey1Point.x + wmax1, tableKey1Point.y );
  CIntPoint tablePoint( tableKey1Point.x + wmax1 + wmax2, tableKey1Point.y );
  CIntPoint stampPoint = plotArranger.GetStampPoint();
  CIntPoint titlePoint;
  CIntPoint commentPoint;

  // jetzt die eigentlichen Grössen setzen
  GetTableKey1()->Update( this, wmax1, tableKey1Point, m_tableHeights ); // Key1 Positionen und Grössen relativ zum Ursprung bestimmen
  GetTableKey2()->Update( this, wmax2, tableKey2Point, m_tableHeights );

  CIntIRect profilRect( GetTable()->Update( this, pView, GetRangeFrom(), GetRangeTo(),
    GetProfil()->GetVisibleRect(), profilSize.cx, tablePoint, GetXValueFormat(), GetYValueFormat(), m_tableHeights ) );

  GetProfil()->SetRect( profilRect );
  GetStempel()->Update( stampPoint, GetRealXScale(), GetRealYScale() );

  // Titel positionieren
  titlePoint.x = gaps.left + ( ( drawSize.cx - gaps.left - gaps.right ) / 2 ); // Mitte des Zeichenbereichs
  titlePoint.y = drawSize.cy;
  GetTitle()->Update( titlePoint, this );

  UpdateHeight((CPlotterView*)pView);

  // Kommentar positionieren ( geht erst, nachdem der Titel positioniert ist )
  commentPoint.x = titlePoint.x;
  commentPoint.y = titlePoint.y;
  // falls der Titel existiert, darunter setzen
  CDrawObj* pTitleObj = GetTitle()->GetTitle();
  if( pTitleObj != NULL && !pTitleObj->IsInvisible() )
    commentPoint.y -= pTitleObj->m_dPosition.Height() + 20;
  UpdateComment( commentPoint, pView );
  
  UpdateRahmen( (CPlotterView*)pView );

  GETPLOTTERAPP->IncStatusBarProgress();

  // alle Objekte invalidieren
  InvalAll(); // leider müssen hier auch die connections invalidiert werden

  UpdateLComment(); // muss nach InvallAll stehen, da sonst die Höhe/Breite der Kommentarzeilen auf 0 gesetzt wird

  GETPLOTTERAPP->IncStatusBarProgress();
  GETPLOTTERAPP->IncStatusBarProgress();
  GETPLOTTERAPP->IncStatusBarProgress();
  GETPLOTTERAPP->DestroyStatusBarProgress();

  // alle Views invalideren
  pos = GetFirstViewPosition();
  while( pos != NULL )
    ((CPlotterView*)GetNextView(pos))->Invalidate();
} // UpdateDrawing

/////////////////////////////////////////////////////////////////////////////
// CPlotterDoc commands

void CPlotterDoc::SetTitle(LPCTSTR lpszTitle) 
{
  CString wname, str, fmt;
  int n;
		
  str = lpszTitle;
  n = GetSections()->GetSize();
  if( n > 0 && GetMSection( n - 1 ) != NULL )
  {
    if( GetMSection( n - 1 )->GetClassType() == CLASS_TYPE_CSECTION )
    {
      if( GetMState( n - 1 ) != NULL )
      {
        wname = GetMState( n - 1 )->GetWaterName();
        str = GetMState( n - 1 )->GetName();
        fmt.Format( "%.4f", ((CrossSection*)GetMSection( n - 1 ) )->GetStation() );
        str.FormatMessage(IDS_STATION_DESC1, wname, str, fmt);
      }
    }
    else
    {
      str = ((LengthSection*)GetMSection( n - 1 ))->GetName();
    }
  }
  CDocument::SetTitle( str );
}

void CPlotterDoc::OnUpdateFileSaveAs(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(WSPFeatures::Instance()->isEnabled("PLOTTER","plot_nodemo"));
}

void CPlotterDoc::OnUpdateFileSave( CCmdUI* pCmdUI ) 
{
 	pCmdUI->Enable(true);
}

BOOL CPlotterDoc::SaveModified() 
{
   if (WSPFeatures::Instance()->isEnabled("PLOTTER","plot_nodemo"))
    return CDrawDoc::SaveModified();
  else
   return TRUE;
}

void CPlotterDoc::OnFileInsert() 
{
	if( GETPLOTTERAPP->GetProject() == NULL )
	{
		CString filePath;
		if(!GETPLOTTERAPP->m_pDocManager->DoPromptFileName(filePath, AFX_IDS_OPENFILE,
			OFN_HIDEREADONLY | OFN_FILEMUSTEXIST, TRUE, NULL))
			return; // open cancelled


		Section* section = CProfDatDoc::TryLoadSection( filePath );
		InsertData( NULL, section, FALSE, TRUE, TRUE );
		UpdateDrawing();

		return;
	}


  COpenDialog dlg( GETPLOTTERAPP->m_pMainWnd, TRUE, FALSE, FALSE );
  int i;
  
  if( GetSections()->GetSize() > 0 )
    dlg.SetCurrentSection( GetMSection( 0 ) );
  if( dlg.DoModal() == IDOK )
  {
    CWaitCursor wait;
    State* state = dlg.m_pState;
    for( i = 0; i < dlg.m_Sections.GetSize(); i++ )
    {
      Section* section = dlg.m_Sections[i];
	  if( !section )
		  continue;
	
	  if( section->GetClassType()==CLASS_TYPE_LSECTION )
	  {
		  m_bAlignLowPoint = FALSE;
		  m_dAlignValue = 0;
	  }
	  else
	  {
		  m_bAlignLowPoint = dlg.m_alignLowPoint;
		  m_dAlignValue = dlg.m_alignValue;
	  }
	  InsertData( state, section, FALSE, TRUE, TRUE );
	  UpdateDrawing();
	}; // for i
  }; // if dlg.DoModal == IDOK
}; // OnFileInsert


void CPlotterDoc::OnUpdateFileInsert(CCmdUI* pCmdUI) 
{
  BOOL bUpdate = TRUE;
  
  POSITION pos = GetFirstViewPosition();
  while( pos != NULL )
  {
    CDrawView* pView = (CDrawView*)GetNextView( pos );
    if( pView->IsKindOf( RUNTIME_CLASS(CPreviewView) ) )
      bUpdate = FALSE;
  }
 
//  if( GETPLOTTERAPP->GetProject() == NULL )
//    bUpdate = FALSE;
  
  pCmdUI->Enable( bUpdate );
}

void CPlotterDoc::OnUpdateFilePrintSetup(CCmdUI* pCmdUI) 
{
  BOOL bUpdate = TRUE;
  CDrawView *pView;
  POSITION pos;
  
  pos = GetFirstViewPosition();
  while (pos!=NULL)
  {
    pView = (CDrawView*)GetNextView(pos);
    if (pView->IsKindOf(RUNTIME_CLASS(CPreviewView)))
      bUpdate = FALSE;
  }
  
  pCmdUI->Enable(bUpdate);
}


void CPlotterDoc::OnFileDxfExport()
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
  str.LoadString(IDS_DXFFILES_ALLFILES);
  CFileDialog dlg(FALSE, "dxf", NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT, str, NULL);
  
  if (dlg.DoModal()==IDOK)
  {
    CSize sizeDrawing = GetDrawingSize();
    CString str, file;
    int i;
    
    CWaitCursor wait;
    
    file = dlg.GetPathName();
    
    CDXFZeichnung zn(this);
    zn.SetSize(sizeDrawing.cx, sizeDrawing.cy, 0, 0);
    //		zeichnung->SetParams(scale, rseite, km, im, xadd, yadd);
    
    
    
    str.LoadString(IDS_STAMP);
    POSITION pos = GetStempel()->GetHeadPosition();
    while (pos!=NULL)
    {
      CDrawObj *pObj;
      
      pObj = GetStempel()->GetNextObject(pos);
      if( !pObj->IsInvisible() )
        pObj->AddToDXF(&zn, str);
    }
    
    str.LoadString(IDS_PROFIL_PLURAL);
    pos = m_pPData->m_profil.GetHeadPosition();
    while (pos!=NULL)
    {
      CDrawObj *pObj;
      
      pObj = m_pPData->m_profil.GetNextObject(pos);
      if (!pObj->IsInvisible())
        pObj->AddToDXF(&zn, str);
    }
    
    str.LoadString(IDS_TABLE);
    for (i=0; i<m_pPData->m_table.GetSize(); i++)
    {
      pos = GetTable()->GetAt( i )->GetHeadPosition();
      while (pos!=NULL)
      {
        CDrawObj *pObj;
        
        pObj = GetTable()->GetAt( i )->GetNextObject( pos );
        if (!pObj->IsInvisible())
          pObj->AddToDXF(&zn, str);
      }
    }
    
    str = "KEY1";
    for (i=0; i<m_pPData->m_tableKey1.GetSize(); i++)
    {
      pos = GetTableKey1()->GetAt( i )->GetHeadPosition();
      while (pos!=NULL)
      {
        CDrawObj *pObj;
        
        pObj = GetTableKey1()->GetAt( i )->GetNextObject( pos );
        if (!pObj->IsInvisible())
          pObj->AddToDXF(&zn, str);
      }
    }
    
    str = "KEY2";
    for (i=0; i<m_pPData->m_tableKey2.GetSize(); i++)
    {
      pos = GetTableKey2()->GetAt( i )->GetHeadPosition();
      while (pos!=NULL)
      {
        CDrawObj *pObj;
        
        pObj = GetTableKey2()->GetAt( i )->GetNextObject( pos );
        if (!pObj->IsInvisible())
          pObj->AddToDXF(&zn, str);
      }
    }
    
    str.LoadString(IDS_FRAME);
    pos = m_pPData->m_rahmen.GetHeadPosition();
    while (pos!=NULL)
    {
      CDrawObj *pObj;
      
      pObj = m_pPData->m_rahmen.GetNextObject(pos);
      if (!pObj->IsInvisible())
        pObj->AddToDXF(&zn, str);
    }
    
    str.LoadString(IDS_HEIGHT);
    if (!m_pPData->m_height->IsInvisible())
      m_pPData->m_height->AddToDXF(&zn, str);

    str.LoadString( IDS_TITLECOMMENT );
    if( !m_pPData->m_comment->IsInvisible() )
      m_pPData->m_comment->AddToDXF( &zn, str );

    str.LoadString( IDS_TITLE );
    CDrawRect* titelRect = GetTitle()->GetTitle();
    if( !titelRect->IsInvisible() )
      titelRect->AddToDXF( &zn, str );
    
    str.LoadString(IDS_USER);
    pos = m_pPData->m_user.GetHeadPosition();
    while (pos!=NULL)
    {
      CDrawObj *pObj;
      
      pObj = m_pPData->m_user.GetNextObject(pos);
      if (!pObj->IsInvisible())
        pObj->AddToDXF(&zn, str);
    }
    
    // Saves a file with DXF format.
    gofstream ofs;
    CString rString;
    
    ofs.open(file, ios::out);
    if (ofs.fail())
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
}

void CPlotterDoc::GetDrawText( CDrawRect* pObj, CString& drawText )
// liefert den Text von pObj
// wird benötigt, da es die Aussnahmen Titel, Kommentar und Bezugshöhe gibt
// Parameter:
//      CDrawRect* pObj: das auzulesende Object
//      CString& drawText: hier wird der Text übergeben
{
  if ( pObj == GetTitle()->GetTitle() )
    drawText = GetTitle()->GetFormatText();
  else if( pObj == GetComment() )
    drawText = m_pPData->m_commentFormatText;
  else if ( pObj == GetHeight() )
    drawText = m_pPData->m_heightFormatText;
  else if ( pObj )
    pObj->GetText( drawText );
}; // GetDrawText

void CPlotterDoc::SetDrawText( CDrawRect* pObj, CString& drawText )
// setzt den Text von pObj, vorher wird er noch Formatiert d.h. 
// Teile der Form <xxx> werden entsprechendersetzt
// Parameter:
//      CDrawRect* pObj: der Text dieses objektes wird gesetzt
//      CString& drawText: und zwar mit diesem Text
{
  if ( pObj == GetTitle()->GetTitle() )
    GetTitle()->SetFormatText( drawText );
  else if( pObj == GetComment() )
    m_pPData->m_commentFormatText = drawText;
  else if ( pObj == GetHeight() )
    m_pPData->m_heightFormatText = drawText;
  else if ( pObj )
  {
    CString str( drawText );
    FormatText( str );
    pObj->SetText( str );
  };
}; // SetDrawText

void CPlotterDoc::UpdateLComment()
// setzt Oberkannte der Kommentar-Rechtecke auf die Unterkante des dargestellten Bereiches
// setzt die Breite des Rechtecks auf drei Textzeilen
{
  CDrawView* pView = GetView();
  CDrawObjList* profilObjects = GetProfil();
  CSize drawingSize = GetDrawingSize();
  int top = drawingSize.cy - GetBorderGaps().top;
  POSITION pos = profilObjects->GetHeadPosition();
  while( pos )
  {
    CDrawObj* pObj = profilObjects->GetNextObject( pos );
    if( pObj->GetType() == DST_LP_TEXT )
    {
      MetersToLogical( pObj->m_dPosition, pObj->m_position, pObj );

      CSize size = ((CDrawRect*)pObj)->GetOutputTextSize( pView );
      CIntIRect rect( pObj->m_position );

      rect.bottom += CProfil::LCOMMENT_DIST; // 5 Millimeter Abstand zum höchsten Profileintrag an dieser Stelle
      rect.top = rect.bottom + size.cy;
      rect.left = rect.left - (int)(size.cx * 1.5);
      rect.right = rect.right + (int)(size.cx * 1.5);

      pObj->m_position = rect;
      
      pObj->CalcDrawingHelpers( pView );
    }
  }; // while pos
}; // UpdateLComment

State* CPlotterDoc::GetMState( const int index ) const
// gibt den Zustand des index. geladenen Profils zurück, NULL bei Fehler
{
  if( index >= 0 && index < GetStates()->GetSize() )
    return GetStates()->GetAt( index );
  else
    return NULL;
}; // GetMSate

Section* CPlotterDoc::GetMSection( const int index ) const
// gibt das Index. geladene Profil zurück, NULL bei Fehler
{
  if( index >= 0 && index < GetSections()->GetSize() )
    return GetSections()->GetAt( index );
  else
    return NULL;
}; // GetMSection

/* virtual */ BOOL CPlotterDoc::HasType( const CDrawObjList* pObs, const int type, int* index /* = NULL */ ) const
// stellt fest, ob mindestens ein Objekt aus der Liste von einem bestimmten Typ ist
{
  POSITION pos = pObs->GetHeadPosition();
  while( pos )
  {
    CDrawObj* pObj = pObs->GetNextObject( pos );
    if( HasType( pObj, type, index ) )
      return TRUE;
  } // while pos

  return FALSE;
}; // HasType

/* virtual */ BOOL CPlotterDoc::HasType( const CDrawObj* pObj, const int type, int* index ) const
// stellt fest, ob ein Objekt von einem bestimmten Typ ist
// Parameter:
//        const CDrawObj* pObj: das zu untersuchende Objekt
//        const int type: der gesuchte Typ
//        int* index: falls ungleich NULL, wird hier noch der Index angegeben
{
  if( pObj != NULL && pObj->GetLayer() == type )
  {
    // bei den Tabellenobjekten noch den index mit zurückgeben
    if( index != NULL && ( type == table || type == tableKey1 || type == tableKey2 ) )
      *index = pObj->GetIndex();

    return TRUE;
  }
  else
    return FALSE;
} // HasType
    

void CPlotterDoc::ChangeType( const CDrawObjList& pObs, const int type )
// Überladung von ChangeType für mehrere Objekte gleichzeitig
// Parameter:
//        CDrawObjList* pObs: bei allen Objekten aus dieser Liste wird der Typ geändert
//        const int type: der neue Typ
{
  POSITION pos = pObs.GetHeadPosition();
  while( pos )
  {
    CDrawObj* pObj = pObs.GetNextObject( pos );
    ChangeType( pObj, type );
  } // while pos
} // ChangeType

void CPlotterDoc::ChangeType( CDrawObj* pObj, const int type )
// Ändert den Typ eines Objektes, sorgt auch dafür, dass 
// die logischen Koordinaten entsprechend gesetzt werden
// TODO: zur Zeit werden nur die Typen profil und user unterstützt
// Parameter:
//        CDrawObj* pObj: das Object, desen Typ geändert wird
//        const int type: der neue Typ des Objektes
{
  if( type == user )
  {
    ASSERT( GetProfil()->RemoveObject( pObj ) ); // dann muss es vorher in profil gewesen sein
    GetUser()->AddTailObject( pObj ); // zu user hinzufügen
    pObj->SetLayer( user );

    // sehr dirty: den Typ des Objekts stets auf DST_UNKNOWN setzen: markiert die Objekte im Profil als spezielle Daten
    pObj->SetType( DST_UNKNOWN );

    // auch noch sehr dirty: die Flags werden hier geändert
    pObj->SetFlags( CDrawObj::moveable );
    pObj->UnsetFlags( CDrawObj::hideable );
  }
  else if( type == profil )
  {
    ASSERT( GetUser()->RemoveObject( pObj ) ); // es muss vorher in user gewesen sein
    GetProfil()->AddTailObject( pObj ); // zu profil hinzufügen
    pObj->SetLayer( profil );

    // sehr dirty: den Typ des Objekts stets auf DST_UnKNOWN setzen: markiert die Objekte im Profil als spezielle Daten
    pObj->SetType( DST_UNKNOWN );

    // auch noch sehr dirty: die Flags werden hier geändert
    pObj->SetFlags( CDrawObj::hideable );
    pObj->UnsetFlags( CDrawObj::moveable );
  }
  else
    ASSERT( FALSE );

  // hier kommen wir nur an, falls tatsächlich der Typ geändert wurde

  // danach die logischen Koordinaten neu ausrechnen
  ResetLogicalCoords( pObj );
} // ChangeType

void CPlotterDoc::ResetLogicalCoords( CDrawObj* pObj )
// rechnet alle logischen Kooridnaten dieses Objekts anhand der metrischen neu aus
{
  if( pObj == NULL )
    return;

  LogicalToMeters( pObj->m_position, pObj->m_dPosition, pObj );

  // für Polygone noch eine Sonderbehandlung ( dirty; sollte eigentlich CDrawPoly selbst machen )
  if( pObj->IsKindOf(  RUNTIME_CLASS( CDrawPoly ) ) )
  {
    CDrawPoly* pPoly = (CDrawPoly*)pObj;
    for( int i = 0; i < pPoly->GetNumPoints(); i++ )
    {
      CIntPoint logPt = pPoly->GetCPoint( i );
      CDoublePoint metPt;
      LogicalToMeters( logPt, metPt, pPoly );
      
      pPoly->SetPoint( i, metPt );
    } // for i
  } // if pObj->IsKindOf( CDrawPoly )
} // ResetLogicalCoords

  /*!
  * Gibt die Ausmasse von Profil und Tabelle (inclusive Key's ) zurück
  *
  * @return CIntIRect  : die Aussmasse, alles 0, falls diese nicht ermittelt werden konnten
*/
CIntIRect CPlotterDoc::GetProfilAndTableExtent()
{
  CProfil* profil = GetProfil();
  CTable* table = GetTable();
  CTableKey1* tK1 = GetTableKey1();
  CTableKey2* tK2 = GetTableKey2();

  if( !profil || !table || !tK1 || !tK2 )
    return CIntIRect( 0, 0, 0, 0 );

  CIntIRect extent( profil->GetRect() );
  extent.CompareAndExpand( table->GetRect() );
  extent.CompareAndExpand( tK1->GetRect() );
  extent.CompareAndExpand( tK2->GetRect() );

  return extent;
}; // GetProfilAndTableExtent

   /*!
   * Sammelt alle Objekte von Profil und Tabelle (incl. Keys) in einer Objektliste
   *
   * @param none
   *
   * @return CDrawObjList*  : 
*/
void CPlotterDoc::GetProfilAndTableObjects( CDrawObjList* pObjList )
{
  // erstmal alle Voraussetzungen prüfen
  CProfil* profil = GetProfil();
  CTable* table = GetTable();
  CTableKey1* tK1 = GetTableKey1();
  CTableKey2* tK2 = GetTableKey2();
  CDrawObj* pHeight = GetHeight();

  if( !profil || !table || !tK1 || !tK2 || !pHeight )
    return;

  // jetzt die Liste mit den Objekten füllen
  profil->CopyTo( pObjList );
  table->CopyTo( pObjList );
  tK1->CopyTo( pObjList );
  tK2->CopyTo( pObjList );
  pObjList->AddTailObject( pHeight );
}; // GetProfilAndTableObjects

/** 
 * DrawingSize so anpassen, dass Sie ein vielfaches der Seiten-Grösse ist.
 * entsprechend viele Seiten setzen.
 */
CSize CPlotterDoc::AdaptDrawingSize( const CSize& drawSize )
{
	// die Aufteilung der Seiten neu ausrechnen
	CSize pageSize = GetPageSize();
	
	UINT x = div( drawSize.cx - 1, pageSize.cx ).quot + 1;
	UINT y = div( drawSize.cy - 1, pageSize.cy ).quot + 1;
	
	CSize newDrawSize( x * pageSize.cx, y * pageSize.cy );
	CSize newPages( x, y );
	
	if( GetPages() != newPages )
	{
		SetPages( newPages );
		
		// macht eigentlich SetDrawingsize schon
		// UpdateAllViews( NULL, HINT_UPDATE_WINDOW, NULL );
	}
	return newDrawSize;
}

