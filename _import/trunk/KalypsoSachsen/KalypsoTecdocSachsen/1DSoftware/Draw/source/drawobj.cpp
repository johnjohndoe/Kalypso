// drawobj.cpp - implementation for drawing objects
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "commonMfc\include\helper.h"

#include "log4cpp\category.hh"

#include "drawdoc.h"
#include "drawvw.h"
#include "drawLayer.h"
#include "graphdlg.h"
#include "draw.h"
#include "dxflayer.h"
#include "dxfzeich.h"
#include "dxflinie.h"
#include "dxfplin.h"
#include "dxftext.h"
#include "cntritem.h"

#include "profil.h"

#include "drawobj.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif


BOOL operator!=(const LOGFONT& lf1, const LOGFONT& lf2)
{
  if (lf1.lfHeight!=lf2.lfHeight)
    return TRUE; 
  if (lf1.lfWidth!=lf2.lfWidth)
    return TRUE;
  if (lf1.lfEscapement!=lf2.lfEscapement)
    return TRUE;
  if (lf1.lfOrientation!=lf2.lfOrientation)
    return TRUE;
  if (lf1.lfWeight!=lf2.lfWeight)
    return TRUE;
  if (lf1.lfItalic!=lf2.lfItalic)
    return TRUE;
  if (lf1.lfUnderline!=lf2.lfUnderline)
    return TRUE;
  if (lf1.lfStrikeOut!=lf2.lfStrikeOut)
    return TRUE;
  if (lf1.lfCharSet!=lf2.lfCharSet)
    return TRUE;
  if (lf1.lfOutPrecision!=lf2.lfOutPrecision)
    return TRUE;
  if (lf1.lfClipPrecision!=lf2.lfClipPrecision)
    return TRUE;
  if (lf1.lfQuality!=lf2.lfQuality)
    return TRUE;
  if (lf1.lfPitchAndFamily!=lf2.lfPitchAndFamily)
    return TRUE;
  if (strcmp(lf1.lfFaceName, lf2.lfFaceName)!=0)
    return TRUE;
  
  return FALSE;
}

IMPLEMENT_SERIAL( CDrawObj, CObject, VERSIONABLE_SCHEMA | 7 )

CDrawObj::CDrawObj()
{
  // wird nur von der Serialisierung aufgerufen, deshalb keine ID erzeugen
  Init( CIntIRect( 0, 0, 0, 0 ), CDoubleIRect( 0, 0, 0, 0 ), NULL );
}

CDrawObj::CDrawObj( const CIntIRect& position, CDrawDoc* pDoc )
{
  Init( position, CDoubleIRect( position ), pDoc );
}

CDrawObj::CDrawObj( const CDoubleIRect& position, CDrawDoc* pDoc )
{
  Init( CIntIRect( 0, 0, 0, 0 ), position, pDoc );
}

CDrawObj::Init( const CIntIRect& position, const CDoubleIRect& dPosition, CDrawDoc* pDoc )
{
  x_flags = font | pen | brush | arrow | user;
  // Set up font as Arial (TRUETYPE)
  m_logfont.lfHeight = -17;
  m_logfont.lfWidth = 0;
  m_logfont.lfEscapement = 0;
  m_logfont.lfOrientation = 0;
  m_logfont.lfWeight = FW_NORMAL;
  m_logfont.lfItalic = FALSE;
  m_logfont.lfUnderline = FALSE;
  m_logfont.lfStrikeOut = FALSE;
  m_logfont.lfCharSet = ANSI_CHARSET;
  m_logfont.lfOutPrecision = OUT_STROKE_PRECIS;
  // set CLIP_LH_ANGLES to ensure the coordinate system for all devices is the same
  m_logfont.lfClipPrecision = CLIP_STROKE_PRECIS | CLIP_LH_ANGLES;
  m_logfont.lfQuality = DRAFT_QUALITY;
  m_logfont.lfPitchAndFamily = 34;
  strcpy(m_logfont.lfFaceName, "Arial");
  
  m_logpen.lopnStyle = PS_SOLID;
  m_logpen.lopnWidth.x = 1;
  m_logpen.lopnWidth.y = 0;
  m_logpen.lopnColor = RGB(0, 0, 0);
  
  m_logbrush.lbStyle = BS_SOLID;
  m_logbrush.lbColor = RGB(192, 192, 192);
  m_logbrush.lbHatch = HS_HORIZONTAL;
  
  m_colorText = 0;
  m_nLeftArrow = noarrow;
  m_nRightArrow = noarrow;
  m_nSectionIndex = 0;
  m_nDataBlockIndex = -1;
  m_nLayer = 0; // Standardlayer ( user )
  m_nIndex = -1;
  
  m_bClip = FALSE;

  m_nType = DST_UNKNOWN;

  m_ptOffset = CIntPoint(0, 0);
  m_pConnections = NULL;

#ifdef _DEBUG
  m_bBreakInDraw = FALSE;
  m_bBreakWhenIntersected = FALSE;
#endif

  m_hPattern = NULL;

  if( pDoc )
    ASSERT_VALID( pDoc );
  m_pDocument = pDoc;

  m_position = position;
  m_dPosition = dPosition;

  m_dFrom = -DBL_MAX;
  m_dTo = DBL_MAX;
  m_dBottom = 0.0;

  m_pDrawLayer = NULL;
}


CDrawObj::~CDrawObj()
{
  RemoveConnections( FALSE );
  DeletePattern();
}


/*!
 * Setzt den Layer dieses Objekt. Das Objekt wird beim alten Layer ab- und beim neuen angemeldet.
 *
 * @param pLayer : Der neue Layer dieses Objekt, kann NULL sein (Standard-Layer)
 *
 */
void CDrawObj::SetDrawLayer( CDrawLayer* pLayer )
{
  // vom alten Layer abmelden
  if( m_pDrawLayer )
    m_pDrawLayer->RemoveObject( this );

  // den Layer ändern
  m_pDrawLayer = pLayer;

  // und beim neuen Layer anmelden
  if( m_pDrawLayer )
    m_pDrawLayer->AddTailObject( this );
}

void CDrawObj::Serialize(CArchive& ar)
{
  CDrawApp* drawApp = GETDRAWAPP;
  
  CObject::Serialize( ar );
  if( ar.IsStoring() )
  {
    ar << m_position;
    ar << m_dPosition;
    ar << x_flags;
    ar.Write( &m_logfont, sizeof( LOGFONT ) );
    ar.Write( &m_logpen, sizeof( LOGPEN ) );

    LONG lbHatch = m_logbrush.lbHatch;
    if( m_logbrush.lbStyle == BS_PATTERN )
      m_logbrush.lbHatch = drawApp->GetBrushIndex( &m_logbrush );
    ar.Write( &m_logbrush, sizeof( LOGBRUSH ) );
    m_logbrush.lbHatch = lbHatch;

    ar << (DWORD)m_colorText;
    ar << m_nType;
    ar << (WORD)m_nLeftArrow;
    ar << (WORD)m_nRightArrow;
    
    if( m_pConnections == NULL )
      ar.WriteCount( 0 );
    else
    {
      ar.WriteCount( m_pConnections->GetObjectCount() );
      POSITION pos = m_pConnections->GetHeadPosition();
      while( pos != NULL )
        ar << m_pConnections->GetNextObject( pos );
    }

    ar << m_ptOffset;
    m_pThisDrawObj = this;
    ar << m_pThisDrawObj;
    ar << m_nSectionIndex;
    ar << m_nDataBlockIndex;
    ar << m_nLayer;
    ar << m_nIndex;

    ar << m_bClip;
    ar << m_dFrom;
    ar << m_dTo;
    ar << m_dBottom;
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
      {
        // get the document back pointer from the archive
        m_pDocument = (CDrawDoc*)ar.m_pDocument;
        ASSERT_VALID(m_pDocument);
        ASSERT_KINDOF(CDrawDoc, m_pDocument);
        ASSERT(m_pConnections==NULL);
        
        WORD wTemp;
        DWORD dwTemp;
        
        ar >> m_position;
        ar >> m_dPosition;
        ar >> x_flags;
        ar.Read( &m_logfont, sizeof(LOGFONT) );
        ar.Read( &m_logpen,sizeof(LOGPEN) );
        ar.Read( &m_logbrush, sizeof(LOGBRUSH) );
        if( m_logbrush.lbStyle == BS_PATTERN )
        {
          LPLOGBRUSH lpLB = drawApp->GetLogBrush( m_logbrush.lbHatch );
          if( lpLB )
            m_logbrush.lbHatch = lpLB->lbHatch;
        }

        if( nVersion <= 4 )
        {
          ar >> wTemp;
          m_colorText = (COLORREF)wTemp;
        }
        else
        {
          ar >> dwTemp; 
          m_colorText = (COLORREF)dwTemp;
        }

        ar >> m_nType;

        ar >> wTemp;
        m_nLeftArrow = (ArrowHead)wTemp;

        ar >> wTemp; m_nRightArrow = (ArrowHead)wTemp;
        
        DWORD nOldSize = ar.ReadCount();
        if( nOldSize > 0 )
          m_pConnections = new CDrawObjList;
        for( int i = 0; i < nOldSize; i++ )
        {
          CDrawObj* pConnection;
          ar >> pConnection;
          m_pConnections->AddTailObject(pConnection);
        }

        ar >> m_ptOffset;
        ar >> m_pThisDrawObj;
        ar >> m_nSectionIndex;
        
        if( nVersion < 2 )
          m_nDataBlockIndex = -1;
        else
          ar >> m_nDataBlockIndex;

        if( nVersion > 5 )
        {
          ar >> m_nLayer;
          ar >> m_nIndex;
        }

        if( nVersion > 6 )
        {
          ar >> m_bClip;
          ar >> m_dFrom;
          ar >> m_dTo;
          ar >> m_dBottom;
        }
      }
      break;
      
    default:
      AfxThrowArchiveException(CArchiveException::badSchema);
      break;
    }
  }
}

void CDrawObj::Remove()
{
  delete this;
}

long CDrawObj::TransformFontHeight( CDC *pDC, const long height )
{
  // Don't use LOGPIXELSY as there are problems with differences in
  // NT and Win95 printer drivers!
  return MulDiv( height, pDC->GetDeviceCaps(VERTRES) * 254, pDC->GetDeviceCaps(VERTSIZE) * 720 );
}

void CDrawObj::DrawTracker(CDC* pDC, TrackerState state)
{
  CIntIRect rect;
  CRect clipRect;
  
  ASSERT_VALID( this );
  
  switch( state )
  {
  case normal:
    break;
    
  case selected:
  case active:
    {
      int nHandleCount = GetHandleCount();
      int width = max((int)(HANDLE_WIDTH*MM_FACTOR), (int)(m_logpen.lopnWidth.x/2));
      if (m_bClip)
      {
        rect = GetAdjustedRect();
        pDC->SaveDC();
        pDC->GetClipBox( &clipRect );
        clipRect.left = rect.left + (int)(rect.Width()*(m_dFrom-m_dPosition.left)/m_dPosition.Width())-width;
        clipRect.right = rect.right + (int)(rect.Width()*(m_dTo-m_dPosition.right)/m_dPosition.Width())+width;
        if (m_dPosition.Height()!=0)
          clipRect.bottom = rect.bottom - (int)(abs(rect.Height())*(m_dBottom-m_dPosition.bottom)/fabs(m_dPosition.Height()));
        else if (m_dPosition.bottom<m_dBottom)
          clipRect = CRect(0, 0, 0, 0);
        pDC->IntersectClipRect(&clipRect);
      }

      for (int nHandle = 1; nHandle <= nHandleCount; nHandle += 1)
      {
        CPoint handle = GetHandle(nHandle);
        pDC->PatBlt(handle.x - width, handle.y - width, 2*width, 2*width, DSTINVERT);
      }
      if (m_bClip)
        pDC->RestoreDC(-1);
    }
    break;
  }
}

// position is in logical
void CDrawObj::MoveTo( const CIntIRect& position, CDrawView* pView )
{
  ASSERT_VALID(this);
  
  if( pView == NULL )
    Invalidate();
  else
    pView->InvalObj( this );

  // falls das Objekt-'Offsetable' ist, nur den Offset entsprechend verändern
  if( IsOffsetable() )
  {
    CIntPoint offset( position.left - m_position.left, position.bottom - m_position.bottom );
    offset += GetOffset();
    SetOffset( offset );
    m_pDocument->MetersToLogical( m_dPosition, m_position, this );
  }
  else
  {
    // ansonsten normal die Position verändern
    CDoubleIRect oldDPosition( m_dPosition );
    double oldWidth = oldDPosition.Width();
    
    m_position = position;
    m_pDocument->LogicalToMeters( m_position, m_dPosition, this );
    
    if( IsClipped() )
    {
      // auch dTo und dFrom anpassen
      if( oldDPosition.Width() != 0 )
      {
        m_dTo = m_dPosition.left + m_dPosition.Width() / oldWidth * ( m_dTo - oldDPosition.left );
        m_dFrom = m_dPosition.left + m_dPosition.Width() / oldWidth * ( m_dFrom - oldDPosition.left );
      }
      else
      {
        // ansonsten nur sicherstellen, dass die Sichtbarkeit vorher und nacher gleich bleibt
        // ist nur ein Hack und funktioniert auch nur für CMultiDoc::MoveIntoExtent
        if( m_dFrom <= oldDPosition.left && oldDPosition.left < m_dTo )
        {
          // war und bleibt sichtbar
          m_dFrom = m_dPosition.left - 1.0;
          m_dTo = m_dPosition.right + 1.0;
        }
        else
        {
          // war und bleibt unsichtbar
          m_dFrom = m_dPosition.left - 2.0;
          m_dTo = m_dPosition.left - 1.0;
        }
        
      }
    } // if clipped
  }
  
  if( pView == NULL )
    Invalidate();
  else
  {
    CalcDrawingHelpers( pView );
    pView->InvalObj( this );
  }
  
  m_pDocument->SetModifiedFlag();
}

// Note: if bSelected, hit-codes start at one for the top-left
// and increment clockwise, 0 means no hit.
// If !bSelected, 0 = no hit, 1 = hit (anywhere)

// point is in logical coordinates
int CDrawObj::HitTest(CPoint point, CDrawView* pView, BOOL bSelected)
{
  ASSERT_VALID(this);
  ASSERT(pView != NULL);
  CIntIRect rect;
  BOOL bIntersects;
  
  rect = CIntIRect(point.x, point.y, point.x+1, point.y+1);
  bIntersects = Intersects(rect);
  if (!bIntersects && !bSelected)
    return 0;
  rect = GetAdjustedRect();
  rect.NormalizeRect();
  if (bSelected)
  {
    int nHandleCount = GetHandleCount();
    for (int nHandle = 1; nHandle <= nHandleCount; nHandle += 1)
    {
      // GetHandleRect returns in logical coords
      CIntIRect rc( GetHandleRect( nHandle,pView ) );
      if (point.x >= rc.left && point.x < rc.right &&
        point.y <= rc.top && point.y > rc.bottom)
        return nHandle;
    }
    if (bIntersects)
      return -1;
  }
  else
  {
    if (point.x >= rect.left && point.x < rect.right &&
      point.y >= rect.top && point.y < rect.bottom)
      return 1;
  }
  return 0;
}

// rect must be in logical coordinates
BOOL CDrawObj::Intersects( const CIntIRect& rect ) const
{
  ASSERT_VALID( this );
  
  CIntIRect fixed;
  fixed = GetAdjustedRect();
  fixed.NormalizeRect();
  CIntIRect rectT( rect );
  rectT.NormalizeRect();
#ifdef _DEBUG
  if (m_bBreakWhenIntersected)
  {
    if (!(rectT & fixed).IsRectEmpty())
      DebugBreak();
  }
#endif
  return !(rectT & fixed).IsRectEmpty();
}

BOOL CDrawObj::IsContained( const CIntIRect& rect ) const
{
  return rect.Contains( GetClippedRect() );
}; // IsContained


int CDrawObj::GetHandleCount()
{
  ASSERT_VALID(this);
  return 8;
}

// returns logical coords of center of handle
CPoint CDrawObj::GetHandle(int nHandle)
{
  ASSERT_VALID(this);
  
  CIntIRect rect( GetAdjustedRect() );
  
  CIntPoint cPt = rect.Center();
  
  int x = 0, y = 0;
  switch( nHandle )
  {
  default:
    ASSERT(FALSE);
    
  case 1:
    x = rect.left;
    y = rect.top;
    break;
    
  case 2:
    x = cPt.x;
    y = rect.top;
    break;
    
  case 3:
    x = rect.right;
    y = rect.top;
    break;
    
  case 4:
    x = rect.right;
    y = cPt.y;
    break;
    
  case 5:
    x = rect.right;
    y = rect.bottom;
    break;
    
  case 6:
    x = cPt.x;
    y = rect.bottom;
    break;
    
  case 7:
    x = rect.left;
    y = rect.bottom;
    break;
    
  case 8:
    x = rect.left;
    y = cPt.y;
    break;
  }
  
  return CPoint( x, y );
}

// return rectange of handle in logical coords
CIntIRect CDrawObj::GetHandleRect(int nHandleID, CDrawView* pView)
{
  ASSERT_VALID(this);
  ASSERT(pView != NULL);
  
  CRect rect;
  // get the center of the handle in logical coords
  CPoint point = GetHandle(nHandleID);
  int width = max((int)(HANDLE_WIDTH*MM_FACTOR), (int)(m_logpen.lopnWidth.x/2));
  CRect rcWidth(0, width, width, 0);
  // convert to client/device coords
  pView->DocToClient(point);
  pView->DocToClient( rcWidth );
  // return CIntIRect of handle in device coords
  width = abs( rcWidth.Width() );
  rect.SetRect( point.x - width, point.y - width, point.x + width, point.y + width );
  pView->ClientToDoc( rect );
  
  return rect;
}

HCURSOR CDrawObj::GetHandleCursor( int nHandle )
{
  ASSERT_VALID(this);
  
  LPCTSTR id;
  if( IsOffsetable() )
    id = IDC_SIZEALL;
  else if( IsMoveable()  )
  {
    switch( nHandle )
    {
    default:
      ASSERT(FALSE);
      
    case -1:
      id = IDC_SIZEALL;
      break;
      
    case 1:
    case 5:
      id = IDC_SIZENWSE;
      break;
      
    case 2:
    case 6:
      id = IDC_SIZENS;
      break;
      
    case 3:
    case 7:
      id = IDC_SIZENESW;
      break;
      
    case 4:
    case 8:
      id = IDC_SIZEWE;
      break;
    }
  }
  else
    id = IDC_ARROW;
  
  return GETDRAWAPP->LoadStandardCursor( id );
}

// point must be in logical
void CDrawObj::MoveHandleTo( int nHandle, CIntPoint point, CDrawView* pView )
{
  ASSERT_VALID(this);
  
  CIntIRect position( GetAdjustedRect() );
  switch( nHandle )
  {
  default:
    ASSERT( FALSE );
    
  case -1:
    return;
    
  case 1:
    position.left = point.x;
    position.top = point.y;
    break;
    
  case 2:
    position.top = point.y;
    break;
    
  case 3:
    position.right = point.x;
    position.top = point.y;
    break;
    
  case 4:
    position.right = point.x;
    break;
    
  case 5:
    position.right = point.x;
    position.bottom = point.y;
    break;
    
  case 6:
    position.bottom = point.y;
    break;
    
  case 7:
    position.left = point.x;
    position.bottom = point.y;
    break;
    
  case 8:
    position.left = point.x;
    break;
  }
  
  MoveTo( position, pView );
}

void CDrawObj::Invalidate(BOOL bConnections /*=TRUE*/)
{
  ASSERT_VALID(this);
  
  if( m_pDocument )
  {
    CDrawView *pView = m_pDocument->GetView();
    
    m_pDocument->MetersToLogical( m_dPosition, m_position, this );
    if( pView != NULL )
      CalcDrawingHelpers( pView );
    m_pDocument->UpdateAllViews( NULL, HINT_UPDATE_DRAWOBJ, this );
    if( bConnections )
      UpdateConnections();
  }; // if m_pDocument
}; // Invalidate

CDrawObj* CDrawObj::Clone( CDrawDoc* pDoc, BOOL bExactCopy /* = FALSE */ )
// Parameter:
//        CDrawDoc* pDoc: das Dokument
//        BOOL bAdd: wird nicht mehr benutzt
//        BOOL bExactCopy: falls TRUE, wird alles kopiert sonst nur??
{
  ASSERT_VALID( this );
  
  CDrawObj* pClone = new CDrawObj( m_position, pDoc );
  pClone->x_flags = x_flags;
  pClone->m_nIndex = m_nIndex;
  pClone->m_nLayer = m_nLayer;
  pClone->m_logfont = m_logfont;
  pClone->m_logpen = m_logpen;
  pClone->m_logbrush = m_logbrush;
  pClone->m_colorText = m_colorText;
  pClone->m_nType = m_nType;
  pClone->m_nLeftArrow = m_nLeftArrow;
  pClone->m_nRightArrow = m_nRightArrow;

  if( bExactCopy && m_pConnections != NULL )
  {
    POSITION pos;
    
    if (pClone->m_pConnections==NULL)
      pClone->m_pConnections = new CDrawObjList;
    pos = m_pConnections->GetHeadPosition();
    while( pos )
    {
      CDrawObj* pCon = m_pConnections->GetNextObject( pos );
      if (pClone->m_pConnections->FindObject(pCon)==NULL)
        pClone->m_pConnections->AddTailObject(pCon);
    }
  }
  else
    pClone->m_pConnections = NULL;
  
  if( bExactCopy )
  {
    pClone->m_dPosition = m_dPosition;
    pClone->m_nSectionIndex = m_nSectionIndex;
    pClone->m_ptOffset = m_ptOffset;
    pClone->m_bClip = m_bClip;
    pClone->m_dFrom = m_dFrom;
    pClone->m_dTo = m_dTo;
    pClone->m_dBottom = m_dBottom;
  }
  ASSERT_VALID(pClone);
  
  return pClone;
} // Clone

void CDrawObj::OnEditProperties(CDrawView* pView)
{
  ASSERT_VALID( this );
  
  CGraphicDialog dlg( IDS_GRAPHIC_ATTRIB, &pView->m_selection, pView );
  
  if( dlg.DoModal() != IDOK )
    return;
} // OnEditProperties

void CDrawObj::OnFormatSchrift(CDrawView* pView)
{
  POSITION pos;
  CDrawObj *pDrawObj;
  
  
  ASSERT_VALID(this);
  
  pos = pView->m_selection.GetHeadPosition();
  while (pos!=NULL)
  {
    pDrawObj = pView->m_selection.GetNextObject( pos );
    if (pDrawObj->IsText())
      break;
  }
  
  CFontDialog dlg(&pDrawObj->m_logfont, CF_EFFECTS | CF_BOTH | CF_TTONLY, NULL, pView);
  dlg.m_cf.rgbColors = pDrawObj->m_colorText;
  
  if (dlg.DoModal() != IDOK)
    return;
  
  pos = pView->m_selection.GetHeadPosition();
  while (pos!=NULL)
  {
    pDrawObj = pView->m_selection.GetNextObject(pos);
    if (pDrawObj->IsText())
    {
      pDrawObj->m_logfont = dlg.m_lf;
      pDrawObj->m_colorText = dlg.m_cf.rgbColors;
      pDrawObj->Invalidate();
    }
  }
  m_pDocument->SetModifiedFlag();
}

void CDrawObj::OnOpen(CDrawView* pView)
{
  OnEditProperties(pView);
}

void CDrawObj::SetLineColor(COLORREF color)
{
  ASSERT_VALID(this);
  
  m_logpen.lopnColor = color;
  Invalidate();
  m_pDocument->SetModifiedFlag();
}

void CDrawObj::SetLineStyle(int style)
{
  ASSERT_VALID(this);
  
  m_logpen.lopnStyle = style;
  if(style==PS_DASH ||style==PS_DOT ||style==PS_DASHDOT ||style==PS_DASHDOTDOT)
    m_logpen.lopnWidth.x=0;//Dick 23.03.2000
}

void CDrawObj::SetFillColor( COLORREF color )
{
  ASSERT_VALID( this );
  
  m_logbrush.lbColor = color;

  Invalidate();
  
  m_pDocument->SetModifiedFlag();
}

void CDrawObj::SetLogBrush( const COLORREF color, const UINT style, const LONG hatch )
{
  ASSERT_VALID( this );

  m_logbrush.lbColor = color;
  m_logbrush.lbStyle = style;
  m_logbrush.lbHatch = hatch;
}

void CDrawObj::SetType(int type)
{
  m_nType = type;
}

void CDrawObj::SetTextOrientation(int angle)
{
  ASSERT(angle>=0 && angle<360);
  
  m_logfont.lfEscapement = angle*10;
  m_logfont.lfOrientation = angle*10;
}

void CDrawObj::SetFlags( long flags, BOOL bUpdate /*=FALSE*/ )
{
  x_flags |= flags;
}; // SetFlags

void CDrawObj::UnsetFlags( long flags, BOOL bUpdate /*=FALSE*/ )
{
  x_flags &= ~flags;
}; // UnsetFlags

BOOL CDrawObj::HasFont() const
{
  if (x_flags & font)
    return 1;
  else
    return 0;
}

BOOL CDrawObj::HasPen() const
{
  if (x_flags & pen)
    return 1;
  else
    return 0;
}

BOOL CDrawObj::HasBrush() const
{
  if( x_flags & brush )
    return 1;
  else
    return 0;
}

BOOL CDrawObj::HasArrow() const
{
  if( x_flags & arrow )
    return 1;
  else
    return 0;
}

BOOL CDrawObj::IsFilled() const
{
  if (x_flags & filled)
    return 1;
  else
    return 0;
}

BOOL CDrawObj::IsUser() const
{
  if( x_flags & user )
    return 1;
  else
    return 0;
}

BOOL CDrawObj::IsInvisible() const
{
  if (x_flags & invisible)
    return 1;
  else
    return 0;
}

BOOL CDrawObj::IsEditable() const
{
  if (x_flags & editable)
    return 1;
  else
    return 0;
}

BOOL CDrawObj::IsMoveable() const
{
  if( x_flags & moveable )
    return 1;
  else
    return 0;
}

BOOL CDrawObj::IsOffsetable() const
{
  if( x_flags & offsetable )
    return TRUE;
  else
    return FALSE;
} // IsFixed

BOOL CDrawObj::IsHideable() const
{
  if( x_flags & hideable )
    return TRUE;
  else
    return FALSE;
} // IsFixed

BOOL CDrawObj::IsMovedFrom( const CDrawObj& pOther ) const
{
  if( IsOffsetable() && GetOffset() != pOther.GetOffset() )
    return TRUE;
  else if( m_dPosition != pOther.m_dPosition )
    return TRUE;
  else
    return FALSE;
}


void CDrawObj::AddConnection(CDrawObj* pObj)
{
  if( m_pConnections == NULL )
    m_pConnections = new CDrawObjList;
  if( m_pConnections->FindObject( pObj ) == NULL )
  {
    m_pConnections->AddTailObject( pObj );
    pObj->AddConnection( this );
  }
}

void CDrawObj::RemoveConnections( BOOL bDeleteContents /* = FALSE */ )
{
  if( m_pConnections != NULL )
  {
    if( bDeleteContents )
      m_pConnections->DeleteContents( FALSE ); // nicht rekursieren!

    delete m_pConnections;
    m_pConnections = NULL;
  }
}

void CDrawObj::UpdateConnections()
{
  POSITION pos;
  CDrawObj *pConnection;
  
  if (m_pConnections!=NULL)
  {
    pos = m_pConnections->GetHeadPosition();
    while (pos!=NULL)
    {
      pConnection = m_pConnections->GetNextObject(pos);
      pConnection->m_logfont = m_logfont;
      pConnection->m_logpen = m_logpen;
      pConnection->m_logbrush = m_logbrush;
      pConnection->m_colorText = m_colorText;
      
      m_pDocument->UpdateAllViews(NULL, HINT_UPDATE_DRAWOBJ, pConnection);
    }
  }
}

void CDrawObj::SetClipRange( double from, double to, double bottom )
{
  m_dFrom = from;
  m_dTo = to;
  m_dBottom = bottom;
  m_bClip = FALSE;
  if( m_dPosition.left < from )
    m_bClip = TRUE;
  if( m_dPosition.right > to )
    m_bClip = TRUE;
  if( m_dPosition.bottom < bottom )
    m_bClip = TRUE;
}

void CDrawObj::DeletePattern()
{
  if (m_hPattern!=NULL)
  {
    ::DeleteObject(m_hPattern);
    m_hPattern = NULL;
  }
}

/* virtual */ CDoubleIRect CDrawObj::CalcBounds( CMap<double, double, double, double>& mins, CMap<double, double, double, double>& maxs )
{
  CDoubleIRect rect( m_dPosition );
  rect.NormalizeRect();

  return m_dPosition; 
}

void CDrawObj::ScaleText( const double faktor )
// Ändert die Textgrösse dieses Objekts, falls es Text besitzt um einen bestimmten Faktor
// Paramter:
//   const double faktor: Skalierungsfaktor in Prozent ( 1.0 = Grösse bleibt gleich )
{
  m_logfont.lfHeight = (long)( m_logfont.lfHeight * faktor );
  m_logfont.lfWidth = (long)( m_logfont.lfWidth * faktor );
} // ScaleText

void CDrawObj::AssertValid()
{
  ASSERT(m_position.left <= m_position.right);
  ASSERT(m_position.bottom <= m_position.top);
}


#ifdef _DEBUG
/*!
* Loggt sich selbst in die übergebende Category
*
* @param cat : hier hinein wird geloggt
*
*/
void CDrawObj::debug( log4cpp::Category& cat ) const
{
  using namespace log4cpp;

  cat.debug( "A CDrawObj at %p with position", this );
  m_position.debug( cat );
  cat.debug( "and dPosition" );
  m_dPosition.debug( cat );
  cat.debug( "and Offset" );
  GetOffset().debug( cat );
}; // Dump
#endif _DEBUG

////////////////////////////////////////////////////////////////////////////
// CDrawObjList

CDrawObjList::CDrawObjList()
{
  m_rect = CIntIRect( 0, 0, 0, 0 );
}

void CDrawObjList::DeleteContents( BOOL bDestroyConns )
// Parameter:
//      BOOL bDestroyConns: zerstört auc alle Objekte aller Connections
{
  POSITION pos = GetHeadPosition();
  while( pos != NULL )
  {
    CDrawObj* pObj = GetNextObject( pos );
    pObj->RemoveConnections( bDestroyConns ); // die connections und alle Ihre Objekte zerstören, wenn gewünscht
    pObj->Remove();
  }
  RemoveAllObjects();
} // DeleteCons

void CDrawObjList::RemoveAllObjects()
{
  m_list.RemoveAll(); 
}; // RemoveAllObjects


void CDrawObjList::RemoveConnections( BOOL bDestroyConns )
// löscht alle Connections von alen Objekten in dieser Liste
// d.h. ruft für jedes Objekt RemoveConnections auf
{
  POSITION pos = GetHeadPosition();
  while( pos != NULL )
  {
    CDrawObj* pObj = GetNextObject( pos );
    pObj->RemoveConnections( bDestroyConns );
  } // while pos
} // RemoveConnections

void CDrawObjList::AddTailObjects( CDrawObjList* pObjs )
// Fügt alle Objekte aus der übergebenen Liste zu dieser hinzu
{
  m_list.AddTail( &pObjs->m_list ); 
}

void CDrawObjList::AddTailObjects( CDrawObjListArray* pObjsArray )
// güt ein ganzes Array von ObjektListen ans Ende an
{
  for( int i = 0; i < pObjsArray->GetSize(); i++ )
    AddTailObjects( pObjsArray->GetAt( i ) );
} // AddTailObjects

BOOL CDrawObjList::RemoveObject( CDrawObj* pObj )
// Löscht ein Objekt aus der Liste
// Rückgabewert:
//        TRUE, falls das Objekt gefunden und gelöscht wurde
{
  POSITION pos = FindObject( pObj );
  if( pos != NULL )
  {
    RemoveObjectAt( pos );
    return TRUE;
  }
  else
    return FALSE;
} // RemoveObject

void CDrawObjList::Serialize( CArchive& ar )
{
  m_list.Serialize( ar );
  
  if( ar.IsStoring() )
    ar << m_rect;
  else
    ar >> m_rect;
}; // Serialize

void CDrawObjList::SerializeNew( CArchive& ar )
{
  if( ar.IsStoring() )
  {
    ar << (WORD)0;  // Versionsnummer
    ar << m_rect;

    ar << GetObjectCount();
    POSITION pos = GetHeadPosition();
    while( pos )
      ar << GetNextObject( pos );
  }
  else
  {
    WORD nVersion;
    ar >> nVersion;

    ar >> m_rect;

    int objCount = 0;
    ar >> objCount;
    while( objCount-- > 0 )
    {
      CDrawObj* pObj = NULL;
      ar >> pObj;
      
      AddTailObject( pObj );
    }; // for i
  }
}; // Serialize


void CDrawObjList::SetVisibility( CDrawObj* pObj, BOOL bVisible, CDrawObjList* pUndoList )
{
  POSITION pos = GetHeadPosition();
  while( pos )
    GetNextObject( pos )->SetVisibility( pObj, bVisible, pUndoList );
}; // SetVisibility

void CDrawObjList::SetLayer( const int layer )
// SetLayer für alle Objekte in der Liste aufrufen
{
  POSITION pos = GetHeadPosition();
  while( pos )
    GetNextObject( pos )->SetLayer( layer );
} // SetLayer

void CDrawObjList::SetIndex( const int index )
// SetLayer für alle Objekte in der Liste aufrufen
{
  POSITION pos = GetHeadPosition();
  while( pos )
    GetNextObject( pos )->SetIndex( index );
} // SetLayer


CSize CDrawObjList::GetExtent() const
// kummuliert die Grössen aller sichtbaren, enthaltenen Objekte und gibt dies als Size zurück
{
  CIntIRect rect( CalcRect() );
  return CSize( (int)rect.Width(), (int)rect.Height() );
} // CalcSize

CIntIRect CDrawObjList::CalcRect() const
{
  if( GetObjectCount() > 0 )
  {
    CIntIRect rect( INT_MAX, INT_MIN, INT_MIN, INT_MAX );
    POSITION pos = GetHeadPosition();
    while( pos!=NULL )
    {
      CDrawObj* pObj = GetNextObject( pos );
      if( !pObj->IsInvisible() )
      {
        CIntIRect objRect( pObj->GetClippedRect() );
        objRect.NormalizeRect();
        if( !objRect.IsNullRect() )
          rect.CompareAndExpand( objRect );
      }
    } // while pos
    
    return rect;
  }
  else
    return CIntIRect( 0, 0, 0, 0 );
}; // GetRect

void CDrawObjList::SetRectSize( const CSize& size )
{
  m_rect.right = m_rect.left + size.cx;
  m_rect.top = m_rect.bottom + size.cy;
};

void CDrawObjList::SetRectPos( const CIntPoint& pos )
{
  m_rect.left = pos.x;
  m_rect.bottom = pos.y;
}

void CDrawObjList::Intersect( CDrawObjList* pObs, BOOL bDestroyObs )
// Löscht alle Objekte, die nicht auch n der Referenzliste sind
// Parameter:
//        CDrawObjList* pObs: nur Objekte, die auch in dieser Liste sind bleiben
//        BOOL bDestroyObs: falls TRUE, werden die Objekte nicht nur dereferenziert, sondern auch gelöscht
{
  CDrawObjList tempList; // termporäre Liste für die Objekte die bleiben sollen

  POSITION pos = GetHeadPosition();
  while( pos != NULL )
  {
    CDrawObj* pOb = GetNextObject( pos );

    // falls das Objekt auch in der anderen Liste ist, in die Temporäre Liste aufnehmen
    if( pObs->FindObject( pOb ) )
      tempList.AddTailObject( pOb );
    else
    {
      // falls das Objekt nicht in der anderen Liste ist, löschen
      if( bDestroyObs )
        pOb->Remove();
    }
  } // while pos

  // jetzt alle Objekte durch die in der TempListe ersetzen
  RemoveAllObjects();
  AddTailObjects( &tempList );
} // Intersect

void CDrawObjList::SetFlags( long flags )
{
  POSITION pos = GetHeadPosition();
  while( pos != NULL )
    GetNextObject( pos )->SetFlags( flags );
}; // SetFlags

void CDrawObjList::UnsetFlags( long flags )
{
  POSITION pos = GetHeadPosition();
  while( pos != NULL )
    GetNextObject( pos )->UnsetFlags( flags );
}; // UnsetFlags

void CDrawObjList::ScaleText( const double faktor )
{
  POSITION pos = GetHeadPosition();
  while( pos != NULL )
    GetNextObject( pos )->ScaleText( faktor );
} // ScaleText


/*!
 * Kopiert den Inhalt in eine andere Liste
 *
 * @param pOther : die Zielliste
 *
 */
void CDrawObjList::CopyTo( CDrawObjList* pOther )
{
  if( !pOther )
    return;

  POSITION pos = GetHeadPosition();
  while( pos )
    pOther->AddTailObject( GetNextObject( pos ) );
}; // CopyTo



/*!
 * Loggt sich selbst mit Priorität debug in eine Kategorie
 *
 */
void CDrawObjList::debug( log4cpp::Category& cat ) const
{
  using namespace log4cpp;

  DEBUG_ONLY( cat.debug( "A CDrawObjList at %p with %d Elements and ", this, GetObjectCount() ) );
  DEBUG_ONLY( m_rect.debug( cat ) );
  DEBUG_ONLY( cat.debug( "\n" ) );

  POSITION pos = GetHeadPosition();
  while( pos )
  {
    CDrawObj* pObj = GetNextObject( pos );
    DEBUG_ONLY( pObj->debug( cat ) );
  }
} // Dump

////////////////////////////////////////////////////////////////////////////
// CDrawObjListArray

CDrawObjListArray::CDrawObjListArray()
{
  m_rect = CIntIRect( 0, 0, 0, 0 );
}

void CDrawObjListArray::DeleteContents()
{
  DeleteAllIndices( TRUE, TRUE );
}

void CDrawObjListArray::DeleteAllIndices( const BOOL bDestroyLists, const BOOL bDestroyObjs )
// zerstört Inhalte der CDrawObjListArray
// Parameter:
//        const BOOL bDestroyLists: falls TRUE, werden die referenzierten Listen auch zerstört
//        const BOOL bDestroyObjects: falls TRUE ( und nur falls auc bDestroyLists TRUE ), werden die
//                                  in den Listen referenzierten Objekte zerstört
{
  // falls Listen zerstört werden sollen dies tun
  if( bDestroyLists == TRUE )
  {
    for( int i = 0; i < GetSize(); i++ )
    {
      if( bDestroyObjs )
        GetAt( i )->DeleteContents( FALSE );
      delete GetAt(i);
    } // for i
  } // if bDetroyLists
  
  m_array.RemoveAll();
} // DeleteAllIndices

int CDrawObjListArray::FindObjectIndex( CDrawObj* pObj )
{
  for( int i = 0; i < GetSize(); i++ )
  {
    if( GetAt( i )->FindObject( pObj ) != NULL )
      return i;
  }
  
  return -1;
} // FindObjectIndex

void CDrawObjListArray::Serialize(CArchive& ar)
{
  CDrawObjList *pObjList;
  
  m_array.CObject::Serialize( ar );
  if (ar.IsStoring())
  {
    ar.WriteCount( GetSize() );
    ar << m_rect;
    for( int i = 0; i < GetSize(); i++ )
      GetAt( i )->Serialize( ar );
  }
  else
  {
    DWORD nOldSize = ar.ReadCount();
    SetSize( nOldSize );
    ar >> m_rect;
    for ( int i = 0; i < GetSize(); i++)
    {
      pObjList = new CDrawObjList;
      pObjList->Serialize( ar );
      m_array.SetAtGrow( i, pObjList );
    }
  }
}; // Serialize

void CDrawObjListArray::SetVisibility( CDrawObj* pObj, BOOL bVisible, CDrawObjList* pUndoList )
{
  for ( int i = 0; i < GetSize(); i++ )
    GetAt( i )->SetVisibility( pObj, bVisible, pUndoList );
}; // SetVisibility

void CDrawObjListArray::InitIndex()
{
  for ( int i = 0; i < GetSize(); i++ )
    GetAt( i )->SetIndex( i );
} // InitIndex

void CDrawObjListArray::SetLayer( const int layer )
// SetLayer für alle Listen aus dem Array aufrufen
{
  for ( int i = 0; i < GetSize(); i++ )
    GetAt( i )->SetLayer( layer );
} // SetLayer

void CDrawObjListArray::SetRectSize( const CSize& size )
{
  m_rect.right = m_rect.left + size.cx;
  m_rect.top = m_rect.bottom + size.cy;
};

void CDrawObjListArray::SetRectPos( const CPoint& pos )
{
  m_rect.left = pos.x;
  m_rect.bottom = pos.y;
}

int CDrawObjListArray::CreateNewIndex()
// Fügt eine neue Liste in das Array ein
// Rückgabewert:
//        int: der Index der neuen Liste
{
  return m_array.Add( new CDrawObjList() );
} // CreateNewIndex

void CDrawObjListArray::AddObject( int index, CDrawObj* pObj )
{
  ASSERT( index >= 0 && index < GetSize() );

  if( index >= 0 && index < GetSize() )
    GetAt( index )->AddTailObject( pObj );
} // Add

BOOL CDrawObjListArray::RemoveObject( CDrawObj* pObj, int* index /* = NULL */ )
// Löscht ein Objekt aus der Tabelle
// Parameter:
//        CDrawObj* pObj: dieses Objekt wird gelöscht
//        int& index: hier wird zurückgegebenm unter welchem Index es inder Tabelle war
// Rückgabewert:
//        TRUE, falls das Objekt in der Tabelle war, sonst FALSE
{
  int id = FindObjectIndex( pObj );
  if( index != NULL )
    *index = id;

  if( id != -1 )
  {
    CDrawObjList* pObjList = GetAt( id );
    POSITION pos = pObjList->FindObject( pObj );
    if ( pos != NULL )
    {
      pObjList->RemoveObjectAt( pos );
      return TRUE;
    }
  }

  return FALSE;
} // Remove

void CDrawObjListArray::InsertIndex( int nIndex, CDrawObjList* newElement, BOOL bChangeIndex )
// fügt eine CDrawObjList an eine bestimmte Stelle im Array ein
// sorgt auch dafür, dass die Objekt Indices stehts stimmen, falls bChangeIndex = TRUE
{
  m_array.InsertAt( nIndex, newElement, 1 ); 

  if( bChangeIndex )
    InitIndex();
}; // InsertIndex

void CDrawObjListArray::SetFlags( long flags )
{
  for( int i = 0; i < GetSize(); i++ )
    GetAt( i )->SetFlags( flags );
} // SetFlags

void CDrawObjListArray::UnsetFlags( long flags )
{
  for( int i = 0; i < GetSize(); i++ )
    GetAt( i )->UnsetFlags( flags );
} // UnSetFlags


/*!
 * Kopiert alle Elemente aller enthaltenen Liste in eine Zielliste.
 *
 * @param pObjList : die Zielliste
 *
 */
void CDrawObjListArray::CopyTo( CDrawObjList* pObjList )
{
  for( int i = 0; i < GetSize(); i++ )
    GetAt( i )->CopyTo( pObjList );
};

#ifdef _DEBUG
void CDrawObjListArray::Dump( CDumpContext& dc ) const
{
  dc << "A CDrawObjListArray with " << GetSize() << " Elements\n";
  dc << "and CIntIRect " << m_rect << "\n";
  for( int i = 0; i < GetSize(); i++ )
    GetAt( i )->Dump( dc );
  dc << "\n";
}
#endif _DEBUG

////////////////////////////////////////////////////////////////////////////
// CDrawRect

IMPLEMENT_SERIAL(CDrawRect, CDrawObj, VERSIONABLE_SCHEMA | 7 )

CDrawRect::CDrawRect() : CDrawObj()
{
  InitializeRect();
}

CDrawRect::CDrawRect( const CIntIRect& position, CDrawDoc* pDoc ) : CDrawObj( position, pDoc )
{
  InitializeRect();
}

CDrawRect::CDrawRect( const CDoubleIRect& position, CDrawDoc* pDoc ) : CDrawObj( position, pDoc )
{
  InitializeRect();
}

void CDrawRect::InitializeRect()
{
  ASSERT_VALID( this );
  
  m_nShape = line;
  SetHorzJust( nojust );
  SetVertJust( nojust );
  SetTextType( normal );
  m_nStempelTextType = STPL_TEXT_NONE;
  m_nHorzAdjust = -1;
  m_nVertAdjust = -1;
  m_nSelStartPos = m_nSelEndPos = 0;
  m_bShowNegative = TRUE;
  m_nPrecision = 3;
}; // Initialize

void CDrawRect::Serialize(CArchive& ar)
{
  ASSERT_VALID(this);
  
  CDrawObj::Serialize(ar);
  if (ar.IsStoring())
  {
    ar << (WORD)m_nShape;
    ar << (WORD)m_nHorzJust;
    ar << (WORD)m_nVertJust;
    ar << (WORD)m_nTextType;
    ar << m_nStempelTextType;
    ar << m_text;
    ar << m_arcLeft;
    ar << m_arcCenter;
    ar << m_arcRight;
    ar << m_nHorzAdjust;
    ar << m_nVertAdjust;
    ar << (WORD)m_bShowNegative;
    ar << m_nPrecision;
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
      {
        WORD wTemp;
        ar >> wTemp;
        m_nShape = (Shape)wTemp;

        ar >> wTemp;
        m_nHorzJust = (Justification)wTemp;

        ar >> wTemp;
        m_nVertJust = (Justification)wTemp;

        ar >> wTemp;
        m_nTextType = (TextType)wTemp;

        ar >> m_nStempelTextType;
        ar >> m_text;

        if( m_nTextType == xcoord || m_nTextType == ycoord )
        {
          // Dezimaltrennzeichen entsprechend setzten, sonst kommt später schrott raus
          char* decPoint = ::localeconv()->decimal_point;
          m_text.Replace( ".", decPoint );
          m_text.Replace( ".", decPoint );
        }
        
        ar >> m_arcLeft;
        ar >> m_arcCenter;
        ar >> m_arcRight;
        ar >> m_nHorzAdjust;
        ar >> m_nVertAdjust;
        if( nVersion >= 3 )
        {
          ar >> wTemp;
          m_bShowNegative = (BOOL)wTemp;
        }

        if( nVersion >= 4 )
          ar >> m_nPrecision;
      }
      break;
      
    default:
      AfxThrowArchiveException(CArchiveException::badSchema);
      break;
    }
  }
}

void CDrawRect::CalcDrawingHelpers(CDrawView *pView)
{
  CIntIRect rcTemp;
  //CString str, temp;
  //CSize sizeText, sizeOutput;
  int x, y;
  
  
  // scale the font
  CDC *pDC = pView->GetDC();
  m_helpers[0] = TransformFontHeight(pDC, m_logfont.lfHeight );
  pView->ReleaseDC(pDC);
  
  CIntIRect rect( GetAdjustedRect() );
  switch (m_nShape)
  {
  default:
    break;
    
  case line:
    rcTemp = rect;
    if (rect.top > rect.bottom)
    {
      if (m_nRightArrow==closed || m_nRightArrow==solid)
        rcTemp.top -= (int)(cos(PI/4)*abs(rect.Height())/FACTOR_ARROW);
      if (m_nLeftArrow==closed || m_nLeftArrow==solid)
        rcTemp.bottom += (int)(cos(PI/4)*abs(rect.Height())/FACTOR_ARROW);
    }
    else
    {
      if (m_nRightArrow==closed || m_nRightArrow==solid)
        rcTemp.top += (int)(cos(PI/4)*abs(rect.Height())/FACTOR_ARROW);
      if (m_nLeftArrow==closed || m_nLeftArrow==solid)
        rcTemp.bottom -= (int)(cos(PI/4)*abs(rect.Height())/FACTOR_ARROW);
    }
    
    if (rect.left > rect.right)
    {
      if (m_nRightArrow==closed || m_nRightArrow==solid)
        rcTemp.left -= (int)(cos(PI/4)*abs(rect.Width())/FACTOR_ARROW);
      if (m_nLeftArrow==closed || m_nLeftArrow==solid)
        rcTemp.right += (int)(cos(PI/4)*abs(rect.Width())/FACTOR_ARROW);
    }
    else
    {
      if (m_nRightArrow==closed || m_nRightArrow==solid)
        rcTemp.left += (int)(cos(PI/4)*abs(rect.Width())/FACTOR_ARROW);
      if (m_nLeftArrow==closed || m_nLeftArrow==solid)
        rcTemp.right -= (int)(cos(PI/4)*abs(rect.Width())/FACTOR_ARROW);
    }
    
    m_helpers[1] = rcTemp.left;
    m_helpers[2] = rcTemp.top;
    m_helpers[3] = rcTemp.right;
    m_helpers[4] = rcTemp.bottom;
    m_helpers[5] = rect.left;
    m_helpers[6] = rect.top;
    m_helpers[7] = rect.right;
    m_helpers[8] = rect.bottom;
    
    switch (m_nRightArrow)
    {
    default:
    case noarrow:
      break;
      
    case open:
      x = (int)(rect.left + (cos(PI/4)*(rect.right-rect.left) + sin(PI/4)*(rect.bottom-rect.top))/FACTOR_ARROW);
      y = (int)(rect.top + (cos(PI/4)*(rect.bottom-rect.top) - sin(PI/4)*(rect.right-rect.left))/FACTOR_ARROW);
      m_helpers[9] = x;
      m_helpers[10] = y;
      x = (int)(rect.left + (cos(-PI/4)*(rect.right-rect.left) + sin(-PI/4)*(rect.bottom-rect.top))/FACTOR_ARROW);
      y = (int)(rect.top + (cos(-PI/4)*(rect.bottom-rect.top) - sin(-PI/4)*(rect.right-rect.left))/FACTOR_ARROW);
      m_helpers[11] = x;
      m_helpers[12] = y;
      break;
      
    case closed:
    case solid:
      x = (int)(rect.left + (cos(PI/4)*(rect.right-rect.left) + sin(PI/4)*(rect.bottom-rect.top))/FACTOR_ARROW);
      y = (int)(rect.top + (cos(PI/4)*(rect.bottom-rect.top) - sin(PI/4)*(rect.right-rect.left))/FACTOR_ARROW);
      m_helpers[9] = x;
      m_helpers[10] = y;
      x = (int)(rect.left + (cos(-PI/4)*(rect.right-rect.left) + sin(-PI/4)*(rect.bottom-rect.top))/FACTOR_ARROW);
      y = (int)(rect.top + (cos(-PI/4)*(rect.bottom-rect.top) - sin(-PI/4)*(rect.right-rect.left))/FACTOR_ARROW);
      m_helpers[11] = x;
      m_helpers[12] = y;
      break;
    }
    if (m_nRightArrow==solid)
    {
      x = (int)(rect.left + (cos(PI/8)*(rect.right-rect.left) + sin(PI/8)*(rect.bottom-rect.top))/(2*FACTOR_ARROW));
      y = (int)(rect.top + (cos(PI/8)*(rect.bottom-rect.top) - sin(PI/8)*(rect.right-rect.left))/(2*FACTOR_ARROW));
      m_helpers[17] = x;
      m_helpers[18] = y;
    }
    switch (m_nLeftArrow)
    {
    default:
    case noarrow:
      break;
      
    case open:
      x = (int)(rect.right + (cos(PI/4)*(rect.left-rect.right) + sin(PI/4)*(rect.top-rect.bottom))/FACTOR_ARROW);
      y = (int)(rect.bottom + (cos(PI/4)*(rect.top-rect.bottom) - sin(PI/4)*(rect.left-rect.right))/FACTOR_ARROW);
      m_helpers[13] = x;
      m_helpers[14] = y;
      x = (int)(rect.right + (cos(-PI/4)*(rect.left-rect.right) + sin(-PI/4)*(rect.top-rect.bottom))/FACTOR_ARROW);
      y = (int)(rect.bottom + (cos(-PI/4)*(rect.top-rect.bottom) - sin(-PI/4)*(rect.left-rect.right))/FACTOR_ARROW);
      m_helpers[15] = x;
      m_helpers[16] = y;
      break;
      
    case closed:
    case solid:
      x = (int)(rect.right + (cos(PI/4)*(rect.left-rect.right) + sin(PI/4)*(rect.top-rect.bottom))/FACTOR_ARROW);
      y = (int)(rect.bottom + (cos(PI/4)*(rect.top-rect.bottom) - sin(PI/4)*(rect.left-rect.right))/FACTOR_ARROW);
      m_helpers[13] = x;
      m_helpers[14] = y;
      x = (int)(rect.right + (cos(-PI/4)*(rect.left-rect.right) + sin(-PI/4)*(rect.top-rect.bottom))/FACTOR_ARROW);
      y = (int)(rect.bottom + (cos(-PI/4)*(rect.top-rect.bottom) - sin(-PI/4)*(rect.left-rect.right))/FACTOR_ARROW);
      m_helpers[15] = x;
      m_helpers[16] = y;
      break;
    }
    if (m_nLeftArrow==solid)
    {
      x = (int)(rect.right + (cos(PI/8)*(rect.left-rect.right) + sin(PI/8)*(rect.top-rect.bottom))/(2*FACTOR_ARROW));
      y = (int)(rect.bottom + (cos(PI/8)*(rect.top-rect.bottom) - sin(PI/8)*(rect.left-rect.right))/(2*FACTOR_ARROW));
      m_helpers[19] = x;
      m_helpers[20] = y;
    }
    break;
    
    case text:
      {
        rect.NormalizeRect();

        // get size of text
        CString temp;
        GetText(temp);

        CString str = ' ' + temp + ' ';

        CSize sizeOutput = GetOutputTextSize(pView);
        CSize sizeText = m_sizeText;
        // calculate size after text is rotated
        double angle = m_logfont.lfEscapement*2*PI/3600;
        sizeOutput.cx = int( fabs(sizeText.cx*cos(angle)) + fabs(sizeText.cy*sin(angle)) );
        sizeOutput.cy = int( fabs(sizeText.cy*cos(angle)) + fabs(sizeText.cx*sin(angle)) );
        
        // calculate offset of first char from center of text
        int x = int( (sizeText.cy*sin(angle)-sizeText.cx*cos(angle))/2 );
        int y = int( (sizeText.cx*sin(angle)+sizeText.cy*cos(angle))/2 );
        
        // find the center of the text for the relevant justification
        switch( m_nHorzJust )
        {
          // text must have a justification
        default:
          ASSERT(FALSE);
          
        case left:
          x += rect.left + int(sizeOutput.cx/2);
          break;
          
        case center:
          x += rect.left + int(rect.Width()/2);
          break;
          
        case right:
          x += rect.right - int(sizeOutput.cx/2);
          break;
        }
        switch (m_nVertJust)
        {
          // text must have a justification
        default:
          ASSERT(FALSE);
          
        case left:    // top
          y += rect.top - int( sizeOutput.cy / 2 );
          break;
          
        case center:
          y += rect.bottom + int( rect.Height() / 2 );
          break;
          
        case right:   // bottom
          y += rect.bottom + int(sizeOutput.cy/2);
          break;
        }
        m_helpers[1] = x;
        m_helpers[2] = y;
      }
      break;
  }
}

void CDrawRect::Draw( CDC* pDC )
{
#ifdef _DEBUG
  if( m_bBreakInDraw )
  {
    DebugBreak();
    m_bBreakInDraw = FALSE;
  }
#endif

  // nur zeichnen, wenn sichtbar
  if( IsInvisible() )
    return;

  LOGFONT logfont;
  int i, x, y;
  CPoint points[3]; // for drawing solid arrows
  CPoint start, end;  // for drawing arcs
  CIntIRect bounding;   // for drawing arcs
  CDoubleIRect temp;
  CDoublePoint point;
  double ax, ay, bx, by, cx, cy, xx, yy, r;
  CString str, strText;
  CSize sizeText, sizeOutput;
  double angle;
  
  HBITMAP hOldBitmap;
  
  ASSERT_VALID(this);
  
  if (m_nShape==line && GetOffset().x==-1)
    return;
  
  CBrush brush;
  if (m_logbrush.lbStyle==BS_PATTERN)
  { // we must correctly color the bitmap used for the pattern
    // save the handle to the original bitmap
    hOldBitmap = (HBITMAP)m_logbrush.lbHatch;
    if (m_hPattern==NULL)
    {
      // create a newly colored bitmap
      m_hPattern = ::CreateNewColoredBitmap(hOldBitmap, RGB(255, 255, 255), RGB(0, 0, 0), RGB(255, 255, 255), m_logbrush.lbColor);
    }
    // set the newly colored bitmap as the one to be used
    m_logbrush.lbHatch = (LONG)m_hPattern;
  }
  if (!brush.CreateBrushIndirect(&m_logbrush))
    return;
  CPen pen, solidpen;
  if (!pen.CreatePenIndirect(&m_logpen))
    return;
  if (m_nShape==line && (m_nLeftArrow==solid || m_nRightArrow==solid))
  {
    LOGPEN logpen = m_logpen;
    logpen.lopnStyle = PS_SOLID;
    if (!solidpen.CreatePenIndirect(&logpen))
      return;
  }
  CFont font;
  logfont = m_logfont;
  // get scaled font height
  logfont.lfHeight = m_helpers[0];
  if (!font.CreateFontIndirect(&logfont))
    return;
  
  CBrush *pOldBrush, *pOldBrush2, *pBrush;
  CPen* pOldPen, *pOldPen2;
  CFont* pOldFont;
  COLORREF colorOld;
  int nOldFillMode;
  
  if (HasBrush() && IsFilled())
    pOldBrush = pDC->SelectObject( &brush );
  else
    pOldBrush = (CBrush*)pDC->SelectStockObject( NULL_BRUSH );
  
  if (HasPen())
    pOldPen = pDC->SelectObject(&pen);
  else
    pOldPen = (CPen*)pDC->SelectStockObject( NULL_PEN );
  
  if (HasFont())
    pOldFont = pDC->SelectObject( &font );
  else
    pOldFont = (CFont*)pDC->SelectStockObject(ANSI_FIXED_FONT);
  
  colorOld = pDC->SetTextColor(m_colorText);
  
  CRect rect, clipRect, rcTemp;
  CIntIRect intRect( GetAdjustedRect() );
  rect = (CRect)intRect;

  if (m_bClip)
  {
    pDC->SaveDC();
    pDC->GetClipBox(&clipRect);
    if (m_nShape==line)
    {
      rect = m_position;
      if (m_nHorzAdjust!=-1)
        rect.right += m_nHorzAdjust;
    }

    if( ( m_dPosition.left < m_dFrom && m_dPosition.right < m_dFrom ) || 
      ( m_dPosition.right > m_dTo && m_dPosition.left > m_dTo ) )
      clipRect = CIntIRect(0, 0, 0, 0);
    else if( m_dPosition.Width() != 0 )
    {
      clipRect.left = rect.left + (int)(rect.Width()*(m_dFrom-m_dPosition.left)/m_dPosition.Width());
      clipRect.right = rect.right + (int)(rect.Width()*(m_dTo-m_dPosition.right)/m_dPosition.Width());
      if (m_nShape==line && m_nHorzJust!=nojust)
      { // adjustment for lines : expand clipRect one pixel to the right
        int hppmm = MulDiv(pDC->GetDeviceCaps(HORZRES), 1, pDC->GetDeviceCaps(HORZSIZE));
        clipRect.right += (int)(2*MM_FACTOR/hppmm);
      }
    }

    if (m_dPosition.Height()!=0)
    {
      clipRect.bottom = rect.bottom - (int)(rect.Height()*(m_dBottom-m_dPosition.bottom)/m_dPosition.Height());
      if (m_nShape==line && m_nVertJust!=nojust)
      { // adjustment for lines : expand clipRect one pixel down
        int vppmm = MulDiv(pDC->GetDeviceCaps(VERTRES), 1, pDC->GetDeviceCaps(VERTSIZE));
        clipRect.bottom -= (int)(2*MM_FACTOR/vppmm);
      }
    }
    else if (m_dPosition.bottom<m_dBottom)
      clipRect = CIntIRect(0, 0, 0, 0);
    pDC->IntersectClipRect(&clipRect);
  }

  pDC->SetBkMode( TRANSPARENT );

  switch( m_nShape )
  {
  case rectangle:
    pDC->Rectangle( rect );
    break;
    
  case ellipse:
    pDC->Ellipse( rect );
    break;
    
  case line:
    rcTemp.SetRect(m_helpers[1], m_helpers[2], m_helpers[3], m_helpers[4]);
    rect.SetRect(m_helpers[5], m_helpers[6], m_helpers[7], m_helpers[8]);
    pDC->MoveTo( rcTemp.TopLeft() );
    pDC->LineTo( rcTemp.BottomRight() );
    switch (m_nRightArrow)
    {
    default:
    case noarrow:
      break;
      
    case open:
      pDC->MoveTo(rect.TopLeft());
      pDC->LineTo(CPoint(m_helpers[9], m_helpers[10]));
      pDC->MoveTo(rect.TopLeft());
      pDC->LineTo(CPoint(m_helpers[11], m_helpers[12]));
      break;
      
    case closed:
      pDC->MoveTo(rect.TopLeft());
      pDC->LineTo(CPoint(m_helpers[9], m_helpers[10]));
      pDC->LineTo(CPoint(m_helpers[11], m_helpers[12]));
      pDC->LineTo(rect.TopLeft());
      break;
      
    case solid:
      pOldPen2 = pDC->SelectObject(&solidpen);
      points[0] = rect.TopLeft();
      points[1] = CPoint(m_helpers[9], m_helpers[10]);
      points[2] = CPoint(m_helpers[11], m_helpers[12]);
      pBrush = new CBrush(pDC->GetNearestColor(m_logpen.lopnColor));
      pOldBrush2 = pDC->SelectObject(pBrush);
      nOldFillMode = pDC->SetPolyFillMode(ALTERNATE);
      pDC->Polygon(points, 3);
      pDC->SetPolyFillMode(nOldFillMode);
      pDC->SelectObject(pOldBrush2);
      delete pBrush;
      pDC->SelectObject(pOldPen2);
      break;
    }
    switch (m_nLeftArrow)
    {
    default:
    case noarrow:
      break;
      
    case open:
      pDC->MoveTo(rect.BottomRight());
      pDC->LineTo(CPoint(m_helpers[13], m_helpers[14]));
      pDC->MoveTo(rect.BottomRight());
      pDC->LineTo(CPoint(m_helpers[15], m_helpers[16]));
      break;
      
    case closed:
      pDC->MoveTo(rect.BottomRight());
      pDC->LineTo(CPoint(m_helpers[13], m_helpers[14]));
      pDC->LineTo(CPoint(m_helpers[15], m_helpers[16]));
      pDC->LineTo(rect.BottomRight());
      break;
      
    case solid:
      pOldPen2 = pDC->SelectObject(&solidpen);
      points[0] = rect.BottomRight();
      points[1] = CPoint(m_helpers[13], m_helpers[14]);
      points[2] = CPoint(m_helpers[15], m_helpers[16]);
      pBrush = new CBrush(pDC->GetNearestColor(m_logpen.lopnColor));
      pOldBrush2 = pDC->SelectObject(pBrush);
      nOldFillMode = pDC->SetPolyFillMode(ALTERNATE);
      pDC->Polygon(points, 3);
      pDC->SetPolyFillMode(nOldFillMode);
      pDC->SelectObject(pOldBrush2);
      delete pBrush;
      pDC->SelectObject(pOldPen2);
      break;
    }
    break;
    
    case text:
      rect.NormalizeRect();
      GetText(strText);
      if (!m_bShowNegative)
      {
        i = strText.Find('-');
        if( i != -1 )
          strText = strText.Left( i ) + strText.Right( strText.GetLength() - i - 1 );
      }
      if( m_nSelEndPos <= m_nSelStartPos )
      {
        // get size of text
        str = ' ';
        str += strText;
        str += ' ';
        x = m_helpers[1];
        y = m_helpers[2];
        // draw text
        pDC->ExtTextOut(x, y, ETO_CLIPPED, rect, str, NULL);
      }
      else
      {
        COLORREF oldTextColor, oldBkColor;
        
        /** left of selection (minimum of 1 space) **/
        str = ' ';
        str += strText.Left( m_nSelStartPos );
        x = m_helpers[1];
        y = m_helpers[2];
        angle = m_logfont.lfEscapement*2*PI/3600;
        
        // draw text
        pDC->ExtTextOut( x, y, ETO_CLIPPED, rect, str, NULL );
        
        sizeText = pDC->GetTextExtent( str );
        x += (int)(sizeText.cx*cos(angle));
        y -= (int)(sizeText.cx*sin(angle));
        
        /** selected text **/
        str = strText.Mid(m_nSelStartPos, m_nSelEndPos-m_nSelStartPos);
        
        // draw text
        oldTextColor = pDC->SetTextColor(GetSysColor(COLOR_HIGHLIGHTTEXT));
        oldBkColor = pDC->SetBkColor(GetSysColor(COLOR_HIGHLIGHT));
        pDC->ExtTextOut(x, y, ETO_CLIPPED, rect, str, NULL);
        pDC->SetTextColor(oldTextColor);
        pDC->SetBkColor(oldBkColor);
        sizeText = pDC->GetTextExtent(str);
        x += (int)(sizeText.cx*cos(angle));
        y -= (int)(sizeText.cx*sin(angle));
        
        /** right of selection (minimum of 1 space) **/
        str = strText.Right(strText.GetLength()-m_nSelEndPos);
        str += ' ';
        pDC->ExtTextOut(x, y, ETO_CLIPPED, rect, str, NULL);
      }
      break;
      
    case egg:
      bounding = CIntIRect(rect.left, rect.top, rect.right, rect.top-2*abs(rect.Height())/3);
      start = CPoint(rect.right, rect.top-abs(rect.Height())/3);
      end = CPoint(rect.left, rect.top-abs(rect.Height())/3);
      pDC->Arc( (CRect)bounding, start, end );
      bounding = CIntIRect(rect.left, rect.bottom+4*abs(rect.Height())/3, rect.right, rect.bottom);
      start = CPoint(rect.left, rect.top-abs(rect.Height())/3);
      end = CPoint(rect.right, rect.top-abs(rect.Height())/3);
      pDC->Arc( (CRect)bounding, start, end);
      pDC->FloodFill(rect.left+abs(rect.Width())/2, rect.bottom+abs(rect.Height())/2, m_logpen.lopnColor);
      break;
      
    case mouth:
      bounding = CIntIRect(rect.left, rect.top, rect.right, rect.top-4*abs(rect.Height())/3);
      start = CPoint(rect.right, rect.top-2*abs(rect.Height())/3);
      end = CPoint(rect.left, rect.top-2*abs(rect.Height())/3);
      pDC->Arc((CRect)bounding, start, end);
      bounding = CIntIRect(rect.left, rect.bottom+2*abs(rect.Height())/3, rect.right, rect.bottom);
      start = CPoint(rect.left, rect.top-2*abs(rect.Height())/3);
      end = CPoint(rect.right, rect.top-2*abs(rect.Height())/3);
      pDC->Arc((CRect)bounding, start, end);
      pDC->FloodFill(rect.left+abs(rect.Width())/2, rect.bottom+abs(rect.Height())/2, m_logpen.lopnColor);
      break;
      
    case arc:
      {
        ax = m_arcLeft.x;
        ay = m_arcLeft.y;
        bx = m_arcCenter.x;
        by = m_arcCenter.y;
        cx = m_arcRight.x;
        cy = m_arcRight.y;
        yy = (ax*ax+ay*ay-bx*bx-by*by)/(2*(ax-bx));
        yy -= (bx*bx+by*by-cx*cx-cy*cy)/(2*(bx-cx));
        yy *= (ax-bx)*(bx-cx);
        yy /= (ay-by)*(bx-cx)-(by-cy)*(ax-bx);
        xx = (ax*ax+ay*ay-bx*bx-by*by)/(2*(ax-bx));
        xx -= yy*(ay-by)/(ax-bx);
        r = (xx-bx)*(xx-bx)+(yy-by)*(yy-by);
        r = sqrt(r);
        temp = CDoubleIRect(xx-r, yy-r, xx+r, yy+r);

        CIntPoint startPoint, endPoint;

        m_pDocument->MetersToLogical( temp, bounding, this );
        point = CDoublePoint(cx, cy);
        m_pDocument->MetersToLogical( point, startPoint, this );
        point = CDoublePoint(ax, ay);
        m_pDocument->MetersToLogical( point, endPoint, this );
        pDC->Arc( (CRect)bounding, startPoint, endPoint);
      }
      break;
  }
  if (m_bClip)
    pDC->RestoreDC(-1);
  
  pDC->SelectObject(pOldBrush);
  pDC->SelectObject(pOldPen);
  pDC->SelectObject(pOldFont);
  pDC->SetTextColor(colorOld);
  if (m_logbrush.lbStyle==BS_PATTERN)
  {
    // reset the original bitmap handle
    m_logbrush.lbHatch = (LONG)hOldBitmap;
  }
}

void CDrawRect::DrawTracker( CDC* pDC, TrackerState state )
{
  switch( state )
  {
  case normal:
    break;
    
  case selected:
  case active:
    {
      int nHandleCount = GetHandleCount();
      int width = max((int)( HANDLE_WIDTH * MM_FACTOR ), (int)( m_logpen.lopnWidth.x / 2 ) );

      CIntIRect rect( GetAdjustedRect() );
      if( m_bClip )
      {
        CRect clipRect;
        pDC->SaveDC();
        pDC->GetClipBox( &clipRect );
        if( m_nShape == line )
        {
          rect = m_position;
          if( m_nHorzAdjust != -1 )
            rect.right += m_nHorzAdjust;
        }
        clipRect.left = rect.left + (int)(rect.Width()*(m_dFrom-m_dPosition.left)/m_dPosition.Width())-width;
        clipRect.right = rect.right + (int)(rect.Width()*(m_dTo-m_dPosition.right)/m_dPosition.Width())+width;
        if( m_dPosition.Height() != 0 )
          clipRect.bottom = rect.bottom - (int)(abs(rect.Height())*(m_dBottom-m_dPosition.bottom)/fabs(m_dPosition.Height()));
        else if( m_dPosition.bottom < m_dBottom )
          clipRect = CIntIRect(0, 0, 0, 0);
        pDC->IntersectClipRect( &clipRect );
      }

      for( int nHandle = 1; nHandle <= nHandleCount; nHandle += 1 )
      {
        CPoint handle = GetHandle( nHandle );
        pDC->PatBlt( handle.x - width, handle.y - width, 2 * width, 2 * width, DSTINVERT );
      }

      if( m_nShape == text )
      {
        CPen* pPen = new CPen( PS_DOT, 0, pDC->GetNearestColor( 0 ) );//Dick 23.03.2000
        CPen* pOldPen = pDC->SelectObject( pPen );
        pDC->MoveTo( rect.left, rect.top );
        pDC->LineTo( rect.right, rect.top );
        pDC->LineTo( rect.right, rect.bottom );
        pDC->LineTo( rect.left, rect.bottom );
        pDC->LineTo( rect.left, rect.top );
        pDC->SelectObject( pOldPen );
        delete pPen;
      }

      if( m_bClip )
        pDC->RestoreDC( -1 );
    }
    break;
  }
} // DrawTracker

int CDrawRect::GetHandleCount()
{
  ASSERT_VALID(this);
  
  return m_nShape == line ? 2 :
  CDrawObj::GetHandleCount();
}

// returns center of handle in logical coordinates
CPoint CDrawRect::GetHandle(int nHandle)
{
  ASSERT_VALID(this);
  
  if (m_nShape == line && nHandle == 2)
    nHandle = 5;
  
  return CDrawObj::GetHandle(nHandle);
}

HCURSOR CDrawRect::GetHandleCursor(int nHandle)
{
  ASSERT_VALID(this);
  
  if (m_nShape == line && nHandle == 2)
    nHandle = 5;
  return CDrawObj::GetHandleCursor(nHandle);
}

// point is in logical coordinates
void CDrawRect::MoveHandleTo(int nHandle, CIntPoint point, CDrawView* pView)
{
  ASSERT_VALID(this);
  
  if (m_nShape == line && nHandle == 2)
    nHandle = 5;
  
  CDrawObj::MoveHandleTo(nHandle, point, pView);
}

// rect must be in logical coordinates
BOOL CDrawRect::Intersects( const CIntIRect& rect ) const
{
  ASSERT_VALID( this );
  
  CIntIRect rectT( rect.left, rect.top, rect.right, rect.bottom );
  CIntIRect fixed( GetClippedRect() );

  rectT.NormalizeRect();
  fixed.NormalizeRect();

  CRgn rgn;
  switch( m_nShape )
  {
  case rectangle:
  case text:
    return !( ( rectT & fixed ).IsRectEmpty() );
    
  case ellipse:
    rectT.SetRect( rectT.left-fixed.left, rectT.top-fixed.top, rectT.right-fixed.left, rectT.bottom-fixed.top );
    fixed.SetRect(0, 0, fixed.right-fixed.left, fixed.bottom-fixed.top);
    rgn.CreateEllipticRgnIndirect( (CRect)fixed );
    break;
    
  case line:
    {
      CDrawApp* drawApp = GETDRAWAPP;

      //fixed = GetAdjustedRect();
      int x = ( m_logpen.lopnWidth.x + 2 * drawApp->m_nSelectTol ) / 2;
      int y = ( m_logpen.lopnWidth.y + 2 * drawApp->m_nSelectTol ) / 2;//Dick 10.05.2000 darf nicht 0 sein sonst kann nicht selektieren

      POINT points[4];
      points[0].x = fixed.left;
      points[0].y = fixed.top;
      points[1].x = fixed.left;
      points[1].y = fixed.top;
      points[2].x = fixed.right;
      points[2].y = fixed.bottom;
      points[3].x = fixed.right;
      points[3].y = fixed.bottom;
      
      if( fixed.left < fixed.right )
      {
        points[0].x -= x;
        points[1].x += x;
        points[2].x += x;
        points[3].x -= x;
      }
      else
      {
        points[0].x += x;
        points[1].x -= x;
        points[2].x -= x;
        points[3].x += x;
      }
      
      if( fixed.top < fixed.bottom )
      {
        points[0].y -= y;
        points[1].y += y;
        points[2].y += y;
        points[3].y -= y;
      }
      else
      {
        points[0].y += y;
        points[1].y -= y;
        points[2].y -= y;
        points[3].y += y;
      }
      rgn.CreatePolygonRgn( points, 4, ALTERNATE );
    }
    break;
    
  case egg:
  /*    rectT.SetRect(rectT.left-fixed.left, rectT.top-fixed.top, rectT.right-fixed.left, rectT.bottom-fixed.top);
  fixed.SetRect(0, 0, fixed.right-fixed.left, fixed.bottom-fixed.top);
  dc.BeginPath();
  bounding = CIntIRect(fixed.left, fixed.top, fixed.right, fixed.top-2*abs(fixed.Height())/3);
  start = CPoint(fixed.right, fixed.top-abs(fixed.Height())/3);
  end = CPoint(fixed.left, fixed.top-abs(fixed.Height())/3);
  dc.MoveTo(start);
  dc.ArcTo(bounding, start, end);
  bounding = CIntIRect(fixed.left, fixed.bottom+4*abs(fixed.Height())/3, fixed.right, fixed.bottom);
  start = CPoint(fixed.left, fixed.top-abs(fixed.Height())/3);
  end = CPoint(fixed.right, fixed.top-abs(fixed.Height())/3);
  dc.LineTo(start);
  dc.ArcTo(bounding, start, end);
  dc.CloseFigure();
  dc.EndPath();
  rgn.CreateFromPath(&dc);
    break;*/
    
  case mouth:
  /*    rectT.SetRect(rectT.left-fixed.left, rectT.top-fixed.top, rectT.right-fixed.left, rectT.bottom-fixed.top);
  fixed.SetRect(0, 0, fixed.right-fixed.left, fixed.bottom-fixed.top);
  dc.BeginPath();
  bounding = CIntIRect(fixed.left, fixed.top, fixed.right, fixed.top+4*abs(fixed.Height())/3);
  start = CPoint(fixed.right, fixed.top+2*abs(fixed.Height())/3);
  end = CPoint(fixed.left, fixed.top+2*abs(fixed.Height())/3);
  ASSERT(dc.Arc(bounding, start, end));
  bounding = CIntIRect(fixed.left, fixed.bottom-2*abs(fixed.Height())/3, fixed.right, fixed.bottom);
  start = CPoint(fixed.left, fixed.top+2*abs(fixed.Height())/3);
  end = CPoint(fixed.right, fixed.top+2*abs(fixed.Height())/3);
  dc.Arc(bounding, start, end);
  dc.CloseFigure();
  dc.EndPath();
  rgn.CreateFromPath(&dc);
    break;*/
  case arc:
    return TRUE;
  }
#ifdef _DEBUG
  if (m_bBreakWhenIntersected)
  {
    if( rgn.RectInRegion( (CRect)rectT ) )
      DebugBreak();
  }
#endif
  return rgn.RectInRegion( CRect( rectT ) );
}

CDrawObj* CDrawRect::Clone(CDrawDoc* pDoc, BOOL bExactCopy /*=FALSE*/)
{
  ASSERT_VALID( this );
  
  CDrawRect* pClone = new CDrawRect( m_position, pDoc );
  pClone->x_flags = x_flags;
  pClone->m_nIndex = m_nIndex;
  pClone->m_nLayer = m_nLayer;
  pClone->m_logfont = m_logfont;
  pClone->m_logpen = m_logpen;
  pClone->m_logbrush = m_logbrush;
  pClone->m_colorText = m_colorText;
  pClone->m_nType = m_nType;
  pClone->m_nShape = m_nShape;
  
  if( m_nShape != line || bExactCopy )
  {
    pClone->m_nHorzJust = m_nHorzJust;
    pClone->m_nVertJust = m_nVertJust;
  }
  else
  {
    pClone->m_nHorzJust = nojust;
    pClone->m_nVertJust = nojust;
  }

  pClone->m_nTextType = m_nTextType;
  pClone->m_nLeftArrow = m_nLeftArrow;
  pClone->m_nRightArrow = m_nRightArrow;
  
  if( bExactCopy && m_pConnections != NULL )
  {
    pClone->m_pConnections = new CDrawObjList;
    POSITION pos = m_pConnections->GetHeadPosition();
    while( pos != NULL )
    {
      CDrawObj* pCon = m_pConnections->GetNextObject( pos );
      pClone->m_pConnections->AddTailObject( pCon );
    }
  }
  else
    pClone->m_pConnections = NULL;
  // set the stempel text type to none if necessary and update text
  // Note that when we clone stempel text in CreateDrawing we must reset
  // the stempel text type afterwards (Clone is also called when copying objects
  // with Ctrl+WM_LBUTTONDOWN)
  if( m_nStempelTextType==STPL_TEXT_NONE || bExactCopy)
    pClone->m_text = m_text;
  else
    pDoc->GetDefaultStempelText(STPL_TEXT_NONE, pClone->m_text);
  if (bExactCopy)
    pClone->m_nStempelTextType = m_nStempelTextType;
  else
    pClone->m_nStempelTextType = STPL_TEXT_NONE;
  
  pClone->m_arcLeft = m_arcLeft;
  pClone->m_arcCenter = m_arcCenter;
  pClone->m_arcRight = m_arcRight;
  pClone->m_bShowNegative = m_bShowNegative;
  pClone->m_nPrecision = m_nPrecision;


  if( bExactCopy )
  {
    pClone->m_dPosition = m_dPosition;
    pClone->m_nSectionIndex = m_nSectionIndex;
    pClone->m_ptOffset = m_ptOffset;
    pClone->m_bClip = m_bClip;
    pClone->m_dFrom = m_dFrom;
    pClone->m_dTo = m_dTo;
    pClone->m_dBottom = m_dBottom;
    pClone->m_nHorzAdjust = m_nHorzAdjust;
    pClone->m_nVertAdjust = m_nVertAdjust;
    pClone->m_sizeText = m_sizeText;
    pClone->m_nSelStartPos = m_nSelStartPos;
    pClone->m_nSelEndPos = m_nSelEndPos;
  }
  
  ASSERT_VALID( pClone );
  return pClone;
}

void CDrawRect::SetShape(int nShape)
{
  m_nShape = (Shape)nShape;
  
  switch (m_nShape)
  {
  default:
    ASSERT(FALSE); // unsuported shape!
    
  case rectangle:
  case ellipse:
    UnsetFlags(CDrawObj::font | CDrawObj::arrow, TRUE);
    SetFlags(CDrawObj::brush | CDrawObj::pen, TRUE);
    break;
    
  case egg:
  case mouth:
  case arc:
    UnsetFlags(CDrawObj::font | CDrawObj::arrow | CDrawObj::brush, TRUE);
    SetFlags(CDrawObj::pen, TRUE);
    break;
    
  case line:
    UnsetFlags(CDrawObj::font | CDrawObj::brush | CDrawObj::filled, TRUE);
    SetFlags(CDrawObj::pen | CDrawObj::arrow, TRUE);
    break;
    
  case text:
    UnsetFlags(CDrawObj::pen | CDrawObj::brush | CDrawObj::arrow | CDrawObj::filled, TRUE);
    SetFlags(CDrawObj::font, TRUE);
    // text must have a justification
    if (m_nHorzJust==CDrawRect::nojust)
      SetHorzJust(center);
    if (m_nVertJust==CDrawRect::nojust)
      SetVertJust(center);
    break;
  }
}

void CDrawRect::SetHorzJust(int nJust)
{
  m_nHorzJust = (Justification)nJust;
}

void CDrawRect::SetVertJust(int nJust)
{
  m_nVertJust = (Justification)nJust;
}

void CDrawRect::SetTextType(int nType)
{
  m_nTextType = (TextType)nType;
}

void CDrawRect::GetText( CString& text )
{
  text = m_text;
  if( m_nTextType == xcoord || m_nTextType == ycoord )
  {
    double value;
    lconv* lc = ::localeconv();

	// HACK: Die Daten könnten mit einer anderen Lokaleinstelung serialisiert worden
	// sein, deswegen einfach mal die gängigen durch die aktuelle ersetzen
	// sollte eigentlich meistens das gewünschte Resultat liefern
	char decPoint = *lc->decimal_point;
	text.Replace( '.', decPoint );
	text.Replace( ',', decPoint );

    if( text.Find( decPoint ) != -1 )
    {
      value = atof( text );
      text.Format( "%.*f", m_nPrecision, value );
    }
  }
}

void CDrawRect::SetStempelTextType(int nType)
{
  if (0<=nType && nType<N_STPLTEXTS)
    m_nStempelTextType = nType;
  else
    m_nStempelTextType = STPL_TEXT_NONE;
}

CIntIRect CDrawRect::GetAdjustedRect() const
{
  if( !m_pDocument )
    return m_position;

  int nXValueFormat = m_pDocument->GetXValueFormat();
  int nYValueFormat = m_pDocument->GetYValueFormat();

  CIntIRect rect( m_position );
  if( m_nShape == line )
  {
    if( m_nHorzAdjust != -1 )
      rect.right += m_nHorzAdjust;
    switch(m_nHorzJust)
    {
    default:
      break;
      
    case center:
      rect.left = rect.left + MulDiv(abs(rect.Width()), 1, 2);
      rect.right = rect.left;
      break;
      
    case left:
      rect.right = rect.left;
      break;
      
    case right:
      rect.left = rect.right;
      break;
    }
    switch(m_nVertJust)
    {
    default:
      break;
      
    case center:
      rect.top = rect.bottom + MulDiv(abs(rect.Height()), 1, 2);
      rect.bottom = rect.top;
      break;
      
    case left:
      rect.bottom = rect.top;
      break;
      
    case right:
      rect.top = rect.bottom;
      break;
    }
  }
  else if (m_nShape==text && m_nHorzAdjust!=-1)
  {
    switch (m_nTextType)
    {
    default:
      break;
      
    case xcoord:
      if (nXValueFormat==CDrawDocData::topLeft || nXValueFormat==CDrawDocData::bottomLeft)
        rect.left = rect.right - m_nHorzAdjust;
      else if (nXValueFormat==CDrawDocData::topRight || nXValueFormat==CDrawDocData::bottomRight)
        rect.left = rect.right + m_nHorzAdjust;
      break;
      
    case ycoord:
      if (nYValueFormat==CDrawDocData::topLeft || nYValueFormat==CDrawDocData::bottomLeft)
        rect.right = rect.left - m_nHorzAdjust;
      else if (nYValueFormat==CDrawDocData::topRight || nYValueFormat==CDrawDocData::bottomRight)
        rect.right = rect.left + m_nHorzAdjust;
      break;
    }
  }
  else if( m_nShape == rectangle )
  {
    if (m_nHorzAdjust!=-1)
    {
      rect.left -= m_nHorzAdjust;
      rect.right += m_nHorzAdjust;
    }
    if (m_nVertAdjust!=-1)
    {
      rect.bottom -= m_nVertAdjust;
      rect.top += m_nVertAdjust;
    }
  }

  return rect;
} // GetAdjustedRect

CIntIRect CDrawRect::GetClippedRect() const
{
  if( !IsClipped() || !m_pDocument )
    return GetAdjustedRect();
  else
  {
    CIntIRect adjRect( GetAdjustedRect() );

    CDoubleIRect clipRect;
    m_pDocument->LogicalToMeters( adjRect, clipRect, this );

    clipRect.left = max( clipRect.left, m_dFrom );
    clipRect.right = min( clipRect.right, m_dTo );

    CIntIRect rect;
    if( clipRect.left <= clipRect.right )
      m_pDocument->MetersToLogical( clipRect, rect, this );
    else
      rect = CIntIRect( 0, 0, 0, 0 );

    return rect;
  }
}; // GetClippedRect

CSize CDrawRect::GetOutputTextSize( CView* pView )
{
  CSize sizeText, sizeOutput;
  double angle;
  CDC *pDC;
  CString text, str;
  
  pDC = pView->GetDC();
  sizeOutput = CSize(0, 0);

  /** initialize DC with font **/
  LOGFONT logfont = m_logfont;
  logfont.lfHeight = TransformFontHeight( pDC, logfont.lfHeight );
  
  CFont font;
  if (!font.CreateFontIndirect(&logfont))
    return sizeOutput;
  
  CFont* pOldFont = NULL;
  if( HasFont() )
    pOldFont = pDC->SelectObject(&font);
  else
    pOldFont = (CFont*)pDC->SelectStockObject(ANSI_FIXED_FONT);

  /** find text extents **/
  GetText(str);
  text = " " + str + "  ";
  sizeText = pDC->GetTextExtent(text);
  pDC->SelectObject(pOldFont);
  // calculate size after text is rotated
  angle = GetTextOrientation()*2*PI/360;
  sizeOutput.cx = int( fabs(sizeText.cx*cos(angle)) + fabs(sizeText.cy*sin(angle)) );
  sizeOutput.cy = int( fabs(sizeText.cy*cos(angle)) + fabs(sizeText.cx*sin(angle)) );
  m_sizeText = sizeText;
  pView->ReleaseDC(pDC);
  return sizeOutput;
}

void CDrawRect::SetTextHeight(int height)
{
  m_logfont.lfHeight = height;
}

void CDrawRect::AddToDXF(CDXFZeichnung* zn, const CString& layerName)
{
  CDXFLayer* pLayer = zn->FindLayer(layerName);
  if( pLayer == NULL )
  {
    pLayer = new CDXFLayer( zn );
    pLayer->SetName( layerName );
    zn->SetLayer( layerName, pLayer );
  }
  
  if (m_nShape==line && GetOffset().x==-1)
    return;
  
  CIntIRect rcTemp, clipRect;
  CIntIRect rect( GetAdjustedRect() );
  clipRect = rect;
  if (m_bClip)
  {
    if (m_nShape==line)
    {
      rect = m_position;
      if (m_nHorzAdjust!=-1)
        rect.right += m_nHorzAdjust;
    }
    if (m_dPosition.Width()!=0)
    {
      clipRect.left = rect.left + (int)(rect.Width()*(m_dFrom-m_dPosition.left)/m_dPosition.Width());
      clipRect.right = rect.right + (int)(rect.Width()*(m_dTo-m_dPosition.right)/m_dPosition.Width());
    }
    else if (m_dPosition.left<m_dFrom || m_dPosition.right>m_dTo)
      return;
    if (m_dPosition.Height()!=0)
      clipRect.bottom = rect.bottom - (int)(abs(rect.Height())*(m_dBottom-m_dPosition.bottom)/fabs(m_dPosition.Height()));
    else if (m_dPosition.bottom<m_dBottom)
      return;
    //    if (!clipRect.IntersectRect(rect, clipRect))
    //      return;
  }
  
  switch (m_nShape)
  {
  case rectangle:
    {
      if (m_bClip)
      {
        CDXFLinie *ln;
        
        if (((rect.right<=clipRect.right && rect.right>=clipRect.left) || (rect.left<=clipRect.right && rect.left>=clipRect.left)) && rect.top>=clipRect.bottom)
        {
          ln = new CDXFLinie(pLayer);
          ln->SetAnf(max(rect.left, clipRect.left), rect.top);
          ln->SetEnd(min(rect.right, clipRect.right), rect.top);
          ln->SetColor(m_logpen.lopnColor);
          ln->SetWidth(m_logpen.lopnWidth.x);
          ln->SetLType(m_logpen.lopnStyle);
          pLayer->AddLinie(ln);
        }
        
        if (rect.right<=clipRect.right && rect.right>=clipRect.left && (rect.top>=clipRect.bottom || rect.bottom>=clipRect.bottom))
        {
          ln = new CDXFLinie(pLayer);
          ln->SetAnf(rect.right, max(rect.top, clipRect.bottom));
          ln->SetEnd(rect.right, max(rect.bottom, clipRect.bottom));
          ln->SetColor(m_logpen.lopnColor);
          ln->SetWidth(m_logpen.lopnWidth.x);
          ln->SetLType(m_logpen.lopnStyle);
          pLayer->AddLinie(ln);
        }
        
        if (((rect.right<=clipRect.right && rect.right>=clipRect.left) || (rect.left<=clipRect.right && rect.left>=clipRect.left)) && rect.bottom>=clipRect.bottom)
        {
          ln = new CDXFLinie(pLayer);
          ln->SetAnf(min(rect.right, clipRect.right), rect.bottom);
          ln->SetEnd(max(rect.left, clipRect.left), rect.bottom);
          ln->SetColor(m_logpen.lopnColor);
          ln->SetWidth(m_logpen.lopnWidth.x);
          ln->SetLType(m_logpen.lopnStyle);
          pLayer->AddLinie(ln);
        }
        
        if (rect.left<=clipRect.right && rect.left>=clipRect.left && (rect.top>=clipRect.bottom || rect.bottom>=clipRect.bottom))
        {
          ln = new CDXFLinie(pLayer);
          ln->SetAnf(rect.left, max(rect.bottom, clipRect.bottom));
          ln->SetEnd(rect.left, max(rect.top, clipRect.bottom));
          ln->SetColor(m_logpen.lopnColor);
          ln->SetWidth(m_logpen.lopnWidth.x);
          ln->SetLType(m_logpen.lopnStyle);
          pLayer->AddLinie(ln);
        }
      }
      else
      {
        CDXFPolylinie *pl = new CDXFPolylinie(pLayer);
        
        pl->AddCoord( rect.left, rect.top );
        pl->AddCoord( rect.right, rect.top );
        pl->AddCoord( rect.right, rect.bottom );
        pl->AddCoord( rect.left, rect.bottom );
        pl->AddCoord( rect.left, rect.top );
        
        pl->SetColor(m_logpen.lopnColor);
        pl->SetWidth(m_logpen.lopnWidth.x);
        pl->SetLType(m_logpen.lopnStyle);
        
        pLayer->AddPolylinie(pl);
      }
    }
    break;
    
  case ellipse:
  /*      {
  double rad, anfw, endw;
  CDXFArc ar = new CDXFArc(pLayer);
  
    ar->Set( 0, 0, rad, anfw, endw );
    
      ar->SetColor(m_logpen.lopnColor);
      ar->SetWidth(m_logpen.lopnWidth.x);
      ar->SetLType(m_logpen.lopnStyle);
      
        pLayer->AddArc(ar);
  }*/
    //      pDC->Ellipse(rect);
    break;
    
  case line:
    {
      CDXFLinie *ln = new CDXFLinie(pLayer);
      
      rcTemp.SetRect(m_helpers[1], m_helpers[2], m_helpers[3], m_helpers[4]);
      rect.SetRect(m_helpers[5], m_helpers[6], m_helpers[7], m_helpers[8]);
      if (m_bClip)
      {
        if (!(rcTemp.right<=clipRect.right && rcTemp.right>=clipRect.left) || !(rcTemp.left<=clipRect.right && rcTemp.left>=clipRect.left) || !(rcTemp.top>=clipRect.bottom || rcTemp.bottom>=clipRect.bottom))
          return;
      }
      rcTemp.bottom = max(rcTemp.bottom, clipRect.bottom);
      if (m_bClip && rcTemp.left!=rcTemp.right)
      {
        if (rcTemp.left<=rcTemp.right)
        {
          ln->SetAnf(max(clipRect.left, rcTemp.left), rcTemp.bottom+(rcTemp.top-rcTemp.bottom)*(max(clipRect.left, rcTemp.left)-rcTemp.right)/(rcTemp.left-rcTemp.right));
          ln->SetEnd(min(clipRect.right, rcTemp.right), rcTemp.bottom+(rcTemp.top-rcTemp.bottom)*(min(clipRect.right, rcTemp.right)-rcTemp.right)/(rcTemp.left-rcTemp.right));
        }
        else
        {
          ln->SetAnf(min(clipRect.right, rcTemp.left), rcTemp.bottom+(rcTemp.top-rcTemp.bottom)*(min(clipRect.right, rcTemp.left)-rcTemp.right)/(rcTemp.left-rcTemp.right));
          ln->SetEnd(max(clipRect.left, rcTemp.right), rcTemp.bottom+(rcTemp.top-rcTemp.bottom)*(max(clipRect.left, rcTemp.right)-rcTemp.right)/(rcTemp.left-rcTemp.right));
        }
      }
      else
      {
        ln->SetAnf(rcTemp.left, rcTemp.top);
        ln->SetEnd(rcTemp.right, rcTemp.bottom);
      }
      
      ln->SetColor(m_logpen.lopnColor);
      ln->SetWidth(m_logpen.lopnWidth.x);
      ln->SetLType(m_logpen.lopnStyle);
      
      pLayer->AddLinie(ln);
      
      switch (m_nRightArrow)
      {
      default:
      case noarrow:
        break;
        
      case open:
        ln = new CDXFLinie(pLayer);
        ln->SetAnf(rect.left, rect.top);
        ln->SetEnd(m_helpers[9], m_helpers[10]);
        ln->SetColor(m_logpen.lopnColor);
        ln->SetWidth(m_logpen.lopnWidth.x);
        ln->SetLType(m_logpen.lopnStyle);
        pLayer->AddLinie(ln);
        
        ln = new CDXFLinie(pLayer);
        ln->SetAnf(rect.left, rect.top);
        ln->SetEnd(m_helpers[11], m_helpers[12]);
        ln->SetColor(m_logpen.lopnColor);
        ln->SetWidth(m_logpen.lopnWidth.x);
        ln->SetLType(m_logpen.lopnStyle);
        pLayer->AddLinie(ln);
        break;
        
      case closed:
      case solid:
        ln = new CDXFLinie(pLayer);
        ln->SetAnf(rect.left, rect.top);
        ln->SetEnd(m_helpers[9], m_helpers[10]);
        ln->SetColor(m_logpen.lopnColor);
        ln->SetWidth(m_logpen.lopnWidth.x);
        ln->SetLType(m_logpen.lopnStyle);
        pLayer->AddLinie(ln);
        
        ln = new CDXFLinie(pLayer);
        ln->SetAnf(m_helpers[9], m_helpers[10]);
        ln->SetEnd(m_helpers[11], m_helpers[12]);
        ln->SetColor(m_logpen.lopnColor);
        ln->SetWidth(m_logpen.lopnWidth.x);
        ln->SetLType(m_logpen.lopnStyle);
        pLayer->AddLinie(ln);
        
        ln = new CDXFLinie(pLayer);
        ln->SetAnf(m_helpers[11], m_helpers[12]);
        ln->SetEnd(rect.left, rect.top);
        ln->SetColor(m_logpen.lopnColor);
        ln->SetWidth(m_logpen.lopnWidth.x);
        ln->SetLType(m_logpen.lopnStyle);
        pLayer->AddLinie(ln);
        break;
      }
      switch (m_nLeftArrow)
      {
      default:
      case noarrow:
        break;
        
      case open:
        ln = new CDXFLinie(pLayer);
        ln->SetAnf(rect.right, rect.bottom);
        ln->SetEnd(m_helpers[13], m_helpers[14]);
        ln->SetColor(m_logpen.lopnColor);
        ln->SetWidth(m_logpen.lopnWidth.x);
        ln->SetLType(m_logpen.lopnStyle);
        pLayer->AddLinie(ln);
        
        ln = new CDXFLinie(pLayer);
        ln->SetAnf(rect.right, rect.bottom);
        ln->SetEnd(m_helpers[15], m_helpers[16]);
        ln->SetColor(m_logpen.lopnColor);
        ln->SetWidth(m_logpen.lopnWidth.x);
        ln->SetLType(m_logpen.lopnStyle);
        pLayer->AddLinie(ln);
        break;
        
      case closed:
      case solid:
        ln = new CDXFLinie(pLayer);
        ln->SetAnf(rect.right, rect.bottom);
        ln->SetEnd(m_helpers[13], m_helpers[14]);
        ln->SetColor(m_logpen.lopnColor);
        ln->SetWidth(m_logpen.lopnWidth.x);
        ln->SetLType(m_logpen.lopnStyle);
        pLayer->AddLinie(ln);
        
        ln = new CDXFLinie(pLayer);
        ln->SetAnf(m_helpers[13], m_helpers[14]);
        ln->SetEnd(m_helpers[15], m_helpers[16]);
        ln->SetColor(m_logpen.lopnColor);
        ln->SetWidth(m_logpen.lopnWidth.x);
        ln->SetLType(m_logpen.lopnStyle);
        pLayer->AddLinie(ln);
        
        ln = new CDXFLinie(pLayer);
        ln->SetAnf(m_helpers[15], m_helpers[16]);
        ln->SetEnd(rect.right, rect.bottom);
        ln->SetColor(m_logpen.lopnColor);
        ln->SetWidth(m_logpen.lopnWidth.x);
        ln->SetLType(m_logpen.lopnStyle);
        pLayer->AddLinie(ln);
        break;
      }
      }
      break;
      
    case text:
      {
        CString str;

        rect.NormalizeRect();
        if( m_nHorzAdjust == 0 )
          return;

        CString strText;
        GetText( strText );
        if( !m_bShowNegative )
        {
          int i = strText.Find('-');
          if( i != -1 )
            strText = strText.Left(i) + strText.Right(strText.GetLength() - i - 1 );
        }
        str = ' ';
        str += strText;
        str += ' ';
        int x = m_helpers[1];
        int y = m_helpers[2];
        
        // draw text
        CDXFText *tx = new CDXFText( pLayer );
        int height = MulDiv( abs( m_logfont.lfHeight ), 254, 720 );
        
        double radAngle = m_logfont.lfEscapement * 2 * PI / 3600;
        double gradAngle = 360 - m_logfont.lfEscapement / 10;
        tx->Set( str, x - 2 * height * sin( radAngle ), y - 2 * height * cos( radAngle ), height, gradAngle );
        tx->SetColor( m_colorText );
        pLayer->AddText( tx );
      }
      break;
      
    case egg:
    /*      bounding = CIntIRect(rect.left, rect.top, rect.right, rect.top-2*abs(rect.Height())/3);
    start = CPoint(rect.right, rect.top-abs(rect.Height())/3);
    end = CPoint(rect.left, rect.top-abs(rect.Height())/3);
    pDC->Arc(bounding, start, end);
    bounding = CIntIRect(rect.left, rect.bottom+4*abs(rect.Height())/3, rect.right, rect.bottom);
    start = CPoint(rect.left, rect.top-abs(rect.Height())/3);
    end = CPoint(rect.right, rect.top-abs(rect.Height())/3);
    pDC->Arc(bounding, start, end);
      pDC->FloodFill(rect.left+abs(rect.Width())/2, rect.bottom+abs(rect.Height())/2, m_logpen.lopnColor);*/
      break;
      
    case mouth:
    /*      bounding = CIntIRect(rect.left, rect.top, rect.right, rect.top-4*abs(rect.Height())/3);
    start = CPoint(rect.right, rect.top-2*abs(rect.Height())/3);
    end = CPoint(rect.left, rect.top-2*abs(rect.Height())/3);
    pDC->Arc(bounding, start, end);
    bounding = CIntIRect(rect.left, rect.bottom+2*abs(rect.Height())/3, rect.right, rect.bottom);
    start = CPoint(rect.left, rect.top-2*abs(rect.Height())/3);
    end = CPoint(rect.right, rect.top-2*abs(rect.Height())/3);
    pDC->Arc(bounding, start, end);
      pDC->FloodFill(rect.left+abs(rect.Width())/2, rect.bottom+abs(rect.Height())/2, m_logpen.lopnColor);*/
      break;
      
    case arc:
    /*      ax = m_arcLeft.dx;
    ay = m_arcLeft.dy;
    bx = m_arcCenter.dx;
    by = m_arcCenter.dy;
    cx = m_arcRight.dx;
    cy = m_arcRight.dy;
    yy = (ax*ax+ay*ay-bx*bx-by*by)/(2*(ax-bx));
    yy -= (bx*bx+by*by-cx*cx-cy*cy)/(2*(bx-cx));
    yy *= (ax-bx)*(bx-cx);
    yy /= (ay-by)*(bx-cx)-(by-cy)*(ax-bx);
    xx = (ax*ax+ay*ay-bx*bx-by*by)/(2*(ax-bx));
    xx -= yy*(ay-by)/(ax-bx);
    r = (xx-bx)*(xx-bx)+(yy-by)*(yy-by);
    r = sqrt(r);
    temp = CDoubleIRect(xx-r, yy-r, xx+r, yy+r);
    m_pDocument->MetersToLogical(temp, bounding, this);
    point = CDoublePoint(cx, cy);
    m_pDocument->MetersToLogical(point, start, this);
    point = CDoublePoint(ax, ay);
    m_pDocument->MetersToLogical(point, end, this);
      pDC->Arc(bounding, start, end);*/
      break;
  }
}; // AddToDxF

void CDrawRect::SetVisibility( CDrawObj* pObj, BOOL bVisible, CDrawObjList* pUndoList )
// setzt die Sichtbarkeit auf die Sichtbarkeit von pObj, falls 
// pObj die gleichen Parameter hat wie this
// Parameter:
//        CDrawObj* pObj: das Vergleichsobjekt
//        BOOL bVisible: solls sichtbar oder unsichtbar sein
//        CDrawobjList* pUndoList: falls != 0, hängt sich this statt sich zu verändern nur an die List an ( für undo )
{
  CString rClass( pObj->GetRuntimeClass()->m_lpszClassName );
  rClass.TrimLeft();
  rClass.TrimRight();

  if ( rClass.CompareNoCase( TEXT("CDrawRect") ) == 0  && 
    pObj->GetType() == GetType() && 
    pObj->GetSectionIndex() == GetSectionIndex() && 
    pObj->GetDataBlockIndex() == GetDataBlockIndex() && 
    ( !pObj->IsText() || ((CDrawRect*)pObj)->GetTextType() == GetTextType() )
    )
  {
    if ( bVisible && IsInvisible() )
    {
      if ( pUndoList )
        pUndoList->AddHeadObject( this );
      else
        UnsetFlags( invisible, TRUE );
    }
    else if( !bVisible && !IsInvisible() )
    {
      if ( pUndoList )
        pUndoList->AddHeadObject( this );
      else
        SetFlags( invisible, TRUE );
    }; // if bVisible ...
  }; // if type == type && ...
}; // SetVisibility

/* virtual */ CDoubleIRect CDrawRect::CalcBounds( CMap<double, double, double, double>& mins, CMap<double, double, double, double>& maxs )
// diese Funktion rechnet die Ausmasse des Objektes aus, und gibt auch noch spezifische Minima und Maxima zurück
// Parameter:
//      CMap<double, double, double, double>& mins: hier werden die unteren Grenzen der linken und rechten Begrezung abgelegt
//      CMap<double, double, double, double>& maxs: dito die Maxima
{
  CDoubleIRect rect( m_dPosition );
  rect.NormalizeRect();

  mins.SetAt( rect.left, rect.bottom );
  mins.SetAt( rect.right, rect.bottom );
  maxs.SetAt( rect.left, rect.top );
  maxs.SetAt( rect.right, rect.top );

  return rect;
} // CalcBounds

void CDrawRect::Dump( CDumpContext& dc ) const
// gibt aktuellen Zustand des objekts in einen DumpContext aus
{
  dc << "a CDrawRect at " << (void*)this << " with " << m_position << " and " << m_dPosition << " and Offset " << GetOffset() << " and Index " << GetIndex() << " and Visibility " << IsInvisible();
}; // Dump

////////////////////////////////////////////////////////////////////////////
// CDrawPoly

IMPLEMENT_SERIAL(CDrawPoly, CDrawObj, VERSIONABLE_SCHEMA | 7 )

#ifdef _DEBUG
  /* static */ 
  log4cpp::Category& CDrawPoly::m_logCat = log4cpp::Category::getInstance( "CDrawPoly" );
#endif _DEBUG


CDrawPoly::CDrawPoly() : CDrawObj()
{
  m_nShape = polyline;
  m_points = NULL;
  m_coords = NULL;
  m_nPoints = 0;
  m_nAllocPoints = 0;
  SetFlags(pen | arrow);
  UnsetFlags(font | brush | filled);
}

CDrawPoly::CDrawPoly(const CIntIRect& position, CDrawDoc* pDoc)
: CDrawObj(position, pDoc)
{
  m_nShape = polyline;
  m_points = NULL;
  m_coords = NULL;
  m_nPoints = 0;
  m_nAllocPoints = 0;
  SetFlags(pen | arrow);
  UnsetFlags(font | brush | filled);
}

CDrawPoly::CDrawPoly(const CDoubleIRect& position, CDrawDoc* pDoc)
: CDrawObj(position, pDoc)
{
  m_nShape = polyline;
  m_points = NULL;
  m_coords = NULL;
  m_nPoints = 0;
  m_nAllocPoints = 0;
  SetFlags(pen | arrow);
  UnsetFlags(font | brush | filled);
}

CDrawPoly::~CDrawPoly()
{
  delete[] m_points;
  delete[] m_coords;
}

CDoublePoint CDrawPoly::GetPoint(int n) const
{
  if(n>=0 && n<m_nPoints)
    return m_coords[n];
  return CDoublePoint(0, 0);
}

CIntPoint CDrawPoly::GetCPoint( int n ) const
{
  if( n >= 0 && n < m_nPoints )
    return m_points[n];
  return CPoint( 0, 0 );
} // GetCPoint

void CDrawPoly::SetPoint(int n, CDoublePoint& pt)
{
  if( n >= 0 && n < m_nPoints )
    m_coords[n] = pt;
}

CIntIRect CDrawPoly::GetClippedRect() const
{
  CIntIRect rect( GetAdjustedRect() );
  
  if( m_pDocument )
  {
    CDoubleIRect clipRect;
    m_pDocument->LogicalToMeters( GetAdjustedRect(), clipRect, this );

    clipRect.left = max( clipRect.left, m_dFrom );
    clipRect.right = min( clipRect.right, m_dTo );
    
    // top und bottom rausfinden
    // nur Linienstücke, welche wirklich innerhalb von left und right liegen nehmen
    double top = -1e36;
    double bottom = 1e36;
    
    for( int i = 0; i < m_nPoints; i++ )
    {
      CDoublePoint pt = m_coords[i];
      if( pt.x > clipRect.left && pt.x < clipRect.right )
      {
        top = max( top, pt.y );
        bottom = min( bottom, pt.y );
      };
    }; // for i
    
    clipRect.top = top;
    clipRect.bottom = bottom;
    
    if( clipRect.left < clipRect.right && clipRect.bottom <= clipRect.top )
      m_pDocument->MetersToLogical( clipRect, rect, this );
    else
      rect = CIntIRect( 0, 0, 0, 0 );
  };

  return rect;
}; // GetCLippedRect

void CDrawPoly::Serialize( CArchive& ar )
{
  int i;
  CDrawObj::Serialize( ar );
  if( ar.IsStoring() )
  {
    ar << (WORD) m_nShape;
    ar << (WORD) m_nPoints;
    ar << (WORD) m_nAllocPoints;
    for (i = 0;i< m_nPoints; i++)
      ar << m_points[i];
    for (i = 0;i< m_nPoints; i++)
      ar << m_coords[i];
  }
  else
  {
    int nVersion = ar.GetObjectSchema();
    ar.SetObjectSchema(nVersion);
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
      {
        WORD wTemp;
        if (nVersion==0)
        {
          m_nShape = polyline;
        }
        else
        {
          ar >> wTemp; m_nShape = (Shape)wTemp;
        }
        ar >> wTemp; m_nPoints = wTemp;
        ar >> wTemp; m_nAllocPoints = wTemp;
        if( m_nAllocPoints > 0 )
        {
          m_points = new CPoint[m_nAllocPoints];
          for( i = 0;i < m_nPoints; i++ )
            ar >> m_points[i];
          m_coords = new CDoublePoint[m_nAllocPoints];
          for (i = 0;i < m_nPoints; i++)
            ar >> m_coords[i];
        }
      }
      break;
      
    default:
      AfxThrowArchiveException(CArchiveException::badSchema);
      break;
    }
  }
}

void CDrawPoly::Draw(CDC* pDC)
{
  // nur zeichnen, wenn sichtbar
  if( IsInvisible() )
    return;

  HBITMAP hOldBitmap;
  
  ASSERT_VALID( this );
  
#ifdef _DEBUG
  if (m_bBreakInDraw)
    DebugBreak();
#endif
  
  CBrush brush;
  if (m_logbrush.lbStyle==BS_PATTERN)
  { // we must correctly color the bitmap used for the pattern
    // save the handle to the original bitmap
    hOldBitmap = (HBITMAP)m_logbrush.lbHatch;
    if (m_hPattern==NULL)
    {
      // create a newly colored bitmap
      m_hPattern = ::CreateNewColoredBitmap(hOldBitmap, RGB(255, 255, 255), RGB(0, 0, 0), RGB(255, 255, 255), m_logbrush.lbColor);
    }
    // set the newly colored bitmap as the one to be used
    m_logbrush.lbHatch = (LONG)m_hPattern;
  }
  if (!brush.CreateBrushIndirect(&m_logbrush))
    return;
  CPen pen;
  if (!pen.CreatePenIndirect(&m_logpen))
    return;
  
  CFont font;
  if (!font.CreateFontIndirect(&m_logfont))
    return;
  
  CBrush* pOldBrush;
  CPen* pOldPen;
  CFont* pOldFont;

  if (HasBrush() && IsFilled())
    pOldBrush = pDC->SelectObject(&brush);
  else
    pOldBrush = (CBrush*)pDC->SelectStockObject(NULL_BRUSH);
  
  if (HasPen())
    pOldPen = pDC->SelectObject(&pen);
  else
    pOldPen = (CPen*)pDC->SelectStockObject(NULL_PEN);
  
  if (HasFont())
    pOldFont = pDC->SelectObject(&font);
  else
    pOldFont = (CFont*)pDC->SelectStockObject(ANSI_FIXED_FONT);
  
  
  CRect clipRect;
  int nOldFillMode;
  
  CIntIRect rect( GetAdjustedRect() );
  if (m_bClip)
  {
    pDC->SaveDC();
    pDC->GetClipBox(&clipRect);
    if (m_dPosition.Width()!=0)
    {
      clipRect.left = rect.left + (int)(rect.Width()*(m_dFrom-m_dPosition.left)/m_dPosition.Width());
      clipRect.right = rect.right + (int)(rect.Width()*(m_dTo-m_dPosition.right)/m_dPosition.Width());
    }
    else
      clipRect = CRect(0, 0, 0, 0);
    if( m_dPosition.Height() != 0 )
      clipRect.bottom = rect.bottom + (int)(abs(rect.Height())*(m_dBottom-m_dPosition.bottom)/fabs(m_dPosition.Height()));
    else if (m_dPosition.bottom<m_dBottom)
      clipRect = CRect(0, 0, 0, 0);
    pDC->IntersectClipRect(&clipRect);
  }

  // TODO: parameter in DrawObj -> user can change it in PropsDlgs
  pDC->SetBkMode( TRANSPARENT );

  switch (m_nShape)
  {
  case polyline:
    pDC->Polyline( m_points, m_nPoints );
    break;
    
  case polygon:
    nOldFillMode = pDC->SetPolyFillMode(ALTERNATE);
    pDC->Polygon(m_points, m_nPoints);
    pDC->SetPolyFillMode(nOldFillMode);
    break;
  }
  if( m_bClip )
    pDC->RestoreDC( -1 );
  
  pDC->SelectObject(pOldBrush);
  pDC->SelectObject(pOldPen);
  pDC->SelectObject(pOldFont);
  if (m_logbrush.lbStyle==BS_PATTERN)
  {
    // reset the original bitmap handle
    m_logbrush.lbHatch = (LONG)hOldBitmap;
  }
}

// position must be in logical coordinates
void CDrawPoly::MoveTo( const CIntIRect& position, CDrawView* pView )
{
  ASSERT_VALID(this);
  if( position == m_position )
    return;
  
  if( !pView )
    Invalidate();
  else
    pView->InvalObj( this );
  
  CIntIRect temp( position );
  CDoubleIRect rect;
  m_pDocument->LogicalToMeters( temp, rect, this );

  double scaleIntX = m_position.Width() != 0 ? position.Width() / m_position.Width() : 1.0;
  double scaleIntY = m_position.Height() != 0 ? position.Height() / m_position.Height() : 1.0;
  double scaleDblX = m_dPosition.Width() != 0 ? rect.Width() / m_dPosition.Width() : 1.0;
  double scaleDblY = m_dPosition.Height() != 0 ? rect.Height() / m_dPosition.Height() : 1.0;

  for( int i = 0; i < m_nPoints; i++ )
  {
    m_points[i].x = ( m_points[i].x - m_position.left ) * scaleIntX + position.left;
    m_points[i].y = ( m_points[i].y - m_position.top ) * scaleIntY + position.top;

    m_coords[i].x = ( m_coords[i].x - m_dPosition.left ) * scaleDblX +  rect.left;
    m_coords[i].y = ( m_coords[i].y - m_dPosition.top ) * scaleDblY +  rect.top;
  }

  CDoubleIRect oldDPosition( m_dPosition );
  
  m_position = position;
  m_pDocument->LogicalToMeters( m_position, m_dPosition, this );

  // dTo und dFrom entsprechend anpasssen
  CDoubleIRect newDPosition( m_dPosition );

  m_dTo = newDPosition.left + newDPosition.Width() / oldDPosition.Width() * ( m_dTo - oldDPosition.left );
  m_dFrom = newDPosition.left + newDPosition.Width() / oldDPosition.Width() * ( m_dFrom - oldDPosition.left );
  
  if( !pView )
    Invalidate();
  else
  {
    CalcDrawingHelpers( pView );
    pView->InvalObj( this );
  }
  m_pDocument->SetModifiedFlag();
}

int CDrawPoly::GetHandleCount()
{
  return m_nPoints;
}

CPoint CDrawPoly::GetHandle(int nHandle)
{
  ASSERT_VALID(this);
  
  ASSERT(nHandle >= 1 && nHandle <= m_nPoints);
  return m_points[nHandle - 1];
}

HCURSOR CDrawPoly::GetHandleCursor( int nHandle )
{
  CDrawApp* drawApp = GETDRAWAPP;
  if( IsMoveable() || IsOffsetable() )
  {
    if( nHandle == -1 || IsOffsetable() )
      return drawApp->LoadStandardCursor( IDC_SIZEALL );
    else
      return drawApp->LoadCursor( IDC_SIZEDIAG );
  }
  else
    return drawApp->LoadStandardCursor( IDC_ARROW );
}

// point is in logical coordinates
void CDrawPoly::MoveHandleTo(int nHandle, CIntPoint point, CDrawView* pView)
{
  ASSERT_VALID(this);
  ASSERT(nHandle >= 1 && nHandle <= m_nPoints);
  if (m_points[nHandle - 1] == (CPoint)point)
    return;
  
  m_points[nHandle - 1] = (CPoint)point;
  m_pDocument->LogicalToMeters( point, m_coords[nHandle - 1], this );
  RecalcBounds(pView);
  
  if (pView == NULL)
    Invalidate();
  else
    pView->InvalObj(this);
  m_pDocument->SetModifiedFlag();
}

// rect must be in logical coordinates
BOOL CDrawPoly::Intersects( const CIntIRect& rect ) const
{
  int i, j;
  int compx, compy;
  CRect clipRect;
  CIntIRect temp;
  
  CDrawApp* drawApp = GETDRAWAPP;
  
  ASSERT_VALID(this);
  CRect rectT( rect.left, rect.top, rect.right, rect.bottom );
  rectT.NormalizeRect();
  
  CRgn rgn;
  if( m_bClip )
  {
    temp = GetAdjustedRect();
    temp.NormalizeRect();
    clipRect = rectT;
    clipRect.left = temp.left + (int)( temp.Width() * ( m_dFrom - m_dPosition.left ) / m_dPosition.Width() );
    clipRect.right = temp.right + (int)( temp.Width() * ( m_dTo - m_dPosition.right ) / m_dPosition.Width() );
    if( m_dPosition.Height() != 0 )
      clipRect.bottom = rect.bottom + (int)( abs( rect.Height() ) * ( m_dBottom - m_dPosition.bottom ) / fabs( m_dPosition.Height() ) ) + 1;
    else if( m_dPosition.bottom < m_dBottom )
      clipRect = CIntIRect( 0, 0, 0, 0 );

    DEBUG_ONLY( m_logCat.debug( "RectT: left %d top %d right %d bottom %d", rectT.left, rectT.top, rectT.right, rectT.bottom ) );
    DEBUG_ONLY( m_logCat.debug( "ClipRect: left %d top %d right %d bottom %d", clipRect.left, clipRect.top, clipRect.right, clipRect.bottom ) );
    rectT.IntersectRect( rectT, clipRect );
    DEBUG_ONLY( m_logCat.debug( "Intersected rectT: left %d top %d right %d bottom %d", rectT.left, rectT.top, rectT.right, rectT.bottom ) );
  }
  int x = (m_logpen.lopnWidth.x + 2*drawApp->m_nSelectTol) / 2;
  int y = (m_logpen.lopnWidth.y + 2*drawApp->m_nSelectTol) / 2;//Dick 10.05.2000 darf nicht 0 sein sonst kann nicht selektieren
  switch (m_nShape)
  {
  case polyline:
    {
      CPoint* points = new CPoint[m_nPoints*2];
      for (i=0; i<m_nPoints; i++)
      {
        if (i+1==m_nPoints)
        {
          compx = -m_points[i-1].x;
          compy = -m_points[i-1].y;
        }
        else
        {
          compx = m_points[i+1].x;
          compy = m_points[i+1].y;
        }
        if (m_points[i].x < compx)
        {
          points[i].x = m_points[i].x - x;
        }
        else
        {
          points[i].x = m_points[i].x + x;
        }
        
        if (m_points[i].y < compy)
        {
          points[i].y = m_points[i].y - y;
        }
        else
        {
          points[i].y = m_points[i].y + y;
        }
      }
      for (i=0, j=m_nPoints-1; i<m_nPoints; i++, j--)
      {
        if (j==0)
        {
          compx = -m_points[j+1].x;
          compy = -m_points[j+1].y;
        }
        else
        {
          compx = m_points[j-1].x;
          compy = m_points[j-1].y;
        }
        if (compx < m_points[j].x)
        {
          points[m_nPoints+i].x = m_points[j].x + x;
        }
        else
        {
          points[m_nPoints+i].x = m_points[j].x - x;
        }
        
        if (compy < m_points[j].y)
        {
          points[m_nPoints+i].y = m_points[j].y + y;
        }
        else
        {
          points[m_nPoints+i].y = m_points[j].y - y;
        }
      }
      rgn.CreatePolygonRgn(points, m_nPoints*2, ALTERNATE);
      delete points;
    }
    break;
    
  case polygon:
    rgn.CreatePolygonRgn(m_points, m_nPoints, ALTERNATE);
    break;
  }
#ifdef _DEBUG
  if (m_bBreakWhenIntersected)
  {
    if (rgn.RectInRegion(rectT))
      DebugBreak();
  }
#endif
  return rgn.RectInRegion(rectT);
}

CDrawObj* CDrawPoly::Clone(CDrawDoc* pDoc, BOOL bExactCopy /*=FALSE*/)
{
  ASSERT_VALID(this);
  
  CDrawPoly* pClone = new CDrawPoly(m_position, pDoc);
  pClone->x_flags = x_flags;
  pClone->m_nIndex = m_nIndex;
  pClone->m_nLayer = m_nLayer;
  pClone->m_logfont = m_logfont;
  pClone->m_logpen = m_logpen;
  pClone->m_logbrush = m_logbrush;
  pClone->m_colorText = m_colorText;
  pClone->m_nType = m_nType;
  pClone->m_nShape = m_nShape;
  pClone->m_nLeftArrow = m_nLeftArrow;
  pClone->m_nRightArrow = m_nRightArrow;
  if (bExactCopy && m_pConnections!=NULL)
  {
    POSITION pos;
    
    if (pClone->m_pConnections==NULL)
      pClone->m_pConnections = new CDrawObjList;
    pos = m_pConnections->GetHeadPosition();
    while (pos!=NULL)
    {
      CDrawObj* pCon;
      
      pCon = m_pConnections->GetNextObject(pos);
      if (pClone->m_pConnections->FindObject( pCon )==NULL)
        pClone->m_pConnections->AddTailObject( pCon );
    }
  }
  else
    pClone->m_pConnections = NULL;
  if (m_nAllocPoints>0)
  {
    pClone->m_points = new CPoint[m_nAllocPoints];
    memcpy(pClone->m_points, m_points, sizeof(CPoint) * m_nPoints);
    pClone->m_coords = new CDoublePoint[m_nAllocPoints];
    memcpy(pClone->m_coords, m_coords, sizeof(CDoublePoint) * m_nPoints);
  }
  pClone->m_nAllocPoints = m_nAllocPoints;
  pClone->m_nPoints = m_nPoints;
  if( bExactCopy )
  {
    pClone->m_dPosition = m_dPosition;
    pClone->m_nSectionIndex = m_nSectionIndex;
    pClone->m_ptOffset = m_ptOffset;
    pClone->m_bClip = m_bClip;
    pClone->m_dFrom = m_dFrom;
    pClone->m_dTo = m_dTo;
    pClone->m_dBottom = m_dBottom;
  }
  ASSERT_VALID(pClone);
  
  return pClone;
}

void CDrawPoly::SetShape(int nShape)
{
  m_nShape = (Shape)nShape;
  
  switch (m_nShape)
  {
  default:
    ASSERT(FALSE); // unsuported shape!
    
  case polyline:
    SetFlags( pen | arrow );
    UnsetFlags( font | brush | filled );
    break;
    
  case polygon:
    SetFlags( pen | brush | filled );
    UnsetFlags( font | arrow );
    break;
  }
}

void CDrawPoly::Invalidate( BOOL bConnections /*=TRUE*/ )
{
  CDrawView *pView = m_pDocument->GetView();
  
  ASSERT_VALID(this);
  
  m_pDocument->MetersToLogical( m_dPosition, m_position, this );
  for (int i=0; i<m_nPoints; i++)
  {
    CIntPoint pt = m_points[i];
    m_pDocument->MetersToLogical( m_coords[i], pt, this );
    m_points[i] = CPoint( pt );
  }
  if (pView!=NULL)
    CalcDrawingHelpers(pView);
  m_pDocument->UpdateAllViews( NULL, HINT_UPDATE_DRAWOBJ, this );
  if (bConnections)
    UpdateConnections();
}

// point is in logical coordinates
void CDrawPoly::AddPoint(const CPoint& point, CDrawView* pView)
{
  ASSERT_VALID(this);
  if (m_nPoints == m_nAllocPoints)
  {
    CPoint* newPoints = new CPoint[m_nAllocPoints + 10];
    if (m_points != NULL)
    {
      memcpy(newPoints, m_points, sizeof(CPoint) * m_nAllocPoints);
      delete[] m_points;
    }
    m_points = newPoints;
    CDoublePoint* newCoords = new CDoublePoint[m_nAllocPoints + 10];
    if (m_coords != NULL)
    {
      memcpy(newCoords, m_coords, sizeof(CDoublePoint) * m_nAllocPoints);
      delete[] m_coords;
    }
    m_coords = newCoords;
    m_nAllocPoints += 10;
  }
  
  if (m_nPoints == 0 || m_points[m_nPoints - 1] != point)
  {
    m_points[m_nPoints] = point;
    m_pDocument->LogicalToMeters( (CIntPoint)m_points[m_nPoints], m_coords[m_nPoints], this);
    m_nPoints++;
    if (!RecalcBounds(pView))
    {
      if (pView == NULL)
        Invalidate();
      else
        pView->InvalObj(this);
    }
    m_pDocument->SetModifiedFlag();
  }
}

// coord is in meters
void CDrawPoly::AddPoint(const CDoublePoint& pt, CDrawView* /*pView*/)
{
  ASSERT_VALID(this);
  if (m_nPoints == m_nAllocPoints)
  {
    CPoint* newPoints = new CPoint[m_nAllocPoints + 10];
    if (m_points != NULL)
    {
      memcpy(newPoints, m_points, sizeof(CPoint) * m_nAllocPoints);
      delete[] m_points;
    }
    m_points = newPoints;
    CDoublePoint* newCoords = new CDoublePoint[m_nAllocPoints + 10];
    if (m_coords != NULL)
    {
      memcpy(newCoords, m_coords, sizeof(CDoublePoint) * m_nAllocPoints);
      delete[] m_coords;
    }
    m_coords = newCoords;
    m_nAllocPoints += 10;
  }
  
  if (m_nPoints == 0 || m_coords[m_nPoints - 1] != pt)
  {
    m_coords[m_nPoints] = pt;
    m_pDocument->MetersToLogical( m_coords[m_nPoints], (CIntPoint)m_points[m_nPoints], this);
    m_nPoints++;
    // Cannot recalc bounds here since pView is not valid
    m_pDocument->SetModifiedFlag();
  }
}

BOOL CDrawPoly::RecalcBounds(CDrawView* pView)
{
  ASSERT_VALID(this);
  
  if (m_nPoints == 0)
    return FALSE;
  
  CIntIRect bounds( m_points[0].x, m_points[0].y, m_points[0].x, m_points[0].y );
  for (int i = 1; i < m_nPoints; ++i)
  {
    if (m_points[i].x < bounds.left)
      bounds.left = m_points[i].x;
    if (m_points[i].x > bounds.right)
      bounds.right = m_points[i].x;
    if (m_points[i].y < bounds.top)
      bounds.top = m_points[i].y;
    if (m_points[i].y > bounds.bottom)
      bounds.bottom = m_points[i].y;
  }
  
  if (bounds == m_position)
    return FALSE;
  
  if (pView == NULL)
    Invalidate();
  else
    pView->InvalObj(this);
  
  m_position = bounds;
  m_pDocument->LogicalToMeters(bounds, m_dPosition, this);
  m_dPosition.NormalizeRect();
  
  if (pView == NULL)
    Invalidate();
  else
    pView->InvalObj(this);
  
  return TRUE;
}

void CDrawPoly::AddToDXF(CDXFZeichnung* zn, const CString& layerName)
{
  CDXFLayer *pLayer;
  int i;
  
  pLayer = zn->FindLayer(layerName);
  if (pLayer==NULL)
  {
    pLayer = new CDXFLayer(zn);
    pLayer->SetName(layerName);
    zn->SetLayer(layerName, pLayer);
  }
  
  CIntIRect rect( GetAdjustedRect() );
  CIntIRect clipRect( rect );
  if( m_bClip )
  {
    clipRect.left = rect.left + (int)(rect.Width()*(m_dFrom-m_dPosition.left)/m_dPosition.Width());
    clipRect.right = rect.right + (int)(rect.Width()*(m_dTo-m_dPosition.right)/m_dPosition.Width());
  }
  
  CDXFPolylinie *pl = new CDXFPolylinie(pLayer);
  BOOL bNewPolyNeeded = FALSE;
  
  pl->SetColor(m_logpen.lopnColor);
  pl->SetWidth(m_logpen.lopnWidth.x);
  pl->SetLType(m_logpen.lopnStyle);
  
  for (i=0; i<m_nPoints; i++)
  {
    if (bNewPolyNeeded)
    {
      if (pl->GetNumCoords()>0)
      {
        pLayer->AddPolylinie(pl);
        
        pl = new CDXFPolylinie(pLayer);
        pl->SetColor(m_logpen.lopnColor);
        pl->SetWidth(m_logpen.lopnWidth.x);
        pl->SetLType(m_logpen.lopnStyle);
      }
      bNewPolyNeeded = FALSE;
    }
    if (m_bClip && (m_points[i].x>clipRect.right || m_points[i].x<clipRect.left))
    {
      BOOL bPointInserted = FALSE;
      if (i>0)
      {
        if (m_points[i-1].x<=clipRect.right && m_points[i-1].x>=clipRect.left)
        {
          int newX, newY;
          
          if (m_points[i].x>clipRect.right)
            newX = clipRect.right;
          else
            newX = clipRect.left;
          newY = m_points[i-1].y+(m_points[i].y-m_points[i-1].y)*(newX-m_points[i-1].x)/(m_points[i].x-m_points[i-1].x);
          pl->AddCoord( newX, newY );
          bPointInserted = TRUE;
        }
      }
      if (i<m_nPoints-1)
      {
        if (m_points[i+1].x<=clipRect.right && m_points[i+1].x>=clipRect.left)
        {
          int newX, newY;
          
          if (m_points[i].x>clipRect.right)
            newX = clipRect.right;
          else
            newX = clipRect.left;
          newY = m_points[i+1].y+(m_points[i].y-m_points[i+1].y)*(newX-m_points[i+1].x)/(m_points[i].x-m_points[i+1].x);
          pl->AddCoord( newX, newY );
          bPointInserted = TRUE;
        }
      }
      if (!bPointInserted)
        bNewPolyNeeded = TRUE;
    }
    else
      pl->AddCoord( m_points[i].x, m_points[i].y );
  }
  
  pLayer->AddPolylinie(pl);
}

void CDrawPoly::SetVisibility( CDrawObj* pObj, BOOL bVisible, CDrawObjList* pUndoList )
// setzt die Sichtbarkeit auf die Sichtbarkeit von pObj, falls 
// pObj die gleichen Parameter hat wie this
// Parameter:
//        CDrawObj* pObj: das Vergleichsobjekt
//        BOOL bVisible: solls sichtbar oder unsichtbar sein
//        CDrawobjList* pUndoList: falls != 0, hängt sich this statt sich zu verändern nur an die List an ( für undo )
{
  CString rClass( pObj->GetRuntimeClass()->m_lpszClassName );
  rClass.TrimLeft();
  rClass.TrimRight();
  
  if ( rClass.CompareNoCase( TEXT("CDrawPoly") ) == 0  && 
    pObj->GetType() == GetType() && 
    pObj->GetSectionIndex() == GetSectionIndex() && 
    pObj->GetDataBlockIndex() == GetDataBlockIndex() && 
    ( ((CDrawPoly*)pObj)->GetShape() == polygon && GetShape() == polygon ||
    ((CDrawPoly*)pObj)->GetShape() != polygon && GetShape() != polygon    )
    )
  {
    if ( bVisible && IsInvisible() )
    {
      if ( pUndoList )
        pUndoList->AddHeadObject( this );
      else
        UnsetFlags( invisible, TRUE );
    }
    else if( !bVisible && !IsInvisible() )
    {
      if ( pUndoList )
        pUndoList->AddHeadObject( this );
      else
        SetFlags( invisible, TRUE );
    };
    Invalidate( FALSE );
    
    if ( m_pDocument )
    {
      // falls das Objekt in einer Dokumentenliste ist
      // ganz nach vorne schieben ->
      // dadurch werden die Füllungen vor den Linien gezeichnet
      // und überdecken diese somit nicht
      CDrawObjList* pObjects = m_pDocument->GetObjects();
      
      POSITION temppos = pObjects->FindObject( this );
      if ( temppos )
      {
        pObjects->RemoveObjectAt( temppos );
        pObjects->AddHeadObject( this );
      }; // if temppos
    }; // if m_pdocument
    
  }; // if type == type && ...
  
}; // SetVisibility


/* virtual */ CDoubleIRect CDrawPoly::CalcBounds( CMap<double, double, double, double>& mins, CMap<double, double, double, double>& maxs )
// diese Funktion rechnet die Ausmasse des Objektes aus, und gibt auch noch spezifische Minima und Maxima zurück
// Parameter:
//      CMap<double, double, double, double>& mins: hier werden die unteren Grenzen der linken und rechten Begrezung abgelegt
//      CMap<double, double, double, double>& maxs: dito die Maxima
{
  // die Grösse wird anhand der Punkte berechnet
  CDoubleIRect rect( 1e36, -1e36, -1e36, 1e36 );
  for( int i = 0; i < GetNumPoints(); i++ )
  {
    CDoublePoint pt = GetPoint( i );

    rect.CompareAndExpand( pt );

    // jetzt noch minima und maxima für jeden Punkt setzen
    mins.SetAt( pt.x, pt.y );
    maxs.SetAt( pt.x, pt.y );
  } // for i

  rect.NormalizeRect();
  return rect;
} // CalcBounds

void CDrawPoly::Dump( CDumpContext& dc ) const
// gibt aktuellen Zustand des objekts in einen DumpContext aus
{
  dc << "a CDrawPoly at " << (void*)this << " with " << m_position << " and " << m_dPosition << " and Offset " << GetOffset();
}; // Dump

////////////////////////////////////////////////////////////////////////////

IMPLEMENT_SERIAL(CDrawOleObj, CDrawObj, VERSIONABLE_SCHEMA | 7 )

/* static */
BOOL CDrawOleObj::c_bShowItems = FALSE;

/* static */
log4cpp::Category& CDrawOleObj::m_logCat = log4cpp::Category::getInstance( "CDrawOleObj" );


CDrawOleObj::CDrawOleObj() : CDrawObj(), m_extent(0,0)
{
  m_pClientItem = NULL;
}

CDrawOleObj::CDrawOleObj(const CIntIRect& position, CDrawDoc* pDoc)
: CDrawObj(position, pDoc), m_extent(0, 0)
{
  m_pClientItem = NULL;
  x_flags = user;
}

CDrawOleObj::CDrawOleObj(const CDoubleIRect& position, CDrawDoc* pDoc)
: CDrawObj(position, pDoc), m_extent(0, 0)
{
  m_pClientItem = NULL;
  x_flags = user;
}

CDrawOleObj::~CDrawOleObj()
{
  if( m_pClientItem )
  {
    m_pClientItem->m_pDrawObj = NULL;
    m_pClientItem->Release();
    m_pClientItem = NULL;
  }
}

void CDrawOleObj::Remove()
{
  if (m_pClientItem != NULL)
  {
    m_pClientItem->Delete();
    m_pClientItem = NULL;
  }
  CDrawObj::Remove();
}

void CDrawOleObj::Serialize( CArchive& ar )
{
  ASSERT_VALID( this );
  
  CDrawObj::Serialize( ar );
  
  if( ar.IsStoring() )
  {
    DEBUG_ONLY( m_logCat.debug( "serializing" ) );

    ar << m_extent;
    ar << m_pClientItem;
  }
  else
  {
    DEBUG_ONLY( m_logCat.debug( "de-serializing" ) );

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
      {
        ar >> m_extent;
        ar >> m_pClientItem;
        m_pClientItem->m_pDrawObj = this;
      }
      break;
      
    default:
      AfxThrowArchiveException(CArchiveException::badSchema);
      break;
    }
  }

  DEBUG_ONLY( debug( m_logCat ) );
} // serialize

CDrawObj* CDrawOleObj::Clone( CDrawDoc* pDoc, BOOL bExactCopy /*=FALSE*/ )
{
  ASSERT_VALID(this);
  
  GETDRAWAPP->BeginWaitCursor();
  
  CDrawOleObj* pClone = NULL;
  CDrawItem* pItem = NULL;
  TRY
  {
    // perform a "deep copy" -- need to copy CDrawOleObj and the CDrawItem
    //  that it points to.

    pClone = new CDrawOleObj( m_position, pDoc );
    pItem = new CDrawItem( pDoc, pClone );

    if( !pItem->CreateCloneFrom( m_pClientItem ) )
      AfxThrowMemoryException();

    // mal wieder eni dirty hack! manchmal ändert CreateCloneFrom die m_position dieses Objekts
    // Grund: ???
    // deswegen kopieren wirs wieder zurück
    m_position = pClone->m_position;

    pClone->m_pClientItem = pItem;
    pClone->x_flags = x_flags;
    pClone->m_nIndex = m_nIndex;
    pClone->m_nLayer = m_nLayer;
    pClone->m_logfont = m_logfont;
    pClone->m_logpen = m_logpen;
    pClone->m_logbrush = m_logbrush;
    pClone->m_colorText = m_colorText;
    pClone->m_nType = m_nType;
    pClone->m_nLeftArrow = m_nLeftArrow;
    pClone->m_nRightArrow = m_nRightArrow;

    if (bExactCopy && m_pConnections!=NULL)
    {
      POSITION pos;
      
      if (pClone->m_pConnections==NULL)
        pClone->m_pConnections = new CDrawObjList;
      pos = m_pConnections->GetHeadPosition();

      while (pos!=NULL)
      {
        CDrawObj* pCon;
        
        pCon = m_pConnections->GetNextObject( pos );
        if (pClone->m_pConnections->FindObject( pCon )==NULL)
          pClone->m_pConnections->AddTailObject( pCon );
      }
    }
    else
      pClone->m_pConnections = NULL;

    if (bExactCopy)
    {
      pClone->m_dPosition = m_dPosition;
      pClone->m_nSectionIndex = m_nSectionIndex;
      pClone->m_ptOffset = m_ptOffset;
      pClone->m_bClip = m_bClip;
      pClone->m_dFrom = m_dFrom;
      pClone->m_dTo = m_dTo;
      pClone->m_dBottom = m_dBottom;
    }

    ASSERT_VALID(pClone);
  }
  CATCH_ALL(e)
  {
    pItem->Delete();
    pClone->m_pClientItem = NULL;
    pClone->Remove();
    GETDRAWAPP->EndWaitCursor();
    
    THROW_LAST();
  }
  END_CATCH_ALL;

    
  GETDRAWAPP->EndWaitCursor();
  return pClone;
}

void CDrawOleObj::Draw(CDC* pDC)
{
  ASSERT_VALID(this);

  if( IsInvisible() )
    return;
  
  CDrawItem* pItem = m_pClientItem;
  if (pItem != NULL)
  {
    // draw the OLE item itself
    pItem->Draw( pDC, (CRect)m_position );
    
    // don't draw tracker in print preview or on printer
    if (!pDC->IsPrinting())
    {
      // use a CRectTracker to draw the standard effects
      CRectTracker tracker;
      tracker.m_rect = m_position;
      pDC->LPtoDP(tracker.m_rect);
      
      if (c_bShowItems)
      {
        // put correct border depending on item type
        if (pItem->GetType() == OT_LINK)
          tracker.m_nStyle |= CRectTracker::dottedLine;
        else
          tracker.m_nStyle |= CRectTracker::solidLine;
      }
      
      // put hatching over the item if it is currently open
      if (pItem->GetItemState() == COleClientItem::openState ||
        pItem->GetItemState() == COleClientItem::activeUIState)
      {
        tracker.m_nStyle |= CRectTracker::hatchInside;
      }
      tracker.Draw(pDC);
    }
  }
}

void CDrawOleObj::OnOpen(CDrawView* pView)
{
  GETDRAWAPP->BeginWaitCursor();
  m_pClientItem->DoVerb(
#ifndef _MAC    
    GetKeyState(VK_CONTROL) < 0 ? OLEIVERB_OPEN : OLEIVERB_PRIMARY,
#else   
    GetKeyState(VK_OPTION) < 0 ? OLEIVERB_OPEN : OLEIVERB_PRIMARY,
#endif    
    pView);
  GETDRAWAPP->EndWaitCursor();
}

void CDrawOleObj::OnEditProperties(CDrawView* /*pView*/)
{
  // using COlePropertiesDialog directly means no scaling
  COlePropertiesDialog dlg(m_pClientItem, 100, 100, NULL);
  
  dlg.DoModal();
}

// position is in logical
void CDrawOleObj::MoveTo(const CIntIRect& position, CDrawView* pView)
{
  ASSERT_VALID(this);
  
  // call base class to update position
  CDrawObj::MoveTo(position, pView);
  
  // update position of in-place editing session on position change
  if( m_pClientItem->IsInPlaceActive() )
    m_pClientItem->SetItemRects();
}

// gibt aktuellen Zustand des objekts in einen DumpContext aus
void CDrawOleObj::Dump( CDumpContext& dc ) const
{
  dc << "a CDrawOleObj at " << (void*)this << " with " << m_position << " and " << m_dPosition << " and Offset " << GetOffset();
}; // Dump


////////////////////////////////////////////////////////////////////////////

