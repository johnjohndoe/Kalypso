#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "plotview.h"
#include "drawdoc.h"
#include "plotter.h"
#include "plotdoc.h"

#include "Table.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

////////////////
// Konstanten //
////////////////

/* static */ UINT CTable::LeftOffset = 10;

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////


/*!
 * Standardkonstruktor von CTable
 */
CTable::CTable()
{
  m_leftoffset = m_rightoffset = 0;

  m_nTableFormat = allLines;
};


/*!
 * Formatiert die Tabelle, sorgt dafür, dass sie an der richtigen Stelle steht
 *
 * @param pDoc : das dokument, wird benötigt, um die Funktion LgicalToMeters aufzurufen
 * @param pView : die view des Dokument, zum ausrechnen der tatsächlichen Textgrösse
 * @param dFrom : einschränkung des Bereichs, der angezeigt wird: ab hier
 * @param dTo : dito, bis hier
 * @param rectProfile : Aussmasse des Profils
 * @param profilWidth : ??, Breite des Profils in mm * MM_FAKTOR
 * @param basePoint : an diesem Punkt wird die Tabelle ausgerichtet ( unten/links )
 * @param nXValueFormat : formatierung für die X-Werte
 * @param nYValueFormat : Formatierung für die Y-Werte
 * @param tableHeights : Höhen der einzelnen Tabellenzeilen ( wurde in CPlotterDoc ausgerechnet )
 *
 * @return CIntIRect  : die Ausmasse des Profils in Blattkoordinaten
 */
CIntIRect CTable::Update( CPlotterDoc* pDoc, CView* pView, const double dFrom, const double dTo, const CDoubleIRect& rectProfile, const UINT profilWidth, const CIntPoint& basePoint, const int nXValueFormat, const int nYValueFormat, const CUIntArray& tableHeights )
{
  SetRect( CIntIRect( 0, 0, 0, 0 ) ); // damit LogicalToMeters hier richtig funktioniert

  m_rightoffset = 0;
  m_leftoffset = LeftOffset;

  GETPLOTTERAPP->IncStatusBarProgress();

  // die einzelnen Zeilen positionieren
  CIntIRect rect( INT_MAX, -INT_MAX, -INT_MAX, INT_MAX );
  for( int i = GetSize() - 1; i >= 0; i-- )
  {
    CIntIRect rect2( UpdateRowSize( i, dFrom, dTo, pDoc, pView, nXValueFormat, nYValueFormat ) );

    rect.CompareAndExpand( rect2 );
    rect2 -= CSize( rect2.left, 0 );
    GetAt( i )->SetRect( rect2 );
    GETPLOTTERAPP->IncStatusBarProgress();
  }

  CIntIRect rect3( rect.left, rect.top, rect.right, rect.bottom );
  
  // den Offset anpassen
  UINT fromOffset = rect3.left;
  if( GetSize() > 0 )
  {
    CDrawObjList* row = GetAt( 0 );
    if( !row->IsEmpty() )
    {
      CDrawObj* pObj = row->GetHeadObject();
      if( pObj != NULL )
      {
        CDoublePoint pFrom( dFrom, 0 );
        CIntPoint pOffset;
        pDoc->MetersToLogical( pFrom, pOffset, pObj );

        fromOffset = pOffset.x;
      }; // if pObj
    }; // if row
  }; // if Size() > 0

  CIntIRect rect1( basePoint.x, basePoint.y, m_leftoffset, basePoint.y );

  rect1 -= CSize( fromOffset, 0 );
  for( i = GetSize() - 1; i >= 0; i-- )
  {
    CDrawObjList* tableRow = GetAt( i );
    int width = tableRow->GetRectSize().cx;
    CIntIRect rect2( 0, tableHeights[i],  width, 0 );
  
    POSITION pos = tableRow->GetHeadPosition();
    while( pos != NULL )
    {
      CDrawObj* pObj = tableRow->GetNextObject( pos );

      if (!pObj->IsInvisible())
      {
        CIntPoint offset = pObj->GetOffset();
        if (offset.x==-1)
          continue;

        // adjust offset of every object
        offset.x += m_leftoffset - fromOffset;
        if( pObj->IsLine() )
        {
          if (((CDrawRect*)pObj)->GetHorzJust()==CDrawRect::nojust && ((CDrawRect*)pObj)->GetVertJust()!=CDrawRect::nojust)
          {
            // top or bottom lines of table row
            pObj->m_dPosition.left = dFrom;
            pObj->m_dPosition.right = dTo;
            offset.x -= m_leftoffset;
            ((CDrawRect*)pObj)->SetHorzAdjust( m_leftoffset + m_rightoffset );
          }
          else if (((CDrawRect*)pObj)->GetHorzJust()!=CDrawRect::nojust && ((CDrawRect*)pObj)->GetVertJust()==CDrawRect::nojust && pObj->m_dPosition.left==pObj->m_dPosition.right)
          {
            // last vertical dividor
            pObj->m_dPosition.left = dTo;
            pObj->m_dPosition.right = dTo;
            ((CDrawRect*)pObj)->SetHorzAdjust( m_rightoffset );
          }
        }
        pObj->SetOffset( offset );
      }
    } // while pos
    // shift rect2 to current top of table
    rect2 += CSize(0, rect1.top);
    // adjust current top of table
    rect1.top += abs(rect2.Height());
    // adjust current right of table
    rect1.right = max(rect1.right, rect2.right + basePoint.x );
    // set left of rect2
    rect2 += CSize(rect1.left, 0);
    // now set new row position
    tableRow->SetRect( rect2 );
    GETPLOTTERAPP->IncStatusBarProgress();
  } // for i
  // now set new table position
  SetRect( rect1 );
  // calculate profil position
  CIntIRect rect2( rect1.left, rect1.top + profilWidth, rect1.right, rect1.top );
  // adjust for left offset
  rect2 += CSize( m_leftoffset - fromOffset, 0 );

  return rect2;
} // UpdateTable

/*!
 * Formatiert die einzelnen Zeilen, entscheidet, welche Zahlen, Teilstrichte sichtbar sind oder nicht
 * Die Zeile wird dabei so ausgerichtet, dass die y-Koordinate 0.0 genau auf dem basePoint liegt
 *
 * @param index : Nummer der Zeile
 * @param dFrom : siehe Update
 * @param dTo : siehe Update
 * @param pDoc : siehe Update
 * @param pView : siehe Update
 * @param nXValueFormat : siehe Update
 * @param nYValueFormat : siehe Update
 *
 * @return CIntIRect  : die Grösse der Zeile in Blattkoordinaten?
 */
CIntIRect CTable::UpdateRowSize( int index,  const double dFrom, const double dTo, CPlotterDoc* pDoc, CView* pView, const int nXValueFormat, const int nYValueFormat )
{
  CDrawObjList* pObjList = GetAt( index );

  pObjList->SetRect( CIntIRect( 0, 0, 0, 0 ) ); // damit LogicalToMeters hier richtig funktioniert

  CDrawRect *pPrevXCoord = NULL;
  CDrawRect *pPrevYCoord = NULL;
  CDrawRect *p2PrevXCoord = NULL;
  CDrawRect *pRealPrevXCoord = NULL;
  CIntIRect rect;
  CDoubleIRect rect1, testrect;
  CSize sizeOutput;
  CIntPoint nullOffset = CIntPoint(0, 0);
  CMap<double, double, int, int> offsetmap;
  CMap<double, double, int, int> keepoffsetmap;
  int nMaxRight = 0;
  double dMaxRight = -1e36;
  BOOL bXLeft = (nXValueFormat==CDrawDocData::topLeft || nXValueFormat==CDrawDocData::bottomLeft);
  BOOL bYLeft = (nYValueFormat==CDrawDocData::topLeft || nYValueFormat==CDrawDocData::bottomLeft);
  int dummy, rightLimit = -1;
  
  POSITION pos = pObjList->GetHeadPosition();
  while (pos!=NULL)
  {
    CDrawObj* pObj = pObjList->GetNextObject(pos);
    if (!pObj->IsInvisible() && pObj->IsText())
      pObj->m_position -= pObj->GetOffset();
  }

  rect1.SetRect(1e36, -1e36, -1e36, 1e36);
  if( m_nTableFormat == onlyLinesWithText )
  {
    POSITION pos = pObjList->GetHeadPosition();
    while( pos!=NULL )
    {
      CDrawObj* pObj = pObjList->GetNextObject( pos );
      CIntIRect rect( pObj->m_position );
      if( !pObj->IsInvisible() )
      {
        if( pObj->IsText() && ((CDrawRect*)pObj)->GetTextType() != CDrawRect::normal )
        {
          if( pObj->m_dPosition.right == dTo )
          {
            CSize sizeOutput = ((CDrawRect*)pObj)->GetOutputTextSize(pView);
            ((CDrawRect*)pObj)->SetHorzAdjust( sizeOutput.cx );
            
            // calculate the actual text horizontal positions
            if( ((CDrawRect*)pObj)->GetTextType() == CDrawRect::xcoord )
            {
              // calculate test rect
              if( bXLeft )
                rect.left = rect.right - sizeOutput.cx;
              else
                rect.left = rect.right + sizeOutput.cx;
              rect.NormalizeRect();
              if( rightLimit==-1 )
                rightLimit = rect.left;
              else
                rightLimit = min( rightLimit, rect.left );
            }
            if( ((CDrawRect*)pObj)->GetTextType() == CDrawRect::ycoord )
            {
              if( pObj->m_dPosition.left != dTo )
                continue;
              // calculate test rect
              if( bYLeft )
                rect.right = rect.left - sizeOutput.cx;
              else
                rect.right = rect.left + sizeOutput.cx;
              rect.NormalizeRect();
              if( rightLimit == -1 )
                rightLimit = rect.left;
              else
                rightLimit = min( rightLimit, rect.left );
            }
          }
        }
      }
    } // while pos
  } //  if onlyLinesWithText
  
  pos = pObjList->GetHeadPosition();
  while( pos != NULL )
  {
    CDrawObj* pObj = pObjList->GetNextObject( pos );
    CIntIRect rect( pObj->m_position );
    pObj->SetOffset( nullOffset );
    pObj->SetClipRange( dFrom, dTo, -1 );
    if( !pObj->IsInvisible() )
    {
      
      if( pObj->IsText() && ((CDrawRect*)pObj)->GetTextType() != CDrawRect::normal )
      {
        CSize sizeOutput = ((CDrawRect*)pObj)->GetOutputTextSize( pView );
        ((CDrawRect*)pObj)->SetHorzAdjust( sizeOutput.cx );
        // calculate the actual text horizontal positions
        if( ((CDrawRect*)pObj)->GetTextType() == CDrawRect::xcoord )
        {
          if( pObj->m_dPosition.right < dFrom || pObj->m_dPosition.right > dTo )
          {
            ((CDrawRect*)pObj)->SetHorzAdjust(0);	//hide
            continue;
          }
          // calculate test rect
          if( bXLeft )
            rect.left = rect.right - sizeOutput.cx;
          else
            rect.left = rect.right + sizeOutput.cx;
          rect.NormalizeRect();
          pDoc->LogicalToMeters( rect, testrect, pObj );
          // adjust left and right offsets
          if (bXLeft)
          {
            if (pObj->m_dPosition.left-fabs(testrect.Width()) <= dFrom )
              m_leftoffset = max( m_leftoffset, sizeOutput.cx );
          }
          else
          {
            if (pObj->m_dPosition.left + fabs(testrect.Width()) >= dTo )
              m_rightoffset = max( m_rightoffset, sizeOutput.cx );
          }
        }
        if (((CDrawRect*)pObj)->GetTextType()==CDrawRect::ycoord)
        {
          if (pObj->m_dPosition.left < dFrom || pObj->m_dPosition.left > dTo )
          {
            ((CDrawRect*)pObj)->SetHorzAdjust(0);	//hide
            continue;
          }
          // calculate test rect
          if (bYLeft)
            rect.right = rect.left - sizeOutput.cx;
          else
            rect.right = rect.left + sizeOutput.cx;
          rect.NormalizeRect();
          pDoc->LogicalToMeters(rect, testrect, pObj);
          // adjust left and right offsets
          if (bYLeft)
          {
            if( pObj->m_dPosition.left - fabs( testrect.Width() ) <= dFrom )
              m_leftoffset = max( m_leftoffset, sizeOutput.cx );
          }
          else
          {
            if( pObj->m_dPosition.left + fabs(testrect.Width()) >= dTo )
              m_rightoffset = max( m_rightoffset, sizeOutput.cx );
          }
        }
        switch( m_nTableFormat )
        {
        case allLines:
          if (((CDrawRect*)pObj)->GetTextType()==CDrawRect::xcoord)
          {
            if (bXLeft && !bYLeft)
            {	// x on left and y on right
              if( pPrevXCoord != NULL )
              {
                if (rect.left<pPrevXCoord->m_position.right+pPrevXCoord->GetOffset().x)
                  ((CDrawRect*)pObj)->SetHorzAdjust(0);	//hide
              }
              if( pPrevYCoord != NULL )
              {
                if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x)
                  ((CDrawRect*)pObj)->SetHorzAdjust(0);	// hide
              }
            }
            else if (!bXLeft && bYLeft)
            {	// x on right and y on left
              if (pPrevXCoord!=NULL)
              {
                if (rect.left<pPrevXCoord->m_position.right+pPrevXCoord->GetHorzAdjust()+pPrevXCoord->GetOffset().x)
                  pPrevXCoord->SetHorzAdjust(0);	//hide
              }
            }
            else if (bXLeft && bYLeft)
            {	// x and y on left
              if (pPrevXCoord!=NULL)
              {
                if (rect.left<pPrevXCoord->m_position.right+pPrevXCoord->GetOffset().x)
                  ((CDrawRect*)pObj)->SetHorzAdjust(0);	//hide
              }
              if (pPrevYCoord!=NULL)
              {
                if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetOffset().x)
                  ((CDrawRect*)pObj)->SetHorzAdjust(0);	// hide
              }
            }
            else
            {	// x and y on right
              if (pPrevXCoord!=NULL)
              {
                if (rect.left<pPrevXCoord->m_position.right+pPrevXCoord->GetHorzAdjust()+pPrevXCoord->GetOffset().x)
                  pPrevXCoord->SetHorzAdjust(0);	//hide
              }
              if (pPrevYCoord!=NULL)
              {
                if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x)
                  pPrevYCoord->SetHorzAdjust(0);	// hide
              }
            }
            if (pObj->m_dPosition.right>dMaxRight)
            {
              nMaxRight = pObj->GetOffset().x+sizeOutput.cx;
              dMaxRight = pObj->m_dPosition.left;
            }
            if (pPrevXCoord!=NULL)
              p2PrevXCoord = pPrevXCoord;
            pPrevXCoord = (CDrawRect*)pObj;
          }
          else if (((CDrawRect*)pObj)->GetTextType()==CDrawRect::ycoord)
          {
            if (bXLeft && !bYLeft)
            {	// x on left and y on right
              if (pPrevYCoord!=NULL)
              {
                if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x)
                  pPrevYCoord->SetHorzAdjust(0);	// hide
              }
            }
            else if (!bXLeft && bYLeft)
            {	// x on right and y on left
              if (p2PrevXCoord!=NULL)
              {
                if (rect.left<p2PrevXCoord->m_position.right+p2PrevXCoord->GetHorzAdjust()+p2PrevXCoord->GetOffset().x)
                  ((CDrawRect*)pObj)->SetHorzAdjust(0);	//hide
              }
              if (pPrevYCoord!=NULL)
              {
                if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetOffset().x)
                  ((CDrawRect*)pObj)->SetHorzAdjust(0);	// hide
              }
            }
            else if (bXLeft && bYLeft)
            {	// x and y on left
              if (pPrevYCoord!=NULL)
              {
                if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetOffset().x)
                  ((CDrawRect*)pObj)->SetHorzAdjust(0);	// hide
              }
            }
            else
            {	// x and y on right
              if (pPrevYCoord!=NULL)
              {
                if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x)
                  pPrevYCoord->SetHorzAdjust(0);	// hide
              }
            }
            if (pObj->m_dPosition.left>dMaxRight)
            {
              nMaxRight = pObj->GetOffset().x;
              dMaxRight = pObj->m_dPosition.left;
            }
            pPrevYCoord = (CDrawRect*)pObj;
          }
          break;
          
          case onlyLinesWithText:
            if (((CDrawRect*)pObj)->GetTextType()==CDrawRect::xcoord)
            {
              if( pObj->m_dPosition.right == dTo )
                continue;	// always visible
              if (rightLimit!=-1 && rect.right>rightLimit)
              {
                ((CDrawRect*)pObj)->SetHorzAdjust(0);	//hide
                SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, -1 );	// hide line
                continue;
              }
              else
                keepoffsetmap.SetAt( pObj->m_dPosition.right, 0 );	// unhide line

              if (bXLeft && !bYLeft)
              {	// x on left and y on right
                if (pPrevXCoord!=NULL)
                {
                  if (rect.left<pPrevXCoord->m_position.right+pPrevXCoord->GetOffset().x)
                  {
                    ((CDrawRect*)pObj)->SetHorzAdjust(0);	//hide
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, -1);	// hide line
                  }
                  else
                    keepoffsetmap.SetAt( pObj->m_dPosition.right, 0);	// unhide line
                }
                if (pPrevYCoord!=NULL)
                {
                  if( rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x )
                  {
                    ((CDrawRect*)pObj)->SetHorzAdjust(0);	// hide
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, -1); // hide line
                  }
                  else
                    keepoffsetmap.SetAt( pObj->m_dPosition.right, 0); // unhide line
                }
              }
              else if (!bXLeft && bYLeft)
              {	// x on right and y on left
                if (pPrevXCoord!=NULL)
                {
                  if (rect.left<pPrevXCoord->m_position.right+pPrevXCoord->GetHorzAdjust()+pPrevXCoord->GetOffset().x)
                  {
                    ((CDrawRect*)pObj)->SetHorzAdjust(0);	//hide
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, -1);	// hide line
                  }
                  else
                    keepoffsetmap.SetAt( pObj->m_dPosition.right, 0);	// unhide line
                }
              }
              else if (bXLeft && bYLeft)
              {	// x and y on left
                if (pPrevXCoord!=NULL)
                {
                  if (rect.left<pPrevXCoord->m_position.right+pPrevXCoord->GetOffset().x)
                  {
                    ((CDrawRect*)pObj)->SetHorzAdjust(0);	//hide
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, -1);	// hide line
                  }
                  else
                    keepoffsetmap.SetAt( pObj->m_dPosition.right, 0);	// unhide line
                }
                if (pPrevYCoord!=NULL)
                {
                  if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetOffset().x)
                  {
                    ((CDrawRect*)pObj)->SetHorzAdjust(0);	// hide
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, -1);	// hide line
                  }
                  else
                    keepoffsetmap.SetAt( pObj->m_dPosition.right, 0);	// unhide line
                }
              }
              else
              {	// x and y on right
                if (pPrevXCoord!=NULL)
                {
                  if (rect.left<pPrevXCoord->m_position.right+pPrevXCoord->GetHorzAdjust()+pPrevXCoord->GetOffset().x)
                  {
                    ((CDrawRect*)pObj)->SetHorzAdjust(0);	//hide
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, -1);	// hide line
                  }
                  else
                    keepoffsetmap.SetAt( pObj->m_dPosition.right, 0);	// unhide line
                }
                if (pPrevYCoord!=NULL)
                {
                  if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x)
                  {
                    ((CDrawRect*)pObj)->SetHorzAdjust(0);	// hide
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, -1);	// hide line
                  }
                  else
                    keepoffsetmap.SetAt( pObj->m_dPosition.right, 0);	// unhide line
                }
              }
              if (pObj->m_dPosition.right>dMaxRight)
              {
                nMaxRight = pObj->GetOffset().x+sizeOutput.cx;
                dMaxRight = pObj->m_dPosition.left;
              }
              if (pPrevXCoord!=NULL)
                p2PrevXCoord = pPrevXCoord;
              if (!offsetmap.Lookup(pObj->m_dPosition.right, dummy))
                pPrevXCoord = (CDrawRect*)pObj;
              pRealPrevXCoord = (CDrawRect*)pObj;
            }
            else if (((CDrawRect*)pObj)->GetTextType()==CDrawRect::ycoord)
            {
              if( pObj->m_dPosition.left == dTo )
                continue;	// always visible
              
              if( offsetmap.Lookup( pObj->m_dPosition.left, dummy ) )
              {
                ((CDrawRect*)pObj)->SetHorzAdjust(0);	// hide
                continue;
              }
              if (rightLimit!=-1 && rect.right>rightLimit)
              {
                ((CDrawRect*)pObj)->SetHorzAdjust(0);	//hide
                SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, -1);	// hide line
                if (pRealPrevXCoord!=NULL)
                  pRealPrevXCoord->SetHorzAdjust(0);
                continue;
              }
              else
                keepoffsetmap.SetAt( pObj->m_dPosition.right, 0);	// unhide line

              if (bXLeft && !bYLeft)
              {	// x on left and y on right
                if (pPrevYCoord!=NULL)
                {
                  if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x)
                  {
                    ((CDrawRect*)pObj)->SetHorzAdjust(0);	// hide
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.left, -1);	// hide line
                  }
                  else
                    keepoffsetmap.SetAt( pObj->m_dPosition.left, 0);	// unhide line
                }
              }
              else if (!bXLeft && bYLeft)
              {	// x on right and y on left
                if (p2PrevXCoord!=NULL)
                {
                  if (rect.left<p2PrevXCoord->m_position.right+p2PrevXCoord->GetHorzAdjust()+p2PrevXCoord->GetOffset().x)
                  {
                    ((CDrawRect*)pObj)->SetHorzAdjust(0);	//hide
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.left, -1);	// hide line
                    if (pRealPrevXCoord!=NULL)
                    {
                      pRealPrevXCoord->SetHorzAdjust(0);	// hide
                      pPrevXCoord = p2PrevXCoord;
                    }
                  }
                  else
                    keepoffsetmap.SetAt( pObj->m_dPosition.left, 0);	// unhide line
                }
                if (pPrevYCoord!=NULL)
                {
                  if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetOffset().x)
                  {
                    ((CDrawRect*)pObj)->SetHorzAdjust(0);	// hide
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.left, -1);	// hide line
                    if (pRealPrevXCoord!=NULL)
                    {
                      pRealPrevXCoord->SetHorzAdjust(0);	// hide
                      pPrevXCoord = p2PrevXCoord;
                    }
                  }
                  else
                    keepoffsetmap.SetAt( pObj->m_dPosition.left, 0);	// unhide line
                }
              }
              else if (bXLeft && bYLeft)
              {	// x and y on left
                if (pPrevYCoord!=NULL)
                {
                  if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetOffset().x)
                  {
                    ((CDrawRect*)pObj)->SetHorzAdjust(0);	// hide
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.left, -1);	// hide line
                  }
                  else
                    keepoffsetmap.SetAt( pObj->m_dPosition.left, 0);	// unhide line
                }
              }
              else
              {	// x and y on right
                if (pPrevYCoord!=NULL)
                {
                  if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x)
                  {
                    ((CDrawRect*)pObj)->SetHorzAdjust(0);	// hide
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.left, -1);	// hide line
                  }
                  else
                    keepoffsetmap.SetAt( pObj->m_dPosition.left, 0);	// unhide line
                }
              }
              if( pObj->m_dPosition.left > dMaxRight )
              {
                nMaxRight = pObj->GetOffset().x;
                dMaxRight = pObj->m_dPosition.left;
              }
              if (!offsetmap.Lookup(pObj->m_dPosition.left, dummy))
                pPrevYCoord = (CDrawRect*)pObj;
            }
            break;
            
          case allValues:
            if (((CDrawRect*)pObj)->GetTextType()==CDrawRect::xcoord)
            {
              if (bXLeft && !bYLeft)
              {	// x on left and y on right
                if (pPrevXCoord!=NULL)
                {
                  if (rect.left<pPrevXCoord->m_position.right+pPrevXCoord->GetOffset().x)
                  {
                    pObj->SetOffset(CIntPoint(pPrevXCoord->m_position.right+pPrevXCoord->GetOffset().x-rect.left, 0));
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, pObj->GetOffset().x);
                  }
                }
                if (pPrevYCoord!=NULL)
                {
                  if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x)
                  {
                    pObj->SetOffset(CIntPoint(pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x-rect.left, 0));
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, pObj->GetOffset().x);
                  }
                }
              }
              else if (!bXLeft && bYLeft)
              {	// x on right and y on left
                if (pPrevXCoord!=NULL)
                {
                  if (rect.left<pPrevXCoord->m_position.right+pPrevXCoord->GetHorzAdjust()+pPrevXCoord->GetOffset().x)
                  {
                    pObj->SetOffset(CIntPoint(pPrevXCoord->m_position.right+pPrevXCoord->GetHorzAdjust()+pPrevXCoord->GetOffset().x-rect.left, 0));
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, pObj->GetOffset().x);
                  }
                }
                if (pPrevYCoord!=NULL)
                {
                  if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetOffset().x)
                  {
                    pObj->SetOffset(CIntPoint(pPrevYCoord->m_position.left+pPrevYCoord->GetOffset().x-rect.left, 0));
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, pObj->GetOffset().x);
                  }
                }
              }
              else if (bXLeft && bYLeft)
              {	// x and y on left
                if (pPrevXCoord!=NULL)
                {
                  if (rect.left<pPrevXCoord->m_position.right+pPrevXCoord->GetOffset().x)
                  {
                    pObj->SetOffset(CIntPoint(pPrevXCoord->m_position.right+pPrevXCoord->GetOffset().x-rect.left, 0));
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, pObj->GetOffset().x);
                  }
                }
                if (pPrevYCoord!=NULL)
                {
                  if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetOffset().x)
                  {
                    pObj->SetOffset(CIntPoint(pPrevYCoord->m_position.left+pPrevYCoord->GetOffset().x-rect.left, 0));
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, pObj->GetOffset().x);
                  }
                }
              }
              else
              {	// x and y on right
                if (pPrevXCoord!=NULL)
                {
                  if (rect.left<pPrevXCoord->m_position.right+pPrevXCoord->GetHorzAdjust()+pPrevXCoord->GetOffset().x)
                  {
                    pObj->SetOffset(CIntPoint(pPrevXCoord->m_position.right+pPrevXCoord->GetHorzAdjust()+pPrevXCoord->GetOffset().x-rect.left, 0));
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, pObj->GetOffset().x);
                  }
                }
                if (pPrevYCoord!=NULL)
                {
                  if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x)
                  {
                    pObj->SetOffset(CIntPoint(pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x-rect.left, 0));
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, pObj->GetOffset().x);
                  }
                }
              }
              if (pObj->m_dPosition.right>dMaxRight)
              {
                nMaxRight = pObj->GetOffset().x+sizeOutput.cx;
                dMaxRight = pObj->m_dPosition.left;
              }
              if (pPrevXCoord!=NULL)
                p2PrevXCoord = pPrevXCoord;
              pPrevXCoord = (CDrawRect*)pObj;
            }
            else if (((CDrawRect*)pObj)->GetTextType()==CDrawRect::ycoord)
            {
              if (bXLeft && !bYLeft)
              {	// x on left and y on right
                if (pPrevYCoord!=NULL)
                {
                  if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x)
                  {
                    pObj->SetOffset(CIntPoint(pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x-rect.left, 0));
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.left, pObj->GetOffset().x);
                  }
                }
                if (pPrevXCoord!=NULL)
                {
                  if (rect.left<pPrevXCoord->m_position.right+pPrevXCoord->GetOffset().x)
                  {
                    pObj->SetOffset(CIntPoint(pPrevXCoord->m_position.right+pPrevXCoord->GetOffset().x-rect.left, 0));
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.left, pObj->GetOffset().x);
                  }
                }
              }
              else if (!bXLeft && bYLeft)
              {	// x on right and y on left
                if (pPrevYCoord!=NULL)
                {
                  if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetOffset().x)
                  {
                    pObj->SetOffset(CIntPoint(pPrevYCoord->m_position.left+pPrevYCoord->GetOffset().x-rect.left, 0));
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.left, pObj->GetOffset().x);
                  }
                }
                if (p2PrevXCoord!=NULL)
                {
                  if (rect.left<p2PrevXCoord->m_position.right+p2PrevXCoord->GetHorzAdjust()+p2PrevXCoord->GetOffset().x)
                  {
                    pObj->SetOffset(CIntPoint(p2PrevXCoord->m_position.right+p2PrevXCoord->GetHorzAdjust()+p2PrevXCoord->GetOffset().x-rect.left, 0));
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.left, pObj->GetOffset().x);
                  }
                }
                if (pPrevXCoord!=NULL)
                {
                  if (pPrevXCoord->m_position.left<pObj->m_position.left+pObj->GetOffset().x)
                  {
                    pPrevXCoord->SetOffset(CIntPoint(pObj->m_position.left+pObj->GetOffset().x-pPrevXCoord->m_position.left, 0));
                    nMaxRight = pPrevXCoord->GetOffset().x + pPrevXCoord->GetOutputTextSize(pView).cx;
                  }
                }
              }
              else if (bXLeft && bYLeft)
              {	// x and y on left
                if (pPrevYCoord!=NULL)
                {
                  if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetOffset().x)
                  {
                    pObj->SetOffset(CIntPoint(pPrevYCoord->m_position.left+pPrevYCoord->GetOffset().x-rect.left, 0));
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.left, pObj->GetOffset().x);
                  }
                }
                if (pPrevXCoord!=NULL)
                {
                  int offsetMax;
                  
                  offsetMax = max(pObj->GetOffset().x, pPrevXCoord->GetOffset().x);
                  pObj->SetOffset(CIntPoint(offsetMax, 0));
                  SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.left, offsetMax);
                }
              }
              else
              {	// x and y on right
                if (pPrevYCoord!=NULL)
                {
                  if (rect.left<pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x)
                  {
                    pObj->SetOffset(CIntPoint(pPrevYCoord->m_position.left+pPrevYCoord->GetHorzAdjust()+pPrevYCoord->GetOffset().x-rect.left, 0));
                    SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.right, pObj->GetOffset().x);
                  }
                }
                if (pPrevXCoord!=NULL)
                {
                  int offsetMax;
                  
                  offsetMax = max(pObj->GetOffset().x, pPrevXCoord->GetOffset().x);
                  pObj->SetOffset(CIntPoint(offsetMax, 0));
                  SetOffset( &offsetmap, &keepoffsetmap, pObj->m_dPosition.left, offsetMax);
                }
              }
              if (pObj->m_dPosition.left>dMaxRight)
              {
                nMaxRight = pObj->GetOffset().x;
                dMaxRight = pObj->m_dPosition.left;
              }
              pPrevYCoord = (CDrawRect*)pObj;
            }
            break;
        }
        rect += pObj->GetOffset();
        //				if (rect.right+pObj->GetOffset().x>m_pPData->m_profil.GetPosition().right)
        //					m_rightoffset = max(m_rightoffset, m_pPData->m_profil.GetPosition().right-rect.right+pObj->GetOffset().x);
      }
      // rect1 calculates the size of the row
      rect1.CompareAndExpand( CDoubleIRect( rect ) );
    }
  }
  if( m_nTableFormat == allValues || m_nTableFormat == onlyLinesWithText )
  {
    POSITION pos = pObjList->GetHeadPosition();
    while( pos )
    {
      CDrawObj* pObj = pObjList->GetNextObject(pos);
      if( !pObj->IsInvisible() && pObj->IsLine() && ((CDrawRect*)pObj)->GetVertJust()==CDrawRect::nojust )
      {
        int offset;
        if( pObj->m_dPosition.left == pObj->m_dPosition.right )
        {
          pObj->SetClipped( FALSE );
          continue;	// last vertical dividor
        }
        
        if( ((CDrawRect*)pObj)->GetHorzJust() == CDrawRect::left )
        {
          if( offsetmap.Lookup(pObj->m_dPosition.left, offset ) )
            pObj->SetOffset( CIntPoint( offset, 0 ) );
        }
        else if( ((CDrawRect*)pObj)->GetHorzJust()==CDrawRect::right )
        {
          if( offsetmap.Lookup(pObj->m_dPosition.right, offset ) )
            pObj->SetOffset(CIntPoint(offset, 0));
        }
      }
    }
  }
  m_rightoffset = max( m_rightoffset, nMaxRight );
  rect.SetRect((int)rect1.left, (int)rect1.top, (int)rect1.right, 0);
  pObjList->SetRect( rect );

  return rect;
} // UpdateRowSize


/*!
 * Hilfsfunktion zum setzen der Offsetmap
 * Verhindert, dass an senkrechten Linien im Gelände der Strich in der Tabelle vershcwindet.
 * Befindet sich in der Map oder in  der keepMap bereits eni eintrag an dieser Stelle, wird nicht
 * eingetragen.
 *
 * @param pOMap : in diese Map wird eingetragen
 * @param pkeepMap : befindet sich in dieser Map bereits ein Eintrag, wird nicht eingetragen
 * @param pos : Schlüssel
 * @param offset : Wert
 *
 */
void CTable::SetOffset( CMap<double, double, int, int>* pOMap, CMap<double, double, int, int>* pkeepMap, const double pos, const int offset )
{
  // verhindert, dass ein Eintrag der bereits belegt ist mit -1 überschrieben wird
  /*
  if( offset == - 1 )
  {
    int oldOffset;
    if( pOMap->Lookup( pos, oldOffset ) || pkeepMap->Lookup( pos, oldOffset ) )
      return;
  }

  Funktioniert nicht! dann bleiben alle Linien da!

  */

  pOMap->SetAt( pos, offset );
}



/*!
 * Erstellt eine Zuodnung TabellenZeilenIndex -> DatenBlockTyp
 *
 * @param dbMap : dies Map wird mit entsprechend gefüllt
 *
 * @return void  : 
 */
void CTable::GetDataBlocks( CMap<int, int, int, int>& dbMap ) const
// Füllt eine Map mit der Zuordnung TableId -> Dataenblock-Typ
// Parameter:
//        CMap<int, int, int, int>& dbMap
{
  dbMap.RemoveAll();
  for( int i = 0; i < GetSize(); i++ )
  {
    CDrawObjList* tableRow = GetAt( i );
    if( tableRow != NULL )
    {
      CDrawObj* pObj = tableRow->GetHeadObject();
      if( pObj != NULL )
        dbMap[i] = pObj->GetType();
    } // if tableRow
  } // for i
} // GetDataBlocks


/*!
 * Rechnet aus, welchen Bereich ein bestimmter DatenBlockTyp einnimmt
 *
 * @param dbType : Typ des Datenblocks
 * @param from : hier wird die kleinste y-Koordinate des DB abgelegt
 * @param to : hier die Grösste
 */
void CTable::GetDbRange( int dbType, double* from, double* to ) const
{
  int index = GetDbIndex( dbType );
  if( index == -1 )
    return;

  CDrawObjList* pTableRow = GetAt( index );
  if( pTableRow != NULL )
  {
    double maxDouble = DBL_MAX;
    double minDouble = DBL_MIN;
    double newFrom = DBL_MAX; // sehr gross
    double newTo = DBL_MIN; // sehr klein

    POSITION pos = pTableRow->GetHeadPosition();
    while( pos != NULL )
    {
      CDrawObj* pObj = pTableRow->GetNextObject( pos );
      if( pObj != NULL && pObj->IsKindOf( RUNTIME_CLASS(CDrawRect) ) && 
        ((CDrawRect*)pObj)->GetShape() == CDrawRect::text )
      {
        CDoubleIRect extent( pObj->m_dPosition );
        newFrom = min( newFrom, extent.left );
        newTo = max( newTo, extent.right );
      } // if pObj
    }; // while pos

    if( newFrom < DBL_MAX && newTo > DBL_MIN )
    {
      *from = newFrom;
      *to = newTo;
    };
  } // if pTableRow
}// GetDbRange



/*!
 * Sucht die Nummer der Zeile zurück, welche einen bestimmten Datenblock enthält
 *
 * @param dbType : gesuchter Datenblocktyp
 *
 * @return int  : Nummer der Zeile; -1, falls keine Zeile mit dem gewünschten Typ gefunden wurde
 */
int CTable::GetDbIndex( int dbType ) const
{
  for( int i = 0; i < GetSize(); i++ )
  {
    CDrawObjList* pRow = GetAt( i );
    if( pRow != NULL )
    {
      CDrawObj* pObj = pRow->GetHeadObject();
      if( pObj != NULL && pObj->GetType() == dbType )
        return i;
    }; // if pRow
  } // for i

  return -1;
} // GetDbIndex