// Implementierung der class CListCtrlEx2

#include "stdafx.h"

#include "lctrlex2.h"

#define MINCOLWIDTH 10      // minimale Breite einer Spalte


/////////////////
// Message Map //
/////////////////

BEGIN_MESSAGE_MAP(CListCtrlEx2, CListCtrl)

ON_WM_SIZE()
ON_WM_KEYDOWN()

END_MESSAGE_MAP()


void CListCtrlEx2::OnSize( UINT nType, int cx, int cy )
{
  CListCtrl::OnSize( nType, cx, cy );

//  AutoSizeColumns();
}; // OnSize

void CListCtrlEx2::OnKeyDown( UINT nChar, UINT nRepCnt, UINT nFlags ) 
{
  switch( nChar )
  {
  case VK_F2:
    int index = GetSelectedID();
    if (index != -1)
      EditLabel( index );
    break;
  }; // switch
  
  CListCtrl::OnKeyDown(nChar, nRepCnt, nFlags);
}


///////////////
// Attribute //
///////////////

int CListCtrlEx2::GetSelectedID() const
// findet erste selektierte Zeile
// Rückgabewert:
//      int:  Index der ersten selektiert Zeile, -1, falls keine Zeile selektiert
{
  for ( int i = 0; i < GetItemCount(); i++ )
  {
    if ( GetItemState( i, LVIS_SELECTED ) == LVIS_SELECTED )
      return i;
  };
  return -1;
}; // GetSelectedID

/////////////////
// Operationen //
/////////////////

void CListCtrlEx2::AutoSizeColumns( const int col /*=-1*/ )
// von: http://www.codeguru.com/listview/autosize_col.shtml, by Roger Onslow
// setzt die Breite der Spalte(n) so, dass gerade alles sichtbar ist
// Parameter:
//        const int col: welche Spalte soll verändert werden ( -1 : alle Spalten )
//
{
  //SetRedraw(false);
  int mincol = col < 0 ? 0 : col;
  int maxcol = col < 0 ? GetColumnCount()-1 : col;
  for ( int spalte = mincol; spalte <= maxcol; spalte++) 
  {
    SetColumnWidth( spalte, LVSCW_AUTOSIZE );
    int wc1 = GetColumnWidth( spalte );
    SetColumnWidth( spalte, LVSCW_AUTOSIZE_USEHEADER );
    int wc2 = GetColumnWidth( spalte );
    int wc = max( MINCOLWIDTH, max( wc1, wc2 ) );
    SetColumnWidth( spalte, wc );
  }
  
  // RecalcHeaderTips(); *** uncomment this if you use my header tips method
  //   SetRedraw(true);
  Invalidate(); // *** uncomment this if you don't use my SetRedraw function
}; // AutoSizeColumns
