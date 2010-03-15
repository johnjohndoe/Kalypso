//////////////////////////////////////////////////
// Klasse CListCtrlEx2                          //
//////////////////////////////////////////////////
// - erweitert Funktionen der Klasse CListCtrl  //
// - war n�tig, da klasse CListCtrl zu viele    //
//   Macken hat und neue Funtkionen nicht       //
//   unterst�tzt                                //
//////////////////////////////////////////////////


#ifndef _LCTRLEX2_H_INCLUDED_
#define _LCTRLEX2_H_INCLUDED_


class CListCtrlEx2 : public CListCtrl
{


// Attribute
public:
  int GetColumnCount() { return GetHeaderCtrl()->GetItemCount(); }; // gibt Anzahl der Spalten zur�ck
  int GetSelectedID() const;
  
// Operationen
public:
  void AutoSizeColumns( const int col = -1 );

  
// Message Map
protected:
  afx_msg void OnSize( UINT nType, int cx, int cy );
  afx_msg void OnKeyDown( UINT nChar, UINT nRepCnt, UINT nFlags );

  afx_msg void OnEndlabeledit( NMHDR* pNMHDR, LRESULT* pResult );
  
  DECLARE_MESSAGE_MAP()
    
}; // class CListCtrlEx2



#endif _LCTRLEX2_H_INCLUDED_