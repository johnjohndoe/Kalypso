// PrintRectLegend.h: Schnittstelle für die Klasse CPrintRectLegend.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_PRINTRECTLEGEND_H__F473DF81_4B98_11D6_B2C2_00104BB3E525__INCLUDED_)
#define AFX_PRINTRECTLEGEND_H__F473DF81_4B98_11D6_B2C2_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "PrintRect.h"

class CDIBSectionLite;

/**
 * ein Druckrechteck, welches die Legende anzeigt
 *
 * VERALTET!
 * Es wird jetzt CPrintRectLegend2 verwendet, diese KLasse exisitert nur noch
 * damit die Serialisierung noch klappt
 */
class CPrintRectLegend : public CPrintRect  
{
  //construktor & destruktor
public:
  CPrintRectLegend();
  ~CPrintRectLegend();

  DECLARE_SERIAL( CPrintRectLegend ); // diese Klasse ist Serialisierbar

protected:
    void DeleteContents();
  
  // Überschreibungen von CPrintRect
public:
  void Paint( CDC* );

  void SetMap( CMoMap* pMoMap );

protected:
  void Serialize( CArchive& ar );
  CPropertyPage* CreatePropPage( UINT captionID );

  void AdjustBounds( CDC* pDC, CRect& bounds );

  ///////////////
  // Attribute //
  ///////////////
public:
  void SetFixSize( const BOOL fixSize );
  BOOL GetFixSize() const { return m_bFixSize; };
protected:
  CSize GetSize() const;

  CTypedPtrArray<CPtrArray, CDIBSectionLite*> m_dibSections; // für jeden Layer eine Bitmap

  BOOL m_bFixSize; // ob die Originalgrösse der Bitmap beibehalten werden soll
};

// Function prototype with default arguments
HBITMAP LoadDIBSectionFromFile( LPCTSTR lpszFileName, LPVOID *ppvBits=NULL, 
                               HANDLE hSection=NULL, DWORD dwOffset=0) ;


#endif // !defined(AFX_PRINTRECTLEGEND_H__F473DF81_4B98_11D6_B2C2_00104BB3E525__INCLUDED_)
