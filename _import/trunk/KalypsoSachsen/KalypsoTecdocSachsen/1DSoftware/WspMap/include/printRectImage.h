// PrintRectImage.h: Schnittstelle für die Klasse CPrintRectImage.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_PRINTRECTIMAGE_H__B117C5C1_55BF_11D6_B2CB_00104BB3E525__INCLUDED_)
#define AFX_PRINTRECTIMAGE_H__B117C5C1_55BF_11D6_B2CB_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "PrintRect.h"

class CDIBSectionLite;

// ein Druckrechteck, welches eine beliebige Bitmap anzeigt
class CPrintRectImage : public CPrintRect  
{
public:
  CPrintRectImage();
  virtual ~CPrintRectImage();
  
  DECLARE_SERIAL( CPrintRectImage ); // diese Klasse ist Serialisierbar
  
  // Überschreibungen von CPrintRect
public:
  void Paint( CDC* );
  
protected:
  void Serialize( CArchive& ar );
  CPropertyPage* CreatePropPage( UINT captionID );

  void AdjustBounds( CDC* pDC, CRect& bounds );
  
  // Operationen
public:
  BOOL LoadBitmap( const CString& fileName );

  //Attribute
public:
  CString GetFileName() const { return m_fileName; };

  void SetFixSize( const BOOL fixSize );
  BOOL GetFixSize() const { return m_bFixSize; };

protected:
  CDIBSectionLite* m_dibSection; 
  CString m_fileName;  // der Dateiname

  BOOL m_bFixSize; // ob immer die originalgrösse der Bitmap beibehalten wird
};

#endif // !defined(AFX_PRINTRECTIMAGE_H__B117C5C1_55BF_11D6_B2CB_00104BB3E525__INCLUDED_)
