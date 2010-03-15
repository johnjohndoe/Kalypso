// PrintRectMap.h: Schnittstelle für die Klasse CPrintRectMap.
//
//////////////////////////////////////////////////////////////////////
#pragma warning(disable:4786)
#pragma warning(disable:4503)

#if !defined(AFX_PRINTRECTMAP_H__F473DF84_4B98_11D6_B2C2_00104BB3E525__INCLUDED_)
#define AFX_PRINTRECTMAP_H__F473DF84_4B98_11D6_B2C2_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "PrintRect.h"
#include "IPrintRectFont.h"

// ein Druckrechteck, welches die Karte anzeigt
class CPrintRectMap : public CPrintRect, public IPrintRectFont
{
public:
  //construktor & destruktor
public:
  CPrintRectMap();
  virtual ~CPrintRectMap();

  DECLARE_SERIAL( CPrintRectMap ); // diese Klasse ist Serialisierbar
  
  // Implementation von CPrintRect
public:
  void Paint( CDC* );

  void SetMap( CMoMap* pMoMap );

  virtual void SetBounds( const CRect&, CDC* );

  // Attribute
protected:
  CPropertyPage* CreatePropPage( UINT captionID );
  void Serialize( CArchive& ar );

  void AdjustBounds( CDC* pDC, CRect& bounds );


  friend class CPrintMapPage; // diese darf Set und GetScale benutzen

  void SetScale( const UINT scale, const BOOL bAdjustRect, const BOOL bDouble = false );
  UINT GetScale() const { return m_scale; };

private:
  void CalcScale( const CRect& rect );

  UINT m_scale;
}; // class CPrintRectMap

#endif // !defined(AFX_PRINTRECTMAP_H__F473DF84_4B98_11D6_B2C2_00104BB3E525__INCLUDED_)
