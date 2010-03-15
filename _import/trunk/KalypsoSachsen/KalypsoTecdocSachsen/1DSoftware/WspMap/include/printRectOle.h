////////////////////////////////////////////////////////
//  An interface for all cinds of elements in rects,  //
////////////////////////////////////////////////////////

#ifndef _PRINT_RECT_OLE_H_INCLUDED_
#define _PRINT_RECT_OLE_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "printRect.h"

// ein Druckrechteck, welches ein beliebiges OLE-Objekt anzeigt
class CPrintRectOle : public CPrintRect
{
public:
  CPrintRectOle();
  virtual ~CPrintRectOle();

  DECLARE_SERIAL( CPrintRectOle ); // diese Klasse ist Serialisierbar
  
  //Methods
public:
  //paint the rect and rectborder
  void Paint( CDC* );

  BOOL CreateOleObject();

protected:
  void UpdateExtent( CDC* pDC );

  //Attribute
protected:
  void Serialize( CArchive& ar );
  CPropertyPage* CreatePropPage( UINT captionID ) { return NULL; }; // eine Eigenschaftsseite erzeugen

  COleDocument m_oleDoc;
  COleClientItem* m_pOleItem;
};

#endif _PRINT_RECT_OLE_H_INCLUDED_