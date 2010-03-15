#ifndef PRINT_RECT_TEXT_H_INCLUDED
#define PRINT_RECT_TEXT_H_INCLUDED

#include "PrintRect.h"
#include "IPrintRectFont.h"

// ein Druckrechteck, welches einen beliebigen Text darstellt
class CPrintRectText : public CPrintRect, public IPrintRectFont
{
  
public:
  CPrintRectText();
  virtual ~CPrintRectText() {};

  DECLARE_SERIAL( CPrintRectText ); // diese Klasse ist Serialisierbar
  
  // Attribute //
public:
  const CString GetText(){ return m_text; }
  void SetText( const CString& );
  
private:
  CString m_text;
  
  // Operationen //
public:
  virtual void Paint( CDC* );
  
protected:
  virtual CPropertyPage* CreatePropPage( UINT captionID );
  void Serialize( CArchive& ar );
};

#endif // PRINT_RECT_TEXT_H_INCLUDED