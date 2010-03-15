// Title.h: Schnittstelle für die Klasse CTitle.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_TITLE_H__E9F1C381_7E99_11D6_B2E9_00104BB3E525__INCLUDED_)
#define AFX_TITLE_H__E9F1C381_7E99_11D6_B2E9_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CDrawRect;
class CPlotterDoc;

class CTitle  
{
  ////////////////
  // Konstanten //
  ////////////////
private:
  static UINT TITLE_GAP;

  ////////////////////////////////
  // Konstruktion / Destruktion //
  ////////////////////////////////
public:
	CTitle();

  /////////////////
  // Operationen //
  /////////////////
public:
  void Update( const CIntPoint& basePoint, CPlotterDoc* pDoc );

  ///////////////
  // Attribute //
  ///////////////
public:
  CString GetFormatText() const { return m_titelFormatText; };
  void SetFormatText( const CString& text ) { m_titelFormatText = text; };

  CDrawRect* GetTitle() { return m_titel; };
  CDrawRect* GetTitle() const { return m_titel; };
  void SetTitle( CDrawRect* obj ) { m_titel = obj; };

private:
  CDrawRect* m_titel;			// Titel object
  CString m_titelFormatText;	// Text used to format title text
};

#endif // !defined(AFX_TITLE_H__E9F1C381_7E99_11D6_B2E9_00104BB3E525__INCLUDED_)
