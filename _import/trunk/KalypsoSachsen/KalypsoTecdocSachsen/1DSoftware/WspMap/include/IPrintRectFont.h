// IPrintRectFont.h: Schnittstelle für die Klasse IPrintRectFont.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_IPRINTRECTFONT_H__753D0AF3_6CB1_11D6_B2DA_00104BB3E525__INCLUDED_)
#define AFX_IPRINTRECTFONT_H__753D0AF3_6CB1_11D6_B2DA_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/** Interface für CPrintRect, welche einen Font haben */
class IPrintRectFont
{
public:
	IPrintRectFont();

  LOGFONT GetFontLogfont() const { return m_logfont; };
  void SetFontLogfont( const LOGFONT logFont ) { m_logfont = logFont; };
  
  COLORREF GetFontColor() const { return m_color; };
  void SetFontColor( const COLORREF color ) { m_color = color; };

  UINT GetFontPtSize() const { return m_ptSize; };
  void SetFontPtSize( const UINT ptSize ) { m_ptSize = ptSize; };

  UINT GetFontAlign() const { return m_align; };
  void SetFontAlign( const UINT align ) { m_align = align; };

  void ReadFont( CArchive& ar );
  void WriteFont( CArchive& ar );

private:
  LOGFONT m_logfont; // der Font
  COLORREF m_color;   // Farbe des Fonts
  UINT m_ptSize;      // Grösse in pt
  UINT m_align;       // Alignment für CDC::DrawText
};

#endif // !defined(AFX_IPRINTRECTFONT_H__753D0AF3_6CB1_11D6_B2DA_00104BB3E525__INCLUDED_)
