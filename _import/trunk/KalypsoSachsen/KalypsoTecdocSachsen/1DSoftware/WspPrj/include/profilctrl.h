#if !defined(AFX_PROFILCTRL_H__9E6B2601_985C_11D5_BE70_00104BB3E525__INCLUDED_)
#define AFX_PROFILCTRL_H__9E6B2601_985C_11D5_BE70_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// profilctrl.h : Header-Datei
//

class Profil;

#include "triple.h"

/////////////////////////////////////////////////////////////////////////////
// Fenster ProfilCtrl 

class ProfilCtrl : public CWnd
{
// Konstruktion/Destruktion
public:
   ProfilCtrl( const CString& title );

// Attribute
public:
   void SetProfil( Profil* profil );
   Profil* GetProfil() const { return m_profil; };

   void SetTriple( TripleArray* tripleArray );
   TripleArray* GetTriple() const { return m_tripleArray; };

   void SetData( const DWORD data ) { m_data = data; };
   DWORD GetData() const { return m_data; };

   void SetBackColor( const COLORREF color ) { m_backColor = color; };
   COLORREF GetBackColor() const { return m_backColor; };

   void SetTextColor( const COLORREF color ) { m_textColor = color; };
   COLORREF GetTextColor() const { return m_textColor; };

   void SetHeaderSize( const UINT size );
   UINT GetHeaderSize() const { return m_headerSize; };

protected:
  Profil* m_profil;
  TripleArray* m_tripleArray;
  DWORD m_data; // zusätzliche Daten für den Benutzer
  CRect m_maxExtent, m_extent; // maximal möglicher sowie aktueller Ausschnitt
  COLORREF m_backColor, m_textColor;
  UINT m_headerSize;
  CString m_headerText;
  CString m_title;

// Operationen / Implementation
protected:
  void ReadProfil();
  void PaintAxes( CDC* dc, CRect* clientRect );
  void NotifyParent( UINT code );


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(ProfilCtrl)
public:
	virtual void WinHelp(DWORD dwData, UINT nCmd = HELP_CONTEXT);
	//}}AFX_VIRTUAL
   virtual BOOL CreateEx( DWORD dwStyleEx, DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID );


	// Generierte Nachrichtenzuordnungsfunktionen
protected:
	//{{AFX_MSG(ProfilCtrl)
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDblClk(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnPaint();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_PROFILCTRL_H__9E6B2601_985C_11D5_BE70_00104BB3E525__INCLUDED_
