// ColorBox.h : header file
//

#ifndef COLORBOX_H
#define COLORBOX_H

//Colors : Standard colors for QcQp
#define BLACK           0		// Schwarz
#define DARKRED         1		// Dunkelrot
#define DARKGREEN       2		// Gruen
#define LIGHTBROWN      3		// Ocker
#define DARKBLUE        4		// Dunkelblau
#define DARKMAGENTA     5		// Purpurrot
#define DARKCYAN        6		// Blaugruen
#define GRAY            7		// Grau
#define LIGHTGRAY       8		// Hellgrau
#define RED             9		// Rot
#define GREEN           10		// Gruen
#define YELLOW          11		// Gelb
#define BLUE            12		// Blau
#define MAGENTA         13		// Lila
#define CYAN            14		// Aquamarin
#define WHITE           15		// Weiss

#define NCOLORS   (16)

#include "grphbox.h"

/////////////////////////////////////////////////////////////////////////////
// CColorComboBox window

// To use this class create a combo box based on this class with
// the styles: Droplist and Variable Owner Draw.
// You must also pass a handle to a palette (using SetPalette) after
// the control has been created (InitDialog is the best place to do this).

/////////////////////////////////////////////////////////////////////////////
class CColorComboBox : public CGraphicComboBox
{
// Construction
public:
	CColorComboBox();

// Attributes
public:

// Operations
public:
	void SetPalette(HPALETTE hPal);
	COLORREF GetColor();
	void SetColor(COLORREF color);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CColorComboBox)
	public:
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CColorComboBox();

protected:
	HPALETTE m_hPal;
	CPoint pointer;

	COLORREF GetPaletteColor(int nIndex);
	CString GetColorText(int nIndex);
	// Generated message map functions
protected:
	//{{AFX_MSG(CColorComboBox)
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

#endif	/* COLORBOX_H */
