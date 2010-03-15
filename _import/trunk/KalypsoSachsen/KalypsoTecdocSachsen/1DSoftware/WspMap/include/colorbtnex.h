#ifndef AFX_COLORBTN_H__B4788002_4335_11D3_A4B9_0080ADAC5D6B__INCLUDED_
#define AFX_COLORBTN_H__B4788002_4335_11D3_A4B9_0080ADAC5D6B__INCLUDED_

// colorbtn.h : Header-Datei
//

#pragma warning(disable:4786)
#pragma warning(disable:4503)

/////////////////////////////////////////////////////////////////////////////
// Fenster CColorButton 

class CColorButtonEx : public CButton
{
public:
	CColorButtonEx(void);
	void SetFaceColor(COLORREF colFace);
	COLORREF colGetFaceColor(void);
	void SetState(BOOL fSelected);
	static UINT idClicked;
protected:
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);
private:
	BOOL m_fSelected;
	COLORREF m_colFace;
	HPALETTE m_hPal;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_COLORBTN_H__B4788002_4335_11D3_A4B9_0080ADAC5D6B__INCLUDED_
