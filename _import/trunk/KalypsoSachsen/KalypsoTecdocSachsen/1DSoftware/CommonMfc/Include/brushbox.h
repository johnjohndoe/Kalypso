// BrushBox.h : header file
//

#ifndef BRUSHBOX_H
#define BRUSHBOX_H

#include "grphbox.h"

/////////////////////////////////////////////////////////////////////////////
// CBrushComboBox window

// To use this class create a combo box based on this class with
// the styles: Droplist and Variable Owner Draw.
// You must also add brushes (using AddLogBrush) after
// the control has been created (InitDialog is the best place to do this).

/////////////////////////////////////////////////////////////////////////////
BOOL operator==(const LOGBRUSH& lb1, const LOGBRUSH& lb2);

class CBrushComboBox : public CGraphicComboBox
{
// Construction
public:
	CBrushComboBox();

// Attributes
public:

// Operations
public:
//	void SetPalette(HPALETTE hPal);
	void AddLogBrush(LPLOGBRUSH lpLogBrush);
	LPLOGBRUSH GetLogBrush();
	void SetLogBrush(LPLOGBRUSH lpLogBrush);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CBrushComboBox)
	public:
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CBrushComboBox();

protected:
	CTypedPtrArray<CPtrArray, LPLOGBRUSH> m_logbrushes;

	// Generated message map functions
protected:
	//{{AFX_MSG(CBrushComboBox)
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

#endif	/* BRUSHBOX_H */
