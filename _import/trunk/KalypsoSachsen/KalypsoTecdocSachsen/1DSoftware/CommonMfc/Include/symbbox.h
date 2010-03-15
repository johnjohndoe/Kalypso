// SymbBox.h : header file
//

#ifndef SYMBBOX_H
#define SYMBBOX_H

#include "grphbox.h"

/////////////////////////////////////////////////////////////////////////////
// CSymbolComboBox window

// To use this class create a combo box based on this class with
// the styles: Droplist and Variable Owner Draw.

/////////////////////////////////////////////////////////////////////////////
class CSymbolComboBox : public CGraphicComboBox
{
// Construction
public:
	CSymbolComboBox();

// Attributes
public:

// Operations
public:
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CSymbolComboBox)
	public:
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CSymbolComboBox();

	// Generated message map functions
protected:
	//{{AFX_MSG(CSymbolComboBox)
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

#endif /*SYMBBOX_H*/