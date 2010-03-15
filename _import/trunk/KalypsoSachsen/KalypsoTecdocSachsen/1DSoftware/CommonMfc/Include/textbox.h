// TextBox.h : header file
//

#ifndef TEXTBOX_H
#define TEXTBOX_H

#include "grphbox.h"

/////////////////////////////////////////////////////////////////////////////
// CTextComboBox window

// To use this class create a combo box based on this class with
// the styles: Droplist and Variable Owner Draw.
// You must also initialize the control after it has been created
// (Call Init for the control from InitDialog).

/////////////////////////////////////////////////////////////////////////////
class CTextComboBox : public CGraphicComboBox
{
// Construction
public:
	CTextComboBox();

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CTextComboBox)
	public:
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CTextComboBox();

	// Generated message map functions
protected:
	//{{AFX_MSG(CTextComboBox)
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

#endif	/* TEXTBOX_H */
