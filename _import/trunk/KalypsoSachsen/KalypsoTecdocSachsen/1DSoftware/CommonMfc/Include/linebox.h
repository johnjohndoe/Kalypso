// LineBox.h : header file
//

#ifndef LINEBOX_H
#define LINEBOX_H

#define NTYPES	5

#include "grphbox.h"

/////////////////////////////////////////////////////////////////////////////
// CLineComboBox window

// To use this class create a combo box based on this class with
// the styles: Droplist and Variable Owner Draw.
// You must also initialize the control after it has been created
// (Call Init for the control from InitDialog).

class CLineComboBox : public CGraphicComboBox
{
// Construction
public:
	CLineComboBox();

// Attributes
public:

// Operations
public:
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLineComboBox)
	public:
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CLineComboBox();

	// Generated message map functions
protected:
	//{{AFX_MSG(CLineComboBox)
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

#endif	/* LINEBOX_H */
