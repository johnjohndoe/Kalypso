#ifndef AFX_INTRPPG4_H__6691F902_9BFC_11D3_BDBE_00104BB3E537__INCLUDED_
#define AFX_INTRPPG4_H__6691F902_9BFC_11D3_BDBE_00104BB3E537__INCLUDED_

// intrppg4.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CInterpPage4 

#include "..\..\commonMfc\commonMfc.h"

class CInterpPage4 : public CPropertyPage
{
	DECLARE_DYNCREATE(CInterpPage4)

// Konstruktion
public:
	CInterpPage4(CInterpSheet *pOwner = NULL);
	~CInterpPage4();

// Dialogfelddaten
	//{{AFX_DATA(CInterpPage4)
	enum { IDD = IDD_INTERP_PAGE4 };
	CListCtrlEx	m_list;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CInterpPage4)
	public:
	virtual BOOL OnSetActive();
	virtual BOOL OnWizardFinish();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	CInterpSheet *m_pOwner;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CInterpPage4)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_INTRPPG4_H__6691F902_9BFC_11D3_BDBE_00104BB3E537__INCLUDED_
