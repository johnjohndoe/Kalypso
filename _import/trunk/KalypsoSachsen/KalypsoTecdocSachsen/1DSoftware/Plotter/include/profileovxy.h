#if !defined(AFX_PROFILEOVXY_H__0BE60713_A2CB_11D5_BE7C_00104BB3E525__INCLUDED_)
#define AFX_PROFILEOVXY_H__0BE60713_A2CB_11D5_BE7C_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// profileovxy.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CProfileOvXY 

class CProfileOvXY : public CDialog
{
// Konstruktion
public:
	CProfileOvXY( UINT width, UINT height, UINT maxWidth, UINT maxHeight, CWnd* pParent = NULL );

// Dialogfelddaten
	//{{AFX_DATA(CProfileOvXY)
	enum { IDD = IDD_PROFILE_OV_XY };
	CComboBox	m_widthCombo;
	CComboBox	m_heightCombo;
	CString	m_heightText;
	CString	m_widthText;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CProfileOvXY)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
public:
  UINT GetHeight() const { return height; };
  UINT GetWidth() const { return width; };

protected:
  UINT maxHeight, maxWidth;
  UINT height, width;

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CProfileOvXY)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_PROFILEOVXY_H__0BE60713_A2CB_11D5_BE7C_00104BB3E525__INCLUDED_
