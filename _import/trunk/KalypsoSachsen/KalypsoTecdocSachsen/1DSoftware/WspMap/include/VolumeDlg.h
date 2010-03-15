#if !defined(AFX_VOLUMEDLG_H__76C4DA94_D2D4_11D9_9696_000C29C56F8A__INCLUDED_)
#define AFX_VOLUMEDLG_H__76C4DA94_D2D4_11D9_9696_000C29C56F8A__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// VolumeDlg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CVolumeDlg 

class CVolumeDlg : public CDialog
{
// Konstruktion
public:
	CVolumeDlg(const CString& volume, CWnd* pParent = NULL);   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(CVolumeDlg)
	enum { IDD = IDD_VOLUME_DIALOG };
	CString	m_volumeText;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CVolumeDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CVolumeDlg)
		// HINWEIS: Der Klassen-Assistent fügt hier Member-Funktionen ein
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_VOLUMEDLG_H__76C4DA94_D2D4_11D9_9696_000C29C56F8A__INCLUDED_
