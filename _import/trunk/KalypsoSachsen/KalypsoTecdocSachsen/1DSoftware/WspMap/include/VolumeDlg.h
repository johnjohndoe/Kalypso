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


// �berschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktions�berschreibungen
	//{{AFX_VIRTUAL(CVolumeDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterst�tzung
	//}}AFX_VIRTUAL

// Implementierung
protected:

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CVolumeDlg)
		// HINWEIS: Der Klassen-Assistent f�gt hier Member-Funktionen ein
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_VOLUMEDLG_H__76C4DA94_D2D4_11D9_9696_000C29C56F8A__INCLUDED_
