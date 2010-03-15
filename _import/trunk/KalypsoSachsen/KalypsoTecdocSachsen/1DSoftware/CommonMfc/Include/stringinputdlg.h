/*! Time-stamp: <@(#)stringinputdlg.h   04.11.02 - 10:34:37   Belger>
 *********************************************************************
 *  @file   : stringinputdlg.h
 *
 *  Author  : Belger                              Date: 04.11.02
 *
 *  Purpose : Ein einfacher Dialog zum Eingeben einer einzelnen 
 *            TextZeile. Ttitel, Beschriftung und Standardwert
 *            könen gewählt werden.
 *
 *********************************************************************
 */
#if !defined(AFX_STRINGINPUTDLG_H__03DACC43_EFCC_11D6_B349_00104BB3E525__INCLUDED_)
#define AFX_STRINGINPUTDLG_H__03DACC43_EFCC_11D6_B349_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CStringInputDlg 

/*!
 * @class
 *   CStringInputDlg 
 *        Ein einfacher Dialog zum Eingeben einer einzelnen 
 *            TextZeile. Ttitel, Beschriftung und Standardwert
 *            könen gewählt werden.
 *
*/
class CStringInputDlg : public CDialog
{
// Konstruktion
public:
  /// Der Standardkonstruktor
	CStringInputDlg( const CString& title, const CString& text, const CString& input, CWnd* pParent = NULL );   // Standardkonstruktor

private:
// Dialogfelddaten
	//{{AFX_DATA(CStringInputDlg)
	enum { IDD = IDD_STRING_INPUT };
	CString	m_text;
	CString	m_input;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CStringInputDlg)
protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

  // Implementierung
public:
  /// Gibt die Eingabe zurück.
  CString GetInput() const { return m_input; };
private:
  CString m_title;

protected:

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CStringInputDlg)
		// HINWEIS: Der Klassen-Assistent fügt hier Member-Funktionen ein
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_STRINGINPUTDLG_H__03DACC43_EFCC_11D6_B349_00104BB3E525__INCLUDED_
