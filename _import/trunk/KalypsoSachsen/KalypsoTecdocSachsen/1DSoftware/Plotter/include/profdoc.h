#ifndef AFX_PROFDOC_H__0B05A3C1_14ED_11D3_A4B8_0080ADAC5D6B__INCLUDED_
#define AFX_PROFDOC_H__0B05A3C1_14ED_11D3_A4B8_0080ADAC5D6B__INCLUDED_

// profdoc.h : Header-Datei
//

#include "plotdoc.h"

/////////////////////////////////////////////////////////////////////////////
// Dokument CProfDatDoc 

class CProfDatDoc : public CPlotterDoc
{
protected:
	CProfDatDoc();           // Dynamische Erstellung verwendet geschützten Konstruktor
	DECLARE_DYNCREATE(CProfDatDoc)
	virtual void DeleteContents();

// Attribute
public:
	CString m_originalFile;

// Operationen
public:

// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CProfDatDoc)
	public:
	virtual void Serialize(CArchive& ar);
	virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);
	virtual void SetTitle(LPCTSTR lpszTitle);
	//}}AFX_VIRTUAL

// Implementierung
public:
	virtual ~CProfDatDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

public:
	static Section* TryLoadSection( CString& fileName );

	// Generierte Nachrichtenzuordnungsfunktionen
protected:
	//{{AFX_MSG(CProfDatDoc)
		// HINWEIS - Der Klassen-Assistent fügt hier Member-Funktionen ein und entfernt diese.
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_PROFDOC_H__0B05A3C1_14ED_11D3_A4B8_0080ADAC5D6B__INCLUDED_
