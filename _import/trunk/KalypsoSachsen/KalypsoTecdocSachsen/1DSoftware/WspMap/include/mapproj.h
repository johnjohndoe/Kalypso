#ifndef AFX_MAPPROJ_H__C8C40C6E_44E4_11D3_A4B9_0080ADAC5D6B__INCLUDED_
#define AFX_MAPPROJ_H__C8C40C6E_44E4_11D3_A4B9_0080ADAC5D6B__INCLUDED_

#pragma warning(disable:4786)
#pragma warning(disable:4503)

class COverviewBar;


#include "mapdoc.h"

#include "..\..\commonMfc\commonMfc.h"

// mapproj.h : Header-Datei
//

class CProjectDocTemplate : public CMultiDocTemplate
{
	DECLARE_DYNAMIC(CProjectDocTemplate)

// Constructors
public:
	CProjectDocTemplate(UINT nIDResource, CRuntimeClass* pDocClass,
		CRuntimeClass* pFrameClass, CRuntimeClass* pViewClass);
	virtual CDocument* OpenDocumentFile(LPCTSTR lpszPathName, 
                                      BOOL bMakeVisible = TRUE);
};

/////////////////////////////////////////////////////////////////////////////
// Dokument CMapProject 

class CMapProject : public CDocument
{
protected:
	CMapProject();           // Dynamische Erstellung verwendet geschützten Konstruktor
	DECLARE_SERIAL(CMapProject)

public:
	virtual ~CMapProject();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CMapProject)
public:
	virtual void Serialize(CArchive& ar);   // Überschrieben für Dokument-Ein-/Ausgabe
	virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);
	virtual void DeleteContents();
	//}}AFX_VIRTUAL

// Operationen
public:
	void UpdateMapDoc( CMapDoc* mapDoc ); //LPCSTR lpszFileName, LPCSTR lpszName, Rect& rect);
  void UpdateOverview();
	void DeleteMapDocFiles(CString& file);
  BOOL CloseMap( CMapDoc* pDoc ); // schliesst eine Karte
  BOOL CloseMaps(); // schliesst alle offenen Karten
  BOOL OpenMap();
  CMapDoc* CreateMap();
  void DeleteMap();
  BOOL OpenMap( int i ); // öffnet die i. Karte
  BOOL ReloadProject();
  BOOL ParseCommand( NMPROJECTMNG* command );

// Attribute
public:
	int GetNumMapDocs() {	return m_strMapDocFiles.GetSize(); };
	CString GetMapDocFile(int i);
	CString GetMapDocName(int i);
	CDoubleRect GetMapDocRect(int i);
  CString GetNewMapDocFile();
  Project* GetProject() { return m_pProject; };
  void SetProject( Project* pProject ) { m_pProject = pProject; };
  void SetOverviewBar( COverviewBar* overviewBar ) { m_overviewBar = overviewBar; };
  COverviewBar* GetOverviewBar() { return m_overviewBar; };

protected:
	Project *m_pProject;
  COverviewBar* m_overviewBar;
  CStringArray m_strMapDocFiles;  // Liste aller existierenden Karten: Dateinamen
  CStringArray m_strMapDocNames;  //  -''-                             Namen
  CArray<CDoubleRect, CDoubleRect&> m_MapDocRects;

	// Generierte Nachrichtenzuordnungsfunktionen

	//{{AFX_MSG(CMapProject)
  afx_msg void OnMapOpen();
	afx_msg void OnMapNew();
	afx_msg void OnUpdateMapExists(CCmdUI* pCmdUI);
	afx_msg void OnMapDelete();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_MAPPROJ_H__C8C40C6E_44E4_11D3_A4B9_0080ADAC5D6B__INCLUDED_
