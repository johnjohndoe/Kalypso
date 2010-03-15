#ifndef AFX_PROFILAUSWAHL_H__CDFDAFD2_1A13_11D5_BDF8_00104BB3E525__INCLUDED_
#define AFX_PROFILAUSWAHL_H__CDFDAFD2_1A13_11D5_BDF8_00104BB3E525__INCLUDED_

#include "..\..\commonMfc\commonMfc.h"

#include "..\..\wspprj\wspprj.h"
#include "resource.h"


class CMapDoc;
class CStateProfilesMap;

/////////////////////////////////////////////////////////////////////////////
// ProfilInfo: Daten für Profilübersicht

struct ProfilInfo
{
  CrossSection* cs;
  State* state; // der Zustand, unter welchen das Profil geladen wurde
  CTime fileTime; // Datum der Profildatei zum Zeitpunkt des Initialisierens des ProfilInfo
  BOOL bInMap;
  BOOL bGeoreferenced;
  BOOL bModInMap;  // in der Karte Modifiziert
  BOOL bModExtern; // Extern Modifiziert
};

typedef CTypedPtrArray<CPtrArray, ProfilInfo*> ProfilInfoArray;

typedef CTypedPtrMap<CMapStringToPtr, CString, CStringArray*> WSPVernetzungsInfo;

/////////////////////////////////////////////////////////////////////////////
// Fenster CProfilAuswahl 

class CProfilAuswahl : public CSizingControlBar
{
// Konstruktion
public:
	CProfilAuswahl();
  ~CProfilAuswahl();

// Attribute
public:
  void SetDocument( CMapDoc* mapDoc, Project* project );
  Project* GetProject() { return m_project; };
  ProfilInfoArray& GetProfilInfo() { return m_profileData; };

private:
  CMapDoc* GetDocument() { return m_mapDoc; };

  CMapDoc* m_mapDoc;
  Project* m_project;
  CImageList m_imageList;
  ProfilInfoArray m_profileData; // List aller Profile des Projektes
  CToolBarEx m_toolbar;
  CListCtrlEx m_profileList;
  CTreeCtrl m_treeCtrl;
  State* m_activeState; // der aktuell ausgewählte Zustand
  WSPVernetzungsInfo m_netz;

// Operationen
public:
  void AddProfilesToMap( ProfilInfoArray& profilInfos, State* state );
  void RemoveProfilesFromMap( ProfilInfoArray& profilInfos );
  void DeleteProfiles( const CStringArray& profilNames, BOOL bRemoveFromStrand );

  BOOL SaveChangesInMap( ProfilInfoArray& profilInfos, BOOL bAsk = TRUE );
  BOOL UpdateExternChanges();
  void UpdateListStatus();
  BOOL ParseCommand( NMPROJECTMNG* command );

  void UpdateVernetzung();
  void DeleteVernetzung();

  void EditProfile( const CString& csFile, const CString& stFile );

  bool AddNewProfile( TripleArray* triples, const CString profilFiles[2], const CString stateFiles[2],
                                    const double profilAbstaende[2], const CString& comment, const bool bLoad = true );

  void AddNewProfile( TripleArray* triples, bool bLoad, State* stateName, const double station, const int vzk, const CString& pk, const CString& comment );

  void ShowPInfo( ProfilInfo* pInfo );
  void SelectState( State* zustand );

public:
// Dialogfelddaten
  //{{AFX_DATA(CProfilAuswahl)
  //}}AFX_DATA
  

// Überschreibungen
protected:
  virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
  //{{AFX_VIRTUAL(CProfilAuswahl)
	//}}AFX_VIRTUAL

// Implementierung
public:

protected:
  void DeleteContents();
  void FillProfileList( State* state, ProfilInfo* pInfo = NULL );

	// Generierte Nachrichtenzuordnungsfunktionen
protected:
  afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
  afx_msg void OnKickIdle();
  afx_msg BOOL OnProfilAuswahlCommand( UINT nID );
  afx_msg void OnUpdateProfilAuswahlToolbar(CCmdUI* pCmdUI);
  afx_msg void OnItemchangedProfileList( NMHDR* pNMHDR, LRESULT* pResult );
  afx_msg void OnSelchangedTreeCtrl( NMHDR* pNMHDR, LRESULT* pResult );

	//{{AFX_MSG(CProfilAuswahl)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_PROFILAUSWAHL_H__CDFDAFD2_1A13_11D5_BDF8_00104BB3E525__INCLUDED_
