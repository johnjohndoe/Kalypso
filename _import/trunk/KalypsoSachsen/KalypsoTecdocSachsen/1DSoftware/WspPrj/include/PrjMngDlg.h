#ifndef AFX_PRJMNGDLG_H__13C266A2_8943_11D4_BD45_00104BB3E525__INCLUDED_
#define AFX_PRJMNGDLG_H__13C266A2_8943_11D4_BD45_00104BB3E525__INCLUDED_

// PrjMngDlg.h : Header-Datei
//

class ProjectList;
class Project;
class CrossSectionArray;
struct NMPROJECTMNG;

#include "lsection.h"

// Definitionen
#define STATE_TYPE_WATER    0
#define STATE_TYPE_STATE    1
#define STATE_TYPE_QS       2
#define STATE_TYPE_LS       3
#define STATE_TYPE_CALC     4
#define STATE_TYPE_OUTFLOW  5


/////////////////////////////////////////////////////////////////////////////
// Dialogfeld ProjectManagerDialog 

class ProjectManagerDialog : public CDialog
{
  // Datentypen
  struct StateTreeData
  {
    StateTreeData( int type, void* data )
    {
      this->type = type;
      this->data = data;
    };

    int type;
    void* data;
  };

// Konstruktion
public:
	ProjectManagerDialog(CWnd* pParent = NULL);   // Standardkonstruktor
	~ProjectManagerDialog();

protected:
  DeleteContents();

// Dialogfelddaten
	//{{AFX_DATA(ProjectManagerDialog)
	enum { IDD = IDD_PROJECT_MANAGER };
	CStatic	m_staticProjekte;
	CStatic	m_staticProfile;
	CListCtrlEx2	m_projectList;
	CTreeCtrl	m_stateTree;
	CListCtrlEx2	m_dataList;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(ProjectManagerDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL


// Attribute
protected:
  WORD m_lastSortDirection;  // in welche Richtung wurde zuletzt sortiert ? ( für alle sortierbaren Listen )
  ProjectList* m_wspProjects; // die wsp.prj
  CString m_wspprjPath;
  CMenu menuBar;  // das Menu
  CToolBar toolBar;  // die Toolbar
  NMPROJECTMNG* m_command;

  Project* GetSelectedProject();
  int GetSelectedProjectID();
  State * GetSelectedState();
  CrossSectionArray* GetSelectedCrossSections();
  LengthSectionArray* GetSelectedLengthSections();

public:
  NMPROJECTMNG* GetCommand() 
  { 
    return m_command;
  };
  

// Operationen
protected:
  void ArrangeControls();
  void UpdateProjectList( const ProjectList* wspProjects, const Project* selProject = NULL );
  void UpdateTitle();

	void UpdateStateTree( Project* projekt, State* alterZustand = NULL, int type = 0 );
  void DeleteStateTree();
  void DeleteStateTreeData( HTREEITEM treeItem );

  void UpdateDataList( StateTreeData* data );

  static void CmdRouteMenu( CWnd* pWnd,CMenu* pMenu );

  void CloseManager( NMPROJECTMNG* command );

private:
  void ExportLengthSectionToCSV( const State* state, const LengthSectionArray* lsArray );
  
protected:	
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(ProjectManagerDialog)
	afx_msg void OnClickProjectList(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnColumnclickProjectList(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnColumnclickDataList(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnSelchangedStateTree(NMHDR* pNMHDR, LRESULT* pResult);
	virtual BOOL OnInitDialog();
	afx_msg void OnKeydownProjectList(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnEndlabeleditProjectList(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnItemchangedProjectList(NMHDR* pNMHDR, LRESULT* pResult);
	//}}AFX_MSG

  afx_msg void OnKickIdle();  // für Update_command_ui handling
  afx_msg BOOL OnDatenimport( UINT nID );
  afx_msg BOOL OnDatenexport( UINT nID );
  afx_msg void OnManagerOpenProject();
  afx_msg BOOL OnManagerAddProject( UINT nID );
  afx_msg BOOL OnManagerDeleteProject( UINT nID );
  afx_msg BOOL OnManagerState( UINT nID );
  afx_msg BOOL OnManagerWsp( UINT nID );
  afx_msg BOOL OnManagerMap( UINT nID );
  afx_msg BOOL OnManagerCS( UINT nID );
  afx_msg void OnManagerPrintProject();

  afx_msg void OnOK();
  afx_msg void OnCancel();
  afx_msg void OnClose();
  afx_msg void OnCreateLSection();

  afx_msg void OnUpdateDatenimport( CCmdUI* pCmdUI );
  afx_msg void OnUpdateDatenexport( CCmdUI* pCmdUI );
  afx_msg void OnUpdateProject( CCmdUI* pCmdUI );
  afx_msg void OnUpdateState( CCmdUI* pCmdUI );
  afx_msg void OnUpdateWsp( CCmdUI* pCmdUI );
  afx_msg void OnUpdateMap( CCmdUI* pCmdUI );
  afx_msg void OnUpdateCS( CCmdUI* pCmdUI );
  
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_PRJMNGDLG_H__13C266A2_8943_11D4_BD45_00104BB3E525__INCLUDED_
