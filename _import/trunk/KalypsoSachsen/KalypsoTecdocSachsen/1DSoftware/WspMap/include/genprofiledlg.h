#if !defined(AFX_GENPROFILEDLG_H__9E21B041_D121_11D5_BEA0_00104BB3E525__INCLUDED_)
#define AFX_GENPROFILEDLG_H__9E21B041_D121_11D5_BEA0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// genprofiledlg.h : Header-Datei
//

class Section;
class ProfilCtrl;
class TripleArray;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CGenProfileDlg 

class CGenProfileDlg : public CDialog
{
// Konstruktion
public:
  CGenProfileDlg::CGenProfileDlg( const CString& titel,  const CString& description,
                                  Profil* profil, const CStringArray& zustaende,
                                  const CString& predFile, const CString& follFile, const CString& newFile,
                                  const double predStation, const double follStation, const double newStation,
                                  const int predVzk, const int follVzk, const CUIntArray& newVzks,
                                  const CString& predPK, const CString& follPk, const CString& newPK,
                                  const double predDist, const double follDist,
                                  const BOOL bPred, const BOOL bFollow,
                                  TripleArray* tripleArray,
                                  CWnd* pParent /*=NULL*/ );
  ~CGenProfileDlg();

// Dialogfelddaten
	//{{AFX_DATA(CGenProfileDlg)
	enum { IDD = IDD_GENERATE_PROFILE };
	CComboBox	m_stateCombo;
	CComboBox	m_vzkCombo;
	CStatic	m_profileDummyCtrl;
	CString	m_description;
	CString	m_editFileFollow;
	CString	m_editFilePred;
	CString	m_editPkFollow;
	CString	m_editPkNew;
	CString	m_editPkPred;
	CString	m_editStationFollow;
	CString	m_editStationNew;
	CString	m_editStationPred;
	CString	m_editVorLFollow;
	CString	m_editFlussFollow;
	CString	m_editFlussPred;
	CString	m_editVorLPred;
	CString	m_editVorRFollow;
	CString	m_editVorRPred;
	CString	m_editVzkFollow;
	CString	m_editVzkPred;
	CString	m_fileText;
	CString	m_followerText;
	CString	m_newText;
	CString	m_pkText;
	CString	m_predText;
	CString	m_stateText;
	CString	m_stationText;
	CString	m_strangDistText;
	CString	m_vorLText;
	CString	m_vorRText;
	CString	m_vzkText;
	CString	m_flussText;
	CString	m_reduceCount;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CGenProfileDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

  // Implementierung
public:
  CString GetNewState() const { return newState; };
  double GetNewStation() const {return newStation; };
  CString GetNewPk() const { return newPK; };
  int GetNewVzk() const { return newVZK; };

protected:
  // für den Eingang
  ProfilCtrl* m_profilCtrl;
  Profil* m_profil;
  CString m_titel;
  CString m_okText;

  CStringArray m_zustaende;
  CUIntArray m_vzks;

  // für den Ausgang
  CString newState;
  double newStation;
  CString newPK;
  int newVZK;

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CGenProfileDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnPaint();
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	virtual void OnOK();
	afx_msg void OnKillfocusEditAusduenn();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

private:
  TripleArray m_tripleArray;
  TripleArray* m_oldTripleArray;

  static double s_ausduennValue;
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_GENPROFILEDLG_H__9E21B041_D121_11D5_BEA0_00104BB3E525__INCLUDED_
