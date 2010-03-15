#if !defined(AFX_VERSCHNEIDDLG_H__474931D1_ACFC_11D5_BE86_00104BB3E525__INCLUDED_)
#define AFX_VERSCHNEIDDLG_H__474931D1_ACFC_11D5_BE86_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// verschneiddlg.h : Header-Datei
//

class CMapLayer;
class CZList;
class CZTable;

#include "layer.h"


/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CVerschneidDlg 

class CVerschneidDlg : public CDialog
{
  // Konstruktion
public:
  CVerschneidDlg( CString nutzDirectory, CMapLayer* mapLayer, CWnd* pParent = NULL );   // Standardkonstruktor
  ~CVerschneidDlg();
  
  // Dialogfelddaten
  //{{AFX_DATA(CVerschneidDlg)
	enum { IDD = IDD_VERSCHNEID };
	CButton	m_themeCheck;
	CButton	m_fieldCheck;
  CEdit	m_editConstraint;
  CComboBox	m_typeCombo;
  CComboBox	m_fieldCombo;
  int		m_action;
  CString	m_staticArt;
	int		m_reli;
	CString	m_staticReli;
	//}}AFX_DATA
  
  
  // Überschreibungen
  // Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
  //{{AFX_VIRTUAL(CVerschneidDlg)
public:
  virtual BOOL DestroyWindow();
protected:
  virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
  //}}AFX_VIRTUAL
  
  // Implementierung
public:
  void AddEntry( CString text, CLayer::LayerType type );
  CString GetFeature() const { return m_feature; };
  CString GetConstraint() const { return m_constraint; };
  CLayer::LayerType GetLayerType() const { return m_type; };
  CZTable* GetZTable() const { return m_zTable; };
  BOOL GetAction() const { return m_action; };
  CString GetReli() const { return m_reliStr; };
  BOOL GetDeleteTheme() const { return m_bDeleteTheme; };
    
protected:
  CStringArray m_strings;
  CArray<CLayer::LayerType, CLayer::LayerType> m_types;
  CMapLayer* m_mapLayer;
  CZList* m_zList;
  // Ergebnisdaten:
  CString m_feature; // das Ausgewählte Feature
  CString m_constraint; // die Einschränkung für Profilbezogenes Verschneiden
  CLayer::LayerType m_type; // der ausgewählte Typ
  CZTable* m_zTable; // die ausgewählte Zurodnungstabelle
  CString m_reliStr; // ob zu rechtem oder linkem Ufer verschnitten werden soll
  BOOL m_bDeleteTheme; // ob das zu erzeugende Thema vorher geleert werden soll
  
  // Generierte Nachrichtenzuordnungsfunktionen
  //{{AFX_MSG(CVerschneidDlg)
  virtual BOOL OnInitDialog();
  virtual void OnOK();
  afx_msg void OnVerschneidRadio();
	afx_msg void OnFieldCheck();
	//}}AFX_MSG
  DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_VERSCHNEIDDLG_H__474931D1_ACFC_11D5_BE86_00104BB3E525__INCLUDED_
