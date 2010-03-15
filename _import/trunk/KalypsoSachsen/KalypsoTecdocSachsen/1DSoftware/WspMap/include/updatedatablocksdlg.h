#if !defined(AFX_UPDATEDATABLOCKSDLG_H__3CB91361_ADC7_11D5_BE87_00104BB3E525__INCLUDED_)
#define AFX_UPDATEDATABLOCKSDLG_H__3CB91361_ADC7_11D5_BE87_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// updatedatablocksdlg.h : Header-Datei
//

#include "..\..\commonMfc\commonMfc.h"

class CLayer;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CUpdateDatablocksDlg 

class CUpdateDatablocksDlg : public CDialog
{
  struct Data
  {
    Data( const CString& name, const int type, const BOOL checked )
    {
      this->name = name;
      this->type = type;
      this->checked = checked;
    };
  
    CString name;
    int type;
    BOOL checked;
    CUIntArray exclusions; // wird dieses Feld aktiviert, werden alle hier angegebenen deaktiviert
    CUIntArray inclusions; // wird dieses Feld aktiviert/deaktiviert, so werden alle hier angegebenen auch aktiviert/deaktiviert
  }; // struct Data

// Konstruktion
public:
	CUpdateDatablocksDlg( CWnd* pParent = NULL );   // Standardkonstruktor
  ~CUpdateDatablocksDlg();

// Dialogfelddaten
	//{{AFX_DATA(CUpdateDatablocksDlg)
	enum { IDD = IDD_UPDATA_DATABLOCKS };
	CListCtrlEx	m_listCtrl;
	CString	m_heading;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CUpdateDatablocksDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
public:
  int AddType( const CString& dbname, const int dbtype, const BOOL checked ) 
                                  { return m_data.Add( new Data( dbname, dbtype, checked ) ); };
  void AddExclusion( const int index, const int exclusion );
  void AddInclusion( const int index, const int inclusion );
  void GetCheckedTypes( CArray<int, int>& typeArray );
  
protected:
  CTypedPtrArray<CPtrArray, Data*> m_data;
  CImageList m_StateImageList;

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CUpdateDatablocksDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnItemchangedDatablocksList(NMHDR* pNMHDR, LRESULT* pResult);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_UPDATEDATABLOCKSDLG_H__3CB91361_ADC7_11D5_BE87_00104BB3E525__INCLUDED_
