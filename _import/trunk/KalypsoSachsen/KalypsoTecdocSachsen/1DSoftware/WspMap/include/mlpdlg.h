#ifndef AFX_MLPDLG_H__42CC0ED1_813A_11D3_BDAC_00104BB3E537__INCLUDED_
#define AFX_MLPDLG_H__42CC0ED1_813A_11D3_BDAC_00104BB3E537__INCLUDED_

// mlpdlg.h : Header-Datei
//

class CMapLayer;
class CMapDoc;
class	CSingleSymbolPage;
class CValueSymbolPage;
class CBreaksSymbolPage;
class CStandardLabelPage;
class CAdvancedLabelPage;


/////////////////////////////////////////////////////////////////////////////
// CMapLayerPropertyDlg

class CMapLayerPropertyDlg : public CPropertySheet
{
public:
  CMapLayerPropertyDlg( CMapLayer* pLayer, long type, CWnd* pParentWnd = NULL, UINT iSelectPage = 0 );
  virtual ~CMapLayerPropertyDlg();
    
  void OnChangeRenderType( CPropertyPage* page );

// Attribute
public:
	CString m_name;

private:
  CMapLayer* m_pLayer;

// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CMapLayerPropertyDlg)
	public:
	virtual BOOL OnInitDialog();
	//}}AFX_VIRTUAL


	// Generierte Nachrichtenzuordnungsfunktionen
protected:
  //{{AFX_MSG(CMapLayerPropertyDlg)
		// HINWEIS - Der Klassen-Assistent fügt hier Member-Funktionen ein und entfernt diese.
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_MLPDLG_H__42CC0ED1_813A_11D3_BDAC_00104BB3E537__INCLUDED_
