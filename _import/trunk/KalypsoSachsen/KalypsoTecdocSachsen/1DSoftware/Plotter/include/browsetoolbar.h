#ifndef AFX_BROWSETOOLBAR_H__2A0D8391_13CB_11D5_BDF2_00104BB3E525__INCLUDED_
#define AFX_BROWSETOOLBAR_H__2A0D8391_13CB_11D5_BDF2_00104BB3E525__INCLUDED_

// browsetoolbar.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Fenster CBrowseToolBar 

class CBrowseToolBar : public CToolBar
{
// Konstruktion
public:
	CBrowseToolBar();

// Attribute
public:
  CComboBox* GetComboBox();

protected:
  CComboBox m_comboBox;
  int m_comboHeight;
  int m_comboWidth;
  UINT m_comboID;
  UINT m_replaceID;
  int m_replaceBitmap;

// Operationen
public:

// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CBrowseToolBar)
	//}}AFX_VIRTUAL
  virtual void OnBarStyleChange(DWORD dwOldStyle, DWORD dwNewStyle);

// Implementierung
public:
	virtual ~CBrowseToolBar();

  BOOL Init( CWnd* pParentWnd, UINT replaceID, UINT comboID, UINT menuID, 
    DWORD dwStyle = WS_CHILD | WS_VISIBLE | CBRS_TOP,  UINT toolbarID = AFX_IDW_TOOLBAR);


protected:
  void ShowComboBox();
  void HideComboBox();

	// Generierte Nachrichtenzuordnungsfunktionen
protected:
	//{{AFX_MSG(CBrowseToolBar)
	//}}AFX_MSG
  afx_msg BOOL OnSelchangeComboBox( UINT nID );
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_BROWSETOOLBAR_H__2A0D8391_13CB_11D5_BDF2_00104BB3E525__INCLUDED_
