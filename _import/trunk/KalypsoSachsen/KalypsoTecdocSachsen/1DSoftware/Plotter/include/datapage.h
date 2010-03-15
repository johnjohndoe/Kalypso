// DataPage.h : header file
//
#ifndef _DATAPAGE_H_INCLUDED_
#define _DATAPAGE_H_INCLUDED_

/////////////////////////////////////////////////////////////////////////////
// CDataPage dialog

class CDrawObjList;
class CPropertyDialog;
class CPlotterDoc;
class CPlotterView;
class CTemplate;

class CDataPage : public CPropertyPage
{
// Strukturen
  
  struct TreeData // Datenstruktur für den linken Baum
  {
    typedef enum { Section, Datablock, Tabel, Profile } TreeType;

  public:
    TreeData( void* data, const BOOL bVisible, const TreeType type, const int position )
    {
      this->data = data;
      this->bVisible = bVisible;
      this->type = type;
      this->position = position;
    };

    void* data;
    BOOL bVisible;
    TreeType type;
    int position;
  }; // struct TreeData

  // Construction
public:
	CDataPage( CPropertyDialog* pParent, CPlotterDoc* pDoc = NULL );
	~CDataPage();

// Dialog Data
	//{{AFX_DATA(CDataPage)
	enum { IDD = IDD_DATEN };
	CTreeCtrl	m_tree2;
	CTreeCtrl	m_tree1;
	//}}AFX_DATA

// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CDataPage)
public:
	virtual BOOL OnApply();
	virtual void OnOK();
  virtual void OnCancel();
protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL
public:

  // Implementation
public:
	void ApplyTemplate( CTemplate* pTemp );
  void UpdateTree1();

// Implementation
protected:
  void UpdateTree2();
  void FillTree2( const HTREEITEM hTI1, const HTREEITEM hTI2 );
  void DeleteTree1( const HTREEITEM hTI );
  HTREEITEM FindTree2( const HTREEITEM hTI2, const HTREEITEM hTI1 ) const;
  void UpdateDrawing( const HTREEITEM hTI, CDrawObjList* pUndoList );
  void SortTable();
  static int CALLBACK Tree2CompareFunc( LPARAM param1, LPARAM param2, LPARAM paramSort );
  void ShowItemAndChildren( const HTREEITEM hTI, const BOOL bShow );
	void MoveItem( const HTREEITEM hFrom, const HTREEITEM hBefore );

// Attribute
protected:
	CPropertyDialog *m_pParent;
	CPlotterDoc *m_pDoc;
	CPlotterView *m_pView;
	CTemplate *m_pTemp;		// used for applying templates

  // 'globale' Variablen für dragging
	CImageList* m_dragImageList;
  HTREEITEM m_dragItem;

	BOOL m_bUpdated;

  // Generated message map functions
	//{{AFX_MSG(CDataPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnBegindragTree2(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnDestroy();
	//}}AFX_MSG
  afx_msg BOOL OnChange( UINT nID );

	DECLARE_MESSAGE_MAP()

}; // class CDataPage


#endif // _DATAPAGE_H_INCLUDED_