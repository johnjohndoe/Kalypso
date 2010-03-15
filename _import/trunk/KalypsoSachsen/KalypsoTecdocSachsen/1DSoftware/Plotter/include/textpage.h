// TextPage.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CTextPage dialog

class CTextPage : public CPropertyPage
{
// Construction
public:
	CTextPage( CPropertyDialog *pParent, CPlotterDoc* pDoc = NULL, BOOL bTemplate = FALSE );
	~CTextPage();

// Dialog Data
	//{{AFX_DATA(CTextPage)
	enum { IDD = IDD_TEXT };
	CSpinButtonCtrl	m_spin2;
	CSpinButtonCtrl	m_spin;
	CTreeCtrl	m_tree;
	int		m_orientation;
	int		m_horz;
	int		m_vert;
	BOOL	m_nonegative;
	int		m_precision;
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CTextPage)
	public:
	virtual void OnOK();
	virtual BOOL OnApply();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
public:
	void UpdateTree();
	void ApplyTemplate(CTemplate *pTemp);

protected:
	CPropertyDialog *m_pParent;
	CPlotterDoc *m_pDoc;
	CPlotterView *m_pView;
	CTemplate *m_pTemp;		// used for applying templates
	BOOL m_bTemplate;
	typedef struct
	{
		COLORREF color;
		int horz;
		int vert;
		int type;
		BOOL bShowNeg;
		int precision;
	} DATA;
	CTypedPtrMap<CMapPtrToPtr, HANDLE, LPLOGFONT> m_logfonts;
	CTypedPtrMap<CMapPtrToPtr, HANDLE, DATA*> m_data;
	CTypedPtrMap<CMapPtrToPtr, HANDLE, CDrawObj*> m_objects;
	CTypedPtrMap<CMapPtrToPtr, HANDLE, CDrawObj*> m_edits;
	CTypedPtrMap<CMapPtrToWord, HANDLE, BOOL> m_updated;
	CTypedPtrList<CPtrList, HANDLE> m_items;
	HTREEITEM m_hTITable; // die root für Elemente der Table
  HTREEITEM m_hTIStamp; // die root für Elemente des Stempels

    enum
	{
		angle = 0x0001,
		horz = 0x0002,
		vert = 0x0004,
    schrift = 0x0008,
		noneg = 0x0010,
		precision = 0x0020
	};

	void UpdateItemAndChildren(HTREEITEM hTI, int format, LPLOGFONT lpLF=NULL, COLORREF color=0);
	void UpdateItemAndParent(HTREEITEM hTI, int format, LPLOGFONT lpLF=NULL, COLORREF color=0);
	BOOL FontIsTrueType(LPLOGFONT lpLogFont);
	// Generated message map functions
	//{{AFX_MSG(CTextPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnBeginlabeleditTree1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnEndlabeleditTree1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnShrift();
	afx_msg void OnDeltaposSpin1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnSelchangedTree1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnSelchangeHoriz();
	afx_msg void OnSelchangeVert();
	afx_msg void OnChangeAngle();
	afx_msg void OnKillfocusAngle();
	afx_msg void OnNoNegative();
	afx_msg void OnChangePrecision();
	afx_msg void OnKillfocusPrecision();
	afx_msg void OnDeltaposSpin2(NMHDR* pNMHDR, LRESULT* pResult);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	friend class PropertyDialog;
};
