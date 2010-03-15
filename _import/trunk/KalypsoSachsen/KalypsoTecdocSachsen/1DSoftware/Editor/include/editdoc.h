// editdoc.h : interface of the CEditorDoc class
//

class CEditorDoc : public CRichEditDoc
{
protected: // create from serialization only
	CEditorDoc();
	DECLARE_DYNCREATE(CEditorDoc)

// Attributes
public:
	int m_nDocType;
	int m_nNewDocType;
	void SetDocType(int nDocType, BOOL bNoOptionChange = FALSE);
	CEditorView* GetView();
	CLSID GetClassID();
	LPCTSTR GetSection();
	short m_dmOrientation;
	short m_dmPaperSize;
	short m_dmPaperWidth;
	short m_dmPaperLength;

// Operations
public:
	virtual CFile* GetFile(LPCTSTR pszPathName, UINT nOpenFlags, 
		CFileException* pException);
	virtual BOOL DoSave(LPCTSTR pszPathName, BOOL bReplace = TRUE);
	int MapType(int nType);
	void ForceDelayed(CFrameWnd* pFrameWnd);

// Overrides
	virtual CRichEditCntrItem* CreateClientItem(REOBJECT* preo) const;
	virtual void OnDeactivateUI(BOOL bUndoable);
	virtual void Serialize(CArchive& ar);
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEditorDoc)
	public:
	virtual BOOL OnNewDocument();
	virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);
	virtual BOOL OnSaveDocument(LPCTSTR lpszPathName);
	virtual BOOL OnCmdMsg(UINT nID, int nCode, void* pExtra, AFX_CMDHANDLERINFO* pHandlerInfo);
	virtual void ReportSaveLoadException(LPCTSTR lpszPathName, CException* e, BOOL bSaving, UINT nIDPDefault);
	protected:
	//}}AFX_VIRTUAL

// Implementation
public:
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

// Generated message map functions
protected:
	//{{AFX_MSG(CEditorDoc)
	afx_msg void OnUpdateOleVerbPopup(CCmdUI* pCmdUI);
	afx_msg void OnFileSendMail();
	afx_msg void OnFormatConvert();
	afx_msg void OnUpdateFormatConvert(CCmdUI* pCmdUI);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	void OEMConvert(BOOL bOEMToANSI);
};

/////////////////////////////////////////////////////////////////////////////
