// cntritem.h : interface of the CEditorCntrItem class
//

class CEditorCntrItem : public CRichEditCntrItem
{
	DECLARE_SERIAL(CEditorCntrItem)

// Constructors
public:
	CEditorCntrItem(REOBJECT* preo = NULL, CEditorDoc* pContainer = NULL);
		// Note: pContainer is allowed to be NULL to enable IMPLEMENT_SERIALIZE.
		//  IMPLEMENT_SERIALIZE requires the class have a constructor with
		//  zero arguments.  Normally, OLE items are constructed with a
		//  non-NULL document pointer.

// Attributes
public:
	CEditorDoc* GetDocument()
		{ return (CEditorDoc*)COleClientItem::GetDocument(); }
	CEditorView* GetActiveView()
		{ return (CEditorView*)COleClientItem::GetActiveView(); }

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEditorCntrItem)
	public:
	protected:
	//}}AFX_VIRTUAL

// Implementation
public:
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
};

/////////////////////////////////////////////////////////////////////////////
