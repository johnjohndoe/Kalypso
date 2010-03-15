// svritem.h : interface of the CEmbeddedItem class
//

#ifndef _SVRITEM_H
#define _SVRITEM_H

class CEmbeddedItem : public COleServerItem
{
	DECLARE_DYNAMIC(CEmbeddedItem)

// Constructors
public:
	CEmbeddedItem(CDrawDoc* pContainerDoc);

// Attributes
	CDrawDoc* GetDocument() const
		{ return (CDrawDoc*) COleServerItem::GetDocument(); }
	CDrawView* GetView() const;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEmbeddedItem)
	public:
	virtual BOOL OnDraw(CDC* pDC, CSize& rSize);
	virtual BOOL OnGetExtent(DVASPECT dwDrawAspect, CSize& rSize);
	//}}AFX_VIRTUAL

protected:
	virtual void Serialize(CArchive& ar);   // overridden for document i/o
	virtual COleDataSource* OnGetClipboardData(BOOL bIncludeLink,
		LPPOINT pptOffset, LPSIZE pSize);
	// GetNativeClipboardData called by overrided OnGetClipboardData
	void GetNativeClipboardData(COleDataSource *pDataSource);
};

#endif

/////////////////////////////////////////////////////////////////////////////
