// svritem.cpp : implementation of the CServerItem class
//


#include "stdafx.h"

#include "drawdoc.h"
#include "drawvw.h"
#include "draw.h"

#include "svritem.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

IMPLEMENT_DYNAMIC(CEmbeddedItem, COleServerItem)

CEmbeddedItem::CEmbeddedItem(CDrawDoc* pContainerDoc)
	: COleServerItem(pContainerDoc, TRUE)
{
}

CDrawView* CEmbeddedItem::GetView() const
{
	CDocument* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
	POSITION pos = pDoc->GetFirstViewPosition();
	if (pos == NULL)
		return NULL;

	CDrawView* pView = (CDrawView*)pDoc->GetNextView(pos);
	ASSERT_VALID(pView);
	ASSERT_KINDOF(CDrawView, pView);
	return pView;
}

void CEmbeddedItem::Serialize(CArchive& ar)
{
	//  CEmbeddedItem::Serialize will be called by the framework if
	//  the item is copied to the clipboard.  This can happen automatically
	//  through the OLE callback OnGetClipboardData.  A good default for
	//  the embedded item is simply to delegate to the document's Serialize
	//  function.  If you support links, then you will want to serialize
	//  just a portion of the document.

	if (!IsLinkedItem())
	{
		CDrawDoc* pDoc = GetDocument();
		ASSERT_VALID(pDoc);
		pDoc->Serialize(ar);
	}
}

BOOL CEmbeddedItem::OnGetExtent(DVASPECT dwDrawAspect, CSize& rSize)
{
	if (dwDrawAspect != DVASPECT_CONTENT)
		return COleServerItem::OnGetExtent(dwDrawAspect, rSize);


	// CEmbeddedItem::OnGetExtent is called to get the extent in
	//  HIMETRIC units of the entire item.
	CDrawDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
	
	rSize = pDoc->GetDrawingSize();
	CClientDC dc(NULL);

	rSize.cx /= MM_FACTOR;	// now in mm
	rSize.cy /= MM_FACTOR;	// now in mm
	rSize.cx *= 100;		// now in HIMETRIC
	rSize.cy *= 100;		// now in HIMETRIC

	return TRUE;
}

BOOL CEmbeddedItem::OnDraw(CDC* pDC, CSize& rSize)
{
	// get view attached to the item
	CDrawView* pView = GetView();

	//  All drawing takes place in the metafile device context (pDC).
	// In some situations, OLE1 servers will ask for the presentation data
	//  during shutdown, even though it is not necessary (since the picture
	//  has not changed).  This will happen when closing a frame window
	//  for example.  By this time all the views are gone and there is no
	//  way to produce the metafile data, since the actual text is
	//  stored by the edit control (the view).  In this case, we simply
	//  fail the call.
	if (pView == NULL)
		return FALSE;

	CDrawDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);

	rSize = pDoc->GetDrawingSize();
	// prepare to draw and remember the extent in himetric units
	pDC->SetWindowOrg(0, 0);
	pDC->OffsetWindowOrg(0, rSize.cy);
	pDC->SetWindowExt(rSize.cx, -rSize.cy);
	pDC->SetViewportExt(rSize);  // Note: only affects the m_hAttribDC

	pDoc->Draw(pDC, pView);

	return TRUE;
}

// OnGetClipboardData is used by CopyToClipboard and DoDragDrop
COleDataSource* CEmbeddedItem::OnGetClipboardData(BOOL bIncludeLink,
	LPPOINT pptOffset, LPSIZE pSize)
{
	ASSERT_VALID(this);

	CDrawDoc* pDoc = GetDocument();
	if (pDoc == NULL)
		return NULL;

	COleDataSource* pDataSource = new COleDataSource;
	TRY
	{
		GetNativeClipboardData(pDataSource);
		GetClipboardData(pDataSource, bIncludeLink, pptOffset, pSize);
	}
	CATCH_ALL(e)
	{
		delete pDataSource;
		THROW_LAST();
	}
	END_CATCH_ALL

	ASSERT_VALID(pDataSource);
	return pDataSource;
}

void CEmbeddedItem::GetNativeClipboardData(COleDataSource *pDataSource)
{
	ASSERT_VALID(this);
	ASSERT(CDrawDoc::m_cfPrivate != NULL);

	// Create a shared file and associate a CArchive with it
	CSharedFile file;
	CArchive ar(&file,CArchive::store);

	// Serialize selected objects to the archive
	CDrawDoc* pDoc = GetDocument();
	pDoc->Serialize(ar);
	ar.Close();

	// put on local format instead of or in addation to
	pDataSource->CacheGlobalData(CDrawDoc::m_cfPrivate,file.Detach());
}

/////////////////////////////////////////////////////////////////////////////