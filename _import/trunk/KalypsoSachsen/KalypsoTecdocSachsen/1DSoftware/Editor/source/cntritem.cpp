// cntritem.cpp : implementation of the CEditorCntrItem class
//

#include "stdafx.h"

#include "global.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CEditorCntrItem implementation

IMPLEMENT_SERIAL(CEditorCntrItem, CRichEditCntrItem, VERSIONABLE_SCHEMA|1)

CEditorCntrItem::CEditorCntrItem(REOBJECT *preo, CEditorDoc* pContainer)
	: CRichEditCntrItem(preo, pContainer)
{
}

/////////////////////////////////////////////////////////////////////////////
// CEditorCntrItem diagnostics

#ifdef _DEBUG
void CEditorCntrItem::AssertValid() const
{
	CRichEditCntrItem::AssertValid();
}

void CEditorCntrItem::Dump(CDumpContext& dc) const
{
	CRichEditCntrItem::Dump(dc);
}
#endif

/////////////////////////////////////////////////////////////////////////////
