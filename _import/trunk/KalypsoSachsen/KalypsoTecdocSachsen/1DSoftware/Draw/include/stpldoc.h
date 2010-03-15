// StplDoc.h : interface of the CStempelDoc class
//
/////////////////////////////////////////////////////////////////////////////

#ifndef _STPLDOC_H
#define _STPLDOC_H

#include "drawdoc.h"

// CStempel serializable data
class CStempelDocData : public CObject
{
protected:
	DECLARE_SERIAL(CStempelDocData);
	CStempelDocData();

// Implementation
public:
	virtual ~CStempelDocData();
	virtual void Serialize(CArchive& ar);
#ifdef _DEBUG
	void AssertValid();
#endif

	// implementation data
protected:
	CString m_name;

	friend class CStempelDoc;
};

class CStempelDoc : public CDrawDoc
{
	DECLARE_DYNCREATE(CStempelDoc)

// Constructors
public:
	CStempelDoc();
	CStempelDoc(CString& name);

	void SetName(CString& name) { m_pSData->m_name = name; }
	void GetName(CString& name) { name = m_pSData->m_name; }

// Operations
public:
	virtual void UpdateDrawing(CDrawView* pView);

// Implementation
public:
	virtual ~CStempelDoc();
	virtual void Serialize(CArchive& ar);   // overridden for document i/o

  static CStempelDoc* LoadStempel( const CString& path );

	// implementation data
protected:
	CStempelDocData* m_pSData;

// Generated message map functions
public:
	//{{AFX_MSG(CStempelDoc)
	virtual BOOL OnNewDocument();
	virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);
	virtual BOOL OnSaveDocument(LPCTSTR lpszPathName);
	virtual void DeleteContents();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif	// _STPLDOC_H

/////////////////////////////////////////////////////////////////////////////
