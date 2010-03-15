// template.h - Einstellungen Klasse
//

#ifndef __TEMPLATE_H__
#define __TEMPLATE_H__

#define TMPL_PROFIL_POLYLINE	N_STPLTEXTS
#define TMPL_PROFIL_POLYGON		N_STPLTEXTS+1
#define TMPL_TABELLE_LINE		N_STPLTEXTS+2
#define TMPL_TABELLE_KEY1TEXT	N_STPLTEXTS+3
#define TMPL_TABELLE_KEY2TEXT	N_STPLTEXTS+4
#define TMPL_TABELLE_NORMTEXT	N_STPLTEXTS+5
#define TMPL_TABELLE_XCOORD		N_STPLTEXTS+6
#define TMPL_TABELLE_YCOORD		N_STPLTEXTS+7
#define TMPL_RAHMEN_LINE		N_STPLTEXTS+8
#define TMPL_RAHMEN_TEXT		N_STPLTEXTS+9
#define TMPL_TITEL				N_STPLTEXTS+10
#define TMPL_HEIGHT				N_STPLTEXTS+11
#define TMPL_CTITEL				N_STPLTEXTS+12
#define TMPL_LTITEL				N_STPLTEXTS+13
#define TMPL_COMMENT      N_STPLTEXTS + 14

class CDrawRect;
#include "plotdoc.h"

// CTemplate serializable data
class CTemplateData : public CObject
{
protected:
	DECLARE_SERIAL(CTemplateData);
	CTemplateData();

// Implementation
public:
	virtual ~CTemplateData();
	virtual void Serialize(CArchive& ar);
#ifdef _DEBUG
	void AssertValid();
#endif

	// implementation data
protected:
	CString m_name;
	CDrawRect* m_pCTitel;
	CDrawRect* m_pLTitel;

	friend class CTemplate;
	friend class CPlotterDoc;
};

/////////////////////////////////////////////////////////////////////////////
// CTemplate

class CTemplate : public CPlotterDoc
{
protected:
	DECLARE_DYNCREATE(CTemplate);
	CTemplate();

// Constructors
public:
	CTemplate( const CString& name );

	void SetName( const CString& name ) { m_pTData->m_name = name; }
	CString GetName() const { return m_pTData->m_name; }

	CDrawObj* GetTemplateObj( int tmpl_type, int dat_type );

// Operations
public:
	virtual void CreateDrawing( BOOL bLoaded = FALSE, BOOL bInserted = FALSE );

// Implementation
public:
	virtual ~CTemplate();
	virtual void Serialize( CArchive& ar );
#ifdef _DEBUG
	void AssertValid();
#endif

protected:
	void AddDataBlockObjectSet( int type );

	// implementation data
protected:
	CTemplateData *m_pTData;

	friend class PrefDialog;
	friend class CPlotterDoc;
	friend class CTemplateData;
};


#endif // __TEMPLATE_H__
