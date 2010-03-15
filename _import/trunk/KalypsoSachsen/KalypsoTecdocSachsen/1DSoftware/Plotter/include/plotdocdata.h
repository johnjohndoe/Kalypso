#ifndef _PLOT_DOC_DATA_H_INCLUDED_
#define _PLOT_DOC_DATA_H_INCLUDED_

#include "profil.h"
#include "title.h"
#include "table.h"
#include "tableKey1.h"
#include "tableKey2.h"
#include "stempel.h"
#include "drawobj.h"
#include "plotdoc.h"

class State;
class Section;

// CPlotterDoc serializable data
class CPlotterDocData : public CObject
{
	friend class CPlotterDoc;
	friend class CTemplate;

protected:
	DECLARE_SERIAL(CPlotterDocData);
	CPlotterDocData();
	void DeleteContents();

// Implementation
public:
	virtual void Serialize(CArchive& ar);

#ifdef _DEBUG
	void AssertValid();
#endif

	// implementation data
protected:
	CDrawObjList m_user;		// all user objects (always visible)
	CStempel m_stempel;		// all objects in Stempelbereich
	CProfil m_profil;		// all objects in Profilbereich which describe data
	
	CDrawObjList m_rahmen;		// all Rahmen objects


  CTableKey1 m_tableKey1;// an array of lists. Each contained list is a row.
  CTableKey2 m_tableKey2;// an array of lists. Each contained list is a row.
  CTable m_table;// an array of lists. Each contained list is a row.

  CDrawRect* m_comment;
	CDrawRect* m_height;		// Height object
  CTitle m_title;
    
private:
	CSize m_sizePage;			// size of page (in logical units)

  CIntIRect m_rectBorderGaps;  // Abstände zum Rand

  // size of table which is to left or right of profile ( x ist Breite der beiden Key-Spalten, 
  // y ist die Summen der beiden Teile der Tabelle, die rechts und links über das Profil hinausragen )
	CSize m_sizeTableMargins;	 // x = Breite Key1 + Breite Key2; y = zusätzliche Breite der Tabelle gegenüber Profil
	CDoublePoint m_scale, m_realScale;
  CTypedPtrArray<CObArray, State*> m_States;
  CTypedPtrArray<CObArray, Section*> m_Sections;

public:
	CString m_eigenschaften;
	CString m_heightFormatText;	// Text used to format height text
  CString m_commentFormatText; // Text used to format comment text

  // Abstaende in 1/10mm
};

#endif // _PLOT_DOC_DATA_H_INCLUDED_