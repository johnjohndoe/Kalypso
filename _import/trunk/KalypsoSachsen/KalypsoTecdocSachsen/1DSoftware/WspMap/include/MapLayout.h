// MapLayout.h: Schnittstelle f�r die Klasse CMapLayout.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAPLAYOUT_H__96875E81_408F_11D6_B2BC_00104BB3E525__INCLUDED_)
#define AFX_MAPLAYOUT_H__96875E81_408F_11D6_B2BC_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CPrinterSettings;

#include "printRect.h"


////////////////////////////////////////////////////////////////////////////////////
// Klasse CMapLayoutListener
// Ein Interface f�r Klassen, die �ber �nderungen im MapLayout informiert werden wollen
// ( Publisher - Subscriber Pattern )
////////////////////////////////////////////////////////////////////////////////////
class CMapLayoutListener
{
public:
  // wird aufgerufen, falls sich das Kartenlayout �ndert:
  // Parameter:
  //          const CRect& rect: der Ausschnitt der sich ge�ndert hat
  //          const BOOL bPages: TRUE; falls sich die Anzahl der Seiten ( m�glicherweise ) 
  //                                          ge�ndert hat
  virtual void Update( const CRect& rect, const BOOL bPages ) = 0;
}; // class CMapLayoutListener


////////////////////////////////////////////////////////////////////////////////////
// Klasse CMapLayout
// Diese Klasse repr�sentiert die ( serialisierbaren ) Daten der Druckvorschau.
// D.h. im wesentlichen die Gesamtheit der Druckrechtecke und die Seiten selbst
////////////////////////////////////////////////////////////////////////////////////
class CMapLayout : public CObject, public CPrintRectListener
{
  ///////////
  // Typen //
  ///////////

public:
  CMapLayout();
  virtual ~CMapLayout();
  
  // Deklarationen
  DECLARE_SERIAL( CMapLayout );
  
  /////////////////
  // Operationen //
  /////////////////
public:
  void DoPrinterSetup( LPCTSTR strTitle = NULL, CWnd* pWnd = NULL );
  void Paint( CDC* pDC );
  void Initialize();
  void CreatePrintRect( const CRect& rect, const UINT type, CDC* pDC );

  void DeleteRect(CPrintRect*);
  void ToFront(CPrintRect*);
  void ToBack(CPrintRect*);

  // Implementation des Interfaces CPrintRectListener
  void Update( const CRect& );

  // Implementation des MapLayout-Publishers
  void AddListener( CMapLayoutListener* pListener );
  void RemoveListener( CMapLayoutListener* pListener );
  void NotifyListeners( const CRect& rect, const BOOL bPages );

  //////////////////////////////////////////////////
  // '�berschreibungen' von CMapPreview - Members //
  //////////////////////////////////////////////////
public:
  BOOL OnPreparePrinting( CPrintInfo* pInfo, CView* pView, CMoMap* pMoMap );
  void OnBeginPrinting( CDC* pDC, CPrintInfo* pInfo );
  void OnPrepareDC(CDC* pDC, CPrintInfo* pInfo, const CSize& clientRect );
  
protected:
  void AddPrintRect( CPrintRect* newRect );

  void Serialize( CArchive& ar );
  void DeleteContents();

  BOOL CalcPageSizes(); // Seitenaufteilung ausrechnen
    
  ///////////////
  // Attribute //
  ///////////////
public:
  BOOL IsPointInPage( const CPoint& point ) const;
  CPrintRect* IsPointInRect( const CPoint& point );

  CSize GetSize() const { return m_printSize; };  // die auf ganze Seiten aufgerundete Gr�sse
  UINT GetPageCount() const { return m_pageCountX * m_pageCountY; }; // Gesamtzahl der Druckseiten
  
  UINT GetZoomFaktor() const { return m_nZoomFaktor; };
  void SetZoomFaktor( UINT zF ) { m_nZoomFaktor = zF; };

protected:
  CSize GetExtent() const; // die minimal erforderliche Gr�sse

private:
    // ein paar Members f�rs Drucken und die Druckvorschau
  UINT m_pageCountX, m_pageCountY; // Anzahl der Druckseiten in X und Y Richtung
  CSize m_printSize; // Gesamtgr�sse des Ausdrucks in Millimetern
  UINT m_nZoomFaktor; // der Zoomfaktor f�r die Druckvorschau

  CPrinterSettings* m_printerSettings; // die Druckereinstellungen

  CTypedPtrArray<CPtrArray, CPrintRect*> m_printRects;

  CMoMap* m_pMoMap; // Referenz auf die eigentliche Karte

  CTypedPtrArray<CPtrArray, CMapLayoutListener*> m_listeners;
};

#endif // !defined(AFX_MAPLAYOUT_H__96875E81_408F_11D6_B2BC_00104BB3E525__INCLUDED_)
