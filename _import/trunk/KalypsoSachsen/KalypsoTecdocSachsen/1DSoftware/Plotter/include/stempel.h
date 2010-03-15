// stempel.h: Schnittstelle f�r die Klasse CStempel.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_STEMPEL_H__EDA71130_9571_11D6_B2FC_00104BB3E525__INCLUDED_)
#define AFX_STEMPEL_H__EDA71130_9571_11D6_B2FC_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "drawobj.h"
#include "plotdoc.h"

//////////////////////////////////////////////////////////////////////////////////
// Diese Klasse beinhaltet alle Objekte der Zeichnung, welche zum Stempel geh�ren
//////////////////////////////////////////////////////////////////////////////////
class CStempel : public CDrawObjList
{
  //////////////////////////////////////////////////////////////////////
  // Konstruktion/Destruktion
  //////////////////////////////////////////////////////////////////////
public:
  CStempel();
  
  //////////////////////////////////////////////////////////////////////
  // Operationen
  //////////////////////////////////////////////////////////////////////
public:
  void AddObject( CDrawObj* pObj );
  BOOL RemoveObject( CDrawObj* pObj );
  
  CSize UpdateScale();
  void Update( const CIntPoint& basePoint, const double realXScale, const double realYScale );

  //////////////////////////////////////////////////////////////////////
  // Attribute
  //////////////////////////////////////////////////////////////////////
public:
  BOOL GetAlignToProfil() const { return m_bAlignToProfil; };
  void SetAlignToProfil( const BOOL bAlignToProfil ) { m_bAlignToProfil = bAlignToProfil; };

  BOOL GetHorizontal() const { return m_bHorizontal; };
  void SetHorizontal( const BOOL bHorizontal ) { m_bHorizontal = bHorizontal; };

  void SetFileName( const CString& name ) { m_fileName = name; };
  CString GetFileName() const { return m_fileName; };

  const CSize& GetMarginsAdjusted();
  const CSize& GetMargins() const { return m_margins; };
  void SetMargins( const CSize& margins ) { m_margins = margins; };

  // setzt und gibt den gew�nschten ZoomFaktor zur�ck
  // die �nderung tritt erst nach dem n�chsten Update in Kraft
  void SetZoomFaktor( const UINT zoomFaktor ) { m_zoomFaktor = zoomFaktor; };
  UINT GetZoomFaktor() const { return m_zoomFaktor; };

  // diese beiden Routinen sind nur f�r die Serialisierung gedacht und sollten sonst nicht aufgerufen werden
  // dadurch wird der gew�nschte Zoomfaktor nach der Serialisierung immer dem aktuellen
  void SetSerialZoomFaktor( const UINT zoomFaktor ) { m_zoomFaktor = m_aktZoomFaktor = zoomFaktor; }
  UINT GetSerialZoomFaktor() const { return m_aktZoomFaktor; };

private:
  CString m_fileName; // Name der Stempel-Datei

  UINT m_zoomFaktor; // um diesen Faktor wird der Stempel skaliert ( Werte in Prozent )
  UINT m_aktZoomFaktor; // um diesen Faktor ist der Stempel aktuell skaliert. Ist nur f�r interne Zwecke
                        // gedacht: stimmen m_zoomFaktor und m_aktZoomFaktor nicht �berein, so wird 
                        // beim n�chsten Update um den Unterschied skaliert

  BOOL m_bAlignToProfil; // soll der Stempel am h�ngen oder am Seitenrand?
  BOOL m_bHorizontal; // Stempel horizontal oder vertikal zum Profil anordnen?

  CSize m_margins;

  static const CSize nullSize; // f�r R�ckgabe aus GetMarginsAdjusted
  static const int X_SCALE_FAKTOR; // Spezialfaktor f�r Lippelaengsschnitt
};

#endif // !defined(AFX_STEMPEL_H__EDA71130_9571_11D6_B2FC_00104BB3E525__INCLUDED_)
