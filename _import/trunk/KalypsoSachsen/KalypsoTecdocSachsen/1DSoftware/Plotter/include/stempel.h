// stempel.h: Schnittstelle für die Klasse CStempel.
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
// Diese Klasse beinhaltet alle Objekte der Zeichnung, welche zum Stempel gehören
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

  // setzt und gibt den gewünschten ZoomFaktor zurück
  // die Änderung tritt erst nach dem nächsten Update in Kraft
  void SetZoomFaktor( const UINT zoomFaktor ) { m_zoomFaktor = zoomFaktor; };
  UINT GetZoomFaktor() const { return m_zoomFaktor; };

  // diese beiden Routinen sind nur für die Serialisierung gedacht und sollten sonst nicht aufgerufen werden
  // dadurch wird der gewünschte Zoomfaktor nach der Serialisierung immer dem aktuellen
  void SetSerialZoomFaktor( const UINT zoomFaktor ) { m_zoomFaktor = m_aktZoomFaktor = zoomFaktor; }
  UINT GetSerialZoomFaktor() const { return m_aktZoomFaktor; };

private:
  CString m_fileName; // Name der Stempel-Datei

  UINT m_zoomFaktor; // um diesen Faktor wird der Stempel skaliert ( Werte in Prozent )
  UINT m_aktZoomFaktor; // um diesen Faktor ist der Stempel aktuell skaliert. Ist nur für interne Zwecke
                        // gedacht: stimmen m_zoomFaktor und m_aktZoomFaktor nicht überein, so wird 
                        // beim nächsten Update um den Unterschied skaliert

  BOOL m_bAlignToProfil; // soll der Stempel am hängen oder am Seitenrand?
  BOOL m_bHorizontal; // Stempel horizontal oder vertikal zum Profil anordnen?

  CSize m_margins;

  static const CSize nullSize; // für Rückgabe aus GetMarginsAdjusted
  static const int X_SCALE_FAKTOR; // Spezialfaktor für Lippelaengsschnitt
};

#endif // !defined(AFX_STEMPEL_H__EDA71130_9571_11D6_B2FC_00104BB3E525__INCLUDED_)
