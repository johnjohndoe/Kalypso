// MarkMapObject.cpp: Implementierung der Klasse CMarkMapObject.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "mapLayer.h"

#include "MarkMapObject.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

/*!
 * Markiert ein GeoObjekt in der Farbe eines bestimmten Themas
 *
 * @param object : Das zu markierende GeoObjekt
 * @param type : Das Symbol dieses Typs wird benutzt
 */
CMarkMapObject::CMarkMapObject( LPDISPATCH dispObject, CMapLayer* mapLayer, CMoRectangle& extent, CMoTrackingLayer trackLayer, const int symbolID )
{
  if( !LPDISPATCH(trackLayer) )
    return;

  m_trackLayer = trackLayer;

  // jetzt die Attribute übertragen, die Farbe invertieren
  CMoSymbol symbol( mapLayer->GetSymbol() );
  CMoSymbol trackSymbol( trackLayer.GetSymbol( symbolID ) );
  trackSymbol.SetSize( symbol.GetSize() * 2 + 1 ); // mindestens Doppelt so dick
  trackSymbol.SetColor( symbol.GetColor() );
  trackSymbol.SetSymbolType( symbol.GetSymbolType() );
  
  switch( trackSymbol.GetSymbolType() )
  {
  case moLineSymbol:
    trackSymbol.SetStyle( moDashLine ); 
    break;
  };
  
  m_pMark = new CMoGeoEvent( trackLayer.AddEvent( dispObject, symbolID ) );
  trackLayer.Refresh( TRUE, CComVariant( extent ) );
}; // MarkObject

CMarkMapObject::~CMarkMapObject()
{
  // den TrackLayer wieder löschen
  if( LPDISPATCH(*m_pMark) )
    m_trackLayer.RemoveEvent( m_pMark->GetIndex() );

  delete m_pMark;
  m_pMark = 0;
} // Destruktor
