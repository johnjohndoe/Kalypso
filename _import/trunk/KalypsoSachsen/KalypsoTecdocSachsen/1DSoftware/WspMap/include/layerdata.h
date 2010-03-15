#ifndef _LAYERDATA_H_INCLUDED_
#define _LAYERDATA_H_INCLUDED_

#include "layer.h"   // für CLayer::LayerType

class CLayerData : public CObject
{
  // Deklarationen
  DECLARE_SERIAL( CLayerData );

  // Konstruction / Destruction
public:
  CLayerData();
  ~CLayerData();

  // öffentlich Attribute
public:
  // Standardeinstellungen für das erstellen eines Layers
  CString layerName;  // Rumpf des Standard-Layernamens
  CString fileName; // Rumpf des Standard-Dateinamens ( der shape-dateien )
  BOOL bRandomColor;
  COLORREF color; // Farbe des Symbols
  int datablock; // Nummer eines evtl. zugeordneten Datenblocks

  int nProfilBezug;  // -1 kein Profilbezug; >= 0: Profilbezogen, maximale Anzahl möglicher Punkte pro Profil ( 0 = unendlich )

  BOOL bObjectGeometryEditable; // die Gemoetrie der GeoObjekte ist editierbar

  BOOL bCutToProfileLines;       // läßt sich mit Profillinien verschneiden
  BOOL bLiesOnPoint; // dieses ( Punkte-Thema ) muss immer mit einem Profilpunkt zusammenfallen

  BOOL bUmrandungZeichnen;     // von diesem Layer kann eine Umrandung gezeichnet werden

  BOOL bKopierbar; // darf das Thema kopiert werden?
  BOOL bKopiereTag; // soll das Tag mitkopiert werden?
  CLayer::LayerType kopierterTyp;

  BOOL bTinCut; // dieser Layer kann mit einem HMO verschnitten werden

  BOOL bAdaptHeight; // ob das Attribut 'Höhe' ( = MO2_FIELD_HEIGHT ) stets an die Geländehöhe angepasst wird

  // Operationen
public:
  void Serialize( CArchive &ar );

};
/////////////////////////////////////////////////////////


#endif // _LAYERDATA_H_INCLUDED_