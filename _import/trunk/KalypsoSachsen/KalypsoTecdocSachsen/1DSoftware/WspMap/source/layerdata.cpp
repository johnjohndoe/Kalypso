// LayerData.cpp: Implementierung der Klasse CMapData.
//
//////////////////////////////////////////////////////////////////////
//
// für jeden LayerTyp spezifische Daten: z.B. Farben, Namen bei 
// Erstellung des Themas
// welche Operationen dürfen mit diesem Thema durchgeführt werden etc.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "layerdata.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

IMPLEMENT_SERIAL( CLayerData, CObject, VERSIONABLE_SCHEMA | 4 )

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CLayerData::CLayerData()
{
  // ein Standard-Thema darf praktisch nichts
  // CString layerName;  // Rumpf des Standard-Layernamens
  // CString fileName; // Rumpf des Standard-Dateinamens ( der shape-dateien )
  // BOOL bRandomColor;
  color = moBlack;
  datablock = -1;
  nProfilBezug = -1;
  bLiesOnPoint = FALSE;
  bObjectGeometryEditable = FALSE;
  bCutToProfileLines = FALSE;
  bUmrandungZeichnen = FALSE;
  bKopierbar = FALSE;
  bKopiereTag = FALSE;
  //CLayer::LayerType kopierterTyp;
  bTinCut = FALSE;
  bAdaptHeight = FALSE;
}

CLayerData::~CLayerData()
{
}


//////////////////////////////////////////////////////////////////////
// Operationen
//////////////////////////////////////////////////////////////////////

void CLayerData::Serialize( CArchive& ar )
{
  if ( ar.IsStoring() )
  {
    ar << layerName;
    ar << fileName;
    ar << bRandomColor;
    ar << color;
    ar << nProfilBezug;
    ar << bObjectGeometryEditable;
    ar << bCutToProfileLines;
    ar << bUmrandungZeichnen;
    ar << bKopierbar;
    ar << bKopiereTag;
    ar << kopierterTyp;
    ar << bTinCut;
    ar << datablock;
    ar << bLiesOnPoint;
    ar << bAdaptHeight;
  } // IsStoring
  else
  {
    UINT nVersion = ar.GetObjectSchema();
    ar >> layerName;
    ar >> fileName;
    ar >> bRandomColor;
    ar >> color;

    if( nVersion > 2 )
      ar >> nProfilBezug;
    else
    {
      BOOL bProfilbezogen;
      ar >> bProfilbezogen;
      if( bProfilbezogen )
        nProfilBezug = 0;
      else
        nProfilBezug = -1;
    }; // if nVersion > 2

    ar >> bObjectGeometryEditable;
    ar >> bCutToProfileLines;
    ar >> bUmrandungZeichnen;
    ar >> bKopierbar;
    ar >> bKopiereTag;

    int type;
    ar >> type;
    kopierterTyp = (CLayer::LayerType)type;

    ar >> bTinCut;

    if( nVersion > 1 )
      ar >> datablock;

    if( nVersion > 2 )
      ar >> bLiesOnPoint;
    else
      bLiesOnPoint = nProfilBezug != -1;

    if( nVersion > 3 )
      ar >> bAdaptHeight;

  }; // IsLoading
}; // Serialize