////////////////////////////////////////////////////////

#ifndef _MAPDOCDATA_H_INCLUDED_
#define _MAPDOCDATA_H_INCLUDED_

#pragma warning(disable:4786)
#pragma warning(disable:4503)

class CMapLayer;

#include "layer.h"


class CLayerArray : public CTypedPtrArray<CObArray, CLayer*>
{
public:
	long ProjectToProfilLineByDistance( CMoPoint &point, const double distance );
  long ProjectToProfilPointByDistance( CMoPoint& point, const double distance );
  double ProjectToProfilLine( CMoPoint& point, const long profilID );
	
  LPDISPATCH GetProfilShape( const long featureID );
	CString GetProfilState( const long featureID );
	CString GetProfilFile( const long featureID );
	double GetProfilStation( const long featureID );
	long GetProfil( CMoPoint& point );
	
  int GetLayerIndex( CLayer* layer );
	CMoRectangle GetExtent();
	CLayerArray();

	virtual void Serialize( CArchive& ar );
	void MatchLayerOrder( CMoMap& map );
  

  // Finding Layers

  CLayer* FindLayer( LPDISPATCH dispatch, POSITION* pos = NULL );
  CMapLayer* FindFirstLayer( const int type, POSITION* pos = NULL );
  CMapLayer* FindNextLayer( int type, POSITION* pos );
  CMapLayer* FindFirstLayerByTagAndType( const CString& tag, const CLayer::LayerType type, POSITION* pos = NULL );
	CLayer* FindFirstLayerByName( const CString& name, POSITION* pos = NULL );
	CLayer* FindFirstLayerByTag( const CString& tag, POSITION* pos = NULL );

};

#endif // _MAPDOCDATA_H_INCLUDED_