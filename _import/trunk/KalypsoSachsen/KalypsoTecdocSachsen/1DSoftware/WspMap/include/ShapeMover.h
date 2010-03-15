// ShapeMover.h: Schnittstelle für die Klasse CShapeMover.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_SHAPEMOVER_H__CE851741_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_)
#define AFX_SHAPEMOVER_H__CE851741_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "IMapChanger.h"

class CMapLayer;

class CShapeMover : public IMapChanger  
{
public:
	CShapeMover( CMoTrackingLayer trackingLayer, CMoSymbol& symbol, LPDISPATCH shape, const long profilID, CMapLayer* pointLayer, CMoPoint& cursor );
	virtual ~CShapeMover();

  CMoGeoEvent GetShape();
  long GetProfilID() { return m_profilID; };
  CMapLayer* GetPointLayer() { return m_pointLayer; };

protected:
  virtual void MoveToInternal( CMoPoint& cursor );

private:
  long m_profilID;
  CMapLayer* m_pointLayer;
};

#endif // !defined(AFX_SHAPEMOVER_H__CE851741_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_)
