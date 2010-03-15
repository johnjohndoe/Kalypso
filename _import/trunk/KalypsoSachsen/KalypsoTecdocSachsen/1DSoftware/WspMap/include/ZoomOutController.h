// ZoomOutController.h: Schnittstelle für die Klasse CZoomOutController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_ZOOMOUTCONTROLLER_H__CE851747_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_)
#define AFX_ZOOMOUTCONTROLLER_H__CE851747_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

class CZoomOutController : public IMapController  
{
public:
	CZoomOutController( CMapView* view );
	virtual ~CZoomOutController();

  virtual void OnMapDblClick();
  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseUp( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseMove( CMoPoint& cursor, short Button, short Shift );

private:
  bool m_bHot;
  CMoPoint m_hotSpot;
};

#endif // !defined(AFX_ZOOMOUTCONTROLLER_H__CE851747_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_)
