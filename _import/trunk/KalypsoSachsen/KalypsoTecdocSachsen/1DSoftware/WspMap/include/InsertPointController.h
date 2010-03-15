// InsertPointController.h: Schnittstelle für die Klasse CInsertPointController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_INSERTPOINTCONTROLLER_H__CE851744_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_)
#define AFX_INSERTPOINTCONTROLLER_H__CE851744_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"
#include "imapchanger.h"

class CMapLayer;

class CInsertPointController : public IMapController  
{
public:
	CInsertPointController( CMapView* view );
	virtual ~CInsertPointController();

  virtual bool IsEnabled();
  virtual long MousePointer() { return moPencil; };

  virtual void OnMapDblClick() {};
  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseMove( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseUp( CMoPoint& cursor, short Button, short Shift );
  virtual void Cancel();

private:
  void reset()
  {
    delete m_mapMover;
    m_mapMover = 0;

    m_lFeatureID = -1;
    m_dwPointData = 0;

    m_mapLayer = 0;
  }

  long m_lFeatureID;
  DWORD m_dwPointData;
  IMapChanger* m_mapMover;
  CMapLayer* m_mapLayer;
};

#endif // !defined(AFX_INSERTPOINTCONTROLLER_H__CE851744_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_)
