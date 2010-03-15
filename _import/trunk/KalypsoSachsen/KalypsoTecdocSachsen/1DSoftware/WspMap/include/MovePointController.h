// MovePointController.h: Schnittstelle für die Klasse CMovePointController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MOVEPOINTCONTROLLER_H__CE851742_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_)
#define AFX_MOVEPOINTCONTROLLER_H__CE851742_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"
#include "imapchanger.h"

class CMapLayer;

class CMovePointController : public IMapController  
{
public:
	CMovePointController( CMapView* view );
	virtual ~CMovePointController();

  virtual bool IsEnabled();
  virtual long MousePointer() { return moSizePointer; };

  virtual void OnMapDblClick() {};
  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseMove( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseUp( CMoPoint& cursor, short Button, short Shift );
  virtual void Cancel();

private:
  void reset()
  {
    m_lFeatureID = -1;

    delete m_mapMover;
    m_mapMover = 0;
    
    m_mapLayer = 0;
    m_dwPointData = 0;
  }

  long m_lFeatureID;
  IMapChanger* m_mapMover;
  CMapLayer* m_mapLayer;
  DWORD m_dwPointData;
};

#endif // !defined(AFX_MOVEPOINTCONTROLLER_H__CE851742_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_)
