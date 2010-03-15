// ShowCoordsController.h: Schnittstelle für die Klasse CShowCoordsController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_SHOWCOORDSCONTROLLER_H__CE851745_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_)
#define AFX_SHOWCOORDSCONTROLLER_H__CE851745_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

class CShowCoordsController : public IMapController  
{
public:
	CShowCoordsController( CMapView* view );
	virtual ~CShowCoordsController();

  virtual void OnMapMouseMove( CMoPoint& cursor, short Button, short Shift );
};

#endif // !defined(AFX_SHOWCOORDSCONTROLLER_H__CE851745_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_)
