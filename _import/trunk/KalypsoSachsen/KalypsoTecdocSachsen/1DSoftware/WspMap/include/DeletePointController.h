// DeletePointController.h: Schnittstelle für die Klasse CDeletePointController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_DELETEPOINTCONTROLLER_H__CE851743_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_)
#define AFX_DELETEPOINTCONTROLLER_H__CE851743_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

class CDeletePointController : public IMapController  
{
public:
	CDeletePointController( CMapView* view );
	virtual ~CDeletePointController();

  virtual bool IsEnabled();
  virtual long MousePointer() { return moCross; };

  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseUp( CMoPoint& cursor, short Button, short Shift );

private:
  long m_lFeatureID;
  DWORD m_dwPointData;
};

#endif // !defined(AFX_DELETEPOINTCONTROLLER_H__CE851743_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_)
