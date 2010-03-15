// FollowProfileControler.h: Schnittstelle für die Klasse CFollowProfileControler.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_FOLLOWPROFILECONTROLER_H__CE851746_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_)
#define AFX_FOLLOWPROFILECONTROLER_H__CE851746_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

class CFollowProfileControler : public IMapController  
{
public:
	CFollowProfileControler( CMapView* view );
	virtual ~CFollowProfileControler();

  void OnMapMouseMove( CMoPoint& cursor, short Button, short Shift );
};

#endif // !defined(AFX_FOLLOWPROFILECONTROLER_H__CE851746_2AF5_11D8_B4A0_00104BB3E525__INCLUDED_)
