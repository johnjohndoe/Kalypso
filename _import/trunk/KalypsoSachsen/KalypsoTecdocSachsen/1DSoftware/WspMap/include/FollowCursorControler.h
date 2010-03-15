// FollowCursorControler.h: Schnittstelle für die Klasse CFollowCursorControler.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_FOLLOWCURSORCONTROLER_H__E8594914_5BA3_11D8_B4C5_00104BB3E525__INCLUDED_)
#define AFX_FOLLOWCURSORCONTROLER_H__E8594914_5BA3_11D8_B4C5_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

class CMapView;

class CFollowCursorControler : public IMapController  
{
public:
	CFollowCursorControler( CMapView* pView );
	virtual ~CFollowCursorControler();

  void OnMapMouseMove( CMoPoint& cursor, short Button, short Shift );
};

#endif // !defined(AFX_FOLLOWCURSORCONTROLER_H__E8594914_5BA3_11D8_B4C5_00104BB3E525__INCLUDED_)
