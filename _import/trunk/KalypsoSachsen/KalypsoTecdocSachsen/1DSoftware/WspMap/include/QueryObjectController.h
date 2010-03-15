// QueryObjectController.h: Schnittstelle für die Klasse CQueryObjectController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_QUERYOBJECTCONTROLLER_H__4106CC42_2265_11D8_B498_00104BB3E525__INCLUDED_)
#define AFX_QUERYOBJECTCONTROLLER_H__4106CC42_2265_11D8_B498_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

class CMapView;

class CQueryObjectController : public IMapController  
{
public:
  CQueryObjectController( CMapView* view ) : IMapController( view ) {};
  virtual ~CQueryObjectController() {};

  virtual bool IsEnabled();
  virtual long MousePointer() { return moArrowQuestion; };

  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
};

#endif // !defined(AFX_QUERYOBJECTCONTROLLER_H__4106CC42_2265_11D8_B498_00104BB3E525__INCLUDED_)
