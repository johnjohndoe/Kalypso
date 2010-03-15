// NilController.h: Schnittstelle für die Klasse CNilController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_NILCONTROLLER_H__5E6DDD56_21BE_11D8_B497_00104BB3E525__INCLUDED_)
#define AFX_NILCONTROLLER_H__5E6DDD56_21BE_11D8_B497_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

class CMapView;

class CNilController : public IMapController  
{
public:
  CNilController( CMapView* view );
  virtual ~CNilController();

  virtual long MousePointer() { return moArrow; };

  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
};

#endif // !defined(AFX_NILCONTROLLER_H__5E6DDD56_21BE_11D8_B497_00104BB3E525__INCLUDED_)
