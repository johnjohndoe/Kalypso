// NewObjectController.h: Schnittstelle für die Klasse CNewObjectController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_NEWOBJECTCONTROLLER_H__F9CF9644_2243_11D8_B498_00104BB3E525__INCLUDED_)
#define AFX_NEWOBJECTCONTROLLER_H__F9CF9644_2243_11D8_B498_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

class CMapView;

class CNewObjectController : public IMapController  
{
public:
  CNewObjectController( CMapView* view ) : IMapController( view ) {};
  virtual ~CNewObjectController() {};

public:
  virtual bool IsEnabled();
  virtual long MousePointer() { return moPencil; };

  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
};

#endif // !defined(AFX_NEWOBJECTCONTROLLER_H__F9CF9644_2243_11D8_B498_00104BB3E525__INCLUDED_)
