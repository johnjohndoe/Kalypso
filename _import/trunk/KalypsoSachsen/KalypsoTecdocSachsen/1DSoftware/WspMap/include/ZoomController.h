// ZoomController.h: Schnittstelle für die Klasse CZoomController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_ZOOMCONTROLLER_H__F9CF9643_2243_11D8_B498_00104BB3E525__INCLUDED_)
#define AFX_ZOOMCONTROLLER_H__F9CF9643_2243_11D8_B498_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

class CZoomController : public IMapController  
{
public:
  CZoomController( CMapView* view ) : IMapController( view ) {};
  virtual ~CZoomController() {};

  virtual long MousePointer() { return moZoomIn; };

  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
};

#endif // !defined(AFX_ZOOMCONTROLLER_H__F9CF9643_2243_11D8_B498_00104BB3E525__INCLUDED_)
