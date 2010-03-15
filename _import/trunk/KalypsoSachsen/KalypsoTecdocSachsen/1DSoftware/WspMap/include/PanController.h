// PanController.h: Schnittstelle für die Klasse CPanController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_PANCONTROLLER_H__F9CF9642_2243_11D8_B498_00104BB3E525__INCLUDED_)
#define AFX_PANCONTROLLER_H__F9CF9642_2243_11D8_B498_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

class CMapView;

class CPanController : public IMapController  
{
public:
  CPanController( CMapView* view, const short Button, const short Shift );

  virtual ~CPanController() {};

public:
  virtual long MousePointer() { return moPan; };
  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseUp( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseMove( CMoPoint& cursor, short Button, short Shift );

private:
  const short m_button;
  const short m_shift;

  bool m_bHot;
  CMoPoint m_hotSpot;
};

#endif // !defined(AFX_PANCONTROLLER_H__F9CF9642_2243_11D8_B498_00104BB3E525__INCLUDED_)
