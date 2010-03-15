// GroupController.h: Schnittstelle für die Klasse CGroupController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_GROUPCONTROLLER_H__A4ACEAD5_2A43_11D8_B49F_00104BB3E525__INCLUDED_)
#define AFX_GROUPCONTROLLER_H__A4ACEAD5_2A43_11D8_B49F_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imultimapcontroller.h"

class CGroupController : public IMultiMapController  
{
public:
  CGroupController( CMapView* view );
  virtual ~CGroupController();

  virtual void Toggle( const long id );
  //virtual bool IsChecked( const long id ) { return id == m_activeID; };

  virtual void OnMapDblClick();
  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseMove( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseUp( CMoPoint& cursor, short Button, short Shift );
  virtual void Cancel();

private:
  long m_activeID;
  IMapController* m_activeController;
};

#endif // !defined(AFX_GROUPCONTROLLER_H__A4ACEAD5_2A43_11D8_B49F_00104BB3E525__INCLUDED_)
