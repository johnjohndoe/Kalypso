// imapcontroller.h: Schnittstelle für die Klasse IMapController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_IMAPCONTROLLER_H__5E6DDD51_21BE_11D8_B497_00104BB3E525__INCLUDED_)
#define AFX_IMAPCONTROLLER_H__5E6DDD51_21BE_11D8_B497_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "../mapobjects/mapobjects2.h"

#include "mapView.h"
#include "mapDoc.h"

/*!
 * @class
 *   IMapController 
 *
 *  Interface für alle MapController
 */
class IMapController  
{
public:
  IMapController( CMapView* view, const bool bChecked = false ) { m_view = view; m_bChecked = bChecked; };

  virtual bool IsEnabled() { return true; };
  virtual bool IsChecked() { return m_bChecked; };
  virtual void Toggle()
  { 
    m_bChecked = !m_bChecked; 
  };
  virtual long MousePointer() { return moPencil; };

  virtual void OnMapDblClick() {};
  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift ) {};
  virtual void OnMapMouseMove( CMoPoint& cursor, short Button, short Shift ) {};
  virtual void OnMapMouseUp( CMoPoint& cursor, short Button, short Shift ) {};
  virtual void Cancel() {};

protected:
  CMapView* m_view;
  bool m_bChecked;
};

typedef std::auto_ptr<IMapController> IMapController_ptr;

#endif // !defined(AFX_IMAPCONTROLLER_H__5E6DDD51_21BE_11D8_B497_00104BB3E525__INCLUDED_)
