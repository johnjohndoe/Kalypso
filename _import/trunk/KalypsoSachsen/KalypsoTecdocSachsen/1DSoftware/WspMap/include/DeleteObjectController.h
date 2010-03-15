// DeleteObjectController.h: Schnittstelle für die Klasse CDeleteObjectController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_DELETEOBJECTCONTROLLER_H__FF5A23A1_2996_11D8_B49E_00104BB3E525__INCLUDED_)
#define AFX_DELETEOBJECTCONTROLLER_H__FF5A23A1_2996_11D8_B49E_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

/**
 * @class Mapcontroller für ID_OBJECT_DELETE
 *
 */
class CDeleteObjectController : public IMapController
{
public:
  CDeleteObjectController( CMapView * );
  virtual ~CDeleteObjectController() {};

  virtual bool IsEnabled();
  virtual long MousePointer() { return moCross; };

  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseMove( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseUp( CMoPoint& cursor, short Button, short Shift );

private:
  long m_featureID;
  CMoPoint m_selectPoint;
};

#endif // !defined(AFX_DELETEOBJECTCONTROLLER_H__FF5A23A1_2996_11D8_B49E_00104BB3E525__INCLUDED_)
