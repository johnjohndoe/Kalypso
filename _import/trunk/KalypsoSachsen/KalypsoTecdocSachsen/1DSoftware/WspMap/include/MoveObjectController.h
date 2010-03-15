// MoveObjectController.h: Schnittstelle für die Klasse CMoveObjectController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MOVEOBJECTCONTROLLER_H__F9CF9645_2243_11D8_B498_00104BB3E525__INCLUDED_)
#define AFX_MOVEOBJECTCONTROLLER_H__F9CF9645_2243_11D8_B498_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

class CShapeMover;

// entspricht ID_OBJECT_MOVE
class CMoveObjectController : public IMapController  
{
public:
  CMoveObjectController( CMapView* view );
  virtual ~CMoveObjectController();

  virtual bool IsEnabled();
  virtual long MousePointer() { return moSizePointer; };
  virtual void Cancel();

  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseMove( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseUp( CMoPoint& cursor, short Button, short Shift );

private:
  long m_lFeatureID;

private:
  CShapeMover* m_shapeMover;
};

#endif // !defined(AFX_MOVEOBJECTCONTROLLER_H__F9CF9645_2243_11D8_B498_00104BB3E525__INCLUDED_)
