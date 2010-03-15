// ClipObjectController.h: Schnittstelle für die Klasse CClipObjectController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_CLIPOBJECTCONTROLLER_H__A36E20E1_2AEB_11D8_B4A0_00104BB3E525__INCLUDED_)
#define AFX_CLIPOBJECTCONTROLLER_H__A36E20E1_2AEB_11D8_B4A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

class CShapeMover;

/**
 * Controller für ID_OBJECT_CLIP
 */
class CClipObjectController : public IMapController  
{
public:
  CClipObjectController( CMapView* view );
  virtual ~CClipObjectController();

  virtual bool IsEnabled();
  virtual long MousePointer() { return moPencil; };

  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseMove( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseUp( CMoPoint& cursor, short Button, short Shift );
  virtual void Cancel();

private:
  CShapeMover* m_shapeMover;
};

#endif // !defined(AFX_CLIPOBJECTCONTROLLER_H__A36E20E1_2AEB_11D8_B4A0_00104BB3E525__INCLUDED_)
