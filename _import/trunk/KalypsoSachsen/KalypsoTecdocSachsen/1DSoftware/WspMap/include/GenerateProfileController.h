// GenerateProfileController.h: Schnittstelle für die Klasse CGenerateProfileController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_GENERATEPROFILECONTROLLER_H__4106CC40_2265_11D8_B498_00104BB3E525__INCLUDED_)
#define AFX_GENERATEPROFILECONTROLLER_H__4106CC40_2265_11D8_B498_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

class CMapView;

class CGenerateProfileController : public IMapController  
{
public:
  CGenerateProfileController( CMapView* view ) :IMapController( view ) {};
  virtual ~CGenerateProfileController() {};

  virtual void Toggle();

  virtual bool IsEnabled();
  virtual long MousePointer() { return moPencil; };
  virtual void Cancel();

  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
};

#endif // !defined(AFX_GENERATEPROFILECONTROLLER_H__4106CC40_2265_11D8_B498_00104BB3E525__INCLUDED_)
