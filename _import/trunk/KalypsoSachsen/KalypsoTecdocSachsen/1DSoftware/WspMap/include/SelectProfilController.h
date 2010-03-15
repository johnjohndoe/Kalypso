// SelectProfilController.h: Schnittstelle für die Klasse CSelectProfilController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_SELECTPROFILCONTROLLER_H__5E6DDD53_21BE_11D8_B497_00104BB3E525__INCLUDED_)
#define AFX_SELECTPROFILCONTROLLER_H__5E6DDD53_21BE_11D8_B497_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "../mapobjects/mapobjects2.h"

#include "imapcontroller.h"

class CMapView;

class CSelectProfilController : public IMapController
{
public:
  CSelectProfilController( CMapView* view ) : IMapController( view )
  {
    m_profilID = -1;
  };

  virtual ~CSelectProfilController() {};

  virtual long MousePointer() { return moCross; };
  virtual bool IsEnabled();
  
  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapDblClick();

private:
   long m_profilID;
};

#endif // !defined(AFX_SELECTPROFILCONTROLLER_H__5E6DDD53_21BE_11D8_B497_00104BB3E525__INCLUDED_)
