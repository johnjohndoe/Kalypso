// MultiMapController.h: Schnittstelle für die Klasse CMultiMapController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MULTIMAPCONTROLLER_H__A4ACEAD2_2A43_11D8_B49F_00104BB3E525__INCLUDED_)
#define AFX_MULTIMAPCONTROLLER_H__A4ACEAD2_2A43_11D8_B49F_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imultimapcontroller.h"

/**
 * Die einfachste implementation des IMultiMapController's
 * Es werden alle Controller gleich behandelt.
 * Jeder Controller kann aktiv oder inaktiv sein.
 * Jeder event wird an alle aktiven Controller weitergeleitet.
 */
class CMultiMapController : public IMultiMapController
{
public:
  CMultiMapController( CMapView* view ) : IMultiMapController( view ) {};
  virtual ~CMultiMapController() {};

  virtual void Toggle( const long id );

  virtual void OnMapDblClick();
  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseMove( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseUp( CMoPoint& cursor, short Button, short Shift );
  virtual void Cancel();
};

#endif // !defined(AFX_MULTIMAPCONTROLLER_H__A4ACEAD2_2A43_11D8_B49F_00104BB3E525__INCLUDED_)
