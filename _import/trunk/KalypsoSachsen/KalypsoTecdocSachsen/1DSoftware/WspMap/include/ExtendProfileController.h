// ExtendProfileController.h: Schnittstelle für die Klasse CExtendProfileController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_EXTENDPROFILECONTROLLER_H__F9CF9640_2243_11D8_B498_00104BB3E525__INCLUDED_)
#define AFX_EXTENDPROFILECONTROLLER_H__F9CF9640_2243_11D8_B498_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

#include "../mapobjects/mapObjects2.h"

/*!
 * @class
 *   CExtendProfileController 
 *
 *  Enspricht der ID_TOOLS_EXTEND_PROFILE
 *
 *
 *  Zum Verlängern von Querprofilen anhand von Geländemodellen
 */
class CExtendProfileController : public IMapController  
{
public:
  CExtendProfileController( CMapView* view ) : IMapController( view )
  {
    m_profilID = -1;
    m_bHot = FALSE;
    MO2CREATE( m_hotSpot, "Point" );
  };

  virtual ~CExtendProfileController() {};

public:
  virtual bool IsEnabled();
  virtual long MousePointer() { return moPencil; };
  virtual void Cancel();

  virtual void OnMapDblClick();
  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseMove( CMoPoint& cursor, short Button, short Shift );
  virtual void OnMapMouseUp( CMoPoint& cursor, short Button, short Shift );

private:
  void StartTracking( CMoTrackingLayer& trackLayer, CMoPoint& cursor );
  void InsertPoint( CMoTrackingLayer& trackLayer, CMoPoint& cursor, CMoGeoEvent& event );

private:
  long m_profilID;
  double m_x;
  double m_y;

  BOOL m_bHot;
  CMoPoint m_hotSpot;
};

#endif // !defined(AFX_EXTENDPROFILECONTROLLER_H__F9CF9640_2243_11D8_B498_00104BB3E525__INCLUDED_)
