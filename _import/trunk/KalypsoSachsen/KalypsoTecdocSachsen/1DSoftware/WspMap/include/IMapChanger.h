// IMapChanger.h: Schnittstelle für die Klasse IMapChanger.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_IMAPCHANGER_H__B7CC05B1_2F02_11D8_B4A3_00104BB3E525__INCLUDED_)
#define AFX_IMAPCHANGER_H__B7CC05B1_2F02_11D8_B4A3_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "maphelper.h"

class CMapLayer;
class CMoTrackingLayer;

/**
 *  Abstrakte Klasse zum interaktiven verändern von Shapes in der Karte
 *
 */
class IMapChanger  
{
public:
	IMapChanger( CMoTrackingLayer trackingLayer, CMoPoint& cursor )
  {
    m_trackingLayer = trackingLayer;

    MO2CREATE( m_lastCursor, "Point" );

    m_lastCursor.SetX( cursor.GetX() );
    m_lastCursor.SetY( cursor.GetY() );
  };

	virtual ~IMapChanger()
  {
  };

  /** Bewegt den aktiven event an dieser Stelle */
  void MoveTo( CMoPoint& cursor )
  {
    MoveToInternal( cursor );

    m_lastCursor.SetX( cursor.GetX() );
    m_lastCursor.SetY( cursor.GetY() );
  }

protected:
  CMoTrackingLayer& GetTrackingLayer() { return m_trackingLayer; };
  CMoPoint& GetLastCursor() { return m_lastCursor; };

  virtual void MoveToInternal( CMoPoint& cursor ) = 0;

private:
  CMoPoint m_lastCursor;
  CMoTrackingLayer m_trackingLayer;
};

#endif // !defined(AFX_IMAPCHANGER_H__B7CC05B1_2F02_11D8_B4A3_00104BB3E525__INCLUDED_)
