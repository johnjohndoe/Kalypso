// MapDocListener.h: Schnittstelle für die Klasse IMapDocListener.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAPDOCLISTENER_H__C9E66043_5723_11D8_B4BF_00104BB3E525__INCLUDED_)
#define AFX_MAPDOCLISTENER_H__C9E66043_5723_11D8_B4BF_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


class CMapDocEvent;
class CLayer;

/**
 * Interface for Listeners for MapDocEvent's
 */
class IMapDocListener  
{
public:
  enum EventType
  {
    THEME_ADDED               = 0x0000001,    // Thema zur Karte hinzugefügt
    THEME_REMOVED             = 0x0000002,  // Thema aus Karte genommen
    THEME_DATA_CHANGED        = 0x0000004, // Attributwerte des Themas geändert
    THEME_PROPS_CHANGED       = 0x0000008, // Themeneigenschaften geändert
    THEME_GEOMETRY_CHANGED    = 0x0000010, // Geometry des Objekts verändert
    THEME_ORDER_CHANGED       = 0x0000020,
    EXTENT_CHANGED            = 0x0000040,
    ACTIVE_PROFILE_CHANGED    = 0x0000080,
    PROFILE_CURSOR_CHANGED    = 0x0000100,
    ACTIVE_THEME_CHANGED      = 0x0000200,
    SCALE_CHANGED             = 0x0000400,
    OVERVIEW_CHANGED          = 0x0000800
  };

public:
  virtual void MapDocChanged( const long type, CLayer* layer ) = 0;
};

#endif // !defined(AFX_MAPDOCLISTENER_H__C9E66043_5723_11D8_B4BF_00104BB3E525__INCLUDED_)
