// IMultiMapController.h: Schnittstelle für die Klasse IMultiMapController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_IMULTIMAPCONTROLLER_H__A4ACEAD6_2A43_11D8_B49F_00104BB3E525__INCLUDED_)
#define AFX_IMULTIMAPCONTROLLER_H__A4ACEAD6_2A43_11D8_B49F_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

/**
 * Diese abstrakte Klasse ist ein Controller, der wiederum aus einer Liste von Controller
 * besteht. 
 *
 * Die Controller werden anhand einer von aussen festzulegenden id identifiziert.
 *
 * Jeder Controller kann auhc noch ge'toggelt werden.
 */
class IMultiMapController : public IMapController  
{
public:
  IMultiMapController( CMapView* view ) : IMapController( view ) {};
  virtual ~IMultiMapController() {};

  virtual void Toggle( const long id ) = 0;

  bool IsChecked( const long id )
  {
    ControllerMap::iterator cIt = Find( id );
    return cIt != GetControllerEnd() && (*cIt).second->IsChecked();
  }
  
  bool IsEnabled( const long id )
  {
    ControllerMap::iterator cIt = Find( id );
    return cIt != GetControllerEnd() && (*cIt).second->IsEnabled();
  };
  
  void AddController( const long id, IMapController_ptr controllerAutoPointer )
  {
    m_controllers[id] = controllerAutoPointer; 
  };

protected:
  typedef std::map<long, IMapController_ptr> ControllerMap;

  ControllerMap::const_iterator GetControllerBegin() const { return m_controllers.begin(); };
  ControllerMap::iterator GetControllerBegin() { return m_controllers.begin(); };
  ControllerMap::const_iterator GetControllerEnd() const { return m_controllers.end(); };
  ControllerMap::iterator GetControllerEnd() { return m_controllers.end(); };
  ControllerMap::iterator Find( const long id ) { return m_controllers.find( id ); };

private:
  ControllerMap m_controllers;
};

#endif // !defined(AFX_IMULTIMAPCONTROLLER_H__A4ACEAD6_2A43_11D8_B49F_00104BB3E525__INCLUDED_)
