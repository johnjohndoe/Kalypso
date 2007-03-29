/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.core.profil.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider;
import org.kalypso.model.wspm.core.profil.IProfilPointProperty;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectProvider;
import org.kalypso.model.wspm.core.profil.impl.marker.PointMarkerComparator;
import org.kalypso.model.wspm.core.profil.impl.points.ProfilPoint;

/**
 * @author kimwerner Basisprofil mit Events, nur die Implementierung von IProfil
 */
public class PlainProfil implements IProfil
{

  private IProfileObject m_building;

  private final Map<String, List<IProfilPointMarker>> m_pointMarker = new HashMap<String, List<IProfilPointMarker>>();

  private final LinkedList<IProfilPoint> m_points = new LinkedList<IProfilPoint>();

  private final Map<Object, Object> m_profilMetaData;

  private final Map<String, IProfilPointProperty> m_pointProperties = new HashMap<String, IProfilPointProperty>();

  /**
   * Der aktive Punkt des Profils: in der Tabelle derjenige, auf welchem der Table-Cursor sitzt. Im Diagramm der zuletzt
   * angeklickte. Die sichtbaren Trenner werden auch hier verwaltet
   */
  private IProfilPoint m_activePoint;

  private IProfilPointProperty m_activeProperty;

  private double m_station = Double.NaN;

  private final String m_type;

  private String m_name = "";

  private String m_comment;

  public PlainProfil( final String type )
  {
    m_type = type;
    m_profilMetaData = new HashMap<Object, Object>();
    m_building = null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.ProfilPoints#addPoint(double,double)
   */
  public boolean addPoint( final IProfilPoint point )
  {
    if( !pointIsValid( point ) )
      return false;
    m_points.add( point );
    return true;
  }

  public IProfilPointMarker addPointMarker( final IProfilPoint point, final String markerId )
  {
    if( !m_points.contains( point ) )
      return null;
    final IProfilPointMarkerProvider pmp = getMarkerProviderFor( markerId );
    if( pmp == null )
      return null;
    final IProfilPointMarker marker = pmp.createMarker( markerId );
    marker.setPoint( point );
    List<IProfilPointMarker> markers = m_pointMarker.get( markerId );
    if( markers == null )
    {
      markers = new ArrayList<IProfilPointMarker>();
      markers.add( marker );
      m_pointMarker.put( markerId, markers );
    }
    else
      markers.add( marker );
    return marker;
  }

  public IProfilPointMarker[] addPointMarker( final IProfilPointMarker marker )
  {
    if( (marker == null) || (!m_points.contains( marker.getPoint() )) )
      return new IProfilPointMarker[0];
    final String markerId = marker.getMarkerId();
    List<IProfilPointMarker> markers = m_pointMarker.get( markerId );
    if( markers == null )
    {
      markers = new ArrayList<IProfilPointMarker>();
      markers.add( marker );
      m_pointMarker.put( markerId, markers );
    }
    else
      markers.add( marker );
    return markers.toArray( new IProfilPointMarker[0] );
  }

  /**
   * @return ein Array aller von der eigefügten PointProperty abhängigen pointProperties
   * @see org.kalypso.model.wspm.core.profil.IProfil#addPointProperty(org.kalypso.model.wspm.core.profil.POINT_PROPERTY)
   */
  public String[] addPointProperty( final String pointProperty )
  {
    final IProfilPointProperty property = m_pointProperties.get( pointProperty );
    final List<String> depending = new ArrayList<String>();
    depending.add( pointProperty );
    depending.addAll( Arrays.asList( getDependenciesFor( pointProperty ) ) );

    if( property != null )
      return depending.toArray( new String[0] );

    final IProfilPointPropertyProvider ppp = getPropertyProviderFor( pointProperty );
    if( ppp == null )
      return new String[0];

    for( String pd : depending )
    {
      final IProfilPointProperty pp = ppp.getPointProperty( pd );
      m_pointProperties.put( pd, pp );

      for( IProfilPoint point : m_points )
      {
        point.addProperty( pd );
      }
    }
    return depending.toArray( new String[0] );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#createProfilPoint()
   */
  public IProfilPoint createProfilPoint( )
  {
    return new ProfilPoint( m_pointProperties.keySet().toArray( new String[0] ) );
  }

  /**
   * @return Returns the activePoint.
   */
  public IProfilPoint getActivePoint( )
  {
    return m_activePoint;
  }

  public IProfilPointProperty getActiveProperty( )
  {
    return m_activeProperty;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#getDependenciesFor(org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY)
   */
  public String[] getDependenciesFor( final String property )
  {
    final IProfilPointProperty pp = m_pointProperties.get( property );
    return pp == null ? new String[0] : pp.getDependencies();
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#getMarkedPoints()
   */
  public IProfilPoint[] getMarkedPoints( )
  {
    ArrayList<IProfilPoint> pointList = new ArrayList<IProfilPoint>();
    for( final List<IProfilPointMarker> markers : m_pointMarker.values() )
    {
      for( final IProfilPointMarker marker : markers )
      {
        if( !pointList.contains( marker.getPoint() ) )
          pointList.add( marker.getPoint() );
      }
    }
    return pointList.toArray( new IProfilPoint[0] );
  }

  public final IProfilPointMarkerProvider getMarkerProviderFor( final String markerId )
  {
    final IProfilPointMarkerProvider[] markerProviders = KalypsoModelWspmCoreExtensions.getMarkerProviders( m_type );
    for( final IProfilPointMarkerProvider pmp : markerProviders )
    {
      if( pmp.providesPointMarker( markerId ) )
        return pmp;
    }
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#getObjectProviderFor(java.lang.String)
   */
  public IProfileObjectProvider getObjectProviderFor( final String profileObjectId )
  {
    final IProfileObjectProvider[] objectProviders = KalypsoModelWspmCoreExtensions.getObjectProviders( m_type );
    for( final IProfileObjectProvider pop : objectProviders )
    {
      if( pop.providesProfileObject( profileObjectId ) )
        return pop;
    }
    return null;
  }

  public IProfilPointMarker[] getPointMarkerFor( final IProfilPoint point )
  {
    ArrayList<IProfilPointMarker> markerList = new ArrayList<IProfilPointMarker>();
    for( final List<IProfilPointMarker> markers : m_pointMarker.values() )
    {
      for( final IProfilPointMarker marker : markers )
      {
        if( marker.getPoint() == point )
          markerList.add( marker );
      }
    }
    return markerList.toArray( new IProfilPointMarker[0] );
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#getDevider(MARKER_TYP[])
   */
  public IProfilPointMarker[] getPointMarkerFor( final String markerType )
  {
    final List<IProfilPointMarker> markerList = m_pointMarker.get( markerType );
    if( markerList == null )
      return new IProfilPointMarker[0];
    Collections.sort( markerList, new PointMarkerComparator( IWspmConstants.POINT_PROPERTY_BREITE ) );
    return markerList.toArray( new IProfilPointMarker[0] );
  }

  public String[] getPointMarkerTypes( )
  {
    return m_pointMarker.keySet().toArray( new String[0] );
  }

  public IProfilPointProperty[] getPointProperties( )
  {
    final List<IProfilPointProperty> props = new ArrayList<IProfilPointProperty>();
    final IProfilPointPropertyProvider[] ppps = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( m_type );
    for( final IProfilPointPropertyProvider ppp : ppps )
    {
      final String[] pointProps = ppp.getPointProperties();
      for( final String pointProp : pointProps )
      {
        if( m_pointProperties.containsKey( pointProp ) )
        {
          props.add( m_pointProperties.get( pointProp ) );
        }
      }
    }
    return props.toArray( new IProfilPointProperty[0] );
  }

  public IProfilPointProperty getPointProperty( String pointPrioperty )

  {

    return m_pointProperties.get( pointPrioperty );
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#getPoints()
   */
  public LinkedList<IProfilPoint> getPoints( )
  {
    return m_points;
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#getBuilding()
   */
  public IProfileObject getProfileObject( )
  {
    return m_building;
  }

  public Object getProperty( Object key )
  {
    return m_profilMetaData.get( key );
  }

  public final IProfilPointPropertyProvider getPropertyProviderFor( final String property )
  {
    final IProfilPointPropertyProvider[] ppps = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( m_type );
    for( final IProfilPointPropertyProvider ppp : ppps )
    {
      if( ppp.providesPointProperty( property ) )
        return ppp;
    }
    return null;
  }

  public double getStation( )
  {
    return m_station;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#getType()
   */
  public String getType( )
  {
    return m_type;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#hasPointProperty(java.lang.String)
   */
  public boolean hasPointProperty( String propertyId )
  {
    return m_pointProperties.containsKey( propertyId );
  }

  private boolean pointIsValid( final IProfilPoint point )
  {
    final String[] propertyIds = point.getProperties();
    for( final String propertyId : propertyIds )
    {
      if( !hasPointProperty( propertyId ) )
        return false;
    }
    for( final String property : m_pointProperties.keySet() )
    {
      if( !point.hasProperty( property ) )
        return false;
    }
    return true;
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#removePoint(org.kalypso.model.wspm.core.profilinterface.IPoint)
   */
  public boolean removePoint( final IProfilPoint point )
  {
    final IProfilPointMarker[] markers = getPointMarkerFor( point );
    if( markers.length == 0 )
      return m_points.remove( point );
    else
      return false;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#removeDevider(org.kalypso.model.wspm.core.profil.IProfilDevider)
   */
  public IProfilPointMarker removePointMarker( final IProfilPointMarker pointMarker )
  {
    final List<IProfilPointMarker> markerList = m_pointMarker.get( pointMarker.getMarkerId() );
    if( markerList == null )
      return null;
    return markerList.remove( pointMarker ) ? pointMarker : null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#removePointProperty(org.kalypso.model.wspm.core.profil.POINT_PROPERTY)
   */
  public boolean removePointProperty( final String pointProperty )
  {
    final IProfilPointProperty property = m_pointProperties.get( pointProperty );
    return property == null ? false : m_pointProperties.remove( pointProperty ) != null;
  }

  /**
   * @throws IllegalProfileOperationException
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#removeBuilding()
   */
  public IProfileObject removeProfileObject( )
  {
    final IProfileObject oldBuilding = m_building;
    final String[] properties = m_building.getPointProperties();
    for( final String property : properties )
    {
      removePointProperty( property );
    }
    m_building = null;
    return oldBuilding;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#removeProperty(java.lang.Object)
   */
  public Object removeProperty( Object key )
  {
    final Object value = m_profilMetaData.get( key );
    m_profilMetaData.remove( key );
    return value;

  }

  public void setActivePoint( final IProfilPoint point )
  {
    m_activePoint = point;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#setActiveproperty(org.kalypso.model.wspm.core.profil.IProfilPointProperty)
   */
  public void setActivePointProperty( String pointProperty )
  {
    if( hasPointProperty( pointProperty ) )
      m_activeProperty = m_pointProperties.get( pointProperty );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#setBuilding(org.kalypso.model.wspm.core.profil.IProfil.BUILDING_TYP)
   */
  public IProfileObject setProfileObject( final IProfileObject building )
  {
    if( building == null )
      return null;
    final IProfileObject oldObject = (m_building != null) ? removeProfileObject() : null;
    m_building = building;
    final String[] properties = building == null ? new String[0] : m_building.getPointProperties();
    for( final String property : properties )
    {
      addPointProperty( property );
    }
    return oldObject;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#setProperty(java.lang.Object, java.lang.Object)
   */
  public void setProperty( Object key, Object value )
  {
    m_profilMetaData.put( key, value );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#setStation(double)
   */
  public void setStation( final double station )
  {
    m_station = station;
  }

  public String getComment( )
  {
    return m_comment;
  }

  public void setComment( final String comment )
  {
    m_comment = comment;
  }

  public String getName( )
  {
    return m_name;
  }

  public void setName( String name )
  {
    m_name = name;
  }
}