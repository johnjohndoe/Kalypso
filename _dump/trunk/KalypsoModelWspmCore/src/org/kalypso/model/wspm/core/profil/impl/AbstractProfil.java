/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.impl.marker.PointMarker;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.phenomenon.IPhenomenon;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

/**
 * @author kimwerner Basisprofil mit Events, nur die Implementierung von IProfil
 */
public abstract class AbstractProfil implements IProfil
{
  protected static String PROFILE_OBJECTS = "org.kalypso.model.wspm.core.profilobjects";

  private final String m_type;

  private double m_station;

  private IPhenomenon m_phenomenon;

  private IRecord m_activePoint;

  private IComponent m_activePointProperty;

  private TupleResult m_result;

  private String m_name;

  private String m_description;

  private List<MetadataObject> m_metaDataList = new ArrayList<MetadataObject>();

  private final Map<Object, Object> m_additionalProfileSettings = new HashMap<Object, Object>();

  public AbstractProfil( final String type )
  {
    m_type = type;
  }

  public boolean addPoint( final IRecord point )
  {
    return getResult().add( point );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#addPointProperty(org.kalypso.model.wspm.core.profil.POINT_PROPERTY)
   */
  public void addPointProperty( final IComponent pointProperty )
  {
    if( pointProperty == null )
      throw new IllegalStateException( "property can't be null!" );

    final IComponent[] pointProperties = getPointProperties();
    if( ArrayUtils.contains( pointProperties, pointProperty ) )
      return;

    getResult().addComponent( pointProperty );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#setProfileObject(org.kalypso.model.wspm.core.profil.IProfileObject[])
   */
  @SuppressWarnings("unchecked")
  public IProfileObject[] addProfileObjects( final IProfileObject[] profileObjects )
  {
    if( m_additionalProfileSettings.get( PROFILE_OBJECTS ) == null )
      m_additionalProfileSettings.put( PROFILE_OBJECTS, new ArrayList<IProfileObject>() );
    final List<IProfileObject> profileObjectList = (ArrayList<IProfileObject>) m_additionalProfileSettings.get( PROFILE_OBJECTS );

    for( final IProfileObject profileObject : profileObjects )
    {
      profileObjectList.add( profileObject );
    }

    return profileObjectList.toArray( new IProfileObject[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#createProfileObjects(org.kalypso.observation.IObservation<org.kalypso.observation.result.TupleResult>[])
   *      override this method if you have got the org.kalypso.model.wspm.core.profil.IProfileObjectProvider for your
   *      m_type
   */
  public void createProfileObjects( final IObservation<TupleResult>[] profileObjects )
  {
    throw new NotImplementedException();

  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#createProfilPoint()
   */
  public IRecord createProfilPoint( )
  {
    return m_result.createRecord();
  }

  /**
   * @return Returns the activePoint.
   */
  public IRecord getActivePoint( )
  {
    if( m_result.isEmpty() )
      return null;
    else if( m_activePoint == null )
      return m_result.get( 0 );
    else
      return m_activePoint;
  }

  public IComponent getActiveProperty( )
  {
    return m_activePointProperty;
  }

  public String getComment( )
  {
    return getDescription();
  }

  /**
   * @see org.kalypso.observation.IObservation#getDescription()
   */
  public String getDescription( )
  {
    return m_description;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#getMarkedPoints()
   */
  public IRecord[] getMarkedPoints( )
  {
    final ArrayList<IRecord> records = new ArrayList<IRecord>();

    for( final IRecord record : getResult() )
    {
      if( getPointMarkerFor( record ).length > 0 )
        records.add( record );
    }
    return records.toArray( new IRecord[] {} );
  }

  /**
   * @see org.kalypso.observation.IObservation#getMetadataList()
   */
  public List<MetadataObject> getMetadataList( )
  {
    return m_metaDataList;
  }

  public String getName( )
  {
    return m_name;
  }

  /**
   * @see org.kalypso.observation.IObservation#getPhenomenon()
   */
  public IPhenomenon getPhenomenon( )
  {
    return m_phenomenon;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#getPointMarkerFor(org.kalypso.observation.result.IComponent)
   */
  public IProfilPointMarker[] getPointMarkerFor( final IComponent markerColumn )
  {

    final List<IProfilPointMarker> markers = new ArrayList<IProfilPointMarker>();

    final TupleResult result = getResult();
    for( final IRecord record : result )
    {
      final Object value = record.getValue( markerColumn );
      if( value != null )
      {
        markers.add( new PointMarker( markerColumn, record ) );
      }
    }

    return markers.toArray( new IProfilPointMarker[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#getPointMarkerFor(org.kalypso.observation.result.IRecord)
   */
  public IProfilPointMarker[] getPointMarkerFor( final IRecord record )
  {
    final ArrayList<IProfilPointMarker> pointMarkers = new ArrayList<IProfilPointMarker>();
    final IComponent[] markers = getPointMarkerTypes();
    for( final IComponent marker : markers )
    {
      if( record.getValue( marker ) != null )
        pointMarkers.add( new PointMarker( marker, record ) );
    }
    return pointMarkers.toArray( new PointMarker[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#getPointMarkerTypes()
   */
  public IComponent[] getPointMarkerTypes( )
  {
    final IProfilPointMarkerProvider[] providers = KalypsoModelWspmCoreExtensions.getMarkerProviders( this.getType() );
    if( providers.length == 0 )
      return new IComponent[] {};

    final List<IComponent> marker = new ArrayList<IComponent>();
    final IComponent[] properties = getPointProperties();

    for( final IProfilPointMarkerProvider provider : providers )
    {
      final String[] markerTypes = provider.getMarkerTypes();
      for( final IComponent component : properties )
      {
        if( ArrayUtils.contains( markerTypes, component.getId() ) && !marker.contains( component ) )
          marker.add( component );
      }
    }
    return marker.toArray( new IComponent[] {} );
  }

  public IComponent[] getPointProperties( )
  {
    final TupleResult result = getResult();
    return result.getComponents();
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#getPoints()
   */
  public LinkedList<IRecord> getPoints( )
  {
    final LinkedList<IRecord> points = new LinkedList<IRecord>();
    for( final IRecord record : getResult() )
    {
      points.add( record );
    }
    return points;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#getProfileObject()
   */
  @SuppressWarnings("unchecked")
  public IProfileObject[] getProfileObjects( )
  {
    final List<IProfileObject> profileObjectList = (ArrayList<IProfileObject>) m_additionalProfileSettings.get( PROFILE_OBJECTS );
    if( profileObjectList == null )
      return new IProfileObject[] {};
    return profileObjectList.toArray( new IProfileObject[] {} );
  }

  public Object getProperty( final Object key )
  {
    return m_additionalProfileSettings.get( key );
  }

  /**
   * @see org.kalypso.observation.IObservation#getResult()
   */
  public TupleResult getResult( )
  {
    return m_result;
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
   * @see org.kalypso.model.wspm.core.profil.IProfil#hasPointProperty(org.kalypso.model.wspm.core.profil.IComponent)
   */
  public boolean hasPointProperty( final IComponent property )
  {
    if( property == null )
      return false;

    final IComponent[] components = getResult().getComponents();
    if( ArrayUtils.contains( components, property ) )
      return true;

    return false;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#hasPointProperty(org.kalypso.model.wspm.core.profil.IComponent)
   */
  public IComponent hasPointProperty( final String propertyId )
  {
    for( final IComponent component : getResult().getComponents() )
    {
      if( component.getId().equals( propertyId ) )
        return component;
    }
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#removePoint(org.kalypso.model.wspm.core.profilinterface.IPoint)
   */
  public boolean removePoint( final IRecord point )
  {
    return getResult().remove( point );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#removePointMarker(org.kalypso.model.wspm.core.profil.IProfilPointMarker)
   */
  public Object removePointMarker( final IProfilPointMarker marker )
  {
    final Object oldValue = marker.getValue();
    marker.setValue( null );

    return oldValue;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#removePointProperty(org.kalypso.model.wspm.core.profil.POINT_PROPERTY)
   */
  public boolean removePointProperty( final IComponent pointProperty )
  {
    return getResult().removeComponent( pointProperty );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#removeProfileObject(org.kalypso.model.wspm.core.profil.IProfileObject)
   */
  @SuppressWarnings("unchecked")
  public boolean removeProfileObject( final IProfileObject profileObject )
  {
    final List<IProfileObject> profileObjectList = (ArrayList<IProfileObject>) m_additionalProfileSettings.get( PROFILE_OBJECTS );
    if( profileObjectList == null )
      return true;
    return profileObjectList.remove( profileObject );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#removeProperty(java.lang.Object)
   */
  public Object removeProperty( final Object key )
  {
    final Object old = m_additionalProfileSettings.get( key );
    m_additionalProfileSettings.remove( key );
    return old;
  }

  public void setActivePoint( final IRecord point )
  {
    m_activePoint = point;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#setActiveproperty(org.kalypso.model.wspm.core.profil.IComponent)
   */
  public void setActivePointProperty( final IComponent pointProperty )
  {
    m_activePointProperty = pointProperty;
  }

  public void setComment( final String comment )
  {
    setDescription( comment );
  }

  /**
   * @see org.kalypso.observation.IObservation#setDescription(java.lang.String)
   */
  public void setDescription( final String desc )
  {
    m_description = desc;
  }

  /**
   * @see org.kalypso.observation.IObservation#setMedataList(java.util.List)
   */
  public void setMedataList( final List<MetadataObject> metaDataList )
  {
    m_metaDataList = metaDataList;

  }

  public void setName( final String name )
  {
    m_name = name;
  }

  /**
   * @see org.kalypso.observation.IObservation#setPhenomenon(org.kalypso.observation.phenomenon.IPhenomenon)
   */
  public void setPhenomenon( final IPhenomenon phenomenon )
  {
    m_phenomenon = phenomenon;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#setProperty(java.lang.Object, java.lang.Object)
   */
  public void setProperty( final Object key, final Object value )
  {
    m_additionalProfileSettings.put( key, value );
  }

  /**
   * @see org.kalypso.observation.IObservation#setResult(java.lang.Object)
   */
  public void setResult( final TupleResult result )
  {
    m_result = result;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#setStation(double)
   */
  public void setStation( final double station )
  {
    m_station = station;
  }

}