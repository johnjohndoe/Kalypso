/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.core.profil;

import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author kimwerner
 */
public interface IProfil extends IObservation<TupleResult>
{
  /**
   * @return true
   *         <p>
   *         adds a new Record at the end of this Observation and copies the values of the Components existing in both
   *         records
   */
  public boolean addPoint( final IRecord point );

  /**
   * @param pointProperty
   */
  public void addPointProperty( final IComponent pointProperty );
  
  public void addPointProperty( final IComponent pointProperty,final IComponent initialValues );

  /**
   * remove the current ProfileObject and adds the given ProfileObject
   * 
   * @return the oldObject
   * @param building
   *            must not be null, in this case use removeProfileObject()
   */
  public IProfileObject[] addProfileObjects( final IProfileObject[] profileObjects );

  public void createProfileObjects( IObservation<TupleResult>[] profileObjects );

  /**
   * @return a valid profilPoint, addable to this profile
   * @see #addPoint(IRecord)
   */
  public IRecord createProfilPoint( );

  /**
   * @return the active Point.
   */
  public IRecord getActivePoint( );

  /**
   * @return the active Pointproperty.
   */
  public IComponent getActiveProperty( );

  /**
   * @return something stored in the profile as Strings
   */
  public String getComment( );

  public IRecord[] getMarkedPoints( );

  /**
   * Gets all PointMarker of the given type in this profile.
   */
  public IProfilPointMarker[] getPointMarkerFor( IComponent pointMarker );

  /**
   * Gets all markers for this record.
   */
  public IProfilPointMarker[] getPointMarkerFor( IRecord record );

  /**
   * @return all Marker-Types stored in This profile, NOT all available Marker-Types registered for this
   *         {@link #getType()}
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider
   */
  public IComponent[] getPointMarkerTypes( );
  
  public int indexOfPoint(final IRecord point);
  
  public int indexOfProperty(final IComponent pointProperty);
  
  public int indexOfProperty(final String id);
  
  public IRecord getPoint(final int index);
  
  /**
   * @return include both , startPoint and endPoint
   */
  public IRecord[] getPoints(final int startPoint,final int endPoint);
  

  /**
   * @return all PointProperties used by this profile
   */
  public IComponent[] getPointProperties( );

  /**
   * @return Points of profile
   */
  public IRecord[] getPoints( );

  /**
   * @return the current building(Tuhh) or other kind of ProfileObject, maybe null
   */
  public IProfileObject[] getProfileObjects( );

  /**
   * @param key
   * @return the value from internal HashMap<Object,Object>
   */
  public Object getProperty( Object key );

  public double getStation( );

  /**
   * Returns the type of the profile.
   * <p>
   * The type controls the following behaviour:
   * <ul>
   * <li>Visualisation (which layers are used)</li>
   * <li>Serialization</li>
   * <li>Validation (which rules are applied)</li>
   * </ul>
   */
  public String getType( );

  /**
   * @return true if the profile contains the property
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider to get addable properties
   */
  public boolean hasPointProperty( final IComponent property );

  /**
   * @return the FIRST component with the given Id, if the profile contains the property otherwise null
   * @note the Id maybe NOT unique in the profiles TupleResult
   * @see #hasPointProperty(IComponent)
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider to get addable properties
   */
  public IComponent hasPointProperty( final String propertyId );

  public boolean removePoint( final IRecord point );

  /*
   * obsolete - point markers will be automatically set by their own setValue() implementation (value will be directly
   * added to observation, and so the point marker is registered)
   */
// public Object addPointMarker( IProfilPointMarker marker );
  public Object removePointMarker( IProfilPointMarker devider );

  /**
   * @param pointProperty
   *            to remove
   * @return false if the pointProperty is not used in this profile
   */
  public boolean removePointProperty( final IComponent pointProperty );

  public boolean removeProfileObject( IProfileObject profileObject );

  /**
   * @param key
   *            removes the key and its value from the profiles internal HashMap<Object,Object>
   */
  public Object removeProperty( final Object key );

  public void setActivePoint( final IRecord point );

  public void setActivePointProperty( final IComponent activeProperty );

  public void setComment( final String comment );

  /**
   * @param key
   * @param value
   *            saves any (key,value-Object) in the profiles internal HashMap
   */
  public void setProperty( final Object key, final Object value );

  public void setStation( final double station );
}