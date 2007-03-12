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
package org.kalypso.model.wspm.core.profil;

import java.util.LinkedList;

/**
 * @author kimwerner
 */
public interface IProfil
{
  /**
   * @return something stored in the profile as Strings
   */
  public String[] getComment( );

  public void addComment( final int lineNumber, final String comment );

  public void addComment( final String comment );

  public void removeComment( final int lineNumber );

  /**
   * @return the friendly name for this profile
   */
  public String getName( );

  public void setName( final String Name );

  /**
   * @return false if the point does not match all the properties in this profile
   */
  public boolean addPoint( final IProfilPoint point );

  /**
   * @return the new pointmarker represented by markerId created on the given point
   */
  public IProfilPointMarker addPointMarker( final IProfilPoint point, final String markerId );

  /**
   * @return all pointmarker in this profile with the same markerId given by marker
   */
  public IProfilPointMarker[] addPointMarker( final IProfilPointMarker marker );

  /**
   * @param pointProperty
   * @return POINT_PROPERTY[] with all current pointproperties
   */
  public String[] addPointProperty( final String pointProperty );

  /**
   * @return a valid profilPoint, addable to this profile
   * @see addPoint(IProfilpoint point)
   */
  public IProfilPoint createProfilPoint( );

  /**
   * @return Returns the active Point.
   */
  public IProfilPoint getActivePoint( );

  /**
   * @return Returns the active Pointproperty.
   */
  public IProfilPointProperty getActiveProperty( );

  /**
   * @return array of PointPropertyIds wich depends somehow on the given propertyId
   */
  public String[] getDependenciesFor( final String property );

  /**
   * @return all profilePoints captured by a PointMarker
   */
  public IProfilPoint[] getMarkedPoints( );

  /**
   * @return the first markerProvider wich provides the given MarkerId
   */
  public IProfilPointMarkerProvider getMarkerProviderFor( final String markerId );

  /**
   * @return the first profileObjectProvider wich provides the given ObjectId
   */
  public IProfileObjectProvider getObjectProviderFor( final String profileObject );

  /**
   * @return all PointMarker with a reference on the given point
   */
  public IProfilPointMarker[] getPointMarkerFor( final IProfilPoint point );

  /**
   * @return all PointMarker equals the given PointMarkerId
   */
  public IProfilPointMarker[] getPointMarkerFor( final String markerType );

  /**
   * @return all PointMarkerIds addable to this profile
   */
  public String[] getPointMarkerTypes( );

  public IProfilPointProperty[] getPointProperties( );

  public IProfilPointProperty getPointProperty( final String pointPrioperty );

  /**
   * @return the nested PointList of this profile, changes will be reflected in the profile. May cause
   *         ConcurrentModificationException
   */
  public LinkedList<IProfilPoint> getPoints( );

  /**
   * @return the current building(Tuhh) or other kind of ProfileObject, maybe null
   */
  public IProfileObject getProfileObject( );

  /**
   * @param key
   * @return the value from HashMap<key,ProfileObject>
   */
  public Object getProperty( Object key );

  /**
   * @return the first pointPropertyProvider wich provides the given propertyId
   */
  public IProfilPointPropertyProvider getPropertyProviderFor( final String property );

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

  public boolean hasPointProperty( final String propertyId );

  /**
   * @param point
   *          to remove
   * @return false if the point is captured by a marker and will NOT remove the point from pointList
   */
  public boolean removePoint( final IProfilPoint point );

  /**
   * @return the removed PointMarker
   */
  public IProfilPointMarker removePointMarker( final IProfilPointMarker pointMarker );

  /**
   * @param pointProperty
   *          to remove
   * @return false if the pointProperty is not used in this profile
   */
  public boolean removePointProperty( final String pointProperty );

  /**
   * @return the extracted ProfileObject
   *         <p>
   *         all pointProperties used by this Object will be removed
   *         <p>
   *         the ProfileObject is set to null
   */
  public IProfileObject removeProfileObject( );

  /**
   * @param key
   *          removes the key and its value of the profiles HashMap
   */
  public Object removeProperty( final Object key );

  public void setActivePoint( final IProfilPoint point );

  public void setActivePointProperty( final String activeProperty );

  /**
   * remove the current ProfileObject and adds the given ProfileObject
   * 
   * @return the oldObject
   * @param building
   *          must not be null, in this case use removeProfileObject()
   */
  public IProfileObject setProfileObject( final IProfileObject building );

  /**
   * @param key
   * @param value
   *          saves the the key,value in its own HashMap
   */
  public void setProperty( final Object key, final Object value );

  public void setStation( final double station );
}