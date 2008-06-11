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
package org.kalypso.model.wspm.core.profil.util;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Profile Observation Helper<br>
 * Functions around new observation stuff of IProfile (extends IObservation)
 * 
 * @author kimwerner, kuch
 */

public class ProfilObsHelper
{
  private static final QName QNAME_PROFIL_OBS_MEMBERS = new QName( IWspmConstants.NS_WSPMPROF, "member" ); //$NON-NLS-1$

  /**
   * @deprecated {@link IProfil} use {@link IRecord[]} instead.
   */
  @Deprecated
  public static LinkedList<IRecord> toProfilPoints( final TupleResult result )
  {
    final LinkedList<IRecord> myPoints = new LinkedList<IRecord>();

    final Object[] arObjects = result.toArray();
    for( final Object objRecord : arObjects )
    {
      if( objRecord instanceof IRecord )
        myPoints.add( (IRecord) objRecord );
    }

    return myPoints;
  }

  /**
   * @deprecated not used
   */
  @Deprecated
  @SuppressWarnings("unchecked")
  public static IObservation<TupleResult>[] getProfileObjects( final Feature profileFeature )
  {

    final List< ? > objects = (List< ? >) profileFeature.getProperty( QNAME_PROFIL_OBS_MEMBERS );
    if( objects.size() == 0 )
      return new IObservation[] {};

    final List<IObservation<TupleResult>> myResults = new ArrayList<IObservation<TupleResult>>();

    // iterate over all profile objects and create its IProfileObject representation
    for( final Object obj : objects )
    {
      final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( (Feature) obj );
      myResults.add( obs );
    }

    return myResults.toArray( new IObservation[] {} );

  }

  /**
   * @deprecated Use {@link IProfil#hasPointProperty(String)} instead.
   */
  @Deprecated
  public static IComponent getPropertyFromId( final IRecord point, final String id )
  {
    if( id == null )
      return null;

    final TupleResult result = point.getOwner();

    final IComponent[] components = result.getComponents();
    for( final IComponent component : components )
    {
      if( id.equals( component.getId() ) )
        return component;
    }

    return null;
  }

  /**
   * @deprecated Use {@link IProfil#hasPointProperty(String)} instead.
   */
  @Deprecated
  public static IComponent getPropertyFromId( final IProfil profil, final String id )
  {
    if( id == null )
      return null;

    final IComponent[] properties = profil.getPointProperties();

    for( final IComponent property : properties )
    {
      if( id.equals( property.getId() ) )
        return property;
    }

    return null;
  }

  /**
   * @deprecated Use {@link IProfil#getPointProperties()} instead.
   */
  @Deprecated
  public static IComponent[] getPropertyFromId( final IProfil profil, final String[] ids )
  {
    final List<IComponent> list = new ArrayList<IComponent>();
    for( final String id : ids )
    {
      list.add( getPropertyFromId( profil, id ) );
    }

    return list.toArray( new IComponent[] {} );
  }

  /**
   * @deprecated
   * @Use {@link IComponent#getPrecision()} instead.
   */
  @Deprecated
  public static double getPrecision( final IComponent property )
  {
    return property.getPrecision();
  }

  /**
   * @deprecated Use {@link IProfileObject#getObjectProperty(String)} instead.
   */
  @Deprecated
  public static IComponent getPropertyFromId( final IProfileObject building, final String id )
  {
    if( id == null )
      return null;

    final IComponent[] objectProperties = building.getObjectProperties();
    for( final IComponent property : objectProperties )
    {
      if( id.equals( property.getId() ) )
        return property;
    }

    final IComponent[] properties = building.getPointProperties();

    for( final IComponent property : properties )
    {
      if( id.equals( property.getId() ) )
        return property;
    }

    return null;
  }

  /**
   * @deprecated Use {@link IProfil#hasPointProperty(String)} instead.
   */
  @Deprecated
  public static IComponent getPropertyFromId( final IProfilPointPropertyProvider[] provider, final String property )
  {
    for( final IProfilPointPropertyProvider ppp : provider )
    {
      if( ppp.providesPointProperty( property ) )
        return ppp.getPointProperty( property );
    }

    return null;
  }

  /**
   * @deprecated Use {@link IProfil#hasPointProperty(String)} instead.
   */
  @Deprecated
  public static IComponent getComponentById( final IComponent[] components, final String id )
  {
    for( final IComponent component : components )
    {
      if( component.getId().equals( id ) )
        return component;
    }

    return null;
  }
}
