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
package org.kalypso.model.wspm.tuhh.core.profile.buildings;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.Record;
import org.kalypso.observation.result.TupleResult;

/**
 * @author Kim Werner, Dirk Kuch
 */
public abstract class AbstractObservationBuilding implements IProfileObject
{
  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getObjectProperty(java.lang.String)
   */
  public IComponent getObjectProperty( String componentId )
  {
    final IComponent[] components = getObjectProperties();
    if (components.length<1)
      return null;
    for (final IComponent component : components )
    {
      if (component.getId().equals( componentId ))
        return component;
    }
    return null;
  }

  private IProfil m_profil;

  private IObservation<TupleResult> m_observation;

  protected void init( final IProfil profil, final IObservation<TupleResult> observation )
  {
    m_profil = profil;
    m_observation = observation;

    final String[] buildingProfileProperties = getProfileProperties();
    final IComponent[] pointProperties = profil.getPointProperties();

    final List<String> profileProperties = new ArrayList<String>();
    for( final IComponent component : pointProperties )
    {
      profileProperties.add( component.getId() );
    }

    for( final String id : buildingProfileProperties )
    {
      if( !profileProperties.contains( id ) )
      {
        profil.addPointProperty( getPointProperty( id ) );
      }
    }
  }

  protected abstract String[] getProfileProperties( );

  protected abstract IComponent getPointProperty( String id );

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getValue(org.kalypso.observation.result.IComponent)
   */
  public Object getValue( final IComponent component )
  {
    final TupleResult result = m_observation.getResult();
    if( result.size() > 1 )
      throw new IllegalStateException( "wspm building always consists of one IRecord-Set row" );
    else if( result.size() == 0 )
      result.add( result.createRecord() );

    return result.get( 0 ).getValue( component );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#setValue(org.kalypso.observation.result.IComponent,
   *      java.lang.Object)
   */
  public void setValue( final IComponent component, final Object value )
  {

    final String[] profileProperties = getProfileProperties();
    for( final String string : profileProperties )
    {

    }

    try
    {
      final TupleResult result = m_observation.getResult();

      IRecord record = null;

      if( result.size() > 1 )
        throw new IllegalStateException( "wspm building always consists of one IRecord-Set row" );
      else if( result.size() == 0 )
      {
        final Set<IComponent> set = new HashSet<IComponent>();
        for( final IComponent c : result.getComponents() )
        {
          set.add( c );
        }

        record = new Record( result, set );
      }

      else
        record = result.get( 0 );

      record.setValue( component, value );
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getObservation()
   */
  public IObservation<TupleResult> getObservation( )
  {
    return m_observation;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getObjectProperties()
   */
  public IComponent[] getObjectProperties( )
  {
    return getObservation().getResult().getComponents();
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getPointProperties()
   */
  public IComponent[] getPointProperties( )
  {
    final IComponent[] profileComponents = m_profil.getResult().getComponents();
    final String[] buildingProfileProperties = getProfileProperties();

    final List<IComponent> myProperties = new ArrayList<IComponent>();

    for( final IComponent comp : profileComponents )
    {
      if( ArrayUtils.contains( buildingProfileProperties, comp.getId() ) )
        myProperties.add( comp );
    }

    return myProperties.toArray( new IComponent[] {} );

  }
}
