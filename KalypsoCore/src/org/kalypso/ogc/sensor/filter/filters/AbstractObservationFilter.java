/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.filter.filters;

import java.net.URL;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
import org.kalypso.ogc.sensor.request.IRequest;

/**
 * AbstractObservationFilter
 * 
 * @author schlienger
 */
public abstract class AbstractObservationFilter implements IObservationFilter
{
  protected IObservation m_obs = null;

  protected Object m_conf = null;

  protected URL m_context = null;

  /**
   * @see org.kalypso.ogc.sensor.filter.IObservationFilter#initFilter(java.lang.Object,
   *      org.kalypso.ogc.sensor.IObservation, java.net.URL)
   */
  public void initFilter( final Object conf, final IObservation obs, final URL context ) throws SensorException
  {
    m_conf = conf;
    m_obs = obs;
    m_context = context;
  }

  public boolean equals( Object obj )
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    return m_obs.equals( obj );
  }

  public IAxis[] getAxisList()
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    return m_obs.getAxisList();
  }

  public String getIdentifier()
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    return m_obs.getIdentifier();
  }

  public MetadataList getMetadataList()
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    return m_obs.getMetadataList();
  }

  public String getName()
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    return m_obs.getName();
  }

  public Object getTarget()
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    return m_obs.getTarget();
  }

  public ITuppleModel getValues( IRequest args ) throws SensorException
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    return m_obs.getValues( args );
  }

  public int hashCode()
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    return m_obs.hashCode();
  }

  public boolean isEditable()
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    return m_obs.isEditable();
  }

  public void setValues( ITuppleModel values ) throws SensorException
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    m_obs.setValues( values );
  }

  public String toString()
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    return m_obs.toString();
  }

  public void addListener( IObservationListener listener )
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    m_obs.addListener( listener );
  }

  public void removeListener( IObservationListener listener )
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    m_obs.removeListener( listener );
  }

  public void clearListeners()
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    m_obs.clearListeners();
  }

  public void fireChangedEvent()
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    m_obs.fireChangedEvent();
  }
  
  public String getHref()
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );

    return m_obs.getHref();
  }
}