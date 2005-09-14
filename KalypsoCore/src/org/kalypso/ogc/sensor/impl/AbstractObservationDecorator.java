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
package org.kalypso.ogc.sensor.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.request.IRequest;

/**
 * AbstractObservationDecorator decorates an IObservation. Decorates all the methods of IObservation and delegates the
 * calls to the underlying observation.
 * <p>
 * This class is used in filter and proxy as a base class due to its common functionality.
 * 
 * @author schlienger
 */
public class AbstractObservationDecorator implements IObservation
{
  protected final IObservation m_obs;

  /**
   * Constructor with base observation
   * 
   * @param obs
   */
  public AbstractObservationDecorator( final IObservation obs )
  {
    m_obs = obs;
  }

  public void addListener( IObservationListener listener )
  {
    m_obs.addListener( listener );
  }

  public boolean equals( Object obj )
  {
    return m_obs.equals( obj );
  }

  public IAxis[] getAxisList()
  {
    return m_obs.getAxisList();
  }

  public String getIdentifier()
  {
    return m_obs.getIdentifier();
  }

  public MetadataList getMetadataList()
  {
    return m_obs.getMetadataList();
  }

  public String getName()
  {
    return m_obs.getName();
  }

  public Object getTarget()
  {
    return m_obs.getTarget();
  }

  public ITuppleModel getValues( IRequest args ) throws SensorException
  {
    return m_obs.getValues( args );
  }

  public int hashCode()
  {
    return m_obs.hashCode();
  }

  public boolean isEditable()
  {
    return m_obs.isEditable();
  }

  public void removeListener( IObservationListener listener )
  {
    m_obs.removeListener( listener );
  }
  
  public void fireChangedEvent()
  {
    m_obs.fireChangedEvent();
  }

  public void setValues( ITuppleModel values ) throws SensorException
  {
    m_obs.setValues( values );
  }

  public String toString()
  {
    return m_obs.toString();
  }

  public String getHref()
  {
    return m_obs.getHref();
  }

  public void clearListeners()
  {
    m_obs.clearListeners();
  }
}