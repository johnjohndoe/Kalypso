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
package org.kalypso.ui.rrm.internal.timeseries.view.imports;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.core.runtime.IStatus;
import org.joda.time.Period;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.visitor.IObservationValueContainer;
import org.kalypso.ogc.sensor.visitor.IObservationVisitor;

/**
 * @author Dirk Kuch
 */
public class ValidateTimestepsVisitor implements IObservationVisitor
{
  private final IStatusCollector m_status = new StatusCollector( KalypsoCorePlugin.getID() );

  private final Period m_timestep;

  private Date m_lastDate;

  private final long m_duration;

  public ValidateTimestepsVisitor( final Period timestep )
  {
    m_timestep = timestep;
    m_duration = m_timestep.toStandardSeconds().getSeconds();
  }

  @Override
  public void visit( final IObservationValueContainer container ) throws SensorException
  {
    final IAxis dateAxis = AxisUtils.findDateAxis( container.getAxes() );
    final Date date = (Date) container.get( dateAxis );

    if( Objects.isNotNull( m_lastDate ) )
    {
      if( date.before( m_lastDate ) )
      {
        final long duration = Math.abs( m_lastDate.getTime() - date.getTime() );
        if( m_duration != duration )
        {
          final SimpleDateFormat sdf = new SimpleDateFormat( "dd.MM.yy HH:mm" );
          m_status.add( IStatus.ERROR, String.format( "Invalid time step detected - between %s and %s", sdf.format( m_lastDate ), sdf.format( date ) ) );
        }
      }
    }

    m_lastDate = date;

  }

  public IStatus getStatus( )
  {
    return m_status.asMultiStatus( "Time Step Validation Status" );
  }

}
