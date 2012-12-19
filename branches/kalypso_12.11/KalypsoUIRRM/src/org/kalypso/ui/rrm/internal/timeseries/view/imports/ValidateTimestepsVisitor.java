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
import java.util.TimeZone;

import org.eclipse.core.runtime.IStatus;
import org.joda.time.Duration;
import org.joda.time.Period;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.visitor.IObservationValueContainer;
import org.kalypso.ogc.sensor.visitor.IObservationVisitor;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Dirk Kuch
 */
public class ValidateTimestepsVisitor implements IObservationVisitor
{
  private final IStatusCollector m_status = new StatusCollector( KalypsoCorePlugin.getID() );

  private Date m_lastDate;

  private final Duration m_duration;

  public ValidateTimestepsVisitor( final Period timestep )
  {
    m_duration = new Duration( timestep.toStandardSeconds().getSeconds() * 1000 );
  }

  @Override
  public void visit( final IObservationValueContainer container ) throws SensorException
  {
    final IAxis dateAxis = AxisUtils.findDateAxis( container.getAxes() );
    final Date date = (Date) container.get( dateAxis );

    if( Objects.isNotNull( m_lastDate ) )
    {
      final Duration duration = new Duration( m_lastDate.getTime(), date.getTime() );
      if( !m_duration.equals( duration ) )
      {
        /** take time zone from source file for displaying (so it's equivalent to source date times!) */
        final SimpleDateFormat sdf = new SimpleDateFormat( Messages.getString( "ValidateTimestepsVisitor_0" ) ); //$NON-NLS-1$
        final TimeZone timeZoneID = MetadataHelper.getTimeZone( container.getMetaData(), KalypsoCorePlugin.getDefault().getTimeZone().getID() );
        sdf.setTimeZone( timeZoneID );

        m_status.add( IStatus.WARNING, String.format( Messages.getString( "ValidateTimestepsVisitor_1" ), sdf.format( m_lastDate ), sdf.format( date ) ) ); //$NON-NLS-1$
      }
    }

    m_lastDate = date;
  }

  public IStatus getStatus( )
  {
    return m_status.asMultiStatus( Messages.getString( "ValidateTimestepsVisitor_2" ) ); //$NON-NLS-1$
  }

}
