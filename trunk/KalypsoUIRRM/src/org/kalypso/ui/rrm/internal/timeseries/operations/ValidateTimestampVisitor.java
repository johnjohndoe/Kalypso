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
package org.kalypso.ui.rrm.internal.timeseries.operations;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.TimeZone;

import org.eclipse.core.runtime.IStatus;
import org.joda.time.LocalTime;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.visitor.IObservationValueContainer;
import org.kalypso.ogc.sensor.visitor.IObservationVisitor;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Dirk Kuch
 */
public class ValidateTimestampVisitor implements IObservationVisitor
{
  private final StatusCollector m_stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );

  private final Set<Integer> m_invalid = new LinkedHashSet<>();

  private final LocalTime m_timestamp;

  private final int m_hour;

  public ValidateTimestampVisitor( final LocalTime timestamp )
  {
    m_timestamp = timestamp;
    m_hour = timestamp.getHourOfDay();
  }

  @Override
  public void visit( final IObservationValueContainer container ) throws SensorException
  {
    final IAxis dateAxis = AxisUtils.findDateAxis( container.getAxes() );
    final Date date = (Date) container.get( dateAxis );

    final LocalTime local = LocalTime.fromMillisOfDay( date.getTime(), m_timestamp.getChronology() );
    final int hour = local.getHourOfDay();

    if( m_hour != hour )
    {
      final SimpleDateFormat sdf = new SimpleDateFormat( Messages.getString( Messages.getString("ValidateTimestampVisitor.0") ) ); //$NON-NLS-1$
      final TimeZone timezone = MetadataHelper.getTimeZone( container.getMetaData(), KalypsoCorePlugin.getDefault().getTimeZone().getID() );
      sdf.setTimeZone( timezone );

      final String msg = String.format( Messages.getString("ValidateTimestampVisitor.1"), sdf.format( date ) ); //$NON-NLS-1$
      m_stati.add( IStatus.WARNING, msg );

      m_invalid.add( container.getIndex() );
    }
  }

  public boolean hasInvalidTimestamps( )
  {
    return !m_invalid.isEmpty();
  }

  public Integer[] getInvalidIndices( )
  {
    return m_invalid.toArray( new Integer[] {} );
  }

  public IStatus getStatus( )
  {
    return m_stati.asMultiStatus( Messages.getString("ValidateTimestampVisitor.2") ); //$NON-NLS-1$
  }
}
