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
package org.kalypso.model.hydrology.timeseries;

import java.util.Date;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.ogc.sensor.DateRange;

/**
 * This operation validates timeseries against a date range.
 * 
 * @author Holger Albert
 */
public class TimeseriesValidatingOperation implements ICoreRunnableWithProgress
{
  private final ITimeseries[] m_timeseries;

  private final DateRange m_dateRange;

  public TimeseriesValidatingOperation( final ITimeseries[] timeseries, final DateRange dateRange )
  {
    m_timeseries = timeseries;
    m_dateRange = dateRange;
  }

  @Override
  public IStatus execute( IProgressMonitor monitor )
  {
    if( monitor == null )
      monitor = new NullProgressMonitor();

    final IStatusCollector collector = new StatusCollector( ModelNA.PLUGIN_ID );

    try
    {
      monitor.beginTask( Messages.getString("TimeseriesValidatingOperation.0"), m_timeseries.length * 100 ); //$NON-NLS-1$

      if( m_timeseries != null && m_dateRange != null )
      {
        final Date from = m_dateRange.getFrom();
        final Date to = m_dateRange.getTo();
        if( from.after( to ) )
        {
          collector.add( new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, Messages.getString("TimeseriesValidatingOperation.1") ) ); //$NON-NLS-1$
          return collector.asMultiStatus( Messages.getString("TimeseriesValidatingOperation.2") ); //$NON-NLS-1$
        }

        for( final ITimeseries timeseries : m_timeseries )
        {
          monitor.subTask( String.format( Messages.getString("TimeseriesValidatingOperation.3"), timeseries.getName() ) ); //$NON-NLS-1$

          final IStatus status = validateTimeseries( timeseries, m_dateRange );
          collector.add( status );
        }
      }

      monitor.worked( 100 );

      return collector.asMultiStatusOrOK( Messages.getString("TimeseriesValidatingOperation.4"), Messages.getString("TimeseriesValidatingOperation.5") ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    catch( final Exception ex )
    {
      collector.add( new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, ex.getLocalizedMessage(), ex ) );
      return collector.asMultiStatus( Messages.getString("TimeseriesValidatingOperation.6") ); //$NON-NLS-1$
    }
    finally
    {
      monitor.done();
    }
  }

  private IStatus validateTimeseries( final ITimeseries timeseries, final DateRange dateRange )
  {
    final String linkLabel = Timeserieses.toLinkLabel( timeseries );
    final DateRange measurementRange = timeseries.getDateRange();
    if( !measurementRange.containsInclusive( dateRange ) )
      return new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, String.format( Messages.getString("TimeseriesValidatingOperation.7"), measurementRange.toString(), linkLabel ) ); //$NON-NLS-1$

    return new Status( IStatus.OK, ModelNA.PLUGIN_ID, Messages.getString("TimeseriesValidatingOperation.8") ); //$NON-NLS-1$
  }
}