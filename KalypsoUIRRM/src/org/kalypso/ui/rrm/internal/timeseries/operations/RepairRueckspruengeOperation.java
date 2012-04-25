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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.Range;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

import com.google.common.base.Objects;

/**
 * @author Dirk Kuch
 */
public class RepairRueckspruengeOperation implements IRepairObservationWorker
{
  private final IObservation m_observation;

  private final Set<Range<Integer>> m_rueckspruenge;

  private SimpleObservation m_repaired;

  public RepairRueckspruengeOperation( final IObservation observation, final Set<Range<Integer>> rueckspruenge )
  {
    m_observation = observation;
    m_rueckspruenge = rueckspruenge;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    try
    {
      final ITupleModel model = m_observation.getValues( null );
      final IAxis[] axes = model.getAxes();

      final List<Object[]> data = new ArrayList<>();

      for( int index = 0; index < model.size(); index++ )
      {
        if( isRuecksprung( index ) )
          continue;

        data.add( copy( model, index, axes ) );
      }

      final SimpleTupleModel copied = new SimpleTupleModel( axes, data.toArray( new Object[][] {} ) );
      m_repaired = new SimpleObservation( m_observation.getHref(), m_observation.getName(), m_observation.getMetadataList(), copied );

      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString("RepairRueckspruengeOperation_0") ); //$NON-NLS-1$
    }
    catch( final SensorException e )
    {
      e.printStackTrace();

      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString("RepairRueckspruengeOperation_1"), e ); //$NON-NLS-1$
    }

  }

  private Object[] copy( final ITupleModel model, final int index, final IAxis[] axes )
  {
    final Object[] data = new Object[axes.length];

    for( int ptrAxis = 0; ptrAxis < axes.length; ptrAxis++ )
    {
      try
      {
        final IAxis axis = axes[ptrAxis];
        data[ptrAxis] = model.get( index, axis );
      }
      catch( final SensorException e )
      {
        e.printStackTrace();
      }
    }

    return data;
  }

  private boolean isRuecksprung( final int index )
  {
    for( final Range<Integer> ruecksprung : m_rueckspruenge )
    {
      if( ruecksprung.contains( index ) )
        return true;
    }

    return false;
  }

  @Override
  public IObservation getObservation( )
  {
    return Objects.firstNonNull( m_repaired, m_observation );
  }

  @Override
  public String getDialogTitle( )
  {
    return Messages.getString("RepairRueckspruengeOperation_2"); //$NON-NLS-1$
  }

  @Override
  public String getDialogMessage( )
  {
    return Messages.getString("RepairRueckspruengeOperation_3"); //$NON-NLS-1$
  }

}
