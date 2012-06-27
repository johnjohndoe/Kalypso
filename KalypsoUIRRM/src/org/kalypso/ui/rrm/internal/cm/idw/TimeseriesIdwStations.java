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
package org.kalypso.ui.rrm.internal.cm.idw;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.rcm.binding.IThiessenStation;
import org.kalypso.model.rcm.binding.IThiessenStationCollection;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.cm.LinearSumHelper;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Point;

/**
 * Helper class that represents all chosen timeseries with their corresponding idw stations.
 * 
 * @author Gernot Belger
 * @author Holger Albert
 */
public class TimeseriesIdwStations
{
  private final Collection<Point> m_idwStations = new ArrayList<>();

  private final Collection<String> m_timeseries = new ArrayList<>();

  public void loadData( ) throws CoreException
  {
    /* Load all stations. */
    final IThiessenStationCollection collection = LinearSumHelper.loadStationsGml();

    /* Fetch all active timeseries with their stations. */
    final IFeatureBindingCollection<IThiessenStation> stations = collection.getStations();
    for( final IThiessenStation station : stations )
    {
      try
      {
        if( station.isActive() )
        {
          final GM_Point gmPoint = station.getStationLocation();
          final Point point = (Point) JTSAdapter.export( gmPoint );
          final IXLinkedFeature timeseriesLink = station.getStation();

          if( timeseriesLink != null )
            addTimeseries( timeseriesLink.getFeature(), point );
        }
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
        final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "TimeseriesIdwStations_0" ), e ); //$NON-NLS-1$
        throw new CoreException( status );
      }
    }
  }

  private void addTimeseries( final Feature feature, final Point point )
  {
    if( !(feature instanceof ITimeseries) )
      return;

    final ITimeseries timeseries = (ITimeseries) feature;
    final ZmlLink dataLink = timeseries.getDataLink();
    if( dataLink == null )
      return;

    final String href = dataLink.getHref();
    if( StringUtils.isBlank( href ) )
      return;

    m_idwStations.add( point );
    m_timeseries.add( href );
  }

  public Point[] getIdwStations( )
  {
    return m_idwStations.toArray( new Point[m_idwStations.size()] );
  }

  public String[] getTimeseries( )
  {
    return m_timeseries.toArray( new String[m_timeseries.size()] );
  }
}
