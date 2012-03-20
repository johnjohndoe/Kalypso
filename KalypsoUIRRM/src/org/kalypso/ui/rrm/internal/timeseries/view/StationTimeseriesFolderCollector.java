/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ui.rrm.internal.timeseries.view;

import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.internal.timeseries.binding.StationCollection;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Dirk Kuch
 */
public class StationTimeseriesFolderCollector implements ICoreRunnableWithProgress
{
  Set<String> m_result = new TreeSet<>();

  private final StationCollection m_stations;

  public StationTimeseriesFolderCollector( final StationCollection stations )
  {
    m_stations = stations;
  }

  public StationTimeseriesFolderCollector( final IStation station )
  {
    final Feature owner = station.getOwner();
    if( owner instanceof StationCollection )
      m_stations = (StationCollection) owner;
    else
      m_stations = null;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    if( Objects.isNull( m_stations ) )
      return Status.CANCEL_STATUS;

    final IFeatureBindingCollection<IStation> stations = m_stations.getStations();
    for( final IStation station : stations )
    {
      m_result.add( station.getTimeseriesFoldername() );
    }

    return Status.OK_STATUS;
  }

  public String[] getResult( )
  {
    return m_result.toArray( new String[] {} );
  }

}
