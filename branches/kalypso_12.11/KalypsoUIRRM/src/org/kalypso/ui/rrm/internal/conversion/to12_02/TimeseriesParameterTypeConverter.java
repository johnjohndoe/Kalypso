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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.model.hydrology.binding.timeseries.IStationCollection;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.project.RrmProject;
import org.kalypso.module.conversion.AbstractLoggingOperation;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Dirk Kuch
 */
public class TimeseriesParameterTypeConverter extends AbstractLoggingOperation
{
  private final File m_stationsFile;

  private final Map<String, Set<TimeseriesIndexEntry>> m_conversionMap;

  public TimeseriesParameterTypeConverter( final File targetDir, final Map<String, Set<TimeseriesIndexEntry>> conversionMap )
  {
    super( Messages.getString("TimeseriesParameterTypeConverter_0") ); //$NON-NLS-1$
    m_conversionMap = conversionMap;

    final IPath stationsGmlPath = RrmProject.getStationsGmlPath();
    m_stationsFile = new File( targetDir, stationsGmlPath.toOSString() );
  }

  @Override
  protected void doExecute( final IProgressMonitor monitor ) throws Exception
  {
    if( m_conversionMap.isEmpty() )
    {
      getLog().add( IStatus.OK, Messages.getString("TimeseriesParameterTypeConverter_1") ); //$NON-NLS-1$
    }

    monitor.beginTask( Messages.getString("TimeseriesParameterTypeConverter_2"), m_conversionMap.size() ); //$NON-NLS-1$

    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( m_stationsFile, null );
    final IStationCollection stations = (IStationCollection) workspace.getRootFeature();

    final Set<Entry<String, Set<TimeseriesIndexEntry>>> entries = m_conversionMap.entrySet();
    for( final Entry<String, Set<TimeseriesIndexEntry>> entry : entries )
    {
      final String targetType = entry.getKey();
      final Set<TimeseriesIndexEntry> timeserieses = entry.getValue();

      monitor.subTask( String.format( Messages.getString("TimeseriesParameterTypeConverter_3"), targetType ) ); //$NON-NLS-1$

      for( final TimeseriesIndexEntry indexEntry : timeserieses )
      {

        final FindTimeseriesStationVisitor visitor = new FindTimeseriesStationVisitor( indexEntry );
        stations.getStations().accept( visitor );
        final ITimeseries timeseries = visitor.getTimeseries();
        if( timeseries == null )
          continue;

        final String sourceType = timeseries.getParameterType();
        if( StringUtils.equals( targetType, sourceType ) )
          continue;

        final ChangeTimeseriesParameterTypeWorker worker = new ChangeTimeseriesParameterTypeWorker( timeseries, targetType );
        worker.execute( new NullProgressMonitor() );
      }

      monitor.worked( 1 );
    }

    GmlSerializer.serializeWorkspace( m_stationsFile, workspace, "UTF-8" ); //$NON-NLS-1$

    monitor.done();
  }
}
